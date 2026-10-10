//! Where sessions publish their socket and discovery record.
//!
//! Mirrors the Julia service (`julia/REPLConnect/src/server.jl`):
//! `$REPLCONNECT_DIR` if set, else `/tmp/replconnect-<uid>`, mode 0700 and
//! owned by this user; `<pid>.sock` (0600) and `<pid>.json` (0600).
//! `TMPDIR`/`XDG_RUNTIME_DIR` are ignored on purpose: the REPL and the editor
//! often see different values.

use serde_json::{json, Value};
use std::fs;
use std::io::{self, Write};
use std::os::unix::fs::{DirBuilderExt, MetadataExt, PermissionsExt};
use std::path::{Path, PathBuf};

/// Socket paths are limited to ~104 bytes on macOS (108 on Linux).
pub const MAX_SOCKET_PATH: usize = 100;

pub fn current_uid() -> u32 {
    // SAFETY: getuid has no preconditions and cannot fail.
    unsafe { libc::getuid() }
}

/// The directory for sockets and discovery files.
pub fn socket_dir(custom: Option<&str>, uid: u32) -> PathBuf {
    match custom {
        Some(dir) if !dir.is_empty() => PathBuf::from(dir),
        _ => PathBuf::from(format!("/tmp/replconnect-{uid}")),
    }
}

/// Create `dir` with mode 0700, or check that an existing one is a real
/// directory owned by `uid` (`/tmp` is shared, so another user could have
/// created it first), then restrict it to 0700.
pub fn ensure_private_dir(dir: &Path, uid: u32) -> io::Result<()> {
    if fs::symlink_metadata(dir).is_err() {
        fs::DirBuilder::new()
            .recursive(true)
            .mode(0o700)
            .create(dir)?;
    }
    let meta = fs::symlink_metadata(dir)?;
    if !meta.is_dir() {
        return Err(io::Error::other(format!(
            "{} is not a directory",
            dir.display()
        )));
    }
    if meta.uid() != uid {
        return Err(io::Error::other(format!(
            "{} is owned by another user",
            dir.display()
        )));
    }
    fs::set_permissions(dir, fs::Permissions::from_mode(0o700))
}

/// Fail early with a clear message if the socket path exceeds the OS limit.
pub fn check_socket_path(path: &Path) -> Result<(), String> {
    let len = path.as_os_str().len();
    if len > MAX_SOCKET_PATH {
        return Err(format!(
            "socket path too long ({len} > {MAX_SOCKET_PATH} bytes): {}; set REPLCONNECT_DIR to a shorter directory",
            path.display()
        ));
    }
    Ok(())
}

/// Write `content` to `path` atomically, readable only by the owner.
pub fn write_private(path: &Path, content: &str) -> io::Result<()> {
    let tmp = path.with_extension("tmp");
    {
        let mut file = fs::File::create(&tmp)?;
        fs::set_permissions(&tmp, fs::Permissions::from_mode(0o600))?;
        file.write_all(content.as_bytes())?;
    }
    fs::rename(&tmp, path)
}

/// Facts about the session that the service cannot know by itself.
#[derive(Debug, Clone, Default)]
pub struct SessionInfo {
    pub cwd: String,
    pub project: String,
    pub r_version: String,
}

/// The `<pid>.json` discovery record. `started_at` must equal the value
/// `initialize` reports: clients use it to recognise a restarted session.
pub fn discovery_record(pid: u32, socket: &Path, started_at: i64, info: &SessionInfo) -> Value {
    json!({
        "pid": pid,
        "socket": socket.to_string_lossy(),
        "cwd": info.cwd,
        "project": info.project,
        "language": crate::LANGUAGE,
        "r_version": info.r_version,
        "protocol_version": crate::PROTOCOL_VERSION,
        "started_at": started_at,
    })
}

#[cfg(test)]
pub(crate) mod tests {
    use super::*;
    use std::sync::atomic::{AtomicUsize, Ordering};

    /// A fresh short directory under /tmp (macOS temp dirs are too long for sockets).
    pub(crate) fn temp_dir() -> PathBuf {
        static N: AtomicUsize = AtomicUsize::new(0);
        let dir = PathBuf::from(format!(
            "/tmp/ri-test-{}-{}",
            std::process::id(),
            N.fetch_add(1, Ordering::SeqCst)
        ));
        let _ = fs::remove_dir_all(&dir);
        dir
    }

    #[test]
    fn chooses_the_directory() {
        assert_eq!(socket_dir(None, 501), PathBuf::from("/tmp/replconnect-501"));
        assert_eq!(
            socket_dir(Some(""), 501),
            PathBuf::from("/tmp/replconnect-501")
        );
        assert_eq!(socket_dir(Some("/x/y"), 501), PathBuf::from("/x/y"));
    }

    #[test]
    fn creates_and_checks_private_directories() {
        let dir = temp_dir();
        ensure_private_dir(&dir, current_uid()).unwrap();
        let meta = fs::metadata(&dir).unwrap();
        assert_eq!(meta.permissions().mode() & 0o777, 0o700);
        // An existing, too-open directory is tightened.
        fs::set_permissions(&dir, fs::Permissions::from_mode(0o755)).unwrap();
        ensure_private_dir(&dir, current_uid()).unwrap();
        assert_eq!(
            fs::metadata(&dir).unwrap().permissions().mode() & 0o777,
            0o700
        );
        // Someone else's directory is refused.
        let err = ensure_private_dir(&dir, current_uid() + 1).unwrap_err();
        assert!(err.to_string().contains("owned by another user"));
        // A file or a symlink is refused.
        let file = dir.join("file");
        fs::write(&file, "").unwrap();
        assert!(ensure_private_dir(&file, current_uid()).is_err());
        let link = dir.join("link");
        std::os::unix::fs::symlink(&dir, &link).unwrap();
        assert!(ensure_private_dir(&link, current_uid()).is_err());
        fs::remove_dir_all(&dir).unwrap();
    }

    #[test]
    fn limits_socket_path_length() {
        assert!(check_socket_path(Path::new("/tmp/replconnect-501/1.sock")).is_ok());
        let long = format!("/tmp/{}/1.sock", "x".repeat(100));
        assert!(check_socket_path(Path::new(&long))
            .unwrap_err()
            .contains("REPLCONNECT_DIR"));
    }

    #[test]
    fn writes_private_files_atomically() {
        let dir = temp_dir();
        ensure_private_dir(&dir, current_uid()).unwrap();
        let path = dir.join("1.json");
        write_private(&path, "{}").unwrap();
        write_private(&path, "{\"a\":1}").unwrap();
        assert_eq!(fs::read_to_string(&path).unwrap(), "{\"a\":1}");
        assert_eq!(
            fs::metadata(&path).unwrap().permissions().mode() & 0o777,
            0o600
        );
        assert!(!dir.join("1.tmp").exists());
        fs::remove_dir_all(&dir).unwrap();
    }

    #[test]
    fn describes_the_session() {
        let info = SessionInfo {
            cwd: "/w".into(),
            project: "".into(),
            r_version: "4.4.2".into(),
        };
        let record = discovery_record(7, Path::new("/tmp/d/7.sock"), 100, &info);
        assert_eq!(
            record,
            json!({"pid": 7, "socket": "/tmp/d/7.sock", "cwd": "/w", "project": "",
                   "language": "r", "r_version": "4.4.2", "protocol_version": 1, "started_at": 100})
        );
    }
}
