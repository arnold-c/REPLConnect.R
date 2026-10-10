//! The Unix-socket server.
//!
//! Threads never touch R. They accept connections, read requests and write
//! replies; requests that need R are queued for R's main thread, which is
//! woken through [`Server::wake_fd`] (registered with R's `addInputHandler`)
//! and handles them in [`Server::drain`] while R is idle at the prompt.
//!
//! Per connection, a reader thread handles one request at a time, so replies
//! keep arrival order; a writer thread sends a bounded outbound queue, so
//! replies and notifications never interleave bytes and a stuck client is
//! disconnected instead of growing memory.
//!
//! `status` is answered on the reader thread: R cannot answer while it
//! evaluates. The reader pings R's main thread and reports `busy` if R does
//! not drain within [`BUSY_AFTER`], or if it drained from inside an
//! evaluation (R runs input handlers during `Sys.sleep`, `browser()` and
//! `readline()`, as Julia answers while evaluating code yields).

use crate::discovery::{self, SessionInfo};
use crate::rpc::{self, Incoming, RpcError};
use serde_json::{json, Value};
use std::collections::{HashMap, VecDeque};
use std::fs::{self, OpenOptions};
use std::io::{self, BufRead, BufReader, Read, Write};
use std::os::fd::{AsRawFd, RawFd};
use std::os::unix::fs::PermissionsExt;
use std::os::unix::net::{UnixListener, UnixStream};
use std::path::{Path, PathBuf};
use std::sync::atomic::{AtomicBool, AtomicI64, AtomicU64, Ordering};
use std::sync::mpsc;
use std::sync::{Arc, Condvar, Mutex};
use std::thread::{self, JoinHandle};
use std::time::{Duration, SystemTime, UNIX_EPOCH};

/// How long R may take to pick up a ping before `status` reports busy.
pub const BUSY_AFTER: Duration = Duration::from_millis(100);
/// Outbound limits per connection; a client exceeding them is disconnected.
pub const MAX_QUEUED_MESSAGES: usize = 64;
pub const MAX_QUEUED_BYTES: usize = 16 * 1024 * 1024;
/// Longest accepted request line.
pub const MAX_LINE: usize = 16 * 1024 * 1024;

/// A request for R's main thread.
#[derive(Debug, Clone, PartialEq)]
pub struct Call {
    pub method: String,
    pub params: Value,
    /// The connection it came from (for `plots_subscribe`).
    pub connection: u64,
}

/// What a drain asks the caller to do afterwards.
#[derive(Debug, Default, PartialEq)]
pub struct DrainOutcome {
    /// Requests handled.
    pub handled: usize,
    /// A client sent `shutdown`: stop the server once this returns.
    pub shutdown: bool,
}

struct Job {
    call: Call,
    id: Value,
    conn: Arc<Conn>,
    done: mpsc::Sender<()>,
}

struct Outbox {
    queue: VecDeque<String>,
    bytes: usize,
}

struct Conn {
    id: u64,
    stream: UnixStream,
    out: Mutex<Outbox>,
    ready: Condvar,
    subscribed: AtomicBool,
    closed: AtomicBool,
    /// The client stopped sending: close once queued replies are written.
    finishing: AtomicBool,
}

impl Conn {
    /// Queue one message line. Never performs socket IO.
    fn send(&self, line: String) -> bool {
        if self.closed.load(Ordering::SeqCst) {
            return false;
        }
        let mut out = self.out.lock().unwrap();
        let size = line.len() + 1;
        if out.queue.len() >= MAX_QUEUED_MESSAGES || out.bytes + size > MAX_QUEUED_BYTES {
            drop(out);
            self.close();
            return false;
        }
        out.bytes += size;
        out.queue.push_back(line);
        self.ready.notify_all();
        true
    }

    fn close(&self) {
        if !self.closed.swap(true, Ordering::SeqCst) {
            let _ = self.stream.shutdown(std::net::Shutdown::Both);
            let mut out = self.out.lock().unwrap();
            out.queue.clear();
            out.bytes = 0;
            self.ready.notify_all();
        }
    }

    /// Close after the queued replies have been written (a client that sends a
    /// request and then shuts down its write side still gets the reply).
    fn finish(&self) {
        self.finishing.store(true, Ordering::SeqCst);
        let _guard = self.out.lock().unwrap();
        self.ready.notify_all();
    }

    fn write_loop(&self, mut stream: UnixStream) {
        loop {
            let line = {
                let mut out = self.out.lock().unwrap();
                while out.queue.is_empty()
                    && !self.closed.load(Ordering::SeqCst)
                    && !self.finishing.load(Ordering::SeqCst)
                {
                    out = self.ready.wait(out).unwrap();
                }
                if self.closed.load(Ordering::SeqCst) {
                    return;
                }
                if out.queue.is_empty() {
                    drop(out);
                    self.close(); // finishing, and everything is written
                    return;
                }
                let line = out.queue.pop_front().unwrap();
                out.bytes -= line.len() + 1;
                line
            };
            if stream
                .write_all(line.as_bytes())
                .and_then(|_| stream.write_all(b"\n"))
                .is_err()
            {
                self.close();
                return;
            }
        }
    }
}

struct Shared {
    stopping: AtomicBool,
    /// Whether R was evaluating at the last drain (not idle at the prompt).
    evaluating: AtomicBool,
    version: AtomicI64,
    started_at: i64,
    jobs: Mutex<VecDeque<Job>>,
    wake_write: Mutex<UnixStream>,
    drains: Mutex<u64>,
    drained: Condvar,
    connections: Mutex<HashMap<u64, Arc<Conn>>>,
    next_conn: AtomicU64,
    log_path: PathBuf,
}

impl Shared {
    fn wake(&self) {
        // Non-blocking: a full pipe already guarantees a wakeup.
        let _ = self.wake_write.lock().unwrap().write(&[1]);
    }

    /// Ask R's main thread to drain; true if it did within `timeout`.
    fn ping(&self, timeout: Duration) -> bool {
        let drains = self.drains.lock().unwrap();
        let before = *drains;
        self.wake();
        let (drains, _) = self
            .drained
            .wait_timeout_while(drains, timeout, |n| {
                *n == before && !self.stopping.load(Ordering::SeqCst)
            })
            .unwrap();
        *drains != before
    }

    fn status(&self) -> Value {
        let busy = !self.ping(BUSY_AFTER) || self.evaluating.load(Ordering::SeqCst);
        json!({"version": self.version.load(Ordering::SeqCst), "busy": busy})
    }

    fn log(&self, message: &str) {
        let line = format!("{} {}\n", unix_time(), message);
        if let Ok(mut file) = OpenOptions::new()
            .create(true)
            .append(true)
            .open(&self.log_path)
        {
            let _ = file.write_all(line.as_bytes());
        }
    }

    fn serve_connection(self: Arc<Self>, conn: Arc<Conn>) {
        let reader = match conn.stream.try_clone() {
            Ok(stream) => stream,
            Err(err) => {
                self.log(&format!("connection failed: {err}"));
                return;
            }
        };
        let mut reader = BufReader::new(reader);
        loop {
            let mut bytes = Vec::new();
            match (&mut reader)
                .take(MAX_LINE as u64 + 1)
                .read_until(b'\n', &mut bytes)
            {
                Ok(0) => break,
                Ok(_) if bytes.len() > MAX_LINE => {
                    self.log("request line too long; disconnecting");
                    break;
                }
                Ok(_) => {}
                Err(_) => break,
            }
            if self.stopping.load(Ordering::SeqCst) {
                break;
            }
            let reply = match String::from_utf8(bytes) {
                Err(_) => Some(rpc::error_message(
                    &Value::Null,
                    &RpcError::new(rpc::PARSE_ERROR, "Parse error"),
                )),
                Ok(line) if line.trim().is_empty() => None,
                Ok(line) => self.handle_line(&conn, &line),
            };
            if let Some(reply) = reply {
                if !conn.send(reply) {
                    break;
                }
            }
            if conn.closed.load(Ordering::SeqCst) {
                break;
            }
        }
        conn.finish();
        self.connections.lock().unwrap().remove(&conn.id);
    }

    /// The reply to send right away, or None (notification, or queued for R:
    /// then R's main thread sends the reply and this returns once it has).
    fn handle_line(&self, conn: &Arc<Conn>, line: &str) -> Option<String> {
        match rpc::parse(line) {
            Incoming::Notification => None,
            Incoming::Invalid(id, err) => Some(rpc::error_message(&id, &err)),
            Incoming::Request { id, method, .. } if method == "status" => {
                Some(rpc::result_message(&id, self.status()))
            }
            Incoming::Request { id, method, params } => {
                let (done, finished) = mpsc::channel();
                let call = Call {
                    method,
                    params,
                    connection: conn.id,
                };
                self.jobs.lock().unwrap().push_back(Job {
                    call,
                    id,
                    conn: Arc::clone(conn),
                    done,
                });
                self.wake();
                // Err: the server stopped and dropped the job.
                let _ = finished.recv();
                None
            }
        }
    }
}

fn unix_time() -> i64 {
    SystemTime::now()
        .duration_since(UNIX_EPOCH)
        .map(|d| d.as_secs() as i64)
        .unwrap_or(0)
}

/// Where and as whom to serve.
#[derive(Debug, Clone)]
pub struct Config {
    pub dir: PathBuf,
    pub pid: u32,
    pub uid: u32,
    pub session: SessionInfo,
}

impl Config {
    /// The default location for this process, honouring `$REPLCONNECT_DIR`.
    pub fn for_process(session: SessionInfo) -> Config {
        let uid = discovery::current_uid();
        let custom = std::env::var("REPLCONNECT_DIR").ok();
        Config {
            dir: discovery::socket_dir(custom.as_deref(), uid),
            pid: std::process::id(),
            uid,
            session,
        }
    }
}

pub struct Server {
    shared: Arc<Shared>,
    socket_path: PathBuf,
    info_path: PathBuf,
    wake_read: UnixStream,
    accept_thread: Mutex<Option<JoinHandle<()>>>,
}

impl Server {
    /// Listen on `<dir>/<pid>.sock` and publish `<dir>/<pid>.json`.
    pub fn start(config: Config) -> io::Result<Server> {
        discovery::ensure_private_dir(&config.dir, config.uid)?;
        let socket_path = config.dir.join(format!("{}.sock", config.pid));
        discovery::check_socket_path(&socket_path).map_err(io::Error::other)?;
        // A stale socket from a crash or an in-place restart (same pid).
        let _ = fs::remove_file(&socket_path);
        let listener = UnixListener::bind(&socket_path)?;
        fs::set_permissions(&socket_path, fs::Permissions::from_mode(0o600))?;

        let started_at = unix_time();
        let info_path = config.dir.join(format!("{}.json", config.pid));
        let record =
            discovery::discovery_record(config.pid, &socket_path, started_at, &config.session);
        discovery::write_private(&info_path, &record.to_string())?;

        let (wake_read, wake_write) = UnixStream::pair()?;
        wake_read.set_nonblocking(true)?;
        wake_write.set_nonblocking(true)?;
        let shared = Arc::new(Shared {
            stopping: AtomicBool::new(false),
            evaluating: AtomicBool::new(false),
            version: AtomicI64::new(0),
            started_at,
            jobs: Mutex::new(VecDeque::new()),
            wake_write: Mutex::new(wake_write),
            drains: Mutex::new(0),
            drained: Condvar::new(),
            connections: Mutex::new(HashMap::new()),
            next_conn: AtomicU64::new(1),
            log_path: config.dir.join(format!("{}.log", config.pid)),
        });
        let accept_shared = Arc::clone(&shared);
        let accept_thread = thread::spawn(move || accept_loop(accept_shared, listener));
        Ok(Server {
            shared,
            socket_path,
            info_path,
            wake_read,
            accept_thread: Mutex::new(Some(accept_thread)),
        })
    }

    pub fn socket_path(&self) -> &Path {
        &self.socket_path
    }

    pub fn info_path(&self) -> &Path {
        &self.info_path
    }

    /// Seconds since the epoch when serving started (as in the discovery record).
    pub fn started_at(&self) -> i64 {
        self.shared.started_at
    }

    /// The descriptor to watch for readability; then call [`Server::drain`].
    pub fn wake_fd(&self) -> RawFd {
        self.wake_read.as_raw_fd()
    }

    /// Whether R was evaluating at the current or last drain.
    pub fn evaluating(&self) -> bool {
        self.shared.evaluating.load(Ordering::SeqCst)
    }

    pub fn version(&self) -> i64 {
        self.shared.version.load(Ordering::SeqCst)
    }

    /// Mark the session changed (after an evaluation, `delete`, `clear`).
    pub fn bump_version(&self) -> i64 {
        self.shared.version.fetch_add(1, Ordering::SeqCst) + 1
    }

    /// Handle every queued request on the calling (R's main) thread.
    /// `evaluating`: R is inside an evaluation (not idle at its prompt); it is
    /// reported as `busy` by `status` and available to handlers through
    /// [`Server::evaluating`]. `shutdown` is answered here; the caller stops
    /// the server afterwards.
    pub fn drain(
        &self,
        evaluating: bool,
        mut handler: impl FnMut(&Call) -> Result<Value, RpcError>,
    ) -> DrainOutcome {
        if self.shared.evaluating.swap(evaluating, Ordering::SeqCst) != evaluating {
            self.log(if evaluating {
                "R is evaluating"
            } else {
                "R is idle at the prompt"
            });
        }
        let mut sink = [0u8; 256];
        while matches!((&self.wake_read).read(&mut sink), Ok(n) if n > 0) {}
        let mut outcome = DrainOutcome::default();
        loop {
            let Some(job) = self.shared.jobs.lock().unwrap().pop_front() else {
                break;
            };
            outcome.handled += 1;
            let reply = if job.call.method == "shutdown" {
                outcome.shutdown = true;
                rpc::result_message(&job.id, json!({}))
            } else {
                match handler(&job.call) {
                    Ok(result) => rpc::result_message(&job.id, result),
                    Err(err) => rpc::error_message(&job.id, &err),
                }
            };
            job.conn.send(reply);
            let _ = job.done.send(());
            if outcome.shutdown {
                break;
            }
        }
        let mut drains = self.shared.drains.lock().unwrap();
        *drains += 1;
        self.shared.drained.notify_all();
        outcome
    }

    /// Mark a connection as wanting `display` notifications.
    pub fn subscribe(&self, connection: u64) {
        if let Some(conn) = self.shared.connections.lock().unwrap().get(&connection) {
            conn.subscribed.store(true, Ordering::SeqCst);
        }
    }

    /// Send a notification to every connection, or only to subscribed ones.
    pub fn notify(&self, method: &str, params: Value, subscribed_only: bool) {
        let line = rpc::notification(method, params);
        let connections: Vec<_> = self
            .shared
            .connections
            .lock()
            .unwrap()
            .values()
            .cloned()
            .collect();
        for conn in connections {
            if !subscribed_only || conn.subscribed.load(Ordering::SeqCst) {
                conn.send(line.clone());
            }
        }
    }

    /// Unsubscribe every connection (plot capture turned off).
    pub fn unsubscribe_all(&self) {
        for conn in self.shared.connections.lock().unwrap().values() {
            conn.subscribed.store(false, Ordering::SeqCst);
        }
    }

    pub fn log(&self, message: &str) {
        self.shared.log(message);
    }

    /// Stop listening, disconnect clients and remove the socket and discovery
    /// files. Queued requests are dropped. Idempotent.
    pub fn stop(&self) {
        if self.shared.stopping.swap(true, Ordering::SeqCst) {
            return;
        }
        // Wake the accept loop so it sees `stopping`.
        let _ = UnixStream::connect(&self.socket_path);
        if let Some(handle) = self.accept_thread.lock().unwrap().take() {
            let _ = handle.join();
        }
        // Give queued replies (e.g. to `shutdown`) a moment to leave.
        let connections: Vec<_> = self
            .shared
            .connections
            .lock()
            .unwrap()
            .values()
            .cloned()
            .collect();
        for conn in &connections {
            for _ in 0..50 {
                if conn.out.lock().unwrap().queue.is_empty() {
                    break;
                }
                thread::sleep(Duration::from_millis(2));
            }
            conn.close();
        }
        self.shared.jobs.lock().unwrap().clear();
        self.shared.drained.notify_all();
        let _ = fs::remove_file(&self.socket_path);
        let _ = fs::remove_file(&self.info_path);
    }
}

impl Drop for Server {
    fn drop(&mut self) {
        self.stop();
    }
}

fn accept_loop(shared: Arc<Shared>, listener: UnixListener) {
    for stream in listener.incoming() {
        if shared.stopping.load(Ordering::SeqCst) {
            break;
        }
        let Ok(stream) = stream else { continue };
        let writer = match stream.try_clone() {
            Ok(writer) => writer,
            Err(err) => {
                shared.log(&format!("accept failed: {err}"));
                continue;
            }
        };
        let conn = Arc::new(Conn {
            id: shared.next_conn.fetch_add(1, Ordering::SeqCst),
            stream,
            out: Mutex::new(Outbox {
                queue: VecDeque::new(),
                bytes: 0,
            }),
            ready: Condvar::new(),
            subscribed: AtomicBool::new(false),
            closed: AtomicBool::new(false),
            finishing: AtomicBool::new(false),
        });
        shared
            .connections
            .lock()
            .unwrap()
            .insert(conn.id, Arc::clone(&conn));
        let writer_conn = Arc::clone(&conn);
        thread::spawn(move || writer_conn.write_loop(writer));
        let reader_shared = Arc::clone(&shared);
        thread::spawn(move || reader_shared.serve_connection(conn));
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::discovery::tests::temp_dir;
    use std::io::BufRead;

    fn config(dir: &Path) -> Config {
        Config {
            dir: dir.to_path_buf(),
            pid: 4242,
            uid: discovery::current_uid(),
            session: SessionInfo {
                cwd: "/work".into(),
                project: "".into(),
                r_version: "4.4.2".into(),
            },
        }
    }

    /// Stands in for R's event loop: drains whenever woken, until stopped.
    struct EventLoop {
        stop: Arc<AtomicBool>,
        handle: Option<JoinHandle<()>>,
    }

    impl EventLoop {
        fn run(server: Arc<Server>, handler: fn(&Call) -> Result<Value, RpcError>) -> EventLoop {
            Self::run_evaluating(server, handler, false)
        }

        /// `evaluating`: drains as R does from inside `Sys.sleep` or `browser()`.
        fn run_evaluating(
            server: Arc<Server>,
            handler: fn(&Call) -> Result<Value, RpcError>,
            evaluating: bool,
        ) -> EventLoop {
            let stop = Arc::new(AtomicBool::new(false));
            let flag = Arc::clone(&stop);
            let handle = thread::spawn(move || {
                while !flag.load(Ordering::SeqCst) {
                    if server.drain(evaluating, handler).shutdown {
                        server.stop();
                    }
                    thread::sleep(Duration::from_millis(5));
                }
            });
            EventLoop {
                stop,
                handle: Some(handle),
            }
        }
    }

    impl Drop for EventLoop {
        fn drop(&mut self) {
            self.stop.store(true, Ordering::SeqCst);
            if let Some(handle) = self.handle.take() {
                let _ = handle.join();
            }
        }
    }

    fn echo(call: &Call) -> Result<Value, RpcError> {
        match call.method.as_str() {
            "echo" => Ok(call.params.clone()),
            "fail" => Err(RpcError::new(rpc::INTERNAL_ERROR, "boom")),
            "slow" => {
                thread::sleep(Duration::from_millis(300));
                Ok(json!("slow"))
            }
            method => Err(RpcError::new(
                rpc::METHOD_NOT_FOUND,
                format!("Method not found: {method}"),
            )),
        }
    }

    struct Client {
        stream: UnixStream,
        reader: BufReader<UnixStream>,
    }

    impl Client {
        fn connect(server: &Server) -> Client {
            let stream = UnixStream::connect(server.socket_path()).unwrap();
            stream
                .set_read_timeout(Some(Duration::from_secs(5)))
                .unwrap();
            let reader = BufReader::new(stream.try_clone().unwrap());
            Client { stream, reader }
        }

        fn send(&mut self, line: &str) {
            self.stream.write_all(line.as_bytes()).unwrap();
            self.stream.write_all(b"\n").unwrap();
        }

        fn read(&mut self) -> Value {
            let mut line = String::new();
            self.reader.read_line(&mut line).unwrap();
            serde_json::from_str(&line).unwrap_or(Value::Null)
        }

        fn ask(&mut self, id: i64, method: &str, params: Value) -> Value {
            self.send(
                &json!({"jsonrpc": "2.0", "id": id, "method": method, "params": params})
                    .to_string(),
            );
            self.read()
        }
    }

    #[test]
    fn publishes_socket_and_discovery_record() {
        let dir = temp_dir();
        let server = Server::start(config(&dir)).unwrap();
        let record: Value =
            serde_json::from_str(&fs::read_to_string(dir.join("4242.json")).unwrap()).unwrap();
        assert_eq!(
            record["socket"],
            json!(dir.join("4242.sock").to_string_lossy())
        );
        assert_eq!(record["language"], json!("r"));
        assert_eq!(record["started_at"], json!(server.started_at()));
        let mode = |p: &Path| fs::metadata(p).unwrap().permissions().mode() & 0o777;
        assert_eq!(mode(&dir.join("4242.sock")), 0o600);
        assert_eq!(mode(&dir.join("4242.json")), 0o600);
        assert_eq!(mode(&dir), 0o700);
        server.stop();
        assert!(!dir.join("4242.sock").exists());
        assert!(!dir.join("4242.json").exists());
        server.stop(); // idempotent
        fs::remove_dir_all(&dir).unwrap();
    }

    #[test]
    fn replaces_a_stale_socket() {
        let dir = temp_dir();
        let first = Server::start(config(&dir)).unwrap();
        // An in-place restart: the old process image is gone without cleanup.
        std::mem::forget(first);
        let second = Server::start(config(&dir)).unwrap();
        let mut client = Client::connect(&second);
        let server = Arc::new(second);
        let _loop = EventLoop::run(Arc::clone(&server), echo);
        assert_eq!(
            client.ask(1, "echo", json!({"a": 1}))["result"],
            json!({"a": 1})
        );
        server.stop();
        fs::remove_dir_all(&dir).unwrap();
    }

    #[test]
    fn answers_requests_on_the_main_thread_in_order() {
        let dir = temp_dir();
        let server = Arc::new(Server::start(config(&dir)).unwrap());
        let _loop = EventLoop::run(Arc::clone(&server), echo);
        let mut client = Client::connect(&server);
        assert_eq!(
            client.ask(1, "echo", json!({"x": [1, 2]})),
            json!({"jsonrpc": "2.0", "id": 1, "result": {"x": [1, 2]}})
        );
        let err = client.ask(2, "fail", json!({}));
        assert_eq!(err["error"], json!({"code": -32603, "message": "boom"}));
        assert_eq!(
            client.ask(3, "nope", json!({}))["error"]["code"],
            json!(-32601)
        );
        // Pipelined requests are answered in arrival order.
        for id in 10..15 {
            client.send(
                &json!({"jsonrpc": "2.0", "id": id, "method": "echo", "params": id}).to_string(),
            );
        }
        for id in 10..15 {
            assert_eq!(client.read()["id"], json!(id));
        }
        server.stop();
        fs::remove_dir_all(&dir).unwrap();
    }

    #[test]
    fn rejects_malformed_input_without_disconnecting() {
        let dir = temp_dir();
        let server = Arc::new(Server::start(config(&dir)).unwrap());
        let _loop = EventLoop::run(Arc::clone(&server), echo);
        let mut client = Client::connect(&server);
        client.send("{not json");
        assert_eq!(client.read()["error"]["code"], json!(-32700));
        client.send("[]");
        assert_eq!(client.read()["error"]["code"], json!(-32600));
        client.send(r#"{"jsonrpc":"2.0","method":"note"}"#); // notification: no reply
        client.send("");
        client.stream.write_all(&[0xff, 0xfe, b'\n']).unwrap();
        assert_eq!(client.read()["error"]["code"], json!(-32700));
        assert_eq!(client.ask(1, "echo", json!(1))["result"], json!(1));
        server.stop();
        fs::remove_dir_all(&dir).unwrap();
    }

    #[test]
    fn status_is_answered_while_r_is_busy() {
        let dir = temp_dir();
        let server = Arc::new(Server::start(config(&dir)).unwrap());
        let mut client = Client::connect(&server);
        // No event loop: R is evaluating and never drains.
        let started = std::time::Instant::now();
        let status = client.ask(1, "status", json!({}));
        assert_eq!(status["result"], json!({"version": 0, "busy": true}));
        assert!(started.elapsed() >= BUSY_AFTER);
        assert!(started.elapsed() < Duration::from_secs(2));
        // Inside an evaluation that runs input handlers (Sys.sleep): requests
        // are answered, but the session is busy.
        {
            let _loop = EventLoop::run_evaluating(Arc::clone(&server), echo, true);
            let started = std::time::Instant::now();
            assert_eq!(
                client.ask(2, "status", json!({}))["result"],
                json!({"version": 0, "busy": true})
            );
            assert!(
                started.elapsed() < BUSY_AFTER,
                "answered by R, not timed out"
            );
            assert!(server.evaluating());
            assert_eq!(
                client.ask(3, "echo", json!("answered"))["result"],
                json!("answered")
            );
        }
        // Idle at the prompt: R drains promptly.
        let _loop = EventLoop::run(Arc::clone(&server), echo);
        server.bump_version();
        assert_eq!(
            client.ask(4, "status", json!({}))["result"],
            json!({"version": 1, "busy": false})
        );
        assert!(!server.evaluating());
        server.stop();
        fs::remove_dir_all(&dir).unwrap();
    }

    #[test]
    fn replies_reach_a_client_that_stopped_sending() {
        // Like `printf '...' | nc -U sock`: send, shut the write side, read.
        let dir = temp_dir();
        let server = Arc::new(Server::start(config(&dir)).unwrap());
        let _loop = EventLoop::run(Arc::clone(&server), echo);
        for method in ["status", "echo"] {
            let mut client = Client::connect(&server);
            client.send(
                &json!({"jsonrpc": "2.0", "id": 1, "method": method, "params": 1}).to_string(),
            );
            client.stream.shutdown(std::net::Shutdown::Write).unwrap();
            assert_eq!(client.read()["id"], json!(1), "{method} reply was dropped");
        }
        server.stop();
        fs::remove_dir_all(&dir).unwrap();
    }

    #[test]
    fn a_disconnect_mid_request_is_harmless() {
        let dir = temp_dir();
        let server = Arc::new(Server::start(config(&dir)).unwrap());
        let _loop = EventLoop::run(Arc::clone(&server), echo);
        {
            let mut client = Client::connect(&server);
            client.send(r#"{"jsonrpc":"2.0","id":1,"method":"slow"}"#);
            thread::sleep(Duration::from_millis(50));
        } // dropped while R handles it
        thread::sleep(Duration::from_millis(400));
        let mut client = Client::connect(&server);
        assert_eq!(
            client.ask(2, "echo", json!("still here"))["result"],
            json!("still here")
        );
        server.stop();
        fs::remove_dir_all(&dir).unwrap();
    }

    #[test]
    fn notifications_reach_subscribers_only() {
        let dir = temp_dir();
        let server = Arc::new(Server::start(config(&dir)).unwrap());
        fn subscribe(call: &Call) -> Result<Value, RpcError> {
            Ok(json!({"connection": call.connection}))
        }
        let _loop = EventLoop::run(Arc::clone(&server), subscribe);
        let mut subscriber = Client::connect(&server);
        let mut other = Client::connect(&server);
        let connection = subscriber.ask(1, "plots_subscribe", json!({}))["result"]["connection"]
            .as_u64()
            .unwrap();
        server.subscribe(connection);
        server.notify("display", json!({"id": 1}), true);
        server.notify("capabilities", json!({"capabilities": []}), false);
        assert_eq!(subscriber.read()["method"], json!("display"));
        assert_eq!(subscriber.read()["method"], json!("capabilities"));
        assert_eq!(other.read()["method"], json!("capabilities"));
        server.unsubscribe_all();
        server.notify("display", json!({"id": 2}), true);
        server.notify("capabilities", json!({"capabilities": ["plots"]}), false);
        assert_eq!(
            subscriber.read()["params"],
            json!({"capabilities": ["plots"]})
        );
        server.stop();
        fs::remove_dir_all(&dir).unwrap();
    }

    #[test]
    fn a_client_that_does_not_read_is_disconnected() {
        let dir = temp_dir();
        let server = Arc::new(Server::start(config(&dir)).unwrap());
        let client = Client::connect(&server);
        thread::sleep(Duration::from_millis(50)); // let the server register it
                                                  // Large notifications fill the socket buffer, then the bounded queue.
        let big = "x".repeat(256 * 1024);
        for _ in 0..(MAX_QUEUED_MESSAGES + 40) {
            server.notify("capabilities", json!({"pad": big}), false);
        }
        thread::sleep(Duration::from_millis(100));
        assert!(
            server.shared.connections.lock().unwrap().is_empty(),
            "slow client was dropped"
        );
        drop(client);
        server.stop();
        fs::remove_dir_all(&dir).unwrap();
    }

    #[test]
    fn shutdown_replies_then_stops() {
        let dir = temp_dir();
        let server = Arc::new(Server::start(config(&dir)).unwrap());
        let _loop = EventLoop::run(Arc::clone(&server), echo);
        let mut client = Client::connect(&server);
        assert_eq!(client.ask(1, "shutdown", json!({}))["result"], json!({}));
        let deadline = std::time::Instant::now() + Duration::from_secs(2);
        while dir.join("4242.sock").exists() && std::time::Instant::now() < deadline {
            thread::sleep(Duration::from_millis(10));
        }
        assert!(!dir.join("4242.sock").exists());
        assert!(!dir.join("4242.json").exists());
        assert!(UnixStream::connect(dir.join("4242.sock")).is_err());
        fs::remove_dir_all(&dir).unwrap();
    }

    #[test]
    fn stop_releases_waiting_readers() {
        let dir = temp_dir();
        let server = Server::start(config(&dir)).unwrap();
        let mut client = Client::connect(&server);
        // Queued for R, which never drains.
        client.send(r#"{"jsonrpc":"2.0","id":1,"method":"echo"}"#);
        thread::sleep(Duration::from_millis(50));
        server.stop();
        let mut line = String::new();
        assert_eq!(
            client.reader.read_line(&mut line).unwrap_or(0),
            0,
            "connection closed"
        );
        fs::remove_dir_all(&dir).unwrap();
    }
}
