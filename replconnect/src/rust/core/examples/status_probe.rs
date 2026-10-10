//! Minimal protocol client: no Neovim, R calls, nc flags or stdin EOF.
//! Usage: status_probe SOCKET [OUTPUT_FILE|-] [SAMPLES] [INTERVAL_MS]
//! Each request uses a fresh connection and keeps it open until the reply.

use serde_json::{json, Value};
use std::fs::File;
use std::io::{self, BufRead, BufReader, Write};
use std::os::unix::net::UnixStream;
use std::time::{Duration, Instant};

fn status(socket: &str, id: usize) -> io::Result<Value> {
    let mut stream = UnixStream::connect(socket)?;
    stream.set_read_timeout(Some(Duration::from_secs(2)))?;
    stream.set_write_timeout(Some(Duration::from_secs(2)))?;
    writeln!(
        stream,
        "{}",
        json!({"jsonrpc": "2.0", "id": id, "method": "status"})
    )?;
    let mut line = String::new();
    if BufReader::new(&stream).read_line(&mut line)? == 0 {
        return Err(io::Error::other("server closed without a reply"));
    }
    let reply: Value = serde_json::from_str(&line)?;
    if reply["jsonrpc"] != "2.0" || reply["id"] != json!(id) {
        return Err(io::Error::other(format!("invalid reply: {reply}")));
    }
    if !reply["result"]["busy"].is_boolean() || !reply["result"]["version"].is_number() {
        return Err(io::Error::other(format!(
            "invalid status or RPC error: {reply}"
        )));
    }
    Ok(reply["result"].clone())
}

fn run() -> io::Result<()> {
    let args: Vec<String> = std::env::args().collect();
    let socket = args.get(1).ok_or_else(|| {
        io::Error::other("usage: status_probe SOCKET [OUTPUT_FILE|-] [SAMPLES] [INTERVAL_MS]")
    })?;
    let mut output: Box<dyn Write> = match args.get(2).map(String::as_str) {
        None | Some("-") => Box::new(io::stdout()),
        Some(path) => Box::new(File::create(path)?),
    };
    let samples: usize = args
        .get(3)
        .map(|s| s.parse())
        .transpose()
        .map_err(io::Error::other)?
        .unwrap_or(1);
    let interval: u64 = args
        .get(4)
        .map(|s| s.parse())
        .transpose()
        .map_err(io::Error::other)?
        .unwrap_or(500);
    if samples == 0 {
        return Err(io::Error::other("SAMPLES must be positive"));
    }
    for sample in 1..=samples {
        let started = Instant::now();
        let result = status(socket, sample)?;
        writeln!(
            output,
            "{}",
            json!({
                "sample": sample, "status": result, "response_ms": started.elapsed().as_millis()
            })
        )?;
        output.flush()?;
        if sample < samples {
            std::thread::sleep(Duration::from_millis(interval));
        }
    }
    Ok(())
}

fn main() {
    if let Err(err) = run() {
        eprintln!("status_probe: {err}");
        std::process::exit(1);
    }
}
