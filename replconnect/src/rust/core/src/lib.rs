//! R-independent transport for the replconnect R package.
//!
//! Everything here is plain Rust so it can be tested with `cargo test`
//! without an R installation. The extendr glue crate (`../src/lib.rs`) is the
//! only code that touches R; it calls into this crate from R's main thread.
//!
//! Wire format: newline-delimited JSON-RPC 2.0 over a Unix-domain socket, as
//! described in `docs/protocol.md` at the repository root.

pub mod discovery;
pub mod rpc;
pub mod server;
pub mod value;

/// Protocol version spoken by this service (`initialize`'s `protocol_version`).
pub const PROTOCOL_VERSION: i64 = 1;

/// Language name reported in discovery files and `initialize`.
pub const LANGUAGE: &str = "r";
