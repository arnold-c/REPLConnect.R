//! extendr glue: the only Rust code that touches R.
//!
//! All R access happens on R's main thread: from the `rs_*` functions R calls,
//! and from [`on_wake`], which R's event loop runs when the server's wake
//! descriptor is readable (registered with `addInputHandler`; arf runs input
//! handlers while it waits at the prompt). Requests are evaluated by calling
//! `replconnect:::dispatch` through `R_tryEval`, so an R error cannot unwind
//! through Rust frames; `dispatch` itself catches errors and muffles output.

use extendr_api::prelude::*;
use extendr_api::Result;
use replconnect_core::discovery::SessionInfo;
use replconnect_core::rpc::{self, RpcError};
use replconnect_core::server::{Call, Config, Server};
use replconnect_core::value::{self, RValue};
use serde_json::Value;
use std::cell::{Cell, RefCell};
use std::os::raw::{c_int, c_void};
use std::sync::Arc;

// R's event loop (R_ext/eventloop.h); not covered by extendr-ffi.
#[repr(C)]
struct InputHandler {
    _private: [u8; 0],
}

type InputHandlerProc = extern "C" fn(*mut c_void);

extern "C" {
    static mut R_InputHandlers: *mut InputHandler;
    fn addInputHandler(
        handlers: *mut InputHandler,
        fd: c_int,
        handler: InputHandlerProc,
        activity: c_int,
    ) -> *mut InputHandler;
    fn removeInputHandler(handlers: *mut *mut InputHandler, it: *mut InputHandler) -> c_int;
}

/// Tag for our handler in R's list (any value other than R's own 1 and 2).
const ACTIVITY: c_int = 77;

/// Diagnostic marker: proves that this native library, not an older .a, loaded.
const NATIVE_BUILD: &str = "r-eventloop-probe-v1";

struct Running {
    server: Arc<Server>,
    handler: *mut InputHandler,
}

thread_local! {
    // R's main thread only.
    static RUNNING: RefCell<Option<Running>> = const { RefCell::new(None) };
    static WAKE_COUNT: Cell<u64> = const { Cell::new(0) };
}

fn current() -> Option<Arc<Server>> {
    RUNNING.with(|r| {
        r.borrow()
            .as_ref()
            .map(|running| Arc::clone(&running.server))
    })
}

fn stop_running() -> bool {
    let Some(running) = RUNNING.with(|r| r.borrow_mut().take()) else {
        return false;
    };
    unsafe {
        removeInputHandler(std::ptr::addr_of_mut!(R_InputHandlers), running.handler);
    }
    running.server.stop();
    true
}

/// A closure is active when R's current closure environment is not global
/// or base. Read before evaluating any diagnostic/dispatch R code. This
/// detects Sys.sleep's closure, but is only a heuristic for evaluation: a
/// top-level browser() or primitive call may still look idle. R_tryEval
/// introduces a top-level evaluation boundary, so do not use it to inspect
/// the original stack. See R's src/main/context.c.
fn r_evaluating() -> bool {
    // SAFETY: called on R's main thread from its event loop; reads only.
    unsafe {
        let current = extendr_ffi::R_GetCurrentEnv();
        current != extendr_ffi::R_GlobalEnv && current != extendr_ffi::R_BaseEnv
    }
}

extern "C" fn on_wake(_data: *mut c_void) {
    // A panic must not cross into R's C frames.
    let result = std::panic::catch_unwind(|| {
        let Some(server) = current() else { return };
        WAKE_COUNT.with(|n| n.set(n.get() + 1));
        let outcome = server.drain(r_evaluating(), dispatch);
        if outcome.shutdown {
            stop_running();
        }
    });
    if result.is_err() {
        if let Some(server) = current() {
            server.log("input handler panicked");
        }
    }
}

/// R value for an R object (shape rules in `replconnect_core::value`).
fn to_rvalue(robj: &Robj) -> RValue {
    let value = match robj.rtype() {
        Rtype::Null => RValue::Null,
        Rtype::Logicals => RValue::Logical(
            robj.as_logical_slice()
                .unwrap_or(&[])
                .iter()
                .map(|b| if b.is_na() { None } else { Some(b.is_true()) })
                .collect(),
        ),
        Rtype::Integers if robj.inherits("factor") => strings(robj),
        Rtype::Integers => RValue::Integer(
            robj.as_integer_slice()
                .unwrap_or(&[])
                .iter()
                .map(|&i| if i.is_na() { None } else { Some(i) })
                .collect(),
        ),
        Rtype::Doubles => RValue::Double(
            robj.as_real_slice()
                .unwrap_or(&[])
                .iter()
                .map(|&x| {
                    if x.is_na() || !x.is_finite() {
                        None
                    } else {
                        Some(x)
                    }
                })
                .collect(),
        ),
        Rtype::Strings => strings(robj),
        Rtype::List => {
            let items = robj
                .as_list()
                .map(|list| list.values().map(|item| to_rvalue(&item)).collect())
                .unwrap_or_default();
            let names = robj.names().map(|names| names.map(String::from).collect());
            RValue::List(items, names)
        }
        other => RValue::Character(vec![Some(format!("<{other:?}>"))]),
    };
    if robj.inherits("AsIs") {
        RValue::AsIs(Box::new(value))
    } else {
        value
    }
}

fn strings(robj: &Robj) -> RValue {
    RValue::Character(
        robj.as_str_iter()
            .map(|iter| {
                iter.map(|s| if s.is_na() { None } else { Some(s.to_string()) })
                    .collect()
            })
            .unwrap_or_default(),
    )
}

/// R object for an R value.
fn from_rvalue(value: &RValue) -> Robj {
    match value {
        RValue::Null => Robj::from(()),
        RValue::Logical(v) => Robj::from(v.clone()),
        RValue::Integer(v) => Robj::from(v.clone()),
        RValue::Double(v) => Robj::from(v.clone()),
        RValue::Character(v) => Robj::from(v.clone()),
        RValue::List(items, names) => {
            let mut list = Robj::from(items.iter().map(from_rvalue).collect::<Vec<Robj>>());
            if let Some(names) = names {
                let _ = list.set_names(names.clone());
            }
            list
        }
        RValue::AsIs(inner) => from_rvalue(inner),
    }
}

fn json_to_robj(value: &Value) -> Robj {
    from_rvalue(&value::from_json(value))
}

fn robj_to_json(robj: &Robj) -> Value {
    value::to_json(&to_rvalue(robj))
}

fn field(robj: &Robj, name: &str) -> Option<Robj> {
    robj.as_list()?
        .iter()
        .find(|(n, _)| *n == name)
        .map(|(_, v)| v)
}

fn internal(message: impl Into<String>) -> RpcError {
    RpcError::new(rpc::INTERNAL_ERROR, message)
}

/// Run `replconnect:::dispatch(method, params, connection)`, which returns
/// `list(ok = TRUE, result = ...)` or `list(ok = FALSE, code = ..., message = ...)`.
fn dispatch(call: &Call) -> std::result::Result<Value, RpcError> {
    let function = eval_string("replconnect:::dispatch")
        .ok()
        .and_then(|f| f.as_function())
        .ok_or_else(|| internal("replconnect:::dispatch is unavailable"))?;
    let args = Pairlist::from_pairs([
        ("", Robj::from(call.method.as_str())),
        ("", json_to_robj(&call.params)),
        ("", Robj::from(call.connection as f64)),
    ]);
    let reply = function
        .call(args)
        .map_err(|err| internal(format!("dispatch failed: {err:?}")))?;
    let ok = field(&reply, "ok")
        .and_then(|ok| ok.as_bool())
        .unwrap_or(false);
    if ok {
        return Ok(field(&reply, "result").map_or(Value::Null, |result| robj_to_json(&result)));
    }
    let code = field(&reply, "code")
        .and_then(|code| code.as_real().or_else(|| code.as_integer().map(f64::from)))
        .map_or(rpc::INTERNAL_ERROR, |code| code as i64);
    let message = field(&reply, "message")
        .and_then(|m| m.as_str().map(String::from))
        .unwrap_or_else(|| "request failed".into());
    Err(RpcError::new(code, message))
}

/// Start serving (idempotent). Returns `list(socket, started_at)`.
/// @noRd
#[extendr]
fn rs_start(cwd: &str, project: &str, r_version: &str) -> Result<Robj> {
    if current().is_none() {
        let session = SessionInfo {
            cwd: cwd.into(),
            project: project.into(),
            r_version: r_version.into(),
        };
        let server =
            Server::start(Config::for_process(session)).map_err(|e| Error::Other(e.to_string()))?;
        let handler =
            unsafe { addInputHandler(R_InputHandlers, server.wake_fd(), on_wake, ACTIVITY) };
        if handler.is_null() {
            server.stop();
            return Err(Error::Other("could not register an R input handler".into()));
        }
        // Startup is logged unconditionally: lack of a transition log does
        // not prove that the input handler or logger did not run.
        server.log(&format!("service started; native_build={NATIVE_BUILD}"));
        WAKE_COUNT.with(|n| n.set(0));
        RUNNING.with(|r| {
            *r.borrow_mut() = Some(Running {
                server: Arc::new(server),
                handler,
            })
        });
    }
    rs_info()
}

/// `list(socket, started_at)` of the running server, or NULL.
/// @noRd
#[extendr]
fn rs_info() -> Result<Robj> {
    let Some(server) = current() else {
        return Ok(Robj::from(()));
    };
    let list = List::from_names_and_values(
        ["socket", "started_at", "dir"],
        [
            Robj::from(server.socket_path().to_string_lossy().to_string()),
            Robj::from(server.started_at() as f64),
            Robj::from(
                server
                    .info_path()
                    .parent()
                    .map(|p| p.to_string_lossy().to_string())
                    .unwrap_or_default(),
            ),
        ],
    )?;
    Ok(list.into())
}

/// Snapshot of native state, without probing R's environment again (this
/// function itself runs inside an R call). Diagnostics are not protocol RPCs.
/// @noRd
#[extendr]
fn rs_diagnostics() -> Result<Robj> {
    let server = current();
    let socket = server
        .as_ref()
        .map(|s| s.socket_path().to_string_lossy().to_string());
    let log = server.as_ref().map(|s| {
        s.info_path()
            .with_extension("log")
            .to_string_lossy()
            .to_string()
    });
    Ok(List::from_names_and_values(
        [
            "native_build",
            "pid",
            "socket",
            "log",
            "wake_count",
            "last_evaluating",
        ],
        [
            Robj::from(NATIVE_BUILD),
            Robj::from(std::process::id() as f64),
            Robj::from(socket),
            Robj::from(log),
            Robj::from(WAKE_COUNT.with(|n| n.get()) as f64),
            Robj::from(server.as_ref().is_some_and(|s| s.evaluating())),
        ],
    )?
    .into())
}

/// Memory address of an R object, to tell a reassigned variable from the
/// same object (R has no other object identity).
/// @noRd
#[extendr]
fn rs_address(x: Robj) -> String {
    // SAFETY: only the pointer value is read; the object is not touched.
    format!("{:p}", unsafe { x.get() })
}

/// Stop serving and remove the socket and discovery files (idempotent).
/// @noRd
#[extendr]
fn rs_stop() -> bool {
    stop_running()
}

/// Mark the session changed; returns the new version (0 when not serving).
/// @noRd
#[extendr]
fn rs_bump_version() -> f64 {
    current().map_or(0.0, |server| server.bump_version() as f64)
}

/// @noRd
#[extendr]
fn rs_version() -> f64 {
    current().map_or(0.0, |server| server.version() as f64)
}

/// Whether the request being handled arrived while R was evaluating (for
/// refusing mutations with BUSY).
/// @noRd
#[extendr]
fn rs_evaluating() -> bool {
    current().is_some_and(|server| server.evaluating())
}

/// Send a notification to every connection, or only subscribed ones.
/// @noRd
#[extendr]
fn rs_notify(method: &str, params: Robj, subscribed_only: bool) {
    if let Some(server) = current() {
        server.notify(method, robj_to_json(&params), subscribed_only);
    }
}

/// @noRd
#[extendr]
fn rs_subscribe(connection: f64) {
    if let Some(server) = current() {
        server.subscribe(connection as u64);
    }
}

/// @noRd
#[extendr]
fn rs_unsubscribe_all() {
    if let Some(server) = current() {
        server.unsubscribe_all();
    }
}

/// Append a line to the server log (`<pid>.log`).
/// @noRd
#[extendr]
fn rs_log(message: &str) {
    if let Some(server) = current() {
        server.log(message);
    }
}

/// JSON text for an R value (for tests of the shape rules from R).
/// @noRd
#[extendr]
fn rs_to_json(x: Robj) -> String {
    robj_to_json(&x).to_string()
}

/// R value for JSON text (for tests).
/// @noRd
#[extendr]
fn rs_from_json(text: &str) -> Result<Robj> {
    let value: Value = serde_json::from_str(text).map_err(|e| Error::Other(e.to_string()))?;
    Ok(json_to_robj(&value))
}

/// Protocol version spoken by the service.
/// @noRd
#[extendr]
fn protocol_version() -> i32 {
    replconnect_core::PROTOCOL_VERSION as i32
}

// Registers the exported functions with R (see `src/entrypoint.c`).
extendr_module! {
    mod replconnect;
    fn protocol_version;
    fn rs_start;
    fn rs_info;
    fn rs_address;
    fn rs_diagnostics;
    fn rs_stop;
    fn rs_bump_version;
    fn rs_version;
    fn rs_evaluating;
    fn rs_notify;
    fn rs_subscribe;
    fn rs_unsubscribe_all;
    fn rs_log;
    fn rs_to_json;
    fn rs_from_json;
}
