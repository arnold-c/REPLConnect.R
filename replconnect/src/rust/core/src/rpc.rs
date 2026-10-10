//! JSON-RPC 2.0 messages: parsing request lines and building replies.

use serde_json::{json, Value};

pub const PARSE_ERROR: i64 = -32700;
pub const INVALID_REQUEST: i64 = -32600;
pub const METHOD_NOT_FOUND: i64 = -32601;
pub const INVALID_PARAMS: i64 = -32602;
pub const INTERNAL_ERROR: i64 = -32603;
/// Mutation refused while the REPL is evaluating.
pub const BUSY: i64 = -32001;

#[derive(Debug, Clone, PartialEq)]
pub struct RpcError {
    pub code: i64,
    pub message: String,
}

impl RpcError {
    pub fn new(code: i64, message: impl Into<String>) -> Self {
        RpcError {
            code,
            message: message.into(),
        }
    }
}

/// One decoded line.
#[derive(Debug, PartialEq)]
pub enum Incoming {
    /// A request expecting a reply. `params` defaults to `{}`.
    Request {
        id: Value,
        method: String,
        params: Value,
    },
    /// A message without an id: never answered.
    Notification,
    /// A malformed message, answered with an error (id is `null` if unknown).
    Invalid(Value, RpcError),
}

/// Decode one line of NDJSON.
pub fn parse(line: &str) -> Incoming {
    let message: Value = match serde_json::from_str(line) {
        Ok(value) => value,
        Err(_) => return Incoming::Invalid(Value::Null, RpcError::new(PARSE_ERROR, "Parse error")),
    };
    let Value::Object(map) = message else {
        return Incoming::Invalid(
            Value::Null,
            RpcError::new(INVALID_REQUEST, "Invalid request"),
        );
    };
    let Some(id) = map.get("id").cloned() else {
        return Incoming::Notification;
    };
    let Some(method) = map.get("method").and_then(Value::as_str) else {
        return Incoming::Invalid(id, RpcError::new(INVALID_REQUEST, "Invalid request"));
    };
    let params = match map.get("params") {
        None | Some(Value::Null) => json!({}),
        Some(params) => params.clone(),
    };
    Incoming::Request {
        id,
        method: method.to_string(),
        params,
    }
}

/// A success reply, without the trailing newline.
pub fn result_message(id: &Value, result: Value) -> String {
    json!({"jsonrpc": "2.0", "id": id, "result": result}).to_string()
}

/// An error reply, without the trailing newline.
pub fn error_message(id: &Value, error: &RpcError) -> String {
    json!({"jsonrpc": "2.0", "id": id, "error": {"code": error.code, "message": error.message}})
        .to_string()
}

/// A server notification, without the trailing newline.
pub fn notification(method: &str, params: Value) -> String {
    json!({"jsonrpc": "2.0", "method": method, "params": params}).to_string()
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn parses_requests_with_default_params() {
        assert_eq!(
            parse(r#"{"jsonrpc":"2.0","id":3,"method":"list"}"#),
            Incoming::Request {
                id: json!(3),
                method: "list".into(),
                params: json!({})
            }
        );
        assert_eq!(
            parse(r#"{"id":"a","method":"inspect","params":{"path":["x"]}}"#),
            Incoming::Request {
                id: json!("a"),
                method: "inspect".into(),
                params: json!({"path": ["x"]})
            }
        );
    }

    #[test]
    fn rejects_malformed_messages() {
        assert_eq!(
            parse("{nope"),
            Incoming::Invalid(Value::Null, RpcError::new(PARSE_ERROR, "Parse error"))
        );
        assert_eq!(
            parse("[1, 2]"),
            Incoming::Invalid(
                Value::Null,
                RpcError::new(INVALID_REQUEST, "Invalid request")
            )
        );
        assert_eq!(
            parse(r#"{"id":1}"#),
            Incoming::Invalid(json!(1), RpcError::new(INVALID_REQUEST, "Invalid request"))
        );
        assert_eq!(
            parse(r#"{"id":1,"method":5}"#),
            Incoming::Invalid(json!(1), RpcError::new(INVALID_REQUEST, "Invalid request"))
        );
    }

    #[test]
    fn messages_without_id_are_notifications() {
        assert_eq!(parse(r#"{"method":"initialized"}"#), Incoming::Notification);
    }

    #[test]
    fn builds_replies() {
        let ok: Value = serde_json::from_str(&result_message(&json!(1), json!({"a": 1}))).unwrap();
        assert_eq!(ok, json!({"jsonrpc": "2.0", "id": 1, "result": {"a": 1}}));
        let err: Value =
            serde_json::from_str(&error_message(&Value::Null, &RpcError::new(BUSY, "busy")))
                .unwrap();
        assert_eq!(
            err,
            json!({"jsonrpc": "2.0", "id": null, "error": {"code": -32001, "message": "busy"}})
        );
        let note: Value = serde_json::from_str(&notification("display", json!({"id": 1}))).unwrap();
        assert_eq!(
            note,
            json!({"jsonrpc": "2.0", "method": "display", "params": {"id": 1}})
        );
        assert!(
            !result_message(&json!(1), json!("a\nb")).contains('\n'),
            "one line per message"
        );
    }
}
