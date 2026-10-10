//! R values as plain Rust data, and their JSON form.
//!
//! The extendr glue maps R objects to [`RValue`] mechanically; the shape rules
//! live here so they can be tested without R:
//!
//! * `NULL` is `null`.
//! * Atomic vectors of length 1 are scalars, other lengths arrays.
//!   `I(x)` ([`RValue::AsIs`]) is always an array, as in jsonlite.
//! * `NA` and non-finite doubles are `null`; whole doubles below 2^53 are
//!   integers (R counts and sizes are usually doubles).
//! * Named lists are objects (an empty named list is `{}`), unnamed lists arrays.
//!
//! Decoding is the inverse: objects become named lists, arrays unnamed lists,
//! numbers integers when they fit, otherwise doubles.

use serde_json::{Map, Number, Value};

#[derive(Debug, Clone, PartialEq)]
pub enum RValue {
    Null,
    Logical(Vec<Option<bool>>),
    Integer(Vec<Option<i32>>),
    Double(Vec<Option<f64>>),
    Character(Vec<Option<String>>),
    /// Elements and, for named lists, their names.
    List(Vec<RValue>, Option<Vec<String>>),
    /// `I(x)`: keep an atomic vector an array even at length 1.
    AsIs(Box<RValue>),
}

fn atomic<T>(values: &[T], as_array: bool, item: impl Fn(&T) -> Value) -> Value {
    if values.len() == 1 && !as_array {
        item(&values[0])
    } else {
        Value::Array(values.iter().map(item).collect())
    }
}

/// Largest double below which every whole number is exact.
const EXACT: f64 = 9_007_199_254_740_992.0; // 2^53

fn double(x: &Option<f64>) -> Value {
    match x {
        Some(x) if x.is_finite() && x.fract() == 0.0 && x.abs() < EXACT => Value::from(*x as i64),
        Some(x) => Number::from_f64(*x).map_or(Value::Null, Value::Number),
        None => Value::Null,
    }
}

fn encode(value: &RValue, as_array: bool) -> Value {
    match value {
        RValue::Null => Value::Null,
        RValue::Logical(v) => atomic(v, as_array, |x| x.map_or(Value::Null, Value::Bool)),
        RValue::Integer(v) => atomic(v, as_array, |x| x.map_or(Value::Null, |i| Value::from(i))),
        RValue::Double(v) => atomic(v, as_array, double),
        RValue::Character(v) => atomic(v, as_array, |x| {
            x.as_ref().map_or(Value::Null, |s| Value::String(s.clone()))
        }),
        RValue::List(items, Some(names)) => {
            let mut map = Map::new();
            for (name, item) in names.iter().zip(items) {
                map.insert(name.clone(), to_json(item));
            }
            Value::Object(map)
        }
        RValue::List(items, None) => Value::Array(items.iter().map(to_json).collect()),
        RValue::AsIs(inner) => encode(inner, true),
    }
}

/// JSON for an R value.
pub fn to_json(value: &RValue) -> Value {
    encode(value, false)
}

/// R value for JSON.
pub fn from_json(value: &Value) -> RValue {
    match value {
        Value::Null => RValue::Null,
        Value::Bool(b) => RValue::Logical(vec![Some(*b)]),
        Value::Number(n) => match n.as_i64().and_then(|i| i32::try_from(i).ok()) {
            Some(i) if i != i32::MIN => RValue::Integer(vec![Some(i)]), // i32::MIN is R's NA
            _ => RValue::Double(vec![n.as_f64()]),
        },
        Value::String(s) => RValue::Character(vec![Some(s.clone())]),
        Value::Array(items) => RValue::List(items.iter().map(from_json).collect(), None),
        Value::Object(map) => RValue::List(
            map.values().map(from_json).collect(),
            Some(map.keys().cloned().collect()),
        ),
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use serde_json::json;

    fn chr(s: &str) -> RValue {
        RValue::Character(vec![Some(s.into())])
    }

    #[test]
    fn scalars_and_vectors() {
        assert_eq!(to_json(&RValue::Null), Value::Null);
        assert_eq!(to_json(&RValue::Logical(vec![Some(true)])), json!(true));
        assert_eq!(
            to_json(&RValue::Integer(vec![Some(1), None])),
            json!([1, null])
        );
        assert_eq!(to_json(&RValue::Integer(vec![])), json!([]));
        assert_eq!(to_json(&chr("a")), json!("a"));
        assert_eq!(to_json(&RValue::Character(vec![None])), Value::Null);
    }

    #[test]
    fn doubles_drop_non_finite_values() {
        let v = RValue::Double(vec![Some(1.5), None, Some(f64::NAN), Some(f64::INFINITY)]);
        assert_eq!(to_json(&v), json!([1.5, null, null, null]));
    }

    #[test]
    fn whole_doubles_are_integers() {
        assert_eq!(
            to_json(&RValue::Double(vec![Some(1712345678.0)])).to_string(),
            "1712345678"
        );
        assert_eq!(to_json(&RValue::Double(vec![Some(-3.0)])).to_string(), "-3");
        assert_eq!(to_json(&RValue::Double(vec![Some(1e300)])), json!(1e300)); // stays a double
    }

    #[test]
    fn as_is_keeps_arrays() {
        assert_eq!(to_json(&RValue::AsIs(Box::new(chr("a")))), json!(["a"]));
        // AsIs only affects the top-level vector, not list contents.
        let list = RValue::List(vec![chr("b")], None);
        assert_eq!(to_json(&RValue::AsIs(Box::new(list))), json!(["b"]));
    }

    #[test]
    fn lists_are_objects_or_arrays() {
        let named = RValue::List(
            vec![chr("x"), RValue::Integer(vec![Some(2)])],
            Some(vec!["name".into(), "n".into()]),
        );
        assert_eq!(to_json(&named), json!({"name": "x", "n": 2}));
        assert_eq!(to_json(&RValue::List(vec![], Some(vec![]))), json!({}));
        assert_eq!(to_json(&RValue::List(vec![], None)), json!([]));
        let nested = RValue::List(vec![named.clone(), RValue::Null], None);
        assert_eq!(to_json(&nested), json!([{"name": "x", "n": 2}, null]));
    }

    #[test]
    fn decodes_json() {
        assert_eq!(from_json(&json!(null)), RValue::Null);
        assert_eq!(from_json(&json!(3)), RValue::Integer(vec![Some(3)]));
        assert_eq!(from_json(&json!(1.5)), RValue::Double(vec![Some(1.5)]));
        assert_eq!(
            from_json(&json!(5_000_000_000_i64)),
            RValue::Double(vec![Some(5e9)])
        );
        assert_eq!(
            from_json(&json!(i32::MIN)),
            RValue::Double(vec![Some(i32::MIN as f64)])
        );
        assert_eq!(from_json(&json!(false)), RValue::Logical(vec![Some(false)]));
        assert_eq!(
            from_json(&json!({"path": ["a", 1]})),
            RValue::List(
                vec![RValue::List(
                    vec![chr("a"), RValue::Integer(vec![Some(1)])],
                    None
                )],
                Some(vec!["path".into()])
            )
        );
    }

    #[test]
    fn round_trips_protocol_shapes() {
        for value in [
            json!({"variables": [], "length": 0, "version": 3}),
            json!({"items": [{"label": "x", "kind": 6}], "start": 0, "stop": 1}),
            json!({"version": 1, "busy": false}),
        ] {
            assert_eq!(to_json(&from_json(&value)), value);
        }
    }
}
