env_with <- function(...) {
  env <- new.env()
  list2env(list(...), envir = env)
  env
}

test_that("scalars use the protocol vocabulary", {
  v <- variable("x", 1, time = 7)
  expect_identical(v$access_key, "x")
  expect_identical(v$display_name, "x")
  expect_identical(v$display_value, "1")
  expect_identical(v$display_type, "numeric")
  expect_identical(v$type_info, "numeric")
  expect_identical(v$kind, "number")
  expect_false(v$has_children)
  expect_false(v$has_viewer)
  expect_false(v$is_truncated)
  expect_identical(v$updated_time, 7)
  expect_gt(v$size, 0)

  expect_identical(variable("b", TRUE)$kind, "boolean")
  expect_identical(variable("r", as.raw(1))$kind, "bytes")
  expect_identical(variable("s", "a")$display_value, '"a"')
  expect_identical(variable("s", "a")$kind, "string")
  null <- variable("n", NULL)
  expect_identical(
    c(null$display_value, null$kind, null$display_type),
    c("NULL", "empty", "NULL")
  )
  expect_false(null$has_children)
})

test_that("vectors, matrices and factors", {
  v <- variable("xs", 1:3)
  expect_identical(v$display_value, "c(1, 2, 3)")
  expect_identical(v$display_type, "integer [3]")
  expect_identical(v$kind, "collection")
  expect_identical(v$length, 3L)
  expect_true(v$has_children)
  expect_true(grepl(
    ELLIPSIS,
    variable("long", 1:20)$display_value,
    fixed = TRUE
  ))

  m <- variable("m", matrix(1:6, 2))
  expect_identical(m$display_type, "matrix [2, 3]")
  expect_identical(m$kind, "collection")
  expect_false(m$has_children)
  expect_identical(m$length, 2L)

  f <- variable("f", factor(c("a", "b")))
  expect_identical(f$display_value, "factor with 2 levels")
  expect_identical(f$display_type, "factor [2]")
  expect_false(f$has_children)
})

test_that("long strings are truncated and flagged", {
  v <- variable("s", strrep("x", 500))
  expect_true(v$is_truncated)
  expect_identical(nchar(v$display_value), MAX_DISPLAY)
})

test_that("tables, lists, environments and functions", {
  df <- variable("df", data.frame(a = 1:3, b = c("x", "y", "z")))
  expect_identical(df$display_value, "[3 rows x 2 columns] data.frame")
  expect_identical(df$display_type, "data.frame [3, 2]")
  expect_identical(df$kind, "table")
  expect_identical(df$length, 3L)
  expect_true(df$has_children)

  expect_identical(variable("l", list(a = 1, b = 2))$kind, "map")
  expect_identical(variable("l", list(a = 1, b = 2))$display_value, "list of 2")
  expect_identical(variable("l", list(1, 2))$kind, "collection")

  env <- variable("e", env_with(a = 1))
  expect_identical(env$kind, "map")
  expect_true(env$has_children)

  f <- variable("f", function(x, y) x)
  expect_identical(f$kind, "function")
  expect_identical(f$display_type, "function")
  expect_match(f$display_value, "^function \\(x, y\\)")
})

test_that("S4 objects expose their slots", {
  methods::setClass("RiPerson", representation(name = "character"))
  on.exit(methods::removeClass("RiPerson"))
  person <- methods::new("RiPerson", name = "Al")
  v <- variable("p", person)
  expect_identical(v$kind, "other")
  expect_true(v$has_children)
  kids <- children(person)
  expect_identical(kids[[1]]$key, "name")
  expect_identical(kids[[1]]$value, "Al")
})

test_that("active bindings are described without being read", {
  env <- new.env()
  makeActiveBinding("act", function() stop("must not be called"), env)
  pairs <- global_pairs(env)
  described <- describe_all(pairs)
  expect_identical(described[[1]]$display_value, "<active binding>")
  expect_identical(snapshot(env)$act, "active")
})

test_that("only visible variables are listed, sorted", {
  env <- env_with(b = 1, a = 2, .hidden = 3)
  expect_identical(visible_names(env), c("a", "b"))
})

test_that("children and paths resolve structurally", {
  env <- env_with(
    l = list(a = list(b = 5), 6),
    v = c(10, 20),
    df = data.frame(x = 1:2, y = c("p", "q"))
  )
  expect_identical(resolve(c("l", "a", "b"), env), 5)
  expect_identical(resolve(c("l", "[2]"), env), 6)
  expect_identical(resolve(c("v", "[2]"), env), 20)
  expect_identical(resolve(c("df", "y"), env), c("p", "q"))
  expect_identical(
    vapply(children(env$l), function(kv) kv$key, ""),
    c("a", "[2]")
  )
  expect_identical(
    vapply(children(env$df), function(kv) kv$key, ""),
    c("x", "y")
  )
  expect_identical(
    vapply(children(env$v), function(kv) kv$key, ""),
    c("[1]", "[2]")
  )

  missing <- function(path) tryCatch(resolve(path, env), error = identity)
  for (path in list(
    "zzz",
    c("l", "nope"),
    c("v", "[3]"),
    c("l", "[0]"),
    character()
  )) {
    e <- missing(path)
    expect_s3_class(e, "replconnect_rpc_error")
    expect_identical(e$code, INVALID_PARAMS)
  }
})

test_that("fingerprints tell reassignment from the same object", {
  a <- numeric(100)
  b <- a + 0
  expect_identical(rs_address(a), rs_address(a))
  expect_false(identical(rs_address(a), rs_address(b)))
  expect_identical(fingerprint(a), fingerprint(a))
  expect_false(identical(fingerprint(a), fingerprint(b)))
  expect_false(identical(fingerprint(1), fingerprint(2)))
  expect_false(identical(fingerprint(1:100), fingerprint(1:101)))
})

test_that("list, changes and inspect follow the protocol", {
  env <- new.env()
  local_mocked_bindings(workspace_env = function() env)
  assigned_names <- function(reply) {
    vapply(reply$result$assigned, function(v) v$access_key, "")
  }
  with_probe_server(function(info) {
    empty <- dispatch("list", list(), 1)
    expect_true(empty$ok)
    expect_match(rs_to_json(empty$result), '"variables":[]', fixed = TRUE)
    v0 <- empty$result$version
    expect_identical(
      dispatch("changes", list(since = v0), 1)$result,
      list(version = v0)
    )

    assign("a", 1, envir = env)
    assign("df", data.frame(x = 1:2), envir = env)
    rs_bump_version()
    changed <- dispatch("changes", list(since = v0), 1)
    expect_identical(assigned_names(changed), c("a", "df"))
    expect_match(rs_to_json(changed$result), '"removed":[]', fixed = TRUE)
    expect_equal(changed$result$version, v0 + 1)
    expect_identical(
      dispatch("changes", list(since = v0 + 1), 1)$result,
      list(version = v0 + 1)
    )

    # A same-shaped replacement of a large object is a reassignment.
    assign("big", numeric(100), envir = env)
    rs_bump_version()
    expect_identical(
      assigned_names(dispatch("changes", list(since = v0 + 1), 1)),
      "big"
    )
    assign("big", numeric(100) + 0, envir = env)
    rs_bump_version()
    expect_identical(
      assigned_names(dispatch("changes", list(since = v0 + 2), 1)),
      "big"
    )

    rm("a", envir = env)
    rs_bump_version()
    removed <- dispatch("changes", list(since = v0 + 3), 1)
    expect_match(rs_to_json(removed$result), '"removed":["a"]', fixed = TRUE)

    expect_true(dispatch("changes", list(since = 9999), 1)$result$refresh)
    expect_identical(dispatch("changes", list(), 1)$code, INVALID_PARAMS)

    columns <- dispatch("inspect", list(path = list("df")), 1)$result
    expect_identical(columns$length, 1L)
    expect_identical(columns$children[[1]]$access_key, "x")
    elements <- dispatch("inspect", list(path = list("df", "x")), 1)$result
    expect_identical(
      vapply(elements$children, function(v) v$access_key, ""),
      c("[1]", "[2]")
    )
    expect_identical(
      dispatch("inspect", list(path = list("zzz")), 1)$code,
      INVALID_PARAMS
    )
    expect_identical(dispatch("inspect", list(), 1)$code, INVALID_PARAMS)
  })
})
