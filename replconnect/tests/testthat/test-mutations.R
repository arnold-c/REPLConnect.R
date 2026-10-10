test_that("delete removes named variables and reports them", {
  env <- new.env()
  assign("a", 1, envir = env)
  assign(".hidden", 2, envir = env)
  assign("last.warning", 3, envir = env)
  local_mocked_bindings(workspace_env = function() env)

  reply <- dispatch(
    "delete",
    list(names = list("a", ".hidden", "last.warning")),
    1
  )
  expect_true(reply$ok)
  expect_identical(as.character(reply$result), c("a", "last.warning"))
  expect_false(exists("a", envir = env, inherits = FALSE))
  expect_false(exists("last.warning", envir = env, inherits = FALSE))
  expect_true(exists(".hidden", envir = env, inherits = FALSE))
  empty <- dispatch("delete", list(names = list("zz")), 1)
  expect_true(empty$ok)
  expect_identical(empty$result, I(character()))
  expect_identical(rs_to_json(empty$result), "[]")
  expect_identical(dispatch("delete", list(), 1)$code, INVALID_PARAMS)
})

test_that("one deleted name is still a JSON array", {
  env <- new.env(parent = emptyenv())
  env$a <- 1
  local_mocked_bindings(workspace_env = function() env)
  reply <- dispatch("delete", list(names = list("a")), 1)
  expect_true(reply$ok)
  expect_identical(rs_to_json(reply$result), '["a"]')
})

test_that("clear removes visible variables only", {
  env <- new.env()
  assign("a", 1, envir = env)
  assign(".hidden", 2, envir = env)
  local_mocked_bindings(workspace_env = function() env)

  reply <- dispatch("clear", list(), 1)
  expect_true(reply$ok)
  expect_identical(reply$result, NULL)
  expect_match(
    rs_to_json(list(result = reply$result)),
    '"result":null',
    fixed = TRUE
  )
  expect_identical(ls(env, all.names = TRUE), ".hidden")
})

test_that("mutations are refused while evaluating", {
  local_mocked_bindings(rs_evaluating = function() TRUE)
  expect_identical(dispatch("delete", list(names = list("a")), 1)$code, BUSY)
  expect_identical(dispatch("clear", list(), 1)$code, BUSY)
})

test_that("mutations bump the version", {
  env <- new.env()
  assign("a", 1, envir = env)
  local_mocked_bindings(workspace_env = function() env)
  with_probe_server(function(info) {
    before <- rs_version()
    dispatch("delete", list(names = list("a")), 1)
    expect_equal(rs_version(), before + 1)
    assign("b", 1, envir = env)
    dispatch("clear", list(), 1)
    expect_equal(rs_version(), before + 2)
  })
})

test_that("delete removes active bindings", {
  env <- new.env()
  makeActiveBinding("act", function() 42, env)
  expect_true(delete_binding(env, "act"))
  expect_false(exists("act", envir = env, inherits = FALSE))
})
