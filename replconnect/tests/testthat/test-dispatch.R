test_that("initialize describes the session", {
  reply <- dispatch("initialize", list(protocol_version = 1L), 1)
  expect_true(reply$ok)
  expect_identical(reply$result$language, "r")
  expect_identical(reply$result$protocol_version, 1L)
  expect_identical(reply$result$pid, Sys.getpid())
  expect_s3_class(reply$result$capabilities, "AsIs")
  expect_true("shutdown" %in% reply$result$capabilities)
})

test_that("unknown methods and failures become JSON-RPC errors", {
  expect_identical(
    dispatch("nope", list(), 1)[c("ok", "code")],
    list(ok = FALSE, code = METHOD_NOT_FOUND)
  )
  expect_identical(dispatch("", list(), 1)$code, METHOD_NOT_FOUND)
  local_mocked_bindings(handlers = function() {
    list(
      boom = function(params, connection) stop("kaput"),
      refuse = function(params, connection) rpc_stop(BUSY, "busy"),
      noisy = function(params, connection) {
        warning("hidden")
        message("hidden")
        "fine"
      }
    )
  })
  expect_identical(
    dispatch("boom", list(), 1),
    list(ok = FALSE, code = INTERNAL_ERROR, message = "kaput")
  )
  expect_identical(
    dispatch("refuse", list(), 1),
    list(ok = FALSE, code = BUSY, message = "busy")
  )
  expect_silent(reply <- dispatch("noisy", list(), 1))
  expect_identical(reply, list(ok = TRUE, result = "fine"))
})
