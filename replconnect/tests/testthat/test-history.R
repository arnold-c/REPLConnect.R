with_history_state <- function(run) {
  old_history <- .state$history
  old_setting <- .state$record_history
  on.exit({
    .state$history <- old_history
    .state$record_history <- old_setting
  })
  .state$history <- NULL
  .state$record_history <- TRUE
  run()
}

test_that("history returns recent completed inputs oldest first", {
  with_history_state(function() {
    record_input(quote(a <- 1))
    record_input(quote(b <- 2))
    record_input(quote(c <- 3))
    reply <- dispatch("history", list(limit = 2L), 1)
    expect_true(reply$ok)
    expect_identical(
      reply$result$entries,
      list(list(input = "b <- 2"), list(input = "c <- 3"))
    )
    expect_identical(length(dispatch("history", list(), 1)$result$entries), 3L)
    expect_identical(
      length(dispatch("history", list(limit = 99999), 1)$result$entries),
      3L
    )
  })
})

test_that("zero, one and multiple entries keep their JSON array shape", {
  with_history_state(function() {
    expect_identical(rs_to_json(handle_history(list(), 1)), '{"entries":[]}')
    record_input(quote(a <- 1))
    expect_identical(
      rs_to_json(handle_history(list(), 1)),
      '{"entries":[{"input":"a <- 1"}]}'
    )
    expect_identical(
      rs_to_json(handle_history(list(limit = 0), 1)),
      '{"entries":[]}'
    )
  })
})

test_that("history limit is validated", {
  for (limit in list(-1, 1.5, NA_real_, Inf, "2", list(1), c(1, 2))) {
    expect_identical(
      dispatch("history", list(limit = limit), 1)$code,
      INVALID_PARAMS
    )
  }
})

test_that("retained entries and each input's text are bounded", {
  with_history_state(function() {
    for (i in seq_len(HISTORY_LIMIT + 5L)) {
      record_input(call("f", i))
    }
    expect_length(.state$history, HISTORY_LIMIT)
    expect_identical(.state$history[[1]], "f(6L)")
    expect_identical(
      .state$history[[HISTORY_LIMIT]],
      paste0("f(", HISTORY_LIMIT + 5L, "L)")
    )
    record_input(call("f", strrep("x", HISTORY_TEXT_LIMIT + 100L)))
    expect_lte(nchar(utils::tail(.state$history, 1)), HISTORY_TEXT_LIMIT)
  })
})

test_that("disabling clears history and announces capability changes only once", {
  notes <- list()
  local_mocked_bindings(rs_notify = function(method, params, subscribed_only) {
    notes[[length(notes) + 1L]] <<- list(
      method = method,
      params = params,
      subscribed_only = subscribed_only
    )
  })
  with_history_state(function() {
    record_input(quote(a <- 1))
    expect_false(record_history(FALSE))
    expect_null(.state$history)
    expect_false("history" %in% service_capabilities())
    record_input(quote(secret <- "not retained"))
    expect_null(.state$history)
    expect_identical(rs_to_json(handle_history(list(), 1)), '{"entries":[]}')
    expect_length(notes, 1L)
    expect_identical(notes[[1]]$method, "capabilities")
    expect_false(notes[[1]]$subscribed_only)
    expect_false("history" %in% notes[[1]]$params$capabilities)
    record_history(FALSE)
    expect_length(notes, 1L)
    expect_true(record_history(TRUE))
    expect_true("history" %in% service_capabilities())
    expect_length(notes, 2L)
    expect_true("history" %in% notes[[2]]$params$capabilities)
    record_input(quote(b <- 2))
    expect_identical(.state$history, "b <- 2")
  })
})

test_that("version tracking continues when recording is off", {
  with_history_state(function() {
    with_probe_server(function(info) {
      record_history(FALSE)
      before <- rs_version()
      expect_true(task_callback(quote(a <- 1), 1, TRUE, FALSE))
      expect_equal(rs_version(), before + 1)
      expect_null(.state$history)
    })
  })
})

test_that("history toggle requires a single logical value", {
  for (enabled in list(NA, NULL, 1, "TRUE", c(TRUE, FALSE))) {
    expect_error(record_history(enabled), "enabled must be TRUE or FALSE")
  }
})
