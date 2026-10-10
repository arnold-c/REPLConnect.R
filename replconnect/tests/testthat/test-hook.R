test_that("the task callback stays registered and records bounded input", {
  .state$history <- NULL
  expect_true(task_callback(quote(x <- 1), 1, TRUE, FALSE))
  expect_identical(.state$history, "x <- 1")
  for (i in seq_len(HISTORY_LIMIT + 20L)) {
    task_callback(call("f", i), NULL, TRUE, FALSE)
  }
  expect_length(.state$history, HISTORY_LIMIT)
  expect_identical(
    .state$history[[HISTORY_LIMIT]],
    paste0("f(", HISTORY_LIMIT + 20L, "L)")
  )
  .state$history <- NULL
})

test_that("hook installation is idempotent and removable", {
  remove_hook()
  expect_false(TASK_CALLBACK %in% getTaskCallbackNames())
  install_hook()
  install_hook()
  expect_identical(sum(getTaskCallbackNames() == TASK_CALLBACK), 1L)
  remove_hook()
  expect_false(TASK_CALLBACK %in% getTaskCallbackNames())
})

test_that("serving installs the hook and each input bumps the version", {
  with_probe_server(function(info) {
    expect_true(TASK_CALLBACK %in% getTaskCallbackNames())
    before <- rs_version()
    task_callback(quote(y <- 2), 2, TRUE, FALSE)
    expect_equal(rs_version(), before + 1)
    shutdown()
    expect_false(TASK_CALLBACK %in% getTaskCallbackNames())
  })
})
