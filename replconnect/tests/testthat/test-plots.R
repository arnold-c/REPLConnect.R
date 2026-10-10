# unigd is replaced by a fake device whose page counter the tests drive.
with_fake_device <- function(run) {
  device <- new.env()
  device$upid <- 0L
  device$hsize <- 0L
  local_mocked_bindings(
    require_unigd = function() invisible(NULL),
    open_plot_device = function() invisible(NULL),
    plot_state = function() list(upid = device$upid, hsize = device$hsize),
    render_plot = function(file) writeBin(as.raw(1:4), file)
  )
  with_probe_server(function(info) {
    on.exit(stop_plots())
    run(device)
  })
}

new_plot <- function(device) {
  device$upid <- device$upid + 1L
  device$hsize <- device$hsize + 1L
}

test_that("capturing adds the capability and announces it", {
  notes <- list()
  local_mocked_bindings(
    rs_notify = function(method, params, subscribed_only) {
      notes[[length(notes) + 1L]] <<- list(method = method, params = params)
    }
  )
  with_fake_device(function(device) {
    expect_false("plots" %in% service_capabilities())
    expect_identical(dispatch("plots", list(), 1)$code, METHOD_NOT_FOUND)
    expect_true(capture_plots(TRUE, limit = 3))
    expect_true("plots" %in% service_capabilities())
    expect_identical(notes[[1]]$method, "capabilities")
    expect_true("plots" %in% notes[[1]]$params$capabilities)
    expect_false(capture_plots(FALSE))
    expect_false("plots" %in% notes[[2]]$params$capabilities)
  })
})

test_that("a changed device publishes one private PNG record", {
  notes <- list()
  local_mocked_bindings(
    rs_notify = function(method, params, subscribed_only) {
      if (method == "display") {
        notes[[length(notes) + 1L]] <<- params
      }
    }
  )
  with_fake_device(function(device) {
    capture_plots(TRUE)
    capture_new_plot()
    expect_length(notes, 0L) # empty device
    new_plot(device)
    capture_new_plot()
    capture_new_plot() # unchanged: nothing new
    expect_length(notes, 1L)
    record <- notes[[1]]
    expect_identical(record$id, 1L)
    expect_identical(record$mime, "image/png")
    expect_true(file.exists(record$path))
    expect_identical(unname(file.mode(record$path)), as.octmode("600"))
    expect_identical(unname(file.mode(dirname(record$path))), as.octmode("700"))
    expect_identical(
      dispatch("plots", list(), 1)$result$plots[[1]]$path,
      record$path
    )
  })
})

test_that("history is pruned to the limit and files are removed", {
  with_fake_device(function(device) {
    capture_plots(TRUE, limit = 2)
    dir <- .state$plots$dir
    for (i in 1:3) {
      new_plot(device)
      capture_new_plot()
    }
    plots <- dispatch("plots", list(), 1)$result$plots
    expect_identical(vapply(plots, function(p) p$id, 0L), c(2L, 3L))
    expect_false(file.exists(file.path(dir, "1.png")))
    capture_plots(TRUE, limit = 1)
    expect_length(dispatch("plots", list(), 1)$result$plots, 1L)
    capture_plots(FALSE)
    expect_false(dir.exists(dir))
  })
})

test_that("subscribing records the connection and returns history", {
  subscribed <- NULL
  local_mocked_bindings(rs_subscribe = function(connection) {
    subscribed <<- connection
  })
  with_fake_device(function(device) {
    capture_plots(TRUE)
    new_plot(device)
    capture_new_plot()
    reply <- dispatch("plots_subscribe", list(), 7)
    expect_identical(subscribed, 7)
    expect_length(reply$result$plots, 1L)
  })
})

test_that("empty history keeps its JSON array shape", {
  with_fake_device(function(device) {
    capture_plots(TRUE)
    expect_identical(
      rs_to_json(dispatch("plots", list(), 1)$result),
      '{"plots":[]}'
    )
  })
})

test_that("a missing unigd is installed only when the user agrees", {
  installed <- FALSE
  agree <- FALSE
  local_mocked_bindings(
    unigd_available = function() installed,
    confirm_install_unigd = function() agree,
    install_unigd = function() installed <<- TRUE
  )
  expect_error(require_unigd(), "needs the 'unigd' package")
  expect_false(installed)
  agree <- TRUE
  expect_silent(require_unigd())
  expect_true(installed)
})

test_that("capture_plots validates arguments", {
  expect_error(capture_plots(NA), "enable")
  expect_error(capture_plots(TRUE, limit = 0), "limit")
})
