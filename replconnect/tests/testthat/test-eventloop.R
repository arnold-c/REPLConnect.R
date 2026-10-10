# These tests use the real R input handler and a separate native socket
# client, not the Rust unit tests' simulated event loop. No editor or nc.
test_that("the loaded native library identifies itself and logs at startup", {
  with_probe_server(function(info) {
    diagnostic <- rs_diagnostics()
    expect_identical(diagnostic$native_build, "r-eventloop-probe-v1")
    expect_equal(diagnostic$pid, Sys.getpid())
    expect_identical(diagnostic$socket, info$socket)
    expect_equal(diagnostic$wake_count, 0)
    expect_true(file.exists(diagnostic$log))
    expect_match(
      paste(readLines(diagnostic$log), collapse = "\n"),
      "service started; native_build=r-eventloop-probe-v1",
      fixed = TRUE
    )
    rs_log("explicit logger probe")
    expect_true(any(grepl(
      "explicit logger probe",
      readLines(diagnostic$log),
      fixed = TRUE
    )))
  })
})

test_that("a real socket request during Sys.sleep wakes R and reports busy", {
  binary <- test_path(
    "..",
    "..",
    "src",
    "rust",
    "target",
    "debug",
    "examples",
    "status_probe"
  )
  # This is required, not skipped: just r_tests builds it first.
  expect_true(
    file.exists(binary),
    info = "build the probe first with just rust_probe"
  )
  if (!file.exists(binary)) {
    return(invisible(NULL))
  }
  binary <- normalizePath(binary)
  with_probe_server(function(info) {
    report <- tempfile("ri-status-")
    errors <- tempfile("ri-errors-")
    on.exit(unlink(c(report, errors)))
    before <- rs_diagnostics()$wake_count
    system2(
      binary,
      c(shQuote(info$socket), shQuote(report)),
      stdout = errors,
      stderr = errors,
      wait = FALSE
    )
    deadline <- Sys.time() + 5
    complete <- FALSE
    while (Sys.time() < deadline) {
      # R's actual Unix event loop can run our handler from inside Sys.sleep.
      Sys.sleep(0.05)
      if (file.exists(report) && file.info(report)$size > 0) {
        complete <- TRUE
        break
      }
    }
    diagnostic <- rs_diagnostics()
    error_text <- if (file.exists(errors)) {
      paste(readLines(errors), collapse = "\n")
    } else {
      ""
    }
    expect_true(complete, info = paste("probe failed:", error_text))
    if (!complete) {
      return(invisible(NULL))
    }
    sample <- rs_from_json(readLines(report)[[1]])
    expect_gt(diagnostic$wake_count, before) # proves the real input handler ran
    expect_true(diagnostic$last_evaluating) # proves environment check, not just timeout
    expect_true(sample$status$busy)
    expect_match(
      paste(readLines(diagnostic$log), collapse = "\n"),
      "R is evaluating",
      fixed = TRUE
    )
  })
})
