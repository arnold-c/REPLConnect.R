test_that("start publishes a discovery record and shutdown removes it", {
  dir <- tempfile("ri", tmpdir = "/tmp")
  old <- Sys.getenv("REPLCONNECT_DIR", unset = NA)
  Sys.setenv(REPLCONNECT_DIR = dir)
  on.exit({
    shutdown()
    if (is.na(old)) {
      Sys.unsetenv("REPLCONNECT_DIR")
    } else {
      Sys.setenv(REPLCONNECT_DIR = old)
    }
    unlink(dir, recursive = TRUE)
  })

  expect_false(serving())
  info <- start()
  expect_true(serving())
  expect_identical(start(), info) # idempotent
  record_path <- file.path(dir, paste0(Sys.getpid(), ".json"))
  expect_true(file.exists(info$socket))
  record <- rs_from_json(paste(
    readLines(record_path, warn = FALSE),
    collapse = ""
  ))
  expect_identical(record$language, "r")
  expect_identical(record$socket, info$socket)
  expect_equal(record$started_at, info$started_at)
  expect_equal(
    dispatch("initialize", list(), 1)$result$started_at,
    info$started_at
  )

  expect_true(shutdown())
  expect_false(serving())
  expect_false(file.exists(info$socket))
  expect_false(file.exists(record_path))
  expect_false(shutdown())
})
