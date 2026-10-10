test_that("an existing Rust library does not suppress Cargo on reinstall", {
  makevars <- test_path("..", "..", "src", "Makevars")
  skip_if_not(
    file.exists(makevars),
    "build rule test needs the package source tree"
  )
  makevars <- normalizePath(makevars)
  dir <- tempfile("ri-build-")
  dir.create(file.path(dir, "release"), recursive = TRUE)
  on.exit(unlink(dir, recursive = TRUE))
  static <- file.path(dir, "release", "libreplconnect.a")
  shared <- file.path(dir, "replconnect.so")
  file.create(static, shared)

  # No real compiler or Cargo invocation: inspect make's plan with both
  # outputs already present, as on the second R CMD INSTALL.
  planned <- system2(
    "make",
    c(
      "-n",
      "-f",
      shQuote(makevars),
      "all",
      shQuote(paste0("SHLIB=", shared)),
      shQuote(paste0("TARGET_DIR=", dir))
    ),
    stdout = TRUE,
    stderr = TRUE
  )
  expect_null(attr(planned, "status"))
  expect_true(
    any(grepl("cargo build", planned, fixed = TRUE)),
    info = paste(planned, collapse = "\n")
  )

  # Prove this test distinguishes the old rule: with FORCE disconnected,
  # existing outputs suppress Cargo completely.
  old_rule <- file.path(dir, "old.Makevars")
  writeLines(
    gsub("$(STATLIB): FORCE", "$(STATLIB):", readLines(makevars), fixed = TRUE),
    old_rule
  )
  old_plan <- system2(
    "make",
    c(
      "-n",
      "-f",
      shQuote(old_rule),
      "all",
      shQuote(paste0("SHLIB=", shared)),
      shQuote(paste0("TARGET_DIR=", dir))
    ),
    stdout = TRUE,
    stderr = TRUE
  )
  expect_null(attr(old_plan, "status"))
  expect_false(any(grepl("cargo build", old_plan, fixed = TRUE)))
})
