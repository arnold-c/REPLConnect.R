definition_reply <- function(path, env) {
  local_mocked_bindings(workspace_env = function() env)
  dispatch("definition", list(path = as.list(path)), 1)
}

test_that("definition is advertised", {
  expect_true("definition" %in% service_capabilities())
})

test_that("an unknown path or a non-function is invalid params", {
  env <- new.env()
  env$x <- 1
  expect_identical(
    definition_reply("__replconnect_missing__", env)$code,
    INVALID_PARAMS
  )
  expect_identical(definition_reply("x", env)$code, INVALID_PARAMS)
})

test_that("a sourced function has its file, line and source", {
  file <- tempfile(fileext = ".R")
  on.exit(unlink(file))
  writeLines(c("# header", "f <- function(x, y = 2) {", "  x + y", "}"), file)
  env <- new.env()
  source(file, local = env, keep.source = TRUE)
  reply <- definition_reply("f", env)
  expect_true(reply$ok)
  expect_s3_class(reply$result$locations, "AsIs")
  loc <- reply$result$locations[[1]]
  expect_identical(loc$signature, "f(x, y)")
  expect_identical(loc$file, normalizePath(file))
  expect_identical(loc$line, 2L)
  expect_identical(loc$source, "function(x, y = 2) {\n  x + y\n}")
})

test_that("a function typed at the prompt has source but no file", {
  env <- new.env()
  eval(parse(text = "g <- function() 1", keep.source = TRUE), env)
  loc <- definition_reply("g", env)$result$locations[[1]]
  expect_identical(loc$source, "function() 1")
  expect_false(file.exists(loc$file))
})

test_that("an S4 generic has one location per method", {
  file <- tempfile(fileext = ".R")
  on.exit(unlink(file))
  writeLines(
    c(
      "setClass('RcCircle', representation(r = 'numeric'))",
      "setClass('RcSquare', representation(s = 'numeric'))",
      "setGeneric('rcArea', function(shape) standardGeneric('rcArea'))",
      "setMethod('rcArea', 'RcCircle', function(shape) {",
      "  pi * shape@r^2",
      "})",
      "setMethod('rcArea', 'RcSquare', function(shape) shape@s^2)"
    ),
    file
  )
  on.exit(
    {
      methods::removeGeneric("rcArea", where = globalenv())
      methods::removeClass("RcCircle", where = globalenv())
      methods::removeClass("RcSquare", where = globalenv())
    },
    add = TRUE
  )
  source(file, local = globalenv(), keep.source = TRUE)
  reply <- definition_reply("rcArea", globalenv())
  expect_true(reply$ok)
  locs <- reply$result$locations
  expect_length(locs, 2L)
  by_signature <- setNames(locs, vapply(locs, `[[`, "", "signature"))
  circle <- by_signature[['rcArea(shape = "RcCircle")']]
  expect_identical(circle$file, normalizePath(file))
  expect_identical(circle$line, 4L)
  expect_match(circle$source, "pi * shape@r^2", fixed = TRUE)
  expect_identical(
    by_signature[['rcArea(shape = "RcSquare")']]$line,
    7L
  )
})

# An environment with an R6 class `Person` (methods on lines 3 and 4) and an
# instance `bob`, sourced from a temp file with srcrefs (removed with tempdir).
r6_env <- function() {
  file <- tempfile(fileext = ".R")
  writeLines(
    c(
      "Person <- R6::R6Class('Person', public = list(",
      "  name = NULL,",
      "  initialize = function(name) self$name <- name,",
      "  greet = function() paste('hi', self$name)",
      "))",
      "bob <- Person$new('bob')"
    ),
    file
  )
  env <- new.env()
  source(file, local = env, keep.source = TRUE)
  env$file <- normalizePath(file)
  env
}

test_that("R6 methods are labelled with their owner", {
  skip_if_not_installed("R6")
  env <- r6_env()
  loc <- definition_reply(c("bob", "greet"), env)$result$locations[[1]]
  expect_identical(loc$signature, "bob$greet()")
  expect_identical(loc$file, env$file)
  expect_identical(loc$line, 4L)
  loc <- definition_reply(
    c("Person", "public_methods", "greet"),
    env
  )$result$locations[[1]]
  expect_identical(loc$signature, "Person$greet()")
})

test_that("a function without srcref returns deparsed source and no file", {
  env <- new.env()
  env$h <- eval(parse(text = "function(a) a", keep.source = FALSE))
  env$p <- sum
  loc <- definition_reply("h", env)$result$locations[[1]]
  expect_null(loc$file)
  expect_match(loc$source, "function (a)", fixed = TRUE)
  expect_true(definition_reply("p", env)$ok)
})
