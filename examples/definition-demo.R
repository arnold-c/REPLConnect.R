# Manual check for `v` / `gd` on functions in the variables pane.
#
# 1. Start R with replconnect attached and connect from Neovim.
# 2. Load this file with source references:
#      source("examples/definition-demo.R", keep.source = TRUE)
#    (or `options(keep.source = TRUE)` first; Rscript defaults to FALSE).
# 3. :REPLVariables, put the cursor on each variable below and try:
#      v   shows the source of every method in the viewer
#      gd  opens this file at the definition (a picker for several methods)
#
# Expected results are noted next to each case.

# Plain function --------------------------------------------------------------
# v: one method `add(x, y)`; gd: jumps to the `function` line below.
add <- function(x, y = 1) {
  x + y
}

# Package function, no srcref -------------------------------------------------
# v: deparsed source, "no source file"; gd: warns, falls back to the LSP.
my_median <- stats::median

# S4 generic and methods -----------------------------------------------------
# On `area`: v lists `area(shape = "Circle")` and `area(shape = "Square")`;
# gd offers a picker between the two setMethod lines.
setClass("Circle", representation(r = "numeric"))
setClass("Square", representation(s = "numeric"))

setGeneric("area", function(shape) standardGeneric("area"))

setMethod("area", "Circle", function(shape) {
  pi * shape@r^2
})

setMethod("area", "Square", function(shape) {
  shape@s^2
})

# S3 generic and methods ------------------------------------------------------
# On `describe`: v lists describe(x, ...), describe.default(x, ...) and
# describe.dog(x, ...); gd offers a picker.
describe <- function(x, ...) UseMethod("describe")

describe.default <- function(x, ...) {
  "some thing"
}

describe.dog <- function(x, ...) {
  paste("a dog called", x$name)
}

rex <- structure(list(name = "Rex"), class = "dog")

# R6 (needs the R6 package) ---------------------------------------------------
# Expand `bob` and use v/gd on `greet`: labelled `bob$greet()`.
# Expand `Person` > public_methods > greet: labelled `Person$greet()`.
# On `Student` / `amy` themselves (kind "class"): the server lists all methods
# including Person's and `super`'s, but the editor only sends `definition` for
# functions until REPLConnect.nvim routes kind "class" too.
if (requireNamespace("R6", quietly = TRUE)) {
  Person <- R6::R6Class(
    "Person",
    public = list(
      name = NULL,
      initialize = function(name) {
        self$name <- name
      },
      greet = function() {
        paste("hi,", self$name)
      }
    )
  )

  Student <- R6::R6Class(
    "Student",
    inherit = Person,
    public = list(
      greet = function() {
        paste(super$greet(), "the student")
      },
      study = function() {
        private$secret()
      }
    ),
    private = list(
      secret = function() "studying"
    )
  )

  bob <- Person$new("Bob")
  amy <- Student$new("Amy")
}

# Staleness ---------------------------------------------------------------------
# After sourcing, add a blank line at the top of this file and save without
# re-sourcing: gd on `add` should warn that line numbers may be out of date.

# Defined at the prompt -------------------------------------------------------
# Type this in the console (not from the file):
#   typed <- function(z) z * 2
# v: shows its source, file "<console>"; gd: warns and searches the LSP.
