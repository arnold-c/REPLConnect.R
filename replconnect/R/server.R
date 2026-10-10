# Serving this R session over a Unix socket (see docs/protocol.md).
#
# The transport is Rust (src/rust): it accepts connections on its own threads
# and wakes R's main thread through an input handler, so requests are handled
# while R is idle at the prompt (arf runs input handlers while it waits for
# input). R code here only answers requests; see dispatch.R.

# Package state; never garbage-collected, so its finalizer runs at exit.
.state <- new.env(parent = emptyenv())

#' Serve this R session to the replconnect Neovim plugin
#'
#' Listens on `/tmp/replconnect-<uid>/<pid>.sock` (or `$REPLCONNECT_DIR`)
#' and writes a discovery record next to it. Idempotent.
#'
#' @return The socket path and start time, invisibly.
#' @export
start <- function() {
  info <- rs_start(getwd(), project_path(), r_version())
  install_hook()
  invisible(info)
}

#' Stop serving this R session
#'
#' Disconnects clients and removes the socket and discovery record. R keeps
#' running. Idempotent.
#'
#' @return `TRUE` if a server was running, invisibly.
#' @export
shutdown <- function() {
  remove_hook()
  stop_plots()
  invisible(rs_stop())
}

#' Is this session being served?
#' @export
serving <- function() {
  !is.null(rs_info())
}

r_version <- function() {
  paste(R.version$major, R.version$minor, sep = ".")
}

# A file inside the session's project, for the editor's session picker (it
# shows the containing directory): renv's lockfile, or a package DESCRIPTION.
project_path <- function() {
  renv <- Sys.getenv("RENV_PROJECT")
  if (nzchar(renv)) {
    return(file.path(renv, "renv.lock"))
  }
  description <- file.path(getwd(), "DESCRIPTION")
  if (file.exists(description)) description else ""
}

.onLoad <- function(libname, pkgname) {
  reg.finalizer(.state, function(e) rs_stop(), onexit = TRUE)
}

# The input handler points into this library: stop before it is unloaded.
.onUnload <- function(libpath) {
  remove_hook()
  stop_plots()
  rs_stop()
}
