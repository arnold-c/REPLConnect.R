# Static plot capture (docs/protocol.md, "Static plots"). unigd is the graphics
# device: after each top-level input the task callback checks whether the
# device changed (`upid`) and, if so, renders its current page to a PNG in a
# private per-server directory and tells subscribed clients. Only metadata and
# files are kept, never plot objects.

PLOT_DEFAULT_LIMIT <- 50L
PLOT_MIME <- "image/png"

capturing_plots <- function() {
  !is.null(.state$plots)
}

# The two unigd calls, separate so tests can replace them.
# NULL when the current device is not unigd (e.g. a window opened before
# capture): only unigd pages can be rendered.
plot_state <- function() {
  if (!identical(names(grDevices::dev.cur()), "unigd")) {
    return(NULL)
  }
  unigd::ugd_state()
}

open_plot_device <- function() {
  if (!identical(names(grDevices::dev.cur()), "unigd")) {
    unigd::ugd()
  }
}

render_plot <- function(file) {
  unigd::ugd_save(file = file, as = "png")
}

unigd_available <- function() {
  requireNamespace("unigd", quietly = TRUE)
}

# unigd is only suggested. In an interactive session offer to install it; the
# user's answer is the consent, nothing is installed silently.
require_unigd <- function() {
  if (unigd_available()) {
    return(invisible(NULL))
  }
  if (confirm_install_unigd()) {
    install_unigd()
  }
  if (!unigd_available()) {
    stop("plot capture needs the 'unigd' package: install.packages(\"unigd\")")
  }
  invisible(NULL)
}

confirm_install_unigd <- function() {
  interactive() &&
    isTRUE(utils::askYesNo(
      "Plot capture needs the 'unigd' package. Install it now?",
      default = FALSE
    ))
}

install_unigd <- function() {
  repos <- getOption("repos")
  if (is.null(repos) || identical(unname(repos["CRAN"]), "@CRAN@")) {
    repos <- c(CRAN = "https://cloud.r-project.org")
  }
  utils::install.packages("unigd", repos = repos)
}

#' Tell every connection the current capabilities (they change with capture
#' and history recording).
#' @noRd
announce_capabilities <- function() {
  rs_notify(
    "capabilities",
    list(capabilities = I(service_capabilities())),
    FALSE
  )
}

#' Capture plots from the unigd device
#'
#' Turning capture on makes unigd the default graphics device and publishes
#' each new plot as a PNG to subscribed editors. Turning it off restores the
#' previous device setting and deletes the retained files. Calling it while
#' capturing only changes the retention limit.
#'
#' @param enable `TRUE` to capture, `FALSE` to stop.
#' @param limit Plots (files) to retain; the oldest are pruned.
#' @return Whether plots are being captured, invisibly.
#' @export
capture_plots <- function(enable = TRUE, limit = PLOT_DEFAULT_LIMIT) {
  if (!is.logical(enable) || length(enable) != 1L || is.na(enable)) {
    stop("enable must be TRUE or FALSE")
  }
  if (
    !is.numeric(limit) ||
      length(limit) != 1L ||
      !is.finite(limit) ||
      limit < 1 ||
      limit != floor(limit)
  ) {
    stop("limit must be a positive integer")
  }
  limit <- as.integer(limit)
  if (!enable) {
    disable_plots()
  } else if (capturing_plots()) {
    .state$plots$limit <- limit
    prune_plots(.state$plots)
  } else {
    enable_plots(limit)
  }
  invisible(capturing_plots())
}

enable_plots <- function(limit) {
  require_unigd()
  info <- rs_info()
  if (is.null(info)) {
    stop("replconnect is not running; call replconnect::start() first")
  }
  dir <- tempfile(paste0(Sys.getpid(), "-plots-"), tmpdir = info$dir)
  dir.create(dir, mode = "0700")
  Sys.chmod(dir, "0700")
  store <- new.env(parent = emptyenv())
  store$dir <- dir
  store$limit <- limit
  store$next_id <- 0L
  store$records <- list()
  store$upid <- NULL
  store$device_option <- getOption("device")
  .state$plots <- store
  options(device = function(...) unigd::ugd(...))
  # An already-open device would keep receiving plots; make unigd current.
  open_plot_device()
  announce_capabilities()
}

# Forget captured plots and restore the device setting. No announcement: used
# by `shutdown()` as well.
stop_plots <- function() {
  store <- .state$plots
  if (is.null(store)) {
    return(invisible(FALSE))
  }
  .state$plots <- NULL
  options(device = store$device_option)
  unlink(store$dir, recursive = TRUE, force = TRUE)
  invisible(TRUE)
}

disable_plots <- function() {
  if (stop_plots()) {
    rs_unsubscribe_all()
    announce_capabilities()
  }
  invisible(NULL)
}

prune_plots <- function(store) {
  while (length(store$records) > store$limit) {
    unlink(store$records[[1L]]$path)
    store$records <- store$records[-1L]
  }
  invisible(NULL)
}

# Render the device's current page and publish it. The file is written under a
# temporary name and renamed, so a published path is complete and immutable.
publish_plot <- function(store) {
  tmp <- tempfile(tmpdir = store$dir, fileext = ".tmp")
  on.exit(unlink(tmp), add = TRUE)
  render_plot(tmp)
  Sys.chmod(tmp, "0600")
  id <- store$next_id + 1L
  path <- file.path(store$dir, paste0(id, ".png"))
  if (!file.rename(tmp, path)) {
    stop("could not publish ", path)
  }
  store$next_id <- id
  record <- list(id = id, mime = PLOT_MIME, path = path)
  store$records <- c(store$records, list(record))
  prune_plots(store)
  rs_notify("display", record, TRUE)
  invisible(NULL)
}

#' Publish the device's page if it changed since the last input. Called from
#' the task callback; only compares a counter unless something changed.
#' @noRd
capture_new_plot <- function() {
  store <- .state$plots
  if (is.null(store)) {
    return(invisible(NULL))
  }
  state <- tryCatch(plot_state(), error = function(e) {
    rs_log(paste("plot state failed:", conditionMessage(e)))
    NULL
  })
  if (
    is.null(state) ||
      !isTRUE(state$hsize >= 1L) ||
      identical(state$upid, store$upid)
  ) {
    return(invisible(NULL))
  }
  store$upid <- state$upid
  tryCatch(
    publish_plot(store),
    error = function(e) {
      rs_log(paste("plot capture failed:", conditionMessage(e)))
    }
  )
  invisible(NULL)
}

require_capturing <- function() {
  if (!capturing_plots()) {
    rpc_stop(
      METHOD_NOT_FOUND,
      "Plot capture is off; replconnect::capture_plots(TRUE)"
    )
  }
}

plot_history <- function() {
  list(plots = I(unname(.state$plots$records)))
}

handle_plots <- function(params, connection) {
  require_capturing()
  plot_history()
}

# Subscribing and snapshotting happen without yielding, so no plot falls
# between the history and the notifications.
handle_plots_subscribe <- function(params, connection) {
  require_capturing()
  rs_subscribe(connection)
  plot_history()
}
