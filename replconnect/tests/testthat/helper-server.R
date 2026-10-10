# Run `run(info)` against a started service in a private temporary directory.
with_probe_server <- function(run) {
  dir <- tempfile("ri-probe-", tmpdir = "/tmp")
  old <- Sys.getenv("REPLCONNECT_DIR", unset = NA_character_)
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
  info <- start()
  run(info)
}
