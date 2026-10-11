# REPLConnect.R

R support for [REPLConnect.nvim](https://github.com/arnold-c/REPLConnect.nvim) — connect Neovim to a
running R session (designed for the [arf](https://github.com/eitsupi/arf)
console).

The repository has two parts:

- `replconnect/` — an R package (Rust core via extendr) that serves the
  session's variables, a grid data viewer, runtime completion, history and
  plots to Neovim over a Unix-domain socket, using the REPLConnect protocol.
  Requests are handled on R's main thread while R is idle at the prompt.
- `lua/replconnect/r.lua` and `plugin/replconnect-r.lua` — a Neovim plugin
  that registers R as a language with the replconnect.nvim core.

## Requirements

- Neovim with replconnect.nvim (the core plugin) installed
- R >= 4.2 on Linux or macOS
- Cargo and rustc, only when building from source (not needed with r-universe binaries)
- Optional: `unigd` (plot capture), `tibble`, `data.table`

## Installation

Install the R package from [r-universe](https://arnold-c.r-universe.dev),
which provides prebuilt binaries (no Rust toolchain needed):

```r
install.packages(
  "replconnect",
  repos = c("https://arnold-c.r-universe.dev", "https://cloud.r-project.org")
)
# or with pak:
# pak::pak("replconnect", repos = "https://arnold-c.r-universe.dev")
```

To build from source instead (requires Cargo and rustc):

```r
pak::pak("arnold-c/REPLConnect.R/replconnect")
```

Add this repository to Neovim's runtimepath alongside replconnect.nvim, e.g.
with lazy.nvim:

```lua
{
    "arnold-c/REPLConnect.R",
    dependencies = { "<owner>/REPLConnect.nvim" }
}
```

## Setup

Add this to your `~/.Rprofile`

```r
if (interactive()) {
  replconnect::start()
  replconnect::capture_plots(TRUE)
}
```

## R API

| Function            | Purpose                                      |
| ------------------- | -------------------------------------------- |
| `start()`           | Start serving the session over a socket      |
| `serving()`         | Check whether the server is running          |
| `shutdown()`        | Stop the server                              |
| `capture_plots()`   | Toggle forwarding of plots to Neovim         |
| `record_history()`  | Record console history for Neovim            |

## Development

Tests live in `replconnect/tests/` (testthat), `spec/` and `tests/` (Neovim),
and the Rust core in `replconnect/src/rust/core` (`cargo test`).

## License

See `replconnect/LICENSE`.
