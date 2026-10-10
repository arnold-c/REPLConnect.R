# REPLConnect.R

R support for [replconnect.nvim](https://github.com/) — connect Neovim to a
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
- Cargo and rustc (the package is compiled on install)
- Optional: `unigd` (plot capture), `tibble`, `data.table`

## Installation

Install the R package from this repository:

```r
install.packages("replconnect", repos = NULL, type = "source")
# or, from a checkout:
# R CMD INSTALL replconnect
```

Add this repository to Neovim's runtimepath alongside replconnect.nvim, e.g.
with lazy.nvim:

```lua
{
    "<owner>/REPLConnect.R",
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
