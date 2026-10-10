tests: nvim_tests r_tests

nvim_tests:
        nvim --headless -u NONE -l tests/launch.lua

# The R service's Rust core (no R installation needed).
rust_tests:
	cargo test --manifest-path replconnect/src/rust/Cargo.toml -p replconnect-core

# Type-check the extendr glue without R (compiles; nothing R-dependent runs).
rust_check:
	cargo test --manifest-path replconnect/src/rust/Cargo.toml -p replconnect --no-run --config replconnect/src/rust/typecheck.toml

# Build the independent Unix-socket status probe (used by R integration tests).
rust_probe:
	cargo build --manifest-path replconnect/src/rust/Cargo.toml -p replconnect-core --example status_probe

# Sample an arf/R session without Neovim or nc, e.g. just r_status /tmp/replconnect-501/123.sock.
r_status socket samples="20": rust_probe
	replconnect/src/rust/target/debug/examples/status_probe {{quote(socket)}} - {{samples}} 500

# A real arf console: idle -> Sys.sleep (busy) -> idle, using the installed package.
arf_probe:
	nvim --headless -u NONE -l spec/arf_probe.lua

r_build:
	R CMD INSTALL replconnect

# Format the R service and its tests with Air.
r_format:
	air format replconnect/R replconnect/tests

# The R service (needs R, Rust and testthat; installs the package first).
r_tests: r_build rust_probe
	Rscript -e 'testthat::test_local("replconnect")'
