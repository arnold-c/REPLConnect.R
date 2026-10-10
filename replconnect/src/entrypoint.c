// Forward R's routine registration to the Rust static library, so the
// linker keeps it.
void R_init_replconnect_extendr(void *dll);

void R_init_replconnect(void *dll) {
    R_init_replconnect_extendr(dll);
}
