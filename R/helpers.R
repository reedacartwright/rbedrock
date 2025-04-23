read_file_raw <- function(file, ...) {
    readBin(file, "raw", file.info(file)$size, ...)
}

write_file_raw <- function(x, file, ...) {
    writeBin(x, file, ...)
}

file_path <- function(...) {
    file.path(..., fsep = "/")
}

normalize_path <- function(...) {
    print(list(...))
    m <- normalizePath(file_path(...), winslash = "/", mustWork = FALSE)
    print(m)
    m
}
