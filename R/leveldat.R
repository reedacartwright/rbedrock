#' export
read_leveldat <- function(path, old=FALSE) {
    path <- .fixup_path(path)
    path <- paste0(path, "/level.dat")
    if (old) {
        path <- paste0(path, "_old")
    }
    con <- file(path, "rb")
    on.exit(close(con))
    seek(con, 8)

    dat <- read_nbt(con)[[1]]
    dat
}

#' export
write_leveldat <- function(path, val, old = FALSE, version = 8L) {
    path <- .fixup_path(path)
    path <- paste0(path, "/level.dat")
    if (old) {
        path <- paste0(path, "_old")
    }
    dat <- write_nbt(raw(), as_nbt(list(val),10L))
    len <- length(dat)
    con <- file(path, "wb")
    on.exit(close(con))
    writeBin(c(version,len), con, size = 4L, endian = "little")
    writeBin(dat, con)
}
