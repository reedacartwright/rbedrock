#' Read and write data from a world's level.dat file.
#'
#' @param path The path to a world folder. If the path does not exist, it is 
#'   assumed to be the base name of a world folder in the local minecraftWorlds
#'   directory.
#' @param old  Read/write to 'level.dat_old' instead.
#' @param object NBT data to be written to level.dat.
#' @param version The level.dat format version for the file header.
#'
#' @return `read_leveldat` returns an nbtnode object.
#'
#' `write_leveldat` returns a copy of the data written.
#'
#' @export
read_leveldat <- function(path, old = FALSE) {
    path <- .fixup_path(path, verify=TRUE)
    path <- file.path(path, "level.dat")
    if (old) {
        path <- paste0(path, "_old")
    }

    rawval <- readr::read_file_raw(path)

    # remove the first 8 bytes
    rawval <- rawval[-(1:8)]

    dat <- read_nbt(rawval)
    dat
}

#' @export
#' @rdname read_leveldat
write_leveldat <- function(object, path, old = FALSE, version = 8L) {
    path <- .fixup_path(path)
    path <- file.path(path, "level.dat")
    if (old) {
        path <- paste0(path, "_old")
    }
    dat <- write_nbt(object)
    len <- length(dat)
    rawval <- writeBin(c(version,len), raw(), size = 4L, endian = "little")
    rawval <- c(rawval,dat)

    readr::write_file(rawval, path)
}
