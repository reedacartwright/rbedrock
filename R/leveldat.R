#' Read data from a world's level.dat file.
#'
#' @param path The path to a world folder. If the path does not exist, it is 
#'   assumed to be the base name of a world folder in the local minecraftWorlds
#'   directory.
#' @param old If TRUE, `read_leveldat` will open level.dat_old instead.
#'
#' @return A list containing nbt data read from level.dat
#'
#' @export
read_leveldat <- function(path, old=FALSE) {
    path <- .fixup_path(path)
    path <- file.path(path, "level.dat")
    if (old) {
        path <- paste0(path, "_old")
    }
    con <- file(path, "rb")
    on.exit(close(con))
    seek(con, 8)

    dat <- read_nbt(con)[[1]]
    dat
}

#' Write data from a world's level.dat file.
#'
#' @param path The path to a world folder. If the path does not exist, it is 
#'   assumed to be the base name of a world folder in the local minecraftWorlds
#'   directory.
#' @param old If TRUE, `read_leveldat` will open level.dat_old instead.
#' @param value NBT data to be written to level.dat. It will be wrapped in a COMPOUND tag before writing.
#' @param version The level.dat format version for the file header.
#'
#' @return A list containing nbt data read from level.dat.
#'
#' @export
write_leveldat <- function(path, value, old = FALSE, version = 8L) {
    path <- .fixup_path(path)
    path <- file.path(path, "level.dat")
    if (old) {
        path <- paste0(path, "_old")
    }
    dat <- write_nbt(raw(), as_nbt(list(value),10L))
    len <- length(dat)
    con <- file(path, "wb")
    on.exit(close(con))
    writeBin(c(version,len), con, size = 4L, endian = "little")
    writeBin(dat, con)
    invisible()
}
