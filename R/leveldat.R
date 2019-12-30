##' export
read_leveldat <- function(path, old=FALSE) {
	path <- .fixup_path(path)
	path <- paste0(path, "/level.dat")
	if (old) {
		path <- paste0(path, "_old")
	}
	con <- file(path, "rb")
	on.exit(close(con))
	seek(con, 8)

	dat <- read_nbt(con)
	dat[[1]]
}
