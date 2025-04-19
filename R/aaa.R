the <- new.env(parent = emptyenv())

# support versions of R < 4.4.0
if (!exists("%||%", envir = baseenv())) {
    `%||%` <- function(x, y) {
        if (is.null(x)) y else x
    }
}
