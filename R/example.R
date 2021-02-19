#' Get path to rbedrock example
#'
#' rbedrock comes bundled with a number of sample files in its `inst/extdata`
#' directory. This function make them easy to access.
#'
#' @param path Name of file or directory. If `NULL`, the examples will be listed.
#' @export
#' @examples
#' rbedrock_example()
#' rbedrock_example("7U8vYCDqAAA=")
rbedrock_example <- function(path = NULL) {
  if (is.null(path)) {
    dir(system.file("extdata", package = "rbedrock"))
  } else {
    system.file("extdata", path, package = "rbedrock", mustWork = TRUE)
  }
}
