#' Get path to rbedrock example
#'
#' rbedrock comes bundled with a number of sample files in its `inst/extdata`
#' directory. This function make them easy to access.
#'
#' @param path Name of file or directory. If `NULL`, the examples will be
#'        listed.
#' @export
#' @examples
#' rbedrock_example()
#' rbedrock_example("example1.mcworld")
#' rbedrock_example_world("example1.mcworld")

rbedrock_example <- function(path = NULL) {
  if (is.null(path)) {
    extdata <- system.file("extdata", package = "rbedrock", mustWork = TRUE)
    basename(list.files(extdata))
  } else {
    system.file("extdata", path, package = "rbedrock", mustWork = TRUE)
  }
}

#' @export
#' @rdname rbedrock_example
rbedrock_example_world <- function(path) {
  path <- rbedrock_example(path)
  destdir <- tempfile("world")
  path <- suppressMessages(import_world(path, destdir))
  normalize_path(path)
}
