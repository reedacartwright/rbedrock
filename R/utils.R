#' Pipe operator
#'
#' @name %>%
#' @rdname pipe
#' @keywords internal
#' @export
#' @importFrom magrittr %>%
#' @usage lhs \%>\% rhs
NULL

#' @export
compact_missing <- function(dat) {
    if(!is.null(attr(dat,"missing"))) {
        dat[attr(dat,"missing")] <- NULL
    }
    dat
}
