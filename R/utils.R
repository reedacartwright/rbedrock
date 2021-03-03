#' Pipe operator
#'
#' @name %>%
#' @rdname pipe
#' @keywords internal
#' @export
#' @importFrom magrittr %>%
#' @usage lhs \%>\% rhs
NULL

# helper function for conversion
.apply_func <- function(object, f) {
    if(is.null(f)) {
        return(NULL)
    } else if(!is.list(object)) {
        return(f(object))
    }
    purrr::map(object,f)
}
