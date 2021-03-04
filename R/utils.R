#' Pipe operator
#'
#' @name %>%
#' @rdname pipe
#' @keywords internal
#' @export
#' @importFrom magrittr %>%
#' @usage lhs \%>\% rhs
NULL

# Silence warnings about undefined global variables
#' @importFrom rlang .data
NULL


# helper function for conversion
.apply_func <- function(object, f, ..., simplify) {
    if(is.null(f)) {
        return(NULL)
    } else if(!is.list(object)) {
        if(isTRUE(simplify)) {
            return(f(object, ...))
        }
        object <- list(object)
    } 
    purrr::map(object, f, ...)
}
