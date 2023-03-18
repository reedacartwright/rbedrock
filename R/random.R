#' Random Number Generation for Minecraft
#'
#' Bedrock Edition's central random number algorithm is MT19937. However, R's
#' MT19937 code is not compatible with Bedrock's. These routines provide an API
#' that is compatible with Bedrock's.
#'
#' @name bedrock_random
#' @examples
#' # seed the global random number generator
#' bedrock_random_seed(5490L)
#'
#' # save and restore rng state
#' saved_state <- bedrock_random_state()
#' bedrock_random_get_uint(10)
#' bedrock_random_state(saved_state)
#' bedrock_random_get_uint(10)
NULL

#' @description
#' `bedrock_random_seed()` seeds the random number generator.
#'
#' @param value a scalar integer
#'
#' @rdname bedrock_random
#' @export
bedrock_random_seed <- function(value) {
    value <- as.integer(value)
    .Call(Cmcpe_random_seed, value)
}

#' @description
#' `bedrock_random_state()` returns the current state of the random number
#' generator as a raw vector.
#'
#' @param new_state a raw vector
#'
#' @rdname bedrock_random
#' @export
bedrock_random_state <- function(new_state = NULL) {
    invisible(.Call(Cmcpe_random_state, new_state))
}

#' @description
#' `bedrock_random_get_uint()` returns a 32-bit random integer.
#' Default range is `[0, 2^32-1]`.
#'
#' @param n number of observations.
#' @param min,max lower and upper limits of the distribution. Must be finite.
#' If only one is specified, it is taken as `max`. If neither is specified,
#' the default range is used.
#'
#' @rdname bedrock_random
#' @export

bedrock_random_get_uint <- function(n, max) {
    n <- as.integer(n)
    if (missing(max)) {
        max <- NULL
    }
    .Call(Cmcpe_random_get_uint, n, max)
}

#' @description
#' `bedrock_random_get_int()` returns a 31-bit random integer.
#' Default range is `[0, 2^31-1]`.
#
#' @rdname bedrock_random
#' @export
bedrock_random_get_int <- function(n, min, max) {
    n <- as.integer(n)
    if (!missing(min) && missing(max)) {
        max <- min
        min <- NULL
    } else {
        if (missing(min)) {
            min <- NULL
        }
        if (missing(max)) {
            max <- NULL
        }
    }

    .Call(Cmcpe_random_get_int, n, min, max)
}

#' @description
#' `bedrock_random_get_float()` returns a random real number.
#' Default range is `[0.0, 1.0)`.
#
#' @rdname bedrock_random
#' @export
bedrock_random_get_float <- function(n, min, max) {
    n <- as.integer(n)
    if (!missing(min) && missing(max)) {
        max <- min
        min <- NULL
    } else {
        if (missing(min)) {
            min <- NULL
        }
        if (missing(max)) {
            max <- NULL
        }
    }

    .Call(Cmcpe_random_get_float, n, min, max)
}

#' @description
#' `bedrock_random_get_double()` returns a random real number
#' Default range is `[0.0, 1.0)`.
#'
#' @rdname bedrock_random
#' @export
bedrock_random_get_double <- function(n) {
    n <- as.integer(n)
    .Call(Cmcpe_random_get_double, n)
}

#' Random Number Seeds for Minecraft
#'
#' Minecraft uses several different kind of seeds during world generation and
#' gameplay.
#'
#' @name bedrock_random_create_seed
#'
#' @param x,z chunk coordinates
#' @param a,b seed parameters
#' @param salt seed parameter
#' @param type which seed type to use
#'
#' @examples
#' # identify slime chunks
#' g <- tidyr::expand_grid(x=1:10, z=1:10)
#' is_slime_chunk <- purrr::pmap_lgl(g, function(x,z) {
#'   seed <- bedrock_random_create_seed(x,z,0x1f1f1f1f,1,0,type=1)
#'   bedrock_random_seed(seed)
#'   bedrock_random_get_uint(1,10) == 0
#' })

#' @description
#' `bedrock_random_create_seed()` constructs a seed using the formulas
#' type 1: `x*a ^ z*b ^ salt`, type 2: `x*a + z*b + salt`, and type 3:
#' `x*a + z*b ^ salt`.
#'
#' @export
bedrock_random_create_seed <- function(x, z, a, b, salt, type) {
    x <- as.integer(x)
    z <- as.integer(z)
    a <- as.integer(a)
    b <- as.integer(b)
    salt <- as.integer(salt)
    type <- as.integer(type)
    .Call(Cmcpe_random_create_seed, x, z, a, b, salt, type)
}
