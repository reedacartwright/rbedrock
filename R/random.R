
mcpe_random_seed <- function(value) {
    value <- as.integer(value)
    .Call(Cmcpe_random_seed, value)
}

mcpe_random_state <- function(new_state = NULL) {
    .Call(Cmcpe_random_state, new_state)
}

mcpe_random_get_uint <- function(n, max) {
    n <- as.integer(n)
    if(missing(max)) {
        max <- NULL
    }
    .Call(Cmcpe_random_get_uint, n, max)
}

mcpe_random_get_int <- function(n, min, max) {
    n <- as.integer(n)
    if(!missing(min) && missing(max)) {
        max <- min
        min <- NULL
    } else {
        if(missing(min)) {
            min <- NULL
        }
        if(missing(max)) {
            max <- NULL
        }
    }

    .Call(Cmcpe_random_get_int, n, min, max)
}

mcpe_random_get_double <- function(n) {
    n <- as.integer(n)
    .Call(Cmcpe_random_get_double, n)
}

mcpe_random_get_float <- function(n, min, max) {
    n <- as.integer(n)
    if(!missing(min) && missing(max)) {
        max <- min
        min <- NULL
    } else {
        if(missing(min)) {
            min <- NULL
        }
        if(missing(max)) {
            max <- NULL
        }
    }

    .Call(Cmcpe_random_get_float, n, min, max)
}