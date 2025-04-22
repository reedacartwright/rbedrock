the$db <- NULL
the$db_is_user_set <- FALSE

#' Get/set the default db connection.
#'
#' The default db is the db connection that rbedrock uses by default. It
#' defaults to the most recently opened db, but can also be set by the user.
#'
#' Invoking `default_db()` returns the current default connection or the most
#' recently opened one. Invoking `default_db(db)` updates the current default
#' and returns the previous set value. `default_db(NULL)` can be used to unset
#' the default db and revert to the last opened one. Closing `db` will unset
#' it as the default db as well.
#'
#' `with_db()` and `local_db()` temporarily change the default db.
#'
#' @param db For `default_db()`, a `bedrockdb` object. For `with_db()` and
#' `local_db()`, a path to the world db to open or an existing `bedrockdb`
#' object.
#' @param code Code to execute in the temporary environment.
#' @param check Check the validity of `db`? Set to `FALSE` to skip the check.
#' @param close Close `db` when done? Set to `TRUE` to close db automatically.
#' @param .local_envir The environment to use for scoping.
#'
#' @return For `default_db()`, the calculated value of the default db.
#' For `default_db(db)`, the previously manually set value of `default_db()`.
#' For `with_db(db, code)`, the result of evaluating `code` with `db` as the
#' default `db`. For `local_db(db)`, the value of `db`.
#'
#' @seealso [withr::with_connection]
#'
#' @examples
#' dbpath <- rbedrock_example_world("example1.mcworld")
#' dbz <- bedrockdb(dbpath)
#' default_db(dbz) # set default
#' default_db() # returns dbz
#' default_db(NULL) # unset default
#' #cleanup
#' close(dbz)
#' with_db(dbpath, length(get_keys))
#' db <- local_db(dbpath)
#' length(get_keys())
#' close(db)
#' unlink(dbpath, recursive = TRUE)
#'
#' @export
default_db <- function(db, check = TRUE) {
    # evaluating db might update the$db, so we will save it first thing
    db_old <- the$db
    if (missing(db)) {
        db <- the$db
        assert_open_db(db, check)
    } else {
        if (!is.null(db)) {
            assert_open_db(db, check)
            the$db_is_user_set <- TRUE
        } else {
            the$db_is_user_set <- FALSE
        }
        the$db <- db
        db <- db_old
    }
    invisible(db)
}

assert_open_db <- function(db, check = TRUE, arg = "db") {
    if (!isFALSE(check) && (!is_bedrockdb(db) || !db$is_open())) {
        msg <- sprintf("`%s` is not an open bedrockdb connection.", arg)
        stop(msg, call. = FALSE)
    }
    invisible(TRUE)
}

#' @rdname default_db
#' @export
with_db <- function(db, code, close = is.character(db)) {
    # evaluating db might update the$db, so we will save it first thing
    old_db <- the$db
    force(close) # evaluate close before updating db
    if (is.character(db)) {
        db <- bedrockdb(db)
    }
    default_db(db)
    on.exit({
        default_db(old_db)
        if (isTRUE(close)) close(db)
    })
    force(code)
}

#' @rdname default_db
#' @export
local_db <- function(db, .local_envir = parent.frame(),
                     close = is.character(db)) {
    # evaluating db might update the$db, so we will save it first thing
    old_db <- the$db
    force(close) # evaluate close before updating db
    if (is.character(db)) {
        db <- bedrockdb(db)
    }
    default_db(db)
    defer(envir = .local_envir, {
        default_db(old_db)
        if (isTRUE(close)) close(db)
    })
    invisible(db)
}
