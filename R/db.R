the <- new.env(parent = emptyenv())
the$db <- NULL
the$last_opened_db <- NULL

#' Get/set the default db connection.
#'
#' The default db is the db connection that rbedrock uses by default. It
#' defaults to the most recently opened db, but can also be set by the user.
#'
#' Invoking `default_db()` returns the current default connection or the most
#' recently opened one. Invoking `default_db(db)` updates the current default
#' and returns the previous set value. `default_db(NULL)` can be used to unset
#' the default db and revert to the last opened one.
#'
#' @param db A `bedrockdb` object.
#' @param check A `logical` specifying whether to check validity.
#'
#' @return For `default_db()`, the calculated value of the default db.
#' For `default_db(db)`, the previously manually set value of `default_db()`.
#'
#' @examples
#' dbpath <- rbedrock_example_world("example1.mcworld")
#' dbz <- bedrockdb(dbpath)
#' default_db(dbz) # set default
#' default_db() # returns dbz
#' default_db(NULL) # unset default
#' #cleanup
#' close(dbz)
#' unlink(dbpath, recursive = TRUE)
#'
#' @export
default_db <- function(db, check = TRUE) {
    if (missing(db)) {
        db <- the$db %||% the$last_opened_db
        assert_open_db(db, check)
    } else {
        if (!is.null(db)) {
            assert_open_db(db, check)
        }
        db_old <- the$db
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
