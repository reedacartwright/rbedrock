# Get/set the default db connection.

The default db is the db connection that rbedrock uses by default. It
defaults to the most recently opened db, but can also be set by the
user.

## Usage

``` r
default_db(db, check = TRUE)

with_db(db, code, close = is.character(db))

local_db(db, .local_envir = parent.frame(), close = is.character(db))
```

## Arguments

- db:

  For `default_db()`, a `bedrockdb` object. For `with_db()` and
  `local_db()`, a path to the world db to open or an existing
  `bedrockdb` object.

- check:

  Check the validity of `db`? Set to `FALSE` to skip the check.

- code:

  Code to execute in the temporary environment.

- close:

  Close `db` when done? Set to `TRUE` to close db automatically.

- .local_envir:

  The environment to use for scoping.

## Value

For `default_db()`, the calculated value of the default db. For
`default_db(db)`, the previously manually set value of `default_db()`.
For `with_db(db, code)`, the result of evaluating `code` with `db` as
the default `db`. For `local_db(db)`, the value of `db`.

## Details

Invoking `default_db()` returns the current default connection or the
most recently opened one. Invoking `default_db(db)` updates the current
default and returns the previous set value. `default_db(NULL)` can be
used to unset the default db and revert to the last opened one. Closing
`db` will unset it as the default db as well.

`with_db()` and `local_db()` temporarily change the default db.

## See also

[withr::with_connection](https://withr.r-lib.org/reference/with_connection.html)

## Examples

``` r
dbpath <- rbedrock_example_world("example1.mcworld")
dbz <- bedrockdb(dbpath)
default_db(dbz) # set default
default_db() # returns dbz
default_db(NULL) # unset default
#cleanup
close(dbz)
with_db(dbpath, length(get_keys))
#> [1] 1
db <- local_db(dbpath)
length(get_keys())
#> Error: `db` is not an open bedrockdb connection.
close(db)
unlink(dbpath, recursive = TRUE)
```
