# Standalone file: do not edit by hand
# Source: https://github.com/r-lib/withr/blob/HEAD/R/standalone-defer.R
# Generated by: usethis::use_standalone("r-lib/withr", "defer")
# ----------------------------------------------------------------------
#
# ---
# repo: r-lib/withr
# file: standalone-defer.R
# last-updated: 2024-01-15
# license: https://unlicense.org
# ---
#
# `defer()` is similar to `on.exit()` but with a better default for
# `add` (hardcoded to `TRUE`) and `after` (`FALSE` by default).
# It also supports adding handlers to other frames which is useful
# to implement `local_` functions.
#
#
# ## Changelog
#
# 2024-01-15:
# * Rewritten to be pure base R.
#
# nocov start

defer <- function(expr, envir = parent.frame(), after = FALSE) {
  thunk <- as.call(list(function() expr))
  do.call(
    on.exit,
    list(thunk, add = TRUE, after = after),
    envir = envir
  )
}

# nocov end
