
get_keys <- function(db, prefix = NULL, readoptions = NULL) {
    if(is.null(prefix)) {
        ret <- db$keys(readoptions)
        return(ret)
    }
    pre <- .key_prefix_to_raw(prefix)
    
    ret <- list()
    
    it <- db$iterator(readoptions)$seek(pre)
    while(it$valid()) {
        k <- it$key(as_raw = TRUE)
        if(!.has_prefix_raw(k,pre)) {
            break;
        }
        ret[length(ret) + 1] <- list(k)
        it$move_next()
    }
    .to_strkey(ret)
}

# get_values <- function(db, keys, as_raw = NULL, missing_value = NULL, missing_report = FALSE,
#         readoptions = NULL) {
#     db$mget(keys, as_raw, missing_value, missing_report, readoptions)
# }

.has_prefix_raw <- function(key, prefix) {
    isTRUE(all(key[seq_along(prefix)] == prefix))
}

.key_prefix_to_raw <- function(pre) {
    .strkey_to_raw(pre)
}


# .compare_rawkeys <- function(a, b) {
#     stopifnot(is.raw(a) && is.raw(b))
#     p <- 1
#     while(p <= length(a)) {
#         if(p > length(b) || b[p] < a[p]) {
#             return(1L)
#         }
#         if(a[p] < b[p]) {
#             return(-1L)
#         }
#         p <- p + 1
#     }
#     if(p <= length(b)) {
#         return(-1L)
#     }
#     return(0L)
# }

