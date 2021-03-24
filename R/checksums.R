read_checksums_value <- function(rawdata) {
    sz <- readBin(rawdata, integer(), n=1L, size= 4L, endian = "little")
    rawdata <- rawdata[-c(1:4)]
    stopifnot(length(rawdata) == sz*11L)
    ret <- list()
    for(i in 1:sz) {
        tag <- readBin(rawdata, integer(), n=1L, size=2L, endian = "little")
        subtag <- as.integer(rawdata[3])
        hash <- rawdata[4:11]
        hash <- paste0(rev(as.character(hash)), collapse="")
        k <- ifelse(tag == 47L, paste(tag, subtag, sep="-"), as.character(tag))
        ret[[k]] <- hash
        rawdata <- rawdata[-c(1:11)]
    }
    storage.mode(ret) <- "character"
    ret
}

write_checksums_value <- function(object) {
    parsed_names <- stringr::str_match(names(object), .CHUNK_KEY_TAG_MATCH)
    stopifnot(!any(is.na(parsed_names[,1])))
    tag <- as.integer(parsed_names[,2])
    subtag <- as.integer(parsed_names[,3])
    # check for NAs introduced by coercion
    stopifnot(is.na(tag) == is.na(parsed_names[,2]) && is.na(subtag) == is.na(parsed_names[,3]))

    subtag[is.na(subtag)] <- 0L

    hash <- purrr::map(object, function(x){
        h <- strsplit(x, character(0L))[[1]]
        h <- paste(h[c(TRUE,FALSE)], h[c(FALSE,TRUE)], sep="")
        as.raw(as.hexmode(rev(h)))
    })

    ret <- writeBin(length(object), raw(), size=4L, endian="little")
    for(i in 1:length(object)) {
        ret <- c(ret,
            writeBin(tag[i], raw(), size=2L, endian="little"),
            as.raw(subtag[i]),
            hash[[i]])
    }
    ret
}

update_checksums_values <- function(db, x, z, dimension) {
    keys <- .process_key_args(x,z,dimension, tag=59L, stop_if_filtered = TRUE)
    purrr::map(keys, .update_checksums_value_impl, db=db)
    invisible(db)
}

.update_checksums_value_impl <- function(db, key) {
    # 45, 47, 49, 50 all need to be updated if they exist
    stem <- stringr::str_replace(key, ":59$", "")
    chunk_keys <- get_keys(db, stem)
    chunk_keys <- stringr::str_subset(chunk_keys, ":(?:47-[^-:]+|45|49|50)$")

    dat <- get_values(db, chunk_keys)
    obj <- purrr::map(dat, .checksum_impl)
    names(obj) <- trim_stem_from_chunk_key(names(obj))

    val <- write_checksums_value(obj)
    put_value(db, key, val)
    invisible()
}

.checksum_impl <- function(x) {
    digest::digest(x, algo="xxhash64", serialize=FALSE, raw=TRUE)
}



# 45, 47-*, 49, 50
