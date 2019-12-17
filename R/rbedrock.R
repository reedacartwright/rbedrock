create_chunk_key <- function(x, z, d, tag, subtag = NA) {
    create_bedrockdb_key(x, z, d, tag, subtag)
}

parse_chunk_keys <- function(keys) {
    if (!is.character(keys)) {
        stop("keys must be a character vector.")
    }
    m <- stringr::str_match(keys, "^@([^:]+):([^:]+):([^:]+):([^:-]+)(?:-([^:]+))?$")
    m <- m[!is.na(m[, 1]), ]
    data.frame(key = m[, 1], x = as.integer(m[, 2]), z = as.integer(m[, 3]), dimension = as.integer(m[, 
        4]), tag = as.integer(m[, 5]), subtag = as.integer(m[, 6]), stringsAsFactors = FALSE)
}

get_hsa <- function(db, keys) {
    if (!is.character(keys)) {
        stop("'keys' must be a character vector.")
    }
    keys <- stringr::str_subset(keys, "^@[^:]+:[^:]+:[^:]+:57$")
    dat <- db$mget(keys, as_raw = TRUE)
    out <- lapply(dat, .read_hsa)
    do.call("rbind", out)
}

put_hsa <- function(db, x1, y1, z1, x2, y2, z2, tag, dimension) {
    # identify all chunks that this HSA overlaps
    x <- range(x1, x2)
    y <- range(y1, y2)
    z <- range(z1, z2)
    chunk_x1 <- x[1] %/% 16
    chunk_z1 <- z[1] %/% 16
    chunk_x2 <- x[2] %/% 16
    chunk_z2 <- z[2] %/% 16
    # create hsa for each chunk
    ret <- NULL
    for (chunk_x in seq.int(chunk_x1, chunk_x2)) {
        for (chunk_z in seq.int(chunk_z1, chunk_z2)) {
            hx1 <- max(x[1], chunk_x * 16)
            hx2 <- min(x[2], chunk_x * 16 + 15)
            hz1 <- max(z[1], chunk_z * 16)
            hz2 <- min(z[2], chunk_z * 16 + 15)
            hy1 <- y[1]
            hy2 <- y[2]
            hsa <- matrix(c(hx1, hy1, hz1, hx2, hy2, hz2, tag), 1, 7)
            ret <- rbind(ret, hsa)
            key <- create_chunk_key(chunk_x, chunk_z, dimension, 57)
            dat <- db$get(key, as_raw = TRUE)
            if (!is.null(dat)) {
                hsa <- rbind(.read_hsa(dat)[, 1:7], hsa)
            }
            hsa <- .write_hsa(hsa)
            db$put(key, hsa)
        }
    }
    ret
}

.read_hsa <- function(val) {
    len <- readBin(val[1:4], integer(), n = 1, size = 4)
    out <- c()
    for (i in 1:len) {
        pos <- 4 + (i - 1) * 25 + 1
        aabb <- readBin(val[seq.int(pos, pos + 23)], integer(), n = 6, size = 4)
        tag <- readBin(val[pos + 24], integer(), 1, 1)
        out <- rbind(out, c(aabb, tag))
    }
    colnames(out) <- c("x1", "y1", "z1", "x2", "y2", "z2", "tag")
    out <- cbind(out, xspot = out[, "x1"] + (out[, "x2"] - out[, "x1"] + 1) %/% 2)
    out <- cbind(out, yspot = pmin.int(out[, "y1"], out[, "y2"]))
    out <- cbind(out, zspot = out[, "z1"] + (out[, "z2"] - out[, "z1"] + 1) %/% 2)
    rownames(out) <- NULL
    out
}

.write_hsa <- function(hsa) {
    len <- nrow(hsa)
    out <- writeBin(as.integer(len), raw(), size = 4, endian = "little")
    for (i in 1:len) {
        n <- as.integer(hsa[i, ])
        out <- c(out, writeBin(n[1:6], raw(), size = 4, endian = "little"))
        out <- c(out, writeBin(n[7], raw(), size = 1, endian = "little"))
    }
    out
}

worlds_path <- function() {
    if (.Platform$OS.type == "windows") {
        datadir <- rappdirs::user_data_dir("Packages\\Microsoft.MinecraftUWP_8wekyb3d8bbwe\\LocalState","")
    } else {
        datadir <- rappdirs::user_data_dir("mcpelauncher")
    }
    normalizePath(stringr::str_c(datadir, "/games/com.mojang/minecraftWorlds"))
}

list_worlds <- function(dir = worlds_path()) {
    folders <- list.dirs(path = dir, full.names = TRUE, recursive = FALSE)
    out <- NULL
    for (folder in folders) {
        levelname <- stringr::str_c(folder, "/levelname.txt")
        if (!file.exists(levelname)) {
            next
        }
        mtime <- as.character(file.mtime(levelname))
        name <- readLines(levelname, 1L, warn = FALSE)
        out <- rbind(out, c(basename(folder), name, mtime))
    }
    colnames(out) <- c("folder", "levelname", "mtime")
    out
}
