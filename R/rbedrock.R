##' @export
create_chunk_key <- function(x, z, d, tag, subtag = NA) {
    create_bedrockdb_key(x, z, d, tag, subtag)
}

##' @export
parse_chunk_keys <- function(keys) {
    if (!is.character(keys)) {
        stop("keys must be a character vector.")
    }
    m <- stringr::str_match(keys, "^@([^:]+):([^:]+):([^:]+):([^:-]+)(?:-([^:]+))?$")
    m <- m[!is.na(m[, 1]), ]
    data.frame(key = m[, 1], x = as.integer(m[, 2]), z = as.integer(m[, 3]), dimension = as.integer(m[, 
        4]), tag = as.integer(m[, 5]), subtag = as.integer(m[, 6]), stringsAsFactors = FALSE)
}

##' @export
worlds_path <- function() {
    if (!requireNamespace("rappdirs", quietly = TRUE)) {
        stop("Package \"rappdirs\" needed for worlds_path function to work. Please install it.", call. = FALSE)
    }
    if (.Platform$OS.type == "windows") {
        datadir <- rappdirs::user_data_dir("Packages\\Microsoft.MinecraftUWP_8wekyb3d8bbwe\\LocalState","")
    } else {
        datadir <- rappdirs::user_data_dir("mcpelauncher")
    }
    normalizePath(stringr::str_c(datadir, "/games/com.mojang/minecraftWorlds"))
}

##' @export
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

.fixup_path <- function(path) {
    if (file.exists(path)) {
        path <- normalizePath(path)
    } else {
        wpath <- paste0(worlds_path(), "/", path)
        if (file.exists(wpath)) {
            path <- normalizePath(wpath)
        }
    }
    path
}
