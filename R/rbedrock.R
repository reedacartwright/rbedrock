#' rbedrock: A package for the analysis and manipulation of Minecraft: Bedrock Edition worlds.
#'
#' @docType package
#' @name rbedrock
NULL

#' Create a database key from chunk information.
#'
#' @param x Chunk x-coordinate.
#' @param z Chunk z-coordinate.
#' @param d Chunk dimension.
#' @param tag The type of information the key holds.
#' @param subtag The subchunk the key refers to. (Only used if \code{tag==47}).
#' @return The database key corresponding to the inputs.
#' @examples
#' create_chunk_key(0, 0, 0, 47, 1)
#' @export
create_chunk_key <- function(x, z, d, tag, subtag = NA) {
    if(is.character(tag)) {
        tag <- chunk_tag_as_int(tag)
    }
    .create_strkey(x, z, d, tag, subtag)
}

#' Extract information from chunk keys.
#'
#' @param keys A character vector of database keys.
#' @return A tibble containing information extracted from chunk keys. Keys that do not contain chunk data are dropped.
#' @examples
#' parse_chunk_keys("@@0:0:0:47-1")
#' @export
parse_chunk_keys <- function(keys) {
    if (!is.character(keys)) {
        stop("keys must be a character vector.")
    }
    m <- keys %>% subset_chunk_keys() %>% split_chunk_keys()

    tibble::tibble(key = m[, 1],
        x = as.integer(m[, 2]),
        z = as.integer(m[, 3]),
        dimension = as.integer(m[, 4]),
        tag = chunk_tag(m[, 5]),
        subtag = as.integer(m[, 6]),
    )
}

#' The local minecraftWorlds directory.
#'
#' @return The likely path to the local minecraftWorlds directory.
#' @export
worlds_path <- function() {
    if (.Platform$OS.type == "windows") {
        datadir <- rappdirs::user_data_dir("Packages\\Microsoft.MinecraftUWP_8wekyb3d8bbwe\\LocalState","")
    } else {
        datadir <- rappdirs::user_data_dir("mcpelauncher")
    }
    normalizePath(stringr::str_c(datadir, "/games/com.mojang/minecraftWorlds"))
}

#' List the worlds in the minecraftWorlds directory.
#'
#' @param dir The path of the minecraftWorlds directory. It defaults to the likely path.
#' @return A data.frame containing information about Minecraft worlds.
#' @export
list_worlds <- function(dir = worlds_path()) {
    folders <- list.dirs(path = dir, full.names = TRUE, recursive = FALSE)
    world_names <- character()
    world_times <- .POSIXct(numeric())
    world_folders <- character()
    for (folder in folders) {
        levelname <- stringr::str_c(folder, "/levelname.txt")
        if (!file.exists(levelname)) {
            next
        }
        world_times <- c(world_times, file.mtime(levelname))
        world_names <- c(world_names, readLines(levelname, 1L, warn = FALSE))
        world_folders <- c(world_folders, basename(folder))
    }
    o <- rev(order(world_times))
    out <- tibble::tibble(
            folder = world_folders[o],
            name = world_names[o],
            last_opened = world_times[o],
        )
    out
}


#' @export
export_world <- function(path, output) {
    path <- .fixup_path(path, verify=TRUE)
    if(length(output) != 1) {
        stop("output must be a scalar")
    }
    if(file.exists(output)) {
        if(dir.exists(output)) {
            stop("output exists, but is a directory")
        }
        file.remove(output)
    }
    wd <- getwd()
    setwd(path)
    f <- list.files()

    if (!requireNamespace("zip", quietly = TRUE)) {
        ret <- utils::zip(output, f, flags = "-r9Xq")
    } else {
        ret <- zip::zipr(output, f)
    }

    setwd(wd)
    invisible(ret)
}

.fixup_path <- function(path, verify=FALSE) {
    if(length(path) != 1) {
        stop("path must be a scalar")
    }
    if (file.exists(path)) {
        path <- normalizePath(path)
    } else {
        wpath <- file.path(worlds_path(), path)
        if (file.exists(wpath)) {
            path <- normalizePath(wpath)
        }
    }
    if(verify) {
        f <- c("db", "level.dat", "levelname.txt")
        if(!all(file.exists(file.path(path, f)))) {
            stop("world folder does not appear to contain Minecraft data")
        }
    }
    path
}


