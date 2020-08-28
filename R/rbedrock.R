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

#' Utilities for working with Minecraft world folders.
#'
#' @param world_folder The path to a world folder. If the path does not exist, it is 
#'   assumed to be the base name of a world folder in \code{worlds_dir}.
#' @param mcworld_path The path to an mcworld file. If exporting, it will be created
#'   and overwritten if it exists. If importing, it will be extracted.
#' @param worlds_dir The path of a \code{minecraftWorlds} directory.
#'
#' @return \code{world_dir_path} returns the most likely path to the \code{minecraftWorlds} directory.
#'         \code{list_worlds} returns a data.frame containing information about Minecraft saved games.
#'
#' @name minecraft_worlds
NULL

#' @rdname minecraft_worlds
#' @export
worlds_dir_path <- function() {
    if (.Platform$OS.type == "windows") {
        datadir <- rappdirs::user_data_dir("Packages\\Microsoft.MinecraftUWP_8wekyb3d8bbwe\\LocalState","")
    } else {
        datadir <- rappdirs::user_data_dir("mcpelauncher")
    }
    normalizePath(file.path(datadir, "games/com.mojang/minecraftWorlds"))
}

#' @rdname minecraft_worlds
#' @export
list_worlds <- function(worlds_dir = worlds_dir_path()) {
    folders <- list.dirs(path = worlds_dir, full.names = TRUE, recursive = FALSE)
    world_names <- character()
    world_times <- .POSIXct(numeric())
    world_folders <- character()
    for (folder in folders) {
        levelname <- file.path(folder, "levelname.txt")
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

#' @rdname minecraft_worlds
#' @export
export_world <- function(world_folder, mcworld_path, worlds_dir = worlds_dir_path()) {
    stopifnot(length(mcworld_path) == 1)

    world_path <- .fixup_path(world_folder, verify=TRUE)
    
    mcworld_path <- .absolute_path(mcworld_path)

    if(file.exists(mcworld_path)) {
        stopifnot(!dir.exists(mcworld_path))
        file.remove(mcworld_path)
    }

    wd <- getwd()
    setwd(world_path)
    f <- list.files()

    if (!requireNamespace("zip", quietly = TRUE)) {
        ret <- utils::zip(mcworld_path, f, flags = "-r9Xq")
    } else {
        ret <- zip::zipr(mcworld_path, f)
    }

    setwd(wd)
    invisible(ret)
}

#' @rdname minecraft_worlds
#' @export
import_world <- function(mcworld_path, worlds_dir = worlds_dir_path()) {
    mcworld_path <- normalizePath(mcworld_path)

    stopifnot(file.exists(mcworld_path))

    # create a random world directory
    while(TRUE) {
        y <- as.raw(sample.int(256L,8L,replace=TRUE)-1L)
        path <- jsonlite::base64_enc(y)
        path <- stringr::str_replace(path, "/", "-")
        ret <- path
        path <- file.path(worlds_dir, path)
        # check for collisions
        if(!file.exists(path)) {
            break
        }
    }

    if (!requireNamespace("zip", quietly = TRUE)) {
        utils::unzip(mcworld_path, exdir = path)
    } else {
        zip::unzip(mcworld_path, exdir = path)
    }

    invisible(ret)
}

.fixup_path <- function(path, verify=FALSE) {
    stopifnot(length(path) == 1)

    if (file.exists(path)) {
        path <- normalizePath(path)
    } else {
        wpath <- file.path(worlds_dir_path(), path)
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

.absolute_path <- function(path) {
    file.path(normalizePath(dirname(path)), basename(path))
}

