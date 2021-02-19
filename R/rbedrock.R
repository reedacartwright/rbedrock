#' rbedrock: A package for the analysis and manipulation of Minecraft:
#' Bedrock Edition worlds.
#'
#' rbedrock is an extension package for R that supports the analysis
#' and management of Minecraft (Bedrock Edition) worlds. This includes
#' Windows 10, Pocket Edition, XBox, and PS4 versions of the game. It
#' does not include Minecraft: Java worlds. @docType package
#' @name rbedrock
NULL

#' Utilities for working with Minecraft world folders.
#'
#' @param world_folder The path to a world folder. If the path does not exist, it is 
#'   assumed to be the base name of a world folder in `worlds_dir`.
#' @param mcworld_path The path to an mcworld file. If exporting, it will be created
#'   and overwritten if it exists. If importing, it will be extracted.
#' @param worlds_dir The path of a `minecraftWorlds` directory.
#' @param default If `TRUE`, return most likely world path on the system.
#'
#' @return `world_dir_path` returns the path to the `minecraftWorlds` directory.
#'         You can use `options(rbedrock.worlds_dir_path = "custom/path")` to customize the path
#'         as needed.
#'         `list_worlds` returns a `data.frame` containing information about Minecraft saved games.
#'
#' @name minecraft_worlds
NULL

#' @rdname minecraft_worlds
#' @export
worlds_dir_path <- function(default = FALSE) {
    if(default) {
        .worlds_dir_path_def()
    } else {
        rbedrock_opt("worlds_dir_path")
    }
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

    world_path <- .fixup_path(world_folder, worlds_dir, verify=TRUE)
    
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

.fixup_path <- function(path, worlds_dir = worlds_dir_path(), verify=FALSE) {
    stopifnot(length(path) == 1)

    if (file.exists(path)) {
        path <- normalizePath(path)
    } else {
        wpath <- file.path(worlds_dir, path)
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



