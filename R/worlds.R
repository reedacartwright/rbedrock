#' Utilities for working with Minecraft world folders.
#'
#' @param id The path to a world folder. If the path does not exist, it is 
#'   assumed to be the base name of a world folder in `worlds_dir`.
#' @param file The path to an mcworld file. If exporting, it will be created
#'   and overwritten if it exists. If importing, it will be extracted.
#' @param worlds_dir The path of a `minecraftWorlds` directory.
#' @param default If `TRUE`, return most likely world path on the system.
#' @param levelname Rename the imported world to this name.
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

    out <- purrr::map_dfr(folders, function(f) { 
        dat <- read_leveldat(f)
        levelname <- payload(dat$LevelName)
        lastplayed <- as.POSIXct(as.numeric(payload(dat$LastPlayed)), 
            origin = "1970-01-01")
        id <- basename(f)
        list(id = id, levelname = levelname, last_opened = lastplayed)
    })

    dplyr::arrange(out, dplyr::desc(.data$last_opened))
}

#' @rdname minecraft_worlds
#' @export
export_world <- function(id, file, worlds_dir = worlds_dir_path()) {
    stopifnot(is.character(file) && length(file) == 1L && !is.na(file))

    world_path <- .fixup_path(id, worlds_dir, verify=TRUE)
    
    file <- .absolute_path(file)

    if(file.exists(file)) {
        stopifnot(!dir.exists(file))
        file.remove(file)
    }

    wd <- getwd()
    setwd(world_path)
    f <- list.files()

    if (!requireNamespace("zip", quietly = TRUE)) {
        ret <- utils::zip(file, f, flags = "-r9Xq")
    } else {
        ret <- zip::zipr(file, f)
    }

    setwd(wd)
    invisible(ret)
}

#' @rdname minecraft_worlds
#' @export
import_world <- function(file, worlds_dir = worlds_dir_path(), levelname = NULL) {
    file <- normalizePath(file)

    stopifnot(file.exists(file))

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
        utils::unzip(file, exdir = path)
    } else {
        zip::unzip(file, exdir = path)
    }

    # update the last opened time to now
    dat <- read_leveldat(path)

    dat$LastPlayed <- nbt_long(as.numeric(Sys.time()))
    # update levelname
    if(is.character(levelname) && length(levelname) == 1L && !is.na(levelname)) {
        dat$LevelName <- nbt_string(levelname)
    }

    write_leveldat(dat, path)

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