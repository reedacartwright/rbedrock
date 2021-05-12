#' Utilities for working with Minecraft world folders.
#'
#' @param id The path to a world folder. If the path is not absolute or does not
#'   exist, it is assumed to be the base name of a world folder in `worlds_dir`.
#'   For `import_world()`, if `id` is `NULL` a unique world id will be
#'   generated. How it is generated is controlled by the
#'   `rbedrock.rand_world_id` global options. Possible values are "pretty" and
#'   "mcpe".
#' @param file The path to an mcworld file. If exporting, it will be created.
#'   If importing, it will be extracted.
#' @param worlds_dir The path of a `minecraftWorlds` directory.
#' @param default If `TRUE`, return most likely world path on the system.
#' @param levelname Rename the imported world to this name.
#' @param replace If `TRUE`, overwrite an existing file if necessary.
#'
#' @return `world_dir_path()` returns the path to the `minecraftWorlds` directory.
#'         You can use `options(rbedrock.worlds_dir_path = "custom/path")` to customize the path
#'         as needed.
#'         `list_worlds()` returns a `data.frame` containing information about Minecraft saved games.
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
    out <- fs::dir_map(worlds_dir, function(f) {
        #skip directories that do not contain a level.dat
        if(!fs::file_exists(fs::path(f, "level.dat"))) {
            return(NULL)
        }
        dat <- read_leveldat(f)
        levelname <- payload(dat$LevelName)
        lastplayed <- as.POSIXct(as.numeric(payload(dat$LastPlayed)), 
            origin = "1970-01-01 00:00:00")
        id <- fs::path_file(f)
        list(id = id, levelname = levelname, last_opened = lastplayed)        
    }, type = "directory")

    out %>% dplyr::bind_rows() %>% 
        dplyr::arrange(dplyr::desc(.data$last_opened))
}

#' @rdname minecraft_worlds
#' @export
export_world <- function(id, file, worlds_dir = worlds_dir_path(),
    replace=FALSE) {
    vec_assert(file, character(), 1L)
    file <- fs::path_abs(file)
    world_path <- .fixup_path(id, worlds_dir, verify=TRUE)
    
    if(fs::file_exists(file)) {
        if(isTRUE(replace) && fs::is_file(file)) {
            fs::file_delete(file)
        } else {
            abort(str_glue("Cannot create '{file}': it already exists."))
        }
    }

    wd <- setwd(world_path)
    on.exit(setwd(wd))
    f <- fs::dir_ls()

    if(is_installed("zip")) {
        ret <- zip::zipr(file, f)
    } else {
        ret <- utils::zip(file, f, flags = "-r9Xq")
    }
    
    inform(str_glue("Success: World '{world_path}' exported to '{file}'."))
    invisible(ret)
}

#' @rdname minecraft_worlds
#' @export
import_world <- function(file, id = NULL, worlds_dir = worlds_dir_path(), levelname = NULL) {
    file <- fs::path_real(file)

    if(is.null(id)) {
        # create a random world directory
        repeat {
            path <- .rand_world_id()
            path <- fs::path(worlds_dir, path)
            # check for collisions
            if(!fs::file_exists(path)) {
                break
            }
        }
    } else {
        path <- .fixup_path(id, worlds_dir)
    }

    if(is_installed("zip")) {
        zip::unzip(file, exdir = path)
    } else {
        utils::unzip(file, exdir = path)
    }

    # update the last opened time to now
    dat <- read_leveldat(path)

    dat$LastPlayed <- nbt_long(as.numeric(Sys.time()))
    # update levelname
    if(!is.null(levelname)) {
        dat$LevelName <- nbt_string(levelname)
    }

    write_leveldat(dat, path)

    inform(str_glue("Success: '{file}' imported to '{path}'."))
    invisible(path)
}

.fixup_path <- function(path, worlds_dir = worlds_dir_path(), verify = FALSE) {
    vec_assert(path, size=1L)
    path <- vec_cast(path, character())

    # if path is absolute, or it already exists don't append it to worlds_dir
    if(!(fs::is_absolute_path(path) || fs::file_exists(path))) {
        path <- fs::path(worlds_dir, path)
    }
    path <- fs::path_abs(path)
    if(verify) {
        if(!fs::dir_exists(path)) {
            abort(str_glue("Folder '{path}' does not exist."))
        }
        f <- c("db", "level.dat", "levelname.txt")
        if(!all(fs::file_exists(fs::path(path, f)))) {
            abort(str_glue("Folder '{path}' does not contain Minecraft data."))
        }
    }
    path
}

.base58 <- c("1", "2", "3", "4", "5", "6", "7", "8", "9", "A", "B", "C", "D",
    "E", "F", "G", "H", "J", "K", "L", "M", "N", "P", "Q", "R", "S", "T", "U",
    "V", "W", "X", "Y", "Z", "a", "b", "c", "d", "e", "f", "g", "h", "i", "j",
    "k", "m", "n", "o", "p", "q", "r", "s", "t", "u", "v", "w", "x", "y", "z")

.rand_world_id <- function() {
    opt <- rbedrock_opt("rand_world_id")
    if(opt == "pretty") {
        return(.rand_world_id_pretty())
    } else if(opt == "mcpe") {
        return(.rand_world_id_mcpe())
    }
    abort(str_glue("Option rand_world_id = '{opt}' is not recognized."))
}

.rand_world_id_pretty <- function() {
    y <- sample(.base58, 10L, replace=TRUE)
    str_c(y, collapse="")
}

.rand_world_id_mcpe <- function() {
    if(!is_installed("jsonlite")) {
        abort("Creating MCPE-like random world id requires the jsonlite package. Please install it and try again.")
    }
    y <- as.raw(sample.int(256L,8L,replace=TRUE)-1L)
    y <- jsonlite::base64_enc(y)
    str_replace(y, "/", "-")
}
