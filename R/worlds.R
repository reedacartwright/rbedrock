#' Utilities for working with Minecraft world folders.
#'
#' @name minecraft_worlds
#' @examples
#' \dontrun{
#'
#' create_world(LevelName = "My World", RandomSeed = 10)
#' }
NULL

#' @description
#' `world_dir_path()` returns the path to the `minecraftWorlds` directory. Use
#' `options(rbedrock.worlds_dir_path = "custom/path")` to customize the path
#' as needed.
#'
#' @param default If `TRUE`, return most likely world path on the system.
#'
#' @rdname minecraft_worlds
#' @export
worlds_dir_path <- function(default = FALSE) {
    if(isTRUE(default)) {
        .worlds_dir_path_def()
    } else {
        rbedrock_opt("worlds_dir_path")
    }
}

#' @description
#' `list_worlds()` returns a `data.frame()` containing information about
#' Minecraft saved games.
#'
#' @param worlds_dir The path of a `minecraftWorlds` directory.
#'
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

#' @description
#' `create_world()` creates a new Minecraft world.
#'
#' @param id The path to a world folder. If the path is not absolute or does not
#'   exist, it is assumed to be the base name of a world folder in `worlds_dir`.
#'   For `import_world()`, if `id` is `NULL` a unique world id will be
#'   generated. How it is generated is controlled by the
#'   `rbedrock.rand_world_id` global options. Possible values are "pretty" and
#'   "mcpe".
#'
#' @param ... Arguments to customize `level.dat` settings.
#'     Supports dynamic dots via `rlang::list2()`.
#'
#' @rdname minecraft_worlds
#' @export
create_world <- function(id = NULL, ..., worlds_dir = worlds_dir_path()) {

    dirpath <- .fixup_or_create_path(id, worlds_dir)
    params <- list2(...)

    params$RandomSeed <- params$RandomSeed %||% .nbt_random_seed()
    params$LastPlayed <- .nbt_now()

    if(fs::file_exists(dirpath)) {
        abort(str_glue("Cannot create '{path}': it already exists."))
    }

    # create world directory and db directory
    dbpath <- fs::path_real(fs::dir_create(fs::path(dirpath, "db")))

    # create db and close
    db <- bedrock_leveldb_open(dbpath, create_if_missing=TRUE)
    bedrock_leveldb_close(db, TRUE)

    dat <- read_leveldat(rbedrock_example("default_level.dat"))

    # apply customizations
    for(n in names(params)) {
        dat[[n]] <- params[[n]]
    }

    write_leveldat(dat, dirpath)

    levelname <- vec_cast(dat$LevelName, character())
    readr::write_file(levelname, fs::path(dirpath, "levelname.txt"))

    inform(str_glue("Success: Minecraft world created at '{dirpath}'."))
    invisible(dirpath)
}

#' @description
#' `export_world()` exports a world to an archive file.
#'
#' @param file The path to an mcworld file. If exporting, it will be created.
#'   If importing, it will be extracted.
#' @param replace If `TRUE`, overwrite an existing file if necessary.
#' @rdname minecraft_worlds
#' @export
export_world <- function(id, file, worlds_dir = worlds_dir_path(),
    replace=FALSE) {
    vec_assert(file, character(), 1L)
    file <- fs::path_abs(file)
    dirpath <- .fixup_path(id, worlds_dir, verify=TRUE)
    
    if(fs::file_exists(file)) {
        if(isTRUE(replace) && fs::is_file(file)) {
            fs::file_delete(file)
        } else {
            abort(str_glue("Cannot create '{file}': it already exists."))
        }
    }

    wd <- setwd(dirpath)
    on.exit(setwd(wd))
    f <- fs::dir_ls()

    if(is_installed("zip")) {
        ret <- zip::zipr(file, f)
    } else {
        ret <- utils::zip(file, f, flags = "-r9Xq")
    }
    
    inform(str_glue("Success: World '{dirpath}' exported to '{file}'."))
    invisible(ret)
}

#' @rdname minecraft_worlds
#' @export
import_world <- function(file, id = NULL, ..., worlds_dir = worlds_dir_path()) {
    file <- fs::path_real(file)
    dirpath <- .fixup_or_create_path(id, worlds_dir)

    params <- list2(...)
    params$LastPlayed <- .nbt_now()

    if(is_installed("zip")) {
        zip::unzip(file, exdir = dirpath)
    } else {
        utils::unzip(file, exdir = dirpath)
    }

    dat <- read_leveldat(dirpath)

    # apply customizations
    for(n in names(params)) {
        dat[[n]] <- params[[n]]
    }

    write_leveldat(dat, dirpath)

    levelname <- vec_cast(dat$LevelName, character())
    readr::write_file(levelname, fs::path(dirpath, "levelname.txt"))

    inform(str_glue("Success: '{file}' imported to '{dirpath}'."))
    invisible(dirpath)
}

#' @rdname minecraft_worlds
#' @export
get_world_path <- function(id, worlds_dir = worlds_dir_path()) {
    .fixup_path(id, worlds_dir = worlds_dir, verify = TRUE)
}

.fixup_path <- function(id, worlds_dir = worlds_dir_path(), verify = FALSE) {
    vec_assert(id, size=1L)
    path <- vec_cast(id, character())

    # if path is absolute, or it already exists don't append it to worlds_dir
    if(!(fs::is_absolute_path(path) || fs::file_exists(path))) {
        path <- fs::path(worlds_dir, path)
    }
    path <- fs::path_abs(path)
    if(verify && fs::is_dir(path)) {
        f <- c("db", "level.dat", "levelname.txt")
        if(!all(fs::file_exists(fs::path(path, f)))) {
            abort(str_glue("Folder '{path}' does not contain Minecraft data."))
        }
    }
    path
}

.fixup_or_create_path <- function(id = NULL, worlds_dir = worlds_dir_path()) {
    if(is.null(id)) {
        # create a random world directory
        repeat {
            path <- .rand_world_id()
            path <- fs::path(worlds_dir, path)
            # check for collisions
            if(!fs::file_exists(path)) {
                return(path)
            }
        }
    }
    .fixup_path(id, worlds_dir)
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

.nbt_now <- function() {
    nbt_long(as.numeric(Sys.time()))
}

.nbt_random_seed <- function() {
    s <- stats::runif(1L,min=-2147483647, max=2147483648)
    storage.mode(s) <- "integer"
    nbt_long(s)
}

#' Compact a world database.
#'
#' @keywords internal
#' @export
compact_world <- function(db) {
    db$compact_range()
}

#' Try to repair a world database.
#'
#' @keywords internal
#' @export
repair_world <- function(id) {
    path <- .fixup_path(id)
    path <- fs::path(path, "db")
    bedrock_leveldb_repair(path)
}
