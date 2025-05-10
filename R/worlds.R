#' Utilities for working with Minecraft world folders.
#'
#' @name minecraft_worlds
#' @examples
#' \dontrun{
#' create_world(LevelName = "My World", RandomSeed = 10)
#' }
NULL

#' @description
#' `world_dir_path()` returns the path to the `minecraftWorlds` directory. Use
#' `options(rbedrock.worlds_dir_path = "custom/path")` to customize the path
#' as needed.
#'
#' @param force_default If `TRUE`, return most likely world path on the system.
#'
#' @rdname minecraft_worlds
#' @export
worlds_dir_path <- function(force_default = FALSE) {
    opt <- getOption("rbedrock.worlds_dir_path")
    if (is_null(opt) || isTRUE(force_default)) {
        if (.Platform$OS.type == "windows") {
            datadir <- rappdirs::user_data_dir(
                "Packages\\Microsoft.MinecraftUWP_8wekyb3d8bbwe\\LocalState", # nolint: indentation_linter
                "")
        } else {
            datadir <- rappdirs::user_data_dir("mcpelauncher")
        }
        opt <- normalize_path(datadir, "games/com.mojang/minecraftWorlds")
    }
    opt
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
    dirs <- list.files(worlds_dir, full.names = TRUE)
    out <- vector("list", length(dirs))
    for (i in seq_along(dirs)) {
        f <- dirs[[i]]
        #skip directories that do not contain a level.dat
        if (!file_exists(normalize_path(f, "level.dat"))) {
            next
        }
        dat <- unnbt(read_leveldat(f))
        levelname <- dat$LevelName
        lastplayed <- as.POSIXct(as.numeric(dat$LastPlayed),
                                 origin = "1970-01-01 00:00:00")
        id <- basename(f)
        out[[i]] <- data.frame(id = id, levelname = levelname,
                               last_opened = lastplayed)
    }
    out <- do.call(rbind, out)
    out <- out[order(out$last_opened, decreasing = TRUE), , drop = FALSE]
    tibble::as_tibble(out)
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
    dirpath <- fixup_or_create_path(id, worlds_dir)
    params <- list2(...)

    params$RandomSeed <- params$RandomSeed %||% nbt_random_seed()
    params$LastPlayed <- nbt_now()

    if (file_exists(dirpath)) {
        stop(paste0("Cannot create '", dirpath, "': it already exists."))
    }

    # create world directory and db directory
    dbpath <- normalize_path(dirpath, "db")
    dir.create(dbpath, recursive = TRUE)

    # create db and close
    db <- bedrock_leveldb_open(dbpath, create_if_missing = TRUE)
    bedrock_leveldb_close(db, TRUE)

    dat <- read_leveldat(rbedrock_example("default_level.dat"))

    # apply customizations
    for (n in names(params)) {
        dat[[n]] <- params[[n]]
    }

    write_leveldat(dat, dirpath)

    levelname <- as.character(dat$LevelName)
    writeLines(levelname, file_path(dirpath, "levelname.txt"))

    msg <- paste0("Success: Minecraft world created at '", dirpath, "'.")
    message(msg)
    invisible(dirpath)
}
#### RULES FOR CREATING DEFAULT LEVEL DAT ####
# nolint start: commented_code_linter
# RandomSeed <- 0
# SpawnX <- NA
# SpawnY <- NA
# SpawnZ <- NA
# Time <- 0
# CurrentTick <- 0
# worldStartCount <- 4294967295
# LevelName <- "My RBedrock World"
# LastPlayed <- 0
# nolint end

#' @description
#' `export_world()` exports a world to an archive file.
#'
#' @param file The path to an mcworld file. If exporting, it will be created.
#'   If importing, it will be extracted.
#' @param replace If `TRUE`, overwrite an existing file if necessary.
#' @rdname minecraft_worlds
#' @export
export_world <- function(id, file, worlds_dir = worlds_dir_path(),
                         replace = FALSE) {
    stopifnot(is.character(file) && length(file) == 1L)
    file <- normalize_path(file)
    dirpath <- fixup_path(id, worlds_dir, verify = TRUE)

    if (path_exists(file)) {
        if (isTRUE(replace) && file_exists(file)) {
            file.remove(file)
        } else {
            msg <- paste0("Cannot create '", file, "': it already exists.")
            stop(msg)
        }
    }

    wd <- setwd(dirpath)
    on.exit(setwd(wd))
    f <- list.files()

    if (is_installed("zip")) {
        ret <- zip::zipr(file, f)
    } else {
        ret <- utils::zip(file, f, flags = "-r9Xq")
    }

    msg <- paste0("Success: World '", dirpath, "' exported to '", file, "'.")
    message(msg)
    invisible(ret)
}

#' @rdname minecraft_worlds
#' @export
import_world <- function(file, id = NULL, ..., worlds_dir = worlds_dir_path()) {
    file <- normalize_path(file)
    dirpath <- fixup_or_create_path(id, worlds_dir)

    params <- list2(...)
    params$LastPlayed <- nbt_now()

    if (is_installed("zip")) {
        zip::unzip(file, exdir = dirpath)
    } else {
        utils::unzip(file, exdir = dirpath)
    }

    dat <- read_leveldat(dirpath)

    # apply customizations
    for (n in names(params)) {
        dat[[n]] <- params[[n]]
    }

    write_leveldat(dat, dirpath)

    levelname <- as.character(dat$LevelName)
    writeLines(levelname, normalize_path(dirpath, "levelname.txt"))

    msg <- paste0("Success: '", file, "' imported to '", dirpath, "'.")
    message(msg)
    invisible(dirpath)
}

#' @rdname minecraft_worlds
#' @export
get_world_path <- function(id, worlds_dir = worlds_dir_path()) {
    fixup_path(id, worlds_dir = worlds_dir, verify = TRUE)
}

fixup_path <- function(id, worlds_dir = worlds_dir_path(), verify = FALSE) {
    stopifnot(length(id) == 1L)
    path <- as.character(id)

    # if path is absolute, or it already exists don't append it to worlds_dir
    if (is_abs_path(path) || path_exists(path)) {
        # if path doesn't exist, normalize_path won't work right. Normalize
        # its directory instead.
        dir <- dirname(path)
        path <- basename(path)
        path <- file_path(normalize_path(dir), path)
    } else {
        path <- file_path(normalize_path(worlds_dir), path)
    }
    if (verify && dir_exists(path)) {
        f <- c("db", "level.dat", "levelname.txt")
        if (!all(path_exists(path, f))) {
            msg <- paste0("Folder '", path,
                          "' does not contain Minecraft data.")
            stop(msg)
        }
    }
    path
}

fixup_or_create_path <- function(id = NULL, worlds_dir = worlds_dir_path()) {
    if (is.null(id)) {
        # create a random world directory
        repeat {
            path <- fixup_path(rand_world_id(), worlds_dir)
            # check for collisions
            if (!path_exists(path)) {
                return(path)
            }
        }
    }
    fixup_path(id, worlds_dir)
}

.base58 <- c("1", "2", "3", "4", "5", "6", "7", "8", "9", "A", "B", "C", "D",
             "E", "F", "G", "H", "J", "K", "L", "M", "N", "P", "Q", "R", "S",
             "T", "U", "V", "W", "X", "Y", "Z", "a", "b", "c", "d", "e", "f",
             "g", "h", "i", "j", "k", "m", "n", "o", "p", "q", "r", "s", "t",
             "u", "v", "w", "x", "y", "z")

rand_world_id <- function() {
    opt <- getOption("rbedrock.rand_world_id")
    if (opt == "pretty") {
        return(rand_world_id_pretty())
    } else if (opt == "mcpe") {
        return(rand_world_id_mcpe())
    }
    msg <- paste0("Option rand_world_id = '", opt, "' is not recognized.")
    stop(msg)
}

rand_world_id_pretty <- function() {
    y <- sample(.base58, 10L, replace = TRUE)
    paste0(y, collapse = "")
}

rand_world_id_mcpe <- function() {
    if (!is_installed("jsonlite")) {
        msg <- "Creating MCPE-like random world id requires the jsonlite package. Please install it and try again." # nolint
        stop(msg)
    }
    y <- as.raw(sample.int(256L, 8L, replace = TRUE) - 1L)
    y <- jsonlite::base64_enc(y)
    gsub("/", "-", y)
}

nbt_now <- function() {
    nbt_long(as.character(as.integer(Sys.time())))
}

nbt_random_seed <- function() {
    s <- bit64::runif64(1)
    nbt_long(as.character(s))
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
    path <- fixup_path(id)
    path <- file_path(path, "db")
    bedrock_leveldb_repair(path)
}
