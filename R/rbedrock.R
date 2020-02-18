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
    .create_bedrockdb_key(x, z, d, tag, subtag)
}

#' Extract information from chunk keys.
#'
#' @param keys A character vector of database keys.
#' @return A data.frame containing information extracted from chunk keys. Keys that do not contain chunk data are dropped.
#' @examples
#' parse_chunk_keys("@@0:0:0:47-1")
#' @export
parse_chunk_keys <- function(keys, fancy = .befancy()) {
    if (!is.character(keys)) {
        stop("keys must be a character vector.")
    }
    m <- stringr::str_match(keys, "^@([^:]+):([^:]+):([^:]+):([^:-]+)(?:-([^:]+))?$")
    m <- m[!is.na(m[, 1]), ]
    out <- data.frame(key = m[, 1],
        x = as.integer(m[, 2]),
        z = as.integer(m[, 3]),
        dimension = as.integer(m[, 4]),
        tag = as.integer(m[, 5]),
        subtag = as.integer(m[, 6]),
        stringsAsFactors = FALSE)
    if(!fancy) {
        return(out)
    }
    out <- tibble::as_tibble(out)
    out$tag <- factor(out$tag, levels=c(45:58, 118),
        labels=c("2DMaps",
                 "2DMapsLegacy",
                 "SubchunkBlocks",
                 "48", # removed
                 "BlockEntities",
                 "Entities",
                 "PendingBlockTicks",
                 "52", # removed
                 "BiomeStates",
                 "Finalization",
                 "55", # removed
                 "BorderBlocks", # Education edition
                 "HardcodedSpawnAreas",
                 "RandomBlockTicks",
                 "ChunkVersion"
        ))
    out
}

#' The local minecraftWorlds directory.
#'
#' Requires the \code{rappdirs} package.
#' @return The likely path to the local minecraftWorlds directory.
#' @export
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

#' List the worlds in the minecraftWorlds directory.
#'
#' @param dir The path of the minecraftWorlds directory. It defaults to the likely path.
#' @return A data.frame containing information about Minecraft worlds.
#' @export
list_worlds <- function(dir = worlds_path(), fancy = .befancy()) {
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
    out <- data.frame(folder = world_folders[o],
                      name = world_names[o],
                      last_opened = world_times[o],
                      stringsAsFactors = FALSE)
    if(fancy) {
        out <- tibble::as_tibble(out)
    }
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

.befancy <- function() {
    requireNamespace("tibble", quietly=TRUE)
}
