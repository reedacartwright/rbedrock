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
#' @return A tibble containing information extracted from chunk keys. Keys that do not contain chunk data are dropped.
#' @examples
#' parse_chunk_keys("@@0:0:0:47-1")
#' @export
parse_chunk_keys <- function(keys) {
    if (!is.character(keys)) {
        stop("keys must be a character vector.")
    }
    components <- stringr::str_match(keys, "^@([^:]+):([^:]+):([^:]+):([^:-]+)(?:-([^:]+))?$")
    rows <- which(!is.na(components[, 1]))
    m <- m[rows, ]

    tibble::tibble(key = m[, 1],
        x = as.integer(m[, 2]),
        z = as.integer(m[, 3]),
        dimension = as.integer(m[, 4]),
        tag = chunk_tag_str(m[, 5]),
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
    out <- tibble::tibble(folder = world_folders[o],
                      name = world_names[o],
                      last_opened = world_times[o],
                      )
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

chunk_tag_str <- function(tags) {
    dplyr::recode(tags,
        `45` = "2DMaps",
        `46` = "2DMapsLegacy",
        `47` = "SubchunkBlocks",
        `48` = "48", # removed
        `49` = "BlockEntities",
        `50` = "Entities",
        `51` = "PendingBlockTicks",
        `52` = "52", # removed
        `53` = "BiomeStates",
        `54` = "Finalization",
        `55` = "55", # removed
        `56` = "BorderBlocks", # Education edition
        `57` = "HardcodedSpawnAreas",
        `58` = "RandomBlockTicks",
        `118` = "ChunkVersion"
    )
}