## code to prepare `vanilla_block_states_df` dataset goes here

# find the most recent release of PyMCTranslate
res <- jsonlite::fromJSON("https://api.github.com/repos/gentlegiantJGC/PyMCTranslate/releases/latest") # nolint

# download release
tarfile <- tempfile(fileext = ".tgz")
download.file(res$tarball_url, tarfile)
# find most recent bedrock version
f <- untar(tarfile, list = TRUE)
u <- grep("json/versions/bedrock[^/]+/$", f, value = TRUE)
v <- basename(u)
v <- sub("^bedrock_", "", v)
v <- numeric_version(gsub("_+", ".", v))
bedrock_latest <- u[which(v == max(v))]
# extract files for most recent version
tardir <- tempfile()
untar(tarfile, exdir = tardir, files = bedrock_latest)
# location of extracted files
bedrock_latest <- file.path(tardir, bedrock_latest)
# location of blockstate jsons
vanilla_blockstates <- file.path(bedrock_latest,
    "block/blockstate/specification/minecraft/vanilla") # nolint

# basic information
info <- jsonlite::fromJSON(file.path(bedrock_latest, "__init__.json"))

# list of block files
blockstate_files <- list.files(vanilla_blockstates)

blockstates <- vector("list", length(blockstate_files))
names(blockstates) <- blockstate_files
for (block in blockstate_files) {
    f <- file.path(vanilla_blockstates, block)
    value <- jsonlite::fromJSON(f)
    # identify the type of each property
    props <- vapply(value$properties, FUN.VALUE = character(1L), FUN = \(x) {
        if (all(grepl("^\".+\"$", x))) {
            "str"
        } else if (all(grepl("^[01]b$", x))) {
            "bit"
        } else if (all(grepl("^-?[0-9]+$", x))) {
            "num"
        } else {
            NA_character_
        }
    })
    # convert default values to bare strings
    defs <- vapply(names(props), FUN.VALUE = character(1L), FUN = \(x) {
        def <- value$defaults[[x, exact = TRUE]]
        type <- props[[x, exact = TRUE]]
        if (is.na(type)) {
            NA_character_
        } else if (type == "str") {
            gsub("^\"|\"$", "", def)
        } else if (type == "bit") {
            sub("b$", "", def)
        } else {
            def
        }
    })
    # convert allowed values to bare strings
    allowed <- lapply(names(props), \(x) {
        a <- value$properties[[x, exact = TRUE]]
        type <- props[[x, exact = TRUE]]
        if (is.na(type)) {
            rep(NA_character_, length(a))
        } else if (type == "str") {
            gsub("^\"|\"$", "", a)
        } else if (type == "bit") {
            sub("b$", "", a)
        } else {
            a
        }
    })

    b <- sub("(.+)\\.json$", "minecraft:\\1", block)

    blockstates[[block]] <- tibble::tibble(
        name = b,
        property = names(props),
        type = unname(props),
        default = defs,
        allowed = allowed
    )
}
names(blockstates) <- sub("\\.json$", "", names(blockstates))

# list all blocks
vanilla_block_list <- paste0("minecraft:", names(blockstates))

# Construct a data frame of all block states
vanilla_block_states_df <- do.call(rbind, c(blockstates,
                                            make.row.names = FALSE))
attr(vanilla_block_states_df, "data_version") <- info$data_version

# list all property types
props <- unique(vanilla_block_states_df$property)
m <- match(props, vanilla_block_states_df$property)
types <- vanilla_block_states_df$type[m]
vanilla_block_property_type_list <- setNames(types, props) # nolint

usethis::use_data(vanilla_block_states_df, overwrite = TRUE)
usethis::use_data(vanilla_block_list, overwrite = TRUE)
usethis::use_data(vanilla_block_property_type_list, overwrite = TRUE)
