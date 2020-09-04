.absolute_path <- function(path) {
    file.path(normalizePath(dirname(path)), basename(path))
}

.worlds_dir_path_def <- function() {
    if (.Platform$OS.type == "windows") {
        datadir <- rappdirs::user_data_dir("Packages\\Microsoft.MinecraftUWP_8wekyb3d8bbwe\\LocalState","")
    } else {
        datadir <- rappdirs::user_data_dir("mcpelauncher")
    }
    .absolute_path(file.path(datadir, "games/com.mojang/minecraftWorlds"))
}

op.rbedrock <- list(
    rbedrock.worlds_dir_path = .worlds_dir_path_def()
)

rbedrock_opt <- function(x) {
    x_rbedrock <- paste0("rbedrock.", x)
    res <- getOption(x_rbedrock)
    if (!is.null(res)) {
        return(res)
    }
    op.rbedrock[[x_rbedrock]]
}
