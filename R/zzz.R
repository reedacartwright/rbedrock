.onLoad <- function(lib, pkg) {
    # Apply default options
    opt <- options()
    opt_rbedrock <- list(
        rbedrock.worlds_dir_path = NULL,
        rbedrock.rand_world_id = "pretty"
    )
    to_set <- !(names(opt_rbedrock) %in% names(opt))
    if (any(to_set)) {
        options(opt_rbedrock[to_set])
    }

    invisible()
}
