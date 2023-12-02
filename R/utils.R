#' Calculate a player-based simulation area
#'
#' @param sim_distance A sim distance setting
#' @param x,z Chunk coordinates where a player is standing
#'
#' @return A `data.frame` containing the chunk coordinates in the simulation
#'         area.
#' @export
simulation_area <- function(sim_distance, x = 0, z = 0) {
    y <- seq.int(-sim_distance, sim_distance)
    dat <- tidyr::expand_grid(x = y, z = y)
    dat <- dplyr::filter(dat, abs(z) + abs(x) <= sim_distance + 1)
    dplyr::mutate(dat, x = x + !!x, z = z + !!z)
}

#' Calculate a player-based spawning area
#'
#' @param sim_distance A sim distance setting
#' @param x,z Chunk coordinates where a player is standing (can be fractional)
#'
#' @return A `data.frame` containing the chunk coordinates in the spawning area.
#' @export
spawning_area <- function(sim_distance, x = 0, z = 0) {
    # Conditions required for a chunk to spawn mobs:
    #  (1) in a simulation area
    #  (2) the `middle` of the chunk must be withing 96 blocks of a player
    #  (3) the chunk must not be on the edge of a simulation distance
    #      (except for sim 4)
    if (sim_distance == 4) {
        return(simulation_area(4, x = x, z = z))
    }
    y <- seq.int(-sim_distance + 1, sim_distance - 1)
    dat <- tidyr::expand_grid(x = y, z = y)
    dat <- dplyr::filter(dat, abs(z) + abs(x) < sim_distance)
    dat <- dplyr::mutate(dat, x = x + !!x, z = z + !!z)
    dat <- dplyr::mutate(dat, xx = as.integer(16 * x + 7.5) - 16 * !!x,
                         zz = as.integer(16 * z + 7.5) - 16 * !!z)
    dat <- dplyr::filter(dat, .data$xx * .data$xx + .data$zz * .data$zz <
                             9216.0)
    dplyr::select(dat, x, z)
}
