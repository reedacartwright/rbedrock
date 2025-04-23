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
    dat <- expand.grid(x = y, z = y)
    dat <- dat[abs(dat$x) + abs(dat$z) <= sim_distance + 1, , drop = FALSE]
    dat$x <- dat$x + x
    dat$z <- dat$z + z
    tibble::as_tibble(dat)
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
    #  (2) the chunk must not be on the edge of a simulation distance
    #      (except for sim 4)
    if (sim_distance == 4) {
        return(simulation_area(4, x = x, z = z))
    }
    y <- seq.int(-sim_distance + 1, sim_distance - 1)
    dat <- expand.grid(x = y, z = y)
    dat <- dat[abs(dat$x) + abs(dat$z) < sim_distance, , drop = FALSE]
    dat$x <- dat$x + x
    dat$z <- dat$z + z
    tibble::as_tibble(dat)
}
