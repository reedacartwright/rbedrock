% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/utils.R
\name{simulation_area}
\alias{simulation_area}
\title{Calculate a player-based simulation area}
\usage{
simulation_area(sim_distance, x = 0, z = 0)
}
\arguments{
\item{sim_distance}{A sim distance setting}

\item{x, z}{Chunk coordinates where a player is standing}
}
\value{
A \code{data.frame} containing the chunk coordinates in the simulation
area.
}
\description{
Calculate a player-based simulation area
}
