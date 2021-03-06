% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/entities.R
\name{Entities}
\alias{Entities}
\alias{get_entities_data}
\alias{put_entities_data}
\title{Load and store Entities NBT data}
\usage{
get_entities_data(db, x = get_keys(db), z, dimension)

put_entities_data(db, data)
}
\arguments{
\item{db}{A bedrockdb object.}

\item{x, z, dimension}{Chunk coordinates to extract data from.
\code{x} can also be a character vector of db keys.}

\item{data}{A named-list of key-value pairs for Entities data.}
}
\description{
Entities data (tag 50) holds a list of NBT values for
mobs and other entities in the game.

\code{get_entities_data()} loads Entities data from a \code{bedrockdb}.
It will silently drop and keys not representing Entities data.

\code{put_entities_data()} stores Entities data into a \code{bedrockdb}.
}
