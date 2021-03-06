% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/maps2d.R
\name{Maps2D}
\alias{Maps2D}
\alias{get_2dmaps_data}
\alias{get_2dmaps_value}
\alias{read_2dmaps_value}
\alias{put_2dmaps_data}
\alias{put_2dmaps_values}
\alias{put_2dmaps_value}
\alias{write_2dmaps_value}
\title{Read and write 2DMaps data}
\usage{
get_2dmaps_data(db, x, z, dimension)

get_2dmaps_value(db, x, z, dimension)

read_2dmaps_value(rawdata)

put_2dmaps_data(db, data)

put_2dmaps_values(db, x, z, dimension, height_maps, biome_maps)

put_2dmaps_value(db, x, z, dimension, height_map, biome_map)

write_2dmaps_value(height_map, biome_map)
}
\arguments{
\item{db}{A bedrockdb object.}

\item{x, z, dimension}{Chunk coordinates to extract data from.
\code{x} can also be a character vector of db keys.}

\item{rawdata}{A raw vector.}

\item{data}{A named-vector of key-value pairs for 2DMaps data.}

\item{height_maps, biome_maps}{Lists of height and biome data.
Values will be recycled if necessary to match the number of keys
to be written to. If \code{biome_maps} is missing, \code{height_maps} should
be in the same format as returned by \code{get_2dmaps_data()}.}

\item{height_map, biome_map}{16x16 arrays containing height and biome data.
Values will be recycled if necessary. If \code{biome_map} is missing, \code{height-map}
should be a list a \code{list()} with both "height_map" and "biome_map" elements.}
}
\value{
\code{get_2dmaps_data()} returns a list of the of the values returned by
\code{get_2dmaps_value()}.

\code{get_2dmaps_values()} returns a list with components "height_map"
and "biome_map".
}
\description{
2DMaps data (tag 45) stores information about surface heights and biomes in a
chunk. 2DMaps data is 768 bytes long and consists of a 256 int16s (heights)
followed by 256 uint8s (biomes).

\code{get_2dmaps_data()} loads 2DMaps data from a \code{bedrockdb}.
It will silently drop and keys not representing 2DMaps data.

\code{get_2dmaps_value()} loads 2DMaps data from a \code{bedrockdb}.
It only supports loading a single value.

\code{read_2dmaps_value} decodes binary 2DMaps data.

\code{put_2dmaps_data()}, \code{put_2dmaps_values()}, and
\code{put_2dmaps_value()} store 2DMaps data into a \code{bedrockdb}.

\code{write_2dmaps_value} encodes 2DMaps data into a raw vector.
}
\examples{
heights <- matrix(63,16,16)
biomes <- matrix(1,16,16)
# Pass heights and biomes as separate parameters
dat <- write_2dmaps_value(heights, biomes)
# Pass them as a list.
obj <- list(height_map = heights, biome_map = biomes)
dat <- write_2dmaps_value(obj)
# Pass them as scalars
dat <- write_2dmaps_value(63, 1)
}
