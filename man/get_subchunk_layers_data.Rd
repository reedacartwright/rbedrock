% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/blocks.R
\name{get_subchunk_layers_data}
\alias{get_subchunk_layers_data}
\alias{get_subchunk_layers_values}
\alias{get_subchunk_layers_value}
\alias{get_subchunk_layers_from_chunk}
\alias{put_subchunk_layers_data}
\alias{put_subchunk_layers_values}
\alias{put_subchunk_layers_value}
\alias{read_subchunk_layers_value}
\alias{write_subchunk_layers_value}
\title{Load and store SubchunkBlocks layers}
\usage{
get_subchunk_layers_data(db, x, z, dimension, subchunk)

get_subchunk_layers_values(db, x, z, dimension, subchunk)

get_subchunk_layers_value(db, x, z, dimension, subchunk)

get_subchunk_layers_from_chunk(db, x, z, dimension)

put_subchunk_layers_data(db, data, ...)

put_subchunk_layers_values(db, x, z, dimension, subchunk, values, ...)

put_subchunk_layers_value(db, x, z, dimension, subchunk, value, ...)

read_subchunk_layers_value(rawdata)

write_subchunk_layers_value(object, version = 9L, missing_offset = NA_integer_)
}
\arguments{
\item{db}{A bedrockdb object.}

\item{x, z, dimension}{Chunk coordinates to extract data from.
\code{x} can also be a character vector of db keys.}

\item{subchunk}{Subchunk indexes to extract data from.}

\item{data}{A named-vector of key-value pairs for SubchunkBlocks data.}

\item{values}{A list of lists of 16x16x16 integer indexes with associated
block_palettes.}

\item{value}{A list of 16x16x16 integer indexes with associated
block_palettes.}

\item{missing_offset}{subchunk offset to use if one is not found in \code{rawdata}}
}
\value{
\code{get_subchunk_layers_data()} returns a list of the of the values
returned by \code{read_subchunk_layers_value()}.

\code{get_subchunk_layers_value()} and \code{read_subchunk_layers_value()}
return a list of block layers. Each block layer is a 16x16x16 array of
integers associated with a block palette. The block palette is stored in the
"palette" attribute of the array.

\code{get_subchunk_layers_value()} returns a list of the of the values
returned by \code{read_subchunk_layers_value()}.
}
\description{
\code{get_subchunk_layers_data()} loads SubchunkBlocks data from a \code{bedrockdb}.
It will silently drop and keys not representing SubchunkBlocks data.
\code{get_subchunk_layers_values()} is a synonym for \code{get_subchunk_layers_data()}.

\code{get_subchunk_layers_value()} loads SubchunkBlocks data from a \code{bedrockdb}.
It only supports loading a single value.

\code{get_subchunk_layers_value()} loads SubchunkBlocks data from a \code{bedrockdb}.
It supports efficiently loading subchunk block data from a single chunk.

\code{put_subchunk_layers_data()}, \code{put_subchunk_layers_values()}, and
\code{put_subchunk_layers_value()} store SubchunkBlocks data into a \code{bedrockdb}.

\code{read_subchunk_layers_value()} decodes binary SubchunkBlock data
into index-mapped arrays and associated block palettes.

\code{write_subchunk_layers_value()} encode SubchunkBlock data
into binary form.
}
\keyword{internal}
