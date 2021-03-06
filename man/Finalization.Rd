% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/finalization.R
\name{Finalization}
\alias{Finalization}
\alias{get_finalization_data}
\alias{get_finalization_value}
\alias{put_finalization_data}
\alias{put_finalization_values}
\alias{put_finalization_value}
\alias{read_finalization_value}
\alias{write_finalization_value}
\title{Load and store Finalization data}
\usage{
get_finalization_data(db, x, z, dimension)

get_finalization_value(db, x, z, dimension)

put_finalization_data(db, data)

put_finalization_values(db, x, z, dimension, values)

put_finalization_value(db, x, z, dimension, value)

read_finalization_value(rawdata)

write_finalization_value(value)
}
\arguments{
\item{db}{A bedrockdb object.}

\item{x, z, dimension}{Chunk coordinates to extract data from.
\code{x} can also be a character vector of db keys.}

\item{data}{A named-vector of key-value pairs for Finalization data.}

\item{values}{An integer vector}

\item{value}{a scalar integer}

\item{rawdata}{a raw vector}
}
\value{
\code{get_finalization_data()} returns a named integer vector
of the values returned by \code{get_finalization_value()}.

\code{get_finalization_value()} and \code{read_finalization_value()}
return an integer.
}
\description{
Finalization data (tag 54) holds a number which
indicates a chunk's state of generation.

\code{get_finalization_data()} loads Finalization data from a \code{bedrockdb}.
It will silently drop and keys not representing Finalization data.

\code{get_finalization_value()} loads Finalization data from a \code{bedrockdb}.
It only supports loading a single value.

\code{put_finalization_data()}, \code{put_finalization_values()}, and
\code{put_finalization_value()} store Finalization data into a \code{bedrockdb}.

\code{read_finalization_value()} parses a binary Finalization record.

\code{write_finalization_value()} converts a Finalization value
to a raw vector.
}
\details{
Finalization data contains the following information.\tabular{lll}{
   Value \tab Name \tab Description \cr
   0 \tab NeedsInstaticking \tab Chunk needs to be ticked \cr
   1 \tab NeedsPopulation \tab Chunk needs to be populated with mobs \cr
   2 \tab Done \tab Chunk generation is fully complete \cr
}
}
