% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/pending_block_ticks.R
\name{PendingBlockTicks}
\alias{PendingBlockTicks}
\alias{get_pending_block_ticks_data}
\alias{put_pending_block_ticks_data}
\title{Load and store PendingBlockTicks NBT data}
\usage{
get_pending_block_ticks_data(db, x = get_keys(db), z, dimension)

put_pending_block_ticks_data(db, data)
}
\arguments{
\item{db}{A bedrockdb object.}

\item{x, z, dimension}{Chunk coordinates to extract data from.
\code{x} can also be a character vector of db keys.}

\item{data}{A named-list of key-value pairs for PendingBlockTicks data.}
}
\description{
PendingBlockTicks data (tag 51) holds a list of NBT values for
pending ticks.

\code{get_pending_block_ticks_data()} loads PendingBlockTicks data from a \code{bedrockdb}.
It will silently drop and keys not representing PendingBlockTicks data.

\code{put_pending_block_ticks_data()} stores PendingBlockTicks data into a \code{bedrockdb}.
}
