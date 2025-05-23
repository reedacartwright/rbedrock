% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/blocks-str.R
\name{blocks_str}
\alias{blocks_str}
\alias{blocks_nbt}
\title{Convert block data from nbt format to a string}
\usage{
blocks_str(x, names_only = FALSE)

blocks_nbt(x)
}
\arguments{
\item{x}{block data, either as a list of nbt values or a vector of strings.}

\item{names_only}{A logical scalar. Return only the names of the blocks,
ignoring block properties.}
}
\description{
Block data is stored in nbt format, which includes a block's name and
properties. \code{blocks_str()} converts the nbt data into strings with the
following format: \code{blockname@prop1=val1@prop2=val2}. Blocks can have
zero or more properties. \code{blocks_nbt()} does the inverse operation.
}
