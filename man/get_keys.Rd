% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/data.R
\name{get_keys}
\alias{get_keys}
\title{Get a list of keys stored in a bedrockdb.}
\usage{
get_keys(prefix = NULL, db = default_db(), readoptions = NULL)
}
\arguments{
\item{prefix}{A string specifying chunk prefix or string prefix.}

\item{db}{A \code{bedrockdb} object}

\item{readoptions}{A \code{bedrock_leveldb_readoptions} object}
}
\value{
A vector containing all the keys found in the bedrockdb.

If \code{prefix} is specified, this vector will be filtered for
based on the specified prefix.
}
\description{
Get a list of keys stored in a bedrockdb.
}
