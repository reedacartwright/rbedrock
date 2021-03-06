% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/data.R
\name{put_values}
\alias{put_values}
\alias{put_value}
\alias{put_data}
\title{Write values to a bedrockdb.}
\usage{
put_values(db, keys, values, writeoptions = NULL)

put_value(db, key, value, writeoptions = NULL)

put_data(db, data, writeoptions = NULL)
}
\arguments{
\item{db}{A \code{bedrockdb} object}

\item{keys}{A character vector of keys.}

\item{values}{A list of raw values.}

\item{writeoptions}{A \code{bedrock_leveldb_writeoptions} object}

\item{key}{A key that will be used to store data.}

\item{value}{A raw vector that contains the information to be written.}

\item{data}{A named-list of raw values, specifying key-value pairs.}
}
\value{
An invisible copy of \code{db}.
}
\description{
Write values to a bedrockdb.
}
