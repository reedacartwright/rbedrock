% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/worlds.R
\name{minecraft_worlds}
\alias{minecraft_worlds}
\alias{worlds_dir_path}
\alias{list_worlds}
\alias{create_world}
\alias{export_world}
\alias{import_world}
\alias{get_world_path}
\title{Utilities for working with Minecraft world folders.}
\usage{
worlds_dir_path(force_default = FALSE)

list_worlds(worlds_dir = worlds_dir_path())

create_world(id = NULL, ..., worlds_dir = worlds_dir_path())

export_world(id, file, worlds_dir = worlds_dir_path(), replace = FALSE)

import_world(file, id = NULL, ..., worlds_dir = worlds_dir_path())

get_world_path(id, worlds_dir = worlds_dir_path())
}
\arguments{
\item{force_default}{If \code{TRUE}, return most likely world path on the system.}

\item{worlds_dir}{The path of a \code{minecraftWorlds} directory.}

\item{id}{The path to a world folder. If the path is not absolute or does not
exist, it is assumed to be the base name of a world folder in \code{worlds_dir}.
For \code{import_world()}, if \code{id} is \code{NULL} a unique world id will be
generated. How it is generated is controlled by the
\code{rbedrock.rand_world_id} global options. Possible values are "pretty" and
"mcpe".}

\item{...}{Arguments to customize \code{level.dat} settings.
Supports dynamic dots via \code{rlang::list2()}.}

\item{file}{The path to an mcworld file. If exporting, it will be created.
If importing, it will be extracted.}

\item{replace}{If \code{TRUE}, overwrite an existing file if necessary.}
}
\description{
\code{world_dir_path()} returns the path to the \code{minecraftWorlds} directory. Use
\code{options(rbedrock.worlds_dir_path = "custom/path")} to customize the path
as needed.

\code{list_worlds()} returns a \code{data.frame()} containing information about
Minecraft saved games.

\code{create_world()} creates a new Minecraft world.

\code{export_world()} exports a world to an archive file.
}
\examples{
\dontrun{
create_world(LevelName = "My World", RandomSeed = 10)
}
}
