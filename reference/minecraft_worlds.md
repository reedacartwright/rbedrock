# Utilities for working with Minecraft world folders.

`world_dir_path()` returns the path to the `minecraftWorlds` directory.
Use `options(rbedrock.worlds_dir_path = "custom/path")` to customize the
path as needed.

`list_worlds()` returns a
[`data.frame()`](https://rdrr.io/r/base/data.frame.html) containing
information about Minecraft saved games.

`create_world()` creates a new Minecraft world.

`export_world()` exports a world to an archive file.

## Usage

``` r
worlds_dir_path(force_default = FALSE)

list_worlds(worlds_dir = worlds_dir_path())

create_world(id = NULL, ..., worlds_dir = worlds_dir_path())

export_world(id, file, worlds_dir = worlds_dir_path(), replace = FALSE)

import_world(file, id = NULL, ..., worlds_dir = worlds_dir_path())

get_world_path(id, worlds_dir = worlds_dir_path())
```

## Arguments

- force_default:

  If `TRUE`, return most likely world path on the system.

- worlds_dir:

  The path of a `minecraftWorlds` directory.

- id:

  The path to a world folder. If the path is not absolute or does not
  exist, it is assumed to be the base name of a world folder in
  `worlds_dir`. For `import_world()`, if `id` is `NULL` a unique world
  id will be generated. How it is generated is controlled by the
  `rbedrock.rand_world_id` global options. Possible values are "pretty"
  and "mcpe".

- ...:

  Arguments to customize `level.dat` settings. Supports dynamic dots via
  [`rlang::list2()`](https://rlang.r-lib.org/reference/list2.html).

- file:

  The path to an mcworld file. If exporting, it will be created. If
  importing, it will be extracted.

- replace:

  If `TRUE`, overwrite an existing file if necessary.

## Examples

``` r
if (FALSE) { # \dontrun{
create_world(LevelName = "My World", RandomSeed = 10)
} # }
```
