# Read and write data from a world's level.dat file.

Read and write data from a world's level.dat file.

## Usage

``` r
read_leveldat(path)

write_leveldat(object, path, version = 8L)
```

## Arguments

- path:

  The path to a world folder. If the path does not exist, it is assumed
  to be the base name of a world folder in the local minecraftWorlds
  directory.

- object:

  NBT data to be written to level.dat.

- version:

  The level.dat format version for the file header.

## Value

`read_leveldat` returns nbt data.

`write_leveldat` returns a copy of the data written.

## Examples

``` r
# Fix level.dat after opening a world in creative.
dbpath <- rbedrock_example_world("example1.mcworld")
dat <- read_leveldat(dbpath)
dat$hasBeenLoadedInCreative <- FALSE
write_leveldat(dat, dbpath)
```
