# Locate the coordinates of blocks in a chunk

Locate the coordinates of blocks in a chunk

## Usage

``` r
locate_blocks(blocks, pattern, negate = FALSE)
```

## Arguments

- blocks:

  A character array containing block data.

- pattern:

  The pattern to look for. Passed to
  [base::grep](https://rdrr.io/r/base/grep.html).

- negate:

  If `TRUE`, return non-matching elements.

## Examples

``` r
dbpath <- rbedrock_example_world("example1.mcworld")
db <- bedrockdb(dbpath)
blocks <- get_blocks_value(db, x=37, z=10, dimension=0)
locate_blocks(blocks, "ore")
#> # A tibble: 268 × 4
#>        x     y     z block             
#>    <dbl> <dbl> <dbl> <chr>             
#>  1   605     3   167 minecraft:iron_ore
#>  2   597     4   168 minecraft:iron_ore
#>  3   599     4   166 minecraft:iron_ore
#>  4   599     4   167 minecraft:iron_ore
#>  5   600     4   166 minecraft:iron_ore
#>  6   600     4   167 minecraft:iron_ore
#>  7   595     5   168 minecraft:gold_ore
#>  8   596     5   168 minecraft:iron_ore
#>  9   598     5   166 minecraft:iron_ore
#> 10   599     5   166 minecraft:iron_ore
#> # ℹ 258 more rows
close(db)
```
