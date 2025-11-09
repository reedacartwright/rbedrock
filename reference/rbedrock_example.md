# Get path to rbedrock example

rbedrock comes bundled with a number of sample files in its
`inst/extdata` directory. This function make them easy to access.

## Usage

``` r
rbedrock_example(path = NULL)

rbedrock_example_world(path)
```

## Arguments

- path:

  Name of file or directory. If `NULL`, the examples will be listed.

## Examples

``` r
rbedrock_example()
#> [1] "default_level.dat" "example1.mcworld"  "example2.mcworld" 
#> [4] "example3.mcworld" 
rbedrock_example("example1.mcworld")
#> [1] "/home/runner/work/_temp/Library/rbedrock/extdata/example1.mcworld"
rbedrock_example_world("example1.mcworld")
#> [1] "/tmp/RtmpBSva6d/world1ffd277cdeed"
```
