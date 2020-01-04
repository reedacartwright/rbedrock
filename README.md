## Overview

rbedrock is an extension package for [R](https://www.r-project.org/) that supports the analysis and management of Minecraft (Bedrock Edition) worlds. This includes Windows 10, Pocket Edition, XBox, and PS4 versions of the game. It does not include Minecraft: Java worlds.

## Installation

``` r
# Install from CRAN [NOT CURRENTLY AVAILABLE]
# install.packages("rbedrock") 

# Or the development version from GitHub
if (!require(devtools)) {
    install.packages("devtools")	
}
devtools::install_github("reedacartwright/rbedrock")
```

To install the development version you will need to have suitable development tools installed on your machine. This includes [CMake](https://cmake.org/download/) and [Rtools](https://cran.r-project.org/bin/windows/Rtools/) if you are on Windows.