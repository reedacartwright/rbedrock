<!-- badges: start -->
[![Discord Chat](https://img.shields.io/discord/710697981677928479.svg)](https://discord.com/invite/sWb4YmX)
![Latest Version](https://www.r-pkg.org/badges/version/rbedrock)
![Downloads per Month](https://cranlogs.r-pkg.org/badges/rbedrock)
![Total Downloads](https://cranlogs.r-pkg.org/badges/grand-total/rbedrock)
[![R-CMD-check](https://github.com/reedacartwright/rbedrock/actions/workflows/R-CMD-check.yaml/badge.svg)](https://github.com/reedacartwright/rbedrock/actions/workflows/R-CMD-check.yaml)
[![Codecov test coverage](https://codecov.io/gh/reedacartwright/rbedrock/branch/master/graph/badge.svg)](https://app.codecov.io/gh/reedacartwright/rbedrock?branch=master)
<!-- badges: end -->

## Overview

rbedrock is an extension package for [R](https://www.r-project.org/) that supports the analysis and management of Minecraft (Bedrock Edition) worlds. This includes Windows 10, Pocket Edition, XBox, and PS4 versions of the game. It does not include Minecraft: Java worlds.

If you are not already using R, you will need to install the latest [R](https://cran.r-project.org/) and optionally [RStudio](https://posit.co/download/rstudio-desktop/#download).

## Installation

```r
# The easiest way to install rbedrock is from CRAN.
install.packages("rbedrock")

# Or the the development version from GitHub:
# install.packages("remotes")
remotes::install_github("reedacartwright/rbedrock")

# Install tidyverse (not needed, but useful)
# install.packages("tidyverse")
```

If you are not on Windows or Mac, this will install rbedrock from source, and you will need to have suitable development tools installed on your machine. This includes [CMake](https://cmake.org/download/).

<!--
## Development Version (Windows)

``` r
# Install the "installr" package if neccessary
if(!require(installr)) {
    install.packages("installr")
}
# download, install, and setup rtools
installr::install.Rtools()
writeLines('PATH="${RTOOLS40_HOME}\\usr\\bin;${PATH}"', con = "~/.Renviron")
```

If you do not have CMake installed, you can install it via installr.

``` r
installr::install.CMake()
```

Now restart R/RStudio so the new .Renviron can take effect.

``` r
# install development version of rbedrock
if (!require(devtools)) {
    install.packages("devtools")
}
devtools::install_github("reedacartwright/rbedrock")
```
-->

## Installation Tutorial Videos

[![Installing R and RStudio](https://img.youtube.com/vi/1irdS8C1ZjA/0.jpg)](https://www.youtube.com/watch?v=1irdS8C1ZjA)

[![Installing RBedrock](https://img.youtube.com/vi/3KI2qwEg3vk/0.jpg)](https://www.youtube.com/watch?v=3KI2qwEg3vk)
