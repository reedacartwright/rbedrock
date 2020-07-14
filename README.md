[![Discord Chat](https://img.shields.io/discord/710697981677928479.svg)](https://discord.gg/sWb4YmX)

## Overview

rbedrock is an extension package for [R](https://www.r-project.org/) that supports the analysis and management of Minecraft (Bedrock Edition) worlds. This includes Windows 10, Pocket Edition, XBox, and PS4 versions of the game. It does not include Minecraft: Java worlds.

## Installation

To install the development version you will need to have suitable development tools installed on your machine. This includes [CMake](https://cmake.org/download/) on Unix.

``` r
# Install from CRAN [NOT CURRENTLY AVAILABLE]
# install.packages("rbedrock") 

# Or the development version from GitHub
# If you are installing on Windows for the first time, see section below
if (!require(devtools)) {
    install.packages("devtools")
}
devtools::install_github("reedacartwright/rbedrock")
```

## Windows Installation

Install the latest [R](https://cloud.r-project.org/bin/windows/base/) and [RStudio](https://rstudio.com/products/rstudio/download/#download). On Windows, you will also need to install the [Rtools40](https://cran.r-project.org/bin/windows/Rtools/) package to build rbedrock from source. This can be accomplished with the code below.


``` r
# Install the "installr" package if neccessary
if(!require(installr)) {
    install.packages("installr")
}
# download, install, and setup rtools
installr::install.URL("https://cran.r-project.org/bin/windows/Rtools/rtools40-x86_64.exe")
writeLines('PATH="${RTOOLS40_HOME}\\usr\\bin;${PATH}"', con = "~/.Renviron")
```

Now restart R/Rstudio so the new .Renviron can take effect.

``` r
# Install from CRAN [NOT CURRENTLY AVAILABLE]
# install.packages("rbedrock") 

# Or the development version from GitHub
if (!require(devtools)) {
    install.packages("devtools")
}
devtools::install_github("reedacartwright/rbedrock")

# Install tidyverse (not needed, but useful)
if(!require(tidyverse)) {
    install.packages("tidyverse")
}    
```
