# load packages -----------------------------------------------------------

## If a package is installed, it will be loaded. If any 
## are not, the missing package(s) will be installed 
## from CRAN and then loaded.

## First specify the packages of interest
packages <-  c('ggspatial', 'cowplot', 'viridis', 'sf', 'mapview', 'htmlwidgets', 'tidyverse', 'tidylog',
'geodata', 'terra')

## Now load or install&load all
package.check <- lapply(
  packages,
  FUN = function(x) {
    if (!require(x, character.only = TRUE)) {
      install.packages(x, dependencies = TRUE)
      library(x, character.only = TRUE)
    }
  }
)
