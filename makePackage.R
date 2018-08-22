

library(devtools)
library(roxygen2)

## create empty package:
## (never need to do this again)
##setwd('~/github/plotMCMC')
##create('plotMCMC', rstudio = FALSE)

## use roxygen comments to generate man/*.Rd help files
setwd('~/github/plotMCMC/plotMCMC')
document()

## build the package tarball file
setwd('~/github/plotMCMC')
system('R CMD BUILD plotMCMC')

## use check() to "check" the package
setwd('~/github/plotMCMC/plotMCMC')
check('.')


## install the package from GitHub:
library(devtools)
install_github('danielturek/plotMCMC', subdir = 'plotMCMC')
library(plotMCMC)



