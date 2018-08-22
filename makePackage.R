

library(devtools)
library(roxygen2)

## create empty package:
## (never need to do this again)
##setwd('~/github/basicMCMCplots')
##create('basicMCMCplots', rstudio = FALSE)

## use roxygen comments to generate man/*.Rd help files
setwd('~/github/basicMCMCplots/basicMCMCplots')
document()

## build the package tarball file
setwd('~/github/basicMCMCplots')
system('R CMD BUILD basicMCMCplots')

## use check() to "check" the package
setwd('~/github/basicMCMCplots/basicMCMCplots')
check('.')


## install the package from GitHub:
library(devtools)
install_github('danielturek/basicMCMCplots', subdir = 'basicMCMCplots')
library(basicMCMCplots)



