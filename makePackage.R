

library(devtools)
library(roxygen2)

## create empty package:
## (never need to do this again)
##setwd('~/github/basicMCMCplots')
##create('basicMCMCplots', rstudio = FALSE)

## use roxygen comments to generate man/*.Rd help files
setwd('~/github/basicMCMCplots/basicMCMCplots')
document()

## check current NAMESPACE file (since it doesn't get auto-generated)
cat(paste0(readLines('NAMESPACE'), collapse='\n'))

## build the package tarball file
setwd('~/github/basicMCMCplots')
system('R CMD BUILD basicMCMCplots')

## use check() to "check" the package
setwd('~/github/basicMCMCplots/basicMCMCplots')
check('.')

## install the newly build (local) 'basicMMCplots' package
remove.packages('basicMCMCplots')
## restart R here
setwd('~/github/basicMCMCplots')
(tarFiles <- grep('\\.tar\\.gz$', list.files(), value = TRUE))
(lastTarFile <- tarFiles[length(tarFiles)])
library(basicMCMCplots)
system(paste0('R CMD install ', lastTarFile))
library(basicMCMCplots)

## install the package from GitHub:
remove.packages('basicMCMCplots')
library(basicMCMCplots)
library(devtools)
install_github('danielturek/basicMCMCplots', subdir = 'basicMCMCplots')
library(basicMCMCplots)



