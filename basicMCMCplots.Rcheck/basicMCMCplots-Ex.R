pkgname <- "basicMCMCplots"
source(file.path(R.home("share"), "R", "examples-header.R"))
options(warn = 1)
base::assign(".ExTimings", "basicMCMCplots-Ex.timings", pos = 'CheckExEnv')
base::cat("name\tuser\tsystem\telapsed\n", file=base::get(".ExTimings", pos = 'CheckExEnv'))
base::assign(".format_ptime",
function(x) {
  if(!is.na(x[4L])) x[1L] <- x[1L] + x[4L]
  if(!is.na(x[5L])) x[2L] <- x[2L] + x[5L]
  options(OutDec = '.')
  format(x[1L:3L], digits = 7L)
},
pos = 'CheckExEnv')

### * </HEADER>
library('basicMCMCplots')

base::assign(".oldSearch", base::search(), pos = 'CheckExEnv')
base::assign(".old_wd", base::getwd(), pos = 'CheckExEnv')
cleanEx()
nameEx("chainsPlot")
### * chainsPlot

flush(stderr()); flush(stdout())

base::assign(".ptime", proc.time(), pos = "CheckExEnv")
### Name: chainsPlot
### Title: Compare trace plots from multiple MCMC chains
### Aliases: chainsPlot

### ** Examples

samples1 <- cbind(rnorm(1000, 1), rgamma(1000, 1), rpois(1000, 1))
colnames(samples1) <- c('alpha', 'beta', 'gamma')
samples2 <- cbind(rnorm(1000, 2), rgamma(1000, 2), rpois(1000, 2))
colnames(samples2) <- c('alpha', 'beta', 'gamma')
samplesList <- list(chain1 = samples1, chain2 = samples2)

chainsPlot(samplesList)

chainsPlot(samplesList, densityplot = FALSE, burnin = 500)

chainsPlot(samplesList, traceplot = FALSE, legend.location = 'topleft', cex = 0.7)




base::assign(".dptime", (proc.time() - get(".ptime", pos = "CheckExEnv")), pos = "CheckExEnv")
base::cat("chainsPlot", base::get(".format_ptime", pos = 'CheckExEnv')(get(".dptime", pos = "CheckExEnv")), "\n", file=base::get(".ExTimings", pos = 'CheckExEnv'), append=TRUE, sep="\t")
cleanEx()
nameEx("chainsSummary")
### * chainsSummary

flush(stderr()); flush(stdout())

base::assign(".ptime", proc.time(), pos = "CheckExEnv")
### Name: chainsSummary
### Title: Compare summary statistics from multiple MCMC chains
### Aliases: chainsSummary

### ** Examples

samples1 <- cbind(rnorm(1000, 1), rgamma(1000, 1), rpois(1000, 1))
colnames(samples1) <- c('alpha', 'beta', 'gamma')
samples2 <- cbind(rnorm(1000, 2), rgamma(1000, 2), rpois(1000, 2))
colnames(samples2) <- c('alpha', 'beta', 'gamma')
samplesList <- list(chain1 = samples1, chain2 = samples2)
chainsSummary(samplesList, nrow = 1, jitter = .3, buffer.left = .5, buffer.right = .5)




base::assign(".dptime", (proc.time() - get(".ptime", pos = "CheckExEnv")), pos = "CheckExEnv")
base::cat("chainsSummary", base::get(".format_ptime", pos = 'CheckExEnv')(get(".dptime", pos = "CheckExEnv")), "\n", file=base::get(".ExTimings", pos = 'CheckExEnv'), append=TRUE, sep="\t")
cleanEx()
nameEx("samplesPlot")
### * samplesPlot

flush(stderr()); flush(stdout())

base::assign(".ptime", proc.time(), pos = "CheckExEnv")
### Name: samplesPlot
### Title: Plot MCMC traceplots and density plots
### Aliases: samplesPlot

### ** Examples

samples <- cbind(rnorm(1000), rgamma(1000, 1))
colnames(samples) <- c('alpha', 'beta')
samplesPlot(samples)




base::assign(".dptime", (proc.time() - get(".ptime", pos = "CheckExEnv")), pos = "CheckExEnv")
base::cat("samplesPlot", base::get(".format_ptime", pos = 'CheckExEnv')(get(".dptime", pos = "CheckExEnv")), "\n", file=base::get(".ExTimings", pos = 'CheckExEnv'), append=TRUE, sep="\t")
### * <FOOTER>
###
cleanEx()
options(digits = 7L)
base::cat("Time elapsed: ", proc.time() - base::get("ptime", pos = 'CheckExEnv'),"\n")
grDevices::dev.off()
###
### Local variables: ***
### mode: outline-minor ***
### outline-regexp: "\\(> \\)?### [*]+" ***
### End: ***
quit('no')
