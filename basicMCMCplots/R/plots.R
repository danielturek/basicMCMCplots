

#### useful testing code !!!!
##library(basicMCMCplots)
## 
##nchains <- 2
##nparams <- 2
##nsamples <- 1000
##samplesList <- vector('list', nchains)
##for(i in 1:nchains) {
##    samples <- array(NA, c(nsamples, nparams))
##    colnames(samples) <- letters[1:nparams]
##    for(j in 1:nparams) {
##        samples[,j] <- rnorm(nsamples, j/10, j/10)
##    }
##    samplesList[[i]] <- samples
##}
##names(samplesList) <- paste0('chain', 1:nchains)
##str(samplesList)
## 
##samplesPlot(samplesList)
## 
##chainsPlot(samplesList, cex = 0.7)
##chainsPlot(samplesList, traceplot = FALSE, cex = 0.7)
##chainsPlot(samplesList, densityplot = FALSE, cex = 0.7)
## 
##chainsSummary(samplesList, jitter = .1, buffer.left = 0.3, buffer.right = 0.3)



#' Plot MCMC traceplots and density plots
#'
#' @param samples Array of MCMC samples, or a list of samples from multiple chains in which case the first chain is used
#' @param var Parameter names to plot
#' @param ind Indices of MCMC samples to plot
#' @param burnin Number of initial MCMC samples to discard (default: 0)
#' @param scale Logical, whether to normalize each posterior chain
#' @param width Width of the plot
#' @param height Height of the plot
#' @param legend Logical, whether to include a legend of parameter names
#' @param legend.location Location of legend
#' @param traceplot Logical, whether to include traceplots (default: TRUE)
#' @param densityplot Logaical, whether to include density plots (default: TRUE)
#' @param file Optional filename to save figure as a file
#'
#' @examples
#' samples <- cbind(rnorm(1000), rgamma(1000, 1))
#' colnames(samples) <- c('alpha', 'beta')
#' samplesPlot(samples)
#' 
#' @export
samplesPlot <- function(samples, var=colnames(samples), ind=NULL, burnin=NULL, scale=FALSE, width=7, height=4, legend=TRUE, legend.location='topright', traceplot=TRUE, densityplot=TRUE, file=NULL) {
    if(!is.null(file)) pdf(file, width=width, height=height) else
    ## orig: if(inherits(try(knitr::opts_chunk$get('dev'), silent=TRUE), 'try-error') || is.null(knitr::opts_chunk$get('dev')))   ## if called from Rmarkdown/knitr
    if(inherits(try(eval(parse(text='knitr::opts_chunk$get(\'dev\')')[[1]]), silent=TRUE), 'try-error') || is.null(eval(parse(text='knitr::opts_chunk$get(\'dev\')')[[1]])))
        dev.new(height=height, width=width)
    par.save <- par(no.readonly = TRUE)
    par(mfrow=c(1,traceplot+densityplot), cex=0.7, cex.main=1.5, cex.axis=0.9, lab=c(3,3,7), mgp=c(0,0.4,0), mar=c(1.6,1.6,2,0.6), oma=c(0,0,0,0), tcl=-0.3, bty='l')
    ## process samples
    if(is.list(samples)) samples <- samples[[1]]    ## if samplesList was provided, then take first chain
    var <- gsub('\\[', '\\\\\\[', gsub('\\]', '\\\\\\]', var))   ## add \\ before any '[' or ']' appearing in var
    var <- unlist(lapply(var, function(n) grep(paste0('^', n,'(\\[.+\\])?$'), colnames(samples), value=TRUE)))  ## expanded any indexing
    samples <- samples[, var, drop=FALSE]
    if(dim(samples)[2] == 0) stop('variable names misspelled', call. = FALSE)
    if(scale) samples <- apply(samples, 2, scale)
    if(!is.null(ind) && !is.null(burnin)) stop('only specify either ind or burnin')
    if(!is.null(ind))     samples <- samples[ind, , drop=FALSE]
    if(!is.null(burnin))  samples <- samples[(burnin+1):dim(samples)[1], , drop=FALSE]
    nparam <- ncol(samples)
    rng <- range(samples)
    if(!traceplot & !densityplot) stop('both traceplot and densityplot are false')
    if(traceplot) {  ## traceplot
        plot(1:nrow(samples), ylim=rng, type='n', main='Traceplots', xlab='', ylab='')
        for(i in 1:nparam)
            lines(samples[,i], col=rainbow(nparam, alpha=0.75)[i])
        if(legend & !densityplot & !is.null(dimnames(samples)) & is.character(dimnames(samples)[[2]]))
            legend(legend=dimnames(samples)[[2]], fill=rainbow(nparam, alpha=0.5), bty='n', x=legend.location)
    }  ## finish traceplot
    if(densityplot) {  ## denstyplot
        xMin <- xMax <- yMax <- NULL
        for(i in 1:nparam) {
            d <- density(samples[,i])
            xMin <- min(xMin,d$x); xMax <- max(xMax,d$x); yMax <- max(yMax, d$y) }
        plot(1, xlim=c(xMin,xMax), ylim=c(0,yMax), type='n', main='Posterior Densities', xlab='', ylab='', yaxt='n')
        for(i in 1:nparam)
            polygon(density(samples[,i]), col=rainbow(nparam, alpha=0.2)[i], border=rainbow(nparam, alpha=0.2)[i])
        if(legend & !is.null(dimnames(samples)) & is.character(dimnames(samples)[[2]]))
            legend(legend=dimnames(samples)[[2]], fill=rainbow(nparam, alpha=0.5), bty='n', x=legend.location)
    }  ## finish densityplot
    invisible(par(par.save))
    if(!is.null(file)) dev.off()
}
 


#' Compare summary statistics from multiple MCMC chains
#'
#' Plots median and 95% credible intervals for each paramter and chain.
#'
#' @param samplesList List of arrays of MCMC samples from different chains
#' @param var Parameter names to plot
#' @param nrows Number of rows in the resulting plot
#' @param scale Logical, whether to normalize each posterior chain
#' @param width Width of figure
#' @param height Height of figure
#' @param legend Logical, whether to include a legend of chain names
#' @param legend.location Legend location
#' @param jitter Scale factor for spreading out lines from each chain
#' @param buffer.right Additional buffer on left side of plot
#' @param buffer.left Additional buffer on right side of plot
#' @param cex Expansion coefficient for text
#' @param file Filename for saving figure to a file
#'
#' @examples
#' samples1 <- cbind(rnorm(1000, 1), rgamma(1000, 1), rpois(1000, 1))
#' colnames(samples1) <- c('alpha', 'beta', 'gamma')
#' samples2 <- cbind(rnorm(1000, 2), rgamma(1000, 2), rpois(1000, 2))
#' colnames(samples2) <- c('alpha', 'beta', 'gamma')
#' samplesList <- list(chain1 = samples1, chain2 = samples2)
#' chainsSummary(samplesList, nrow = 1, jitter = .3, buffer.left = .5, buffer.right = .5)
#'
#' @export
chainsSummary <- function(samplesList, var=NULL, nrows=NULL, scale=FALSE, width=7, height=NULL, legend=!is.null(names(samplesList)), legend.location='topright', jitter=1, buffer.right=0, buffer.left=0, cex=1, file=NULL) {
    if(!(class(samplesList) %in% c('list', 'mcmc.list'))) samplesList <- list(samplesList)
    if(!is.null(var)) samplesList <- lapply(samplesList, function(samples) {
        var <- gsub('\\[', '\\\\\\[', gsub('\\]', '\\\\\\]', var))   ## add \\ before any '[' or ']' appearing in var
        theseVar <- unlist(lapply(var, function(n) grep(paste0('^', n,'(\\[.+\\])?$'), colnames(samples), value=TRUE)))  ## expanded any indexing
        ret <- samples[, theseVar, drop=FALSE]
        if(dim(ret)[2] == 0) stop('variable names misspelled', call. = FALSE)
        ret
    })
    chainParamNamesList <- lapply(samplesList, function(s) colnames(s))
    nChains <- length(samplesList)
    cols <- rainbow(nChains)
    paramNamesAll <- unique(unlist(lapply(samplesList, function(s) colnames(s))))
    nParamsAll <- length(paramNamesAll)
    if(is.null(nrows)) nrows <- min(ceiling(nParamsAll/7), 3)
    if(is.null(height)) height <- if(nrows==1) 3 else if(nrows==2) 4 else 7
    ## this section moved lower, after we know nrows:
    if(!is.null(file)) pdf(file, width=width, height=height) else
    ## orig: if(inherits(try(knitr::opts_chunk$get('dev'), silent=TRUE), 'try-error') || is.null(knitr::opts_chunk$get('dev')))   ## if called from Rmarkdown/knitr
    if(inherits(try(eval(parse(text='knitr::opts_chunk$get(\'dev\')')[[1]]), silent=TRUE), 'try-error') || is.null(eval(parse(text='knitr::opts_chunk$get(\'dev\')')[[1]])))
        dev.new(height=height, width=width)
    par.save <- par(no.readonly = TRUE)
    ##par(mfrow=c(nrows,1), oma=c(3,1,1,1), mar=c(4,1,0,1), mgp=c(3,0.5,0))
    mar1 <- if(nrows<=2) 2 else 4
    par(mfrow=c(nrows,1), oma=c(3,1,1,1), mar=c(mar1,1,0,1), mgp=c(3,0.5,0))
    ## construct 3D summary array:
    summary <- array(as.numeric(NA), dim = c(nChains, 3, nParamsAll))
    if(!is.null(names(samplesList))) dimnames(summary)[[1]] <- names(samplesList)
    dimnames(summary)[[2]] <- c('mean','low','upp')
    dimnames(summary)[[3]] <- paramNamesAll
    for(iChain in 1:nChains) {
        theseSamples <- samplesList[[iChain]]
        if(scale) theseSamples <- apply(theseSamples, 2, scale)
        thisSummary <- rbind(mean = apply(theseSamples, 2, mean),
                             low  = apply(theseSamples, 2, function(x) quantile(x, 0.025)),
                             upp  = apply(theseSamples, 2, function(x) quantile(x, 0.975)))
        summary[iChain,c('mean','low','upp'),colnames(thisSummary)] <- thisSummary
    }
    nParamsPerRow <- ceiling(nParamsAll/nrows)
    sq <- if(nChains==1) 0 else seq(-1,1,length=nChains)
    scale <- width/nParamsPerRow * jitter * 0.1  ## adjust jitter scale factor at end
    for(iRow in 1:nrows) {
        rowParamInd <- (1+(iRow-1)*nParamsPerRow) : ifelse(iRow==nrows,nParamsAll,iRow*nParamsPerRow)
        nRowParams <- length(rowParamInd)
        rowParamNames <- paramNamesAll[rowParamInd]
        xs <- 1:nRowParams
        names(xs) <- rowParamNames
        ylim <- range(summary[,c('low','upp'),rowParamNames], na.rm=TRUE)
        plot(x=-100, y=0, xlim=c(1-buffer.left,nParamsPerRow+buffer.right), ylim=ylim, xaxt='n', ylab='', xlab='', tcl=-0.3, cex.axis=cex)
        axis(1, at=1:nRowParams, labels=FALSE, tcl=-0.3)
        text(x=1:nRowParams, y=par()$usr[3]-0.1*(par()$usr[4]-par()$usr[3]), labels=rowParamNames, srt=45, adj=1, xpd=TRUE, cex=0.9*cex)
        for(iChain in 1:nChains) {
            ps <- intersect(rowParamNames, chainParamNamesList[[iChain]])
            xsJittered <- xs + sq[iChain]*scale
            points(x=xsJittered[ps], y=summary[iChain,'mean',ps], pch=16, col=cols[iChain])
            segments(x0=xsJittered[ps], y0=summary[iChain,'low',ps], y1=summary[iChain,'upp',ps], lwd=1, col=cols[iChain])
        }
        if(legend & !is.null(names(samplesList))) legend(legend.location, legend=names(samplesList), pch=16, col=cols, cex=cex)
    }
    invisible(par(par.save))
    if(!is.null(file)) dev.off()
}





#' Compare trace plots from multiple MCMC chains
#'
#' Overlays trace plots from each MCMC chain, for each parameter
#'
#' @param samplesList List of arrays of MCMC samples from different chains
#' @param var Parameter names to plot
#' @param ind Indices of MCMC samples to plot
#' @param burnin Number of initial samples to discard from each MCMC chain (default: 0)
#' @param scale Logical, whether to normalize each posterior chain (default: FALSE)
#' @param ncols Number of columns in grid of parameter traceplots or densityplots
#' @param width Width of the plot
#' @param height Height of the plot
#' @param legend Logical, whether to include a legend of chain names
#' @param legend.location Legend location
#' @param cex Expansion coefficient for text (default: 1)
#' @param traceplot Logical, whether to generate posterior trace plots (default: TRUE)
#' @param densityplot Logical, whether to generate posterior density plots (default: TRUE)
#' @param file Filename for saving figure to a file
#'
#' @examples
#' samples1 <- cbind(rnorm(1000, 1), rgamma(1000, 1), rpois(1000, 1))
#' colnames(samples1) <- c('alpha', 'beta', 'gamma')
#' samples2 <- cbind(rnorm(1000, 2), rgamma(1000, 2), rpois(1000, 2))
#' colnames(samples2) <- c('alpha', 'beta', 'gamma')
#' samplesList <- list(chain1 = samples1, chain2 = samples2)
#' 
#' chainsPlot(samplesList)
#' 
#' chainsPlot(samplesList, densityplot = FALSE, burnin = 500)
#'
#' chainsPlot(samplesList, traceplot = FALSE, legend.location = 'topleft', cex = 0.7)
#'
#' @export
chainsPlot <- function(samplesList, var=NULL, ind=NULL, burnin=NULL, scale=FALSE, ncols=NULL, width=7, height=NULL, legend=!is.null(names(samplesList)), legend.location='topright', cex=1, traceplot=TRUE, densityplot=TRUE, file=NULL) {
    if(!(class(samplesList) %in% c('list', 'mcmc.list'))) samplesList <- list(samplesList)
    if(!is.null(var)) samplesList <- lapply(samplesList, function(samples) {
        var <- gsub('\\[', '\\\\\\[', gsub('\\]', '\\\\\\]', var))   ## add \\ before any '[' or ']' appearing in var
        theseVar <- unlist(lapply(var, function(n) grep(paste0('^', n,'(\\[.+\\])?$'), colnames(samples), value=TRUE)))  ## expanded any indexing
        ret <- samples[, theseVar, drop=FALSE]
        if(dim(ret)[2] == 0) stop('variable names misspelled', call. = FALSE)
        ret
    })
    chainParamNamesList <- lapply(samplesList, function(s) colnames(s))
    nChains <- length(samplesList)
    paramNamesAll <- unique(unlist(lapply(samplesList, function(s) colnames(s))))
    nParamsAll <- length(paramNamesAll)
    if(!is.null(ind) && !is.null(burnin)) stop('only specify either ind or burnin')
    if(!is.null(ind))    samplesList <- lapply(samplesList, function(samples) samples[ind, , drop=FALSE])
    if(!is.null(burnin)) samplesList <- lapply(samplesList, function(samples) samples[(burnin+1):nrow(samples), , drop=FALSE])
    if(!traceplot && !densityplot) stop('must specify either traceplot = TRUE, or densityplot = TRUE, or both')
    if(traceplot + densityplot == 1) {
        if(is.null(ncols)) ncols <- min(nParamsAll, 3);     nrows <- ceiling(nParamsAll / ncols)     } else {  ## traceplots or densityplots (but not both)
        ncols <- 2;                                         nrows <- nParamsAll                      }         ## both traceplots and densityplots
    if(is.null(height)) height <- if(nrows==1) 3 else if(nrows==2) 4 else if(nrows==3) 5 else if(nrows==4) 6 else 6.5
    if(!is.null(file)) pdf(file, width=width, height=height) else
    ## orig: if(inherits(try(knitr::opts_chunk$get('dev'), silent=TRUE), 'try-error') || is.null(knitr::opts_chunk$get('dev')))   ## if called from Rmarkdown/knitr
    if(inherits(try(eval(parse(text='knitr::opts_chunk$get(\'dev\')')[[1]]), silent=TRUE), 'try-error') || is.null(eval(parse(text='knitr::opts_chunk$get(\'dev\')')[[1]])))
        dev.new(width=width, height=height)
    par.save <- par(no.readonly = TRUE)
    oma1 <- if(nrows==1) 0.4 else if(nrows==2) 0.4 else 0.5
    mai1 <- if(traceplot & !densityplot) 0.1 else 0.2
    par(mfrow=c(nrows,ncols), mai=c(mai1,0.2,0.2,0.2), oma=c(oma1,.3,0,0), mgp=c(2,0.3,0))
    for(iParam in 1:nParamsAll) {
        thisParamName <- paramNamesAll[iParam]
        if(traceplot) {  ## trace plots
            cols <- rainbow(nChains)
            xlim <- c(1, max(unlist(lapply(samplesList, function(s) if(thisParamName %in% colnames(s)) dim(s)[1] else NULL))))
            ylim <- range(unlist(lapply(samplesList, function(s) if(thisParamName %in% colnames(s)) s[,thisParamName] else NULL)))
            plot(-1000, -1, xlim=xlim, ylim=ylim, xlab='', ylab='', main=thisParamName, cex.main=cex, cex.axis=0.8*cex, tcl=-0.2, xaxt='n')
            if(iParam==1 & legend & !is.null(names(samplesList))) legend(legend.location, legend=names(samplesList), lty=1, col=cols, cex=cex)
            for(iChain in 1:nChains) {
                if(!(thisParamName %in% colnames(samplesList[[iChain]]))) next
                ys <- samplesList[[iChain]][,thisParamName]
                lines(seq_along(ys), ys, col=cols[iChain]) }
        }  ## end trace plots
        if(densityplot) {  ## density plots
            xMin <- xMax <- yMax <- NULL
            for(iChain in 1:nChains) {
                if(!(thisParamName %in% colnames(samplesList[[iChain]]))) next
                d <- density(samplesList[[iChain]][,thisParamName])
                xMin <- min(xMin,d$x); xMax <- max(xMax,d$x); yMax <- max(yMax, d$y) }
            plot(-1000, -1, xlim=c(xMin,xMax), ylim=c(0,yMax), type='n', main=thisParamName, xlab='', ylab='', tcl=-0.2, yaxt='n', cex.main=cex, cex.axis=0.8*cex)
            if(iParam==1 & legend & !is.null(names(samplesList))) legend(legend.location, legend=names(samplesList), fill=rainbow(nChains, alpha=0.5), bty='n', cex=cex)
            for(iChain in 1:nChains) {
                if(!(thisParamName %in% colnames(samplesList[[iChain]]))) next
                ys <- samplesList[[iChain]][,thisParamName]
                polygon(density(ys), col=rainbow(nChains, alpha=0.2)[iChain], border=rainbow(nChains, alpha=0.2)[iChain])
            }
        }  ## end density plots
    }
    invisible(par(par.save))
    if(!is.null(file)) dev.off()
}





