
preferred_mfrow <- function(n, max_cols = 3) {
    cols <- min(n, max_cols)
    rows <- ceiling(n / cols)
    return(c(rows, cols))
}


plotSpict.data <- function(x, do.plot=NULL,
                           stamp=get.version(),
                           automfrow=TRUE){
    ## if check.inp has not been run yet
    if(!inherits(x, "spictcls")){
        x <- try(check.inp(x, verbose = FALSE, mancheck = FALSE), silent=TRUE)
        if(inherits(x, "try-error")){
            stop("Provided object 'x' needs to be an input list or a fitted spict object!")
        }
    }
    ## if fitted object
    if(inherits(x, "spictcls") && any(names(x) == "opt")){
        inp <- x$inp
        rep <- x
        isRep <- TRUE
    }else{
        inp <- check.inp(x, verbose = FALSE, mancheck = FALSE)
        rep <- NULL
        isRep <- FALSE
    }
    if(!is.null(do.plot)) automfrow <- FALSE
    if(automfrow) {
        if(any(!is.null(inp))) {
            noi <- ifelse(inherits(inp$obsI, "list"),
                          length(inp$obsI), 1)
        } else {
            noi <- 1
        }
        op <- par(mfrow=n2mfrow(noi,asp=2))
        on.exit(par(op))
    }
    ## spict::plotspict.data(dat$dataExplo$inp)
    inpin = inp
    MSY = NULL
    one.index = NULL
    qlegend = TRUE
    isrep <- ifelse(inherits(inpin, "spictcls") && "opt" %in%
                    names(inpin), 1, 0)
    if (isrep) {
        inpin <- inpin$inp
    }
    inp <- check.inp(inpin)
    nseries <- inp$nseries + as.numeric(inp$logmcovflag)
    if ("true" %in% names(inp)) {
        nseries <- nseries + 2
        if (inp$timevaryinggrowth) {
            nseries <- nseries + 1
        }
    }
    xlim <- range(inp$timeC, unlist(inp$timeI), inp$timeE)
    if ("true" %in% names(inp)) {
        if (inp$timevaryinggrowth) {
            plot(inp$true$time, inp$true$mre, typ = "l", xlim = xlim,
                 xlab = "Time", ylab = "m", lwd = 1.5, col = true.col(),
                 main = "True MSY")
            box(lwd = 1.5)
        }
        plot(inp$true$time, inp$true$F, typ = "l", col = true.col(),
             xlim = xlim, xlab = "Time", ylab = expression(F[t]),
             lwd = 1.5, main = "True F")
        box(lwd = 1.5)
        ylab <- spict:::add.catchunit(expression(B[t]), inp$catchunit)
        plot(inp$true$time, inp$true$B, typ = "l", xlim = xlim,
             xlab = "Time", ylab = ylab, lwd = 1.5, col = true.col(),
             main = "True biomass")
        box(lwd = 1.5)
    }
    main <- paste0("Nobs C: ", inp$nobsC)
    ylab <- "Catch"
    ylab <- spict:::add.catchunit(ylab, inp$catchunit)
    plot(inp$timeC, inp$obsC, typ = "l", ylab = ylab, xlab = "Time",
         main = main, xlim = xlim)
    grid()
    spict:::plot.col(inp$timeC, inp$obsC, do.line = FALSE, cex = 0.6,
                     add = TRUE, add.legend = qlegend)
    if (!is.null(MSY)) {
        abline(h = MSY, lty = 2)
    }
    box(lwd = 1.5)
    if (inp$nindex > 0) {
        i <- 1
        main <- paste0("Nobs I: ", inp$nobsI[i])
        plot(inp$timeI[[i]], inp$obsI[[i]], typ = "l", ylab = paste("Index",
                                                                    i), xlab = "Time", main = main, xlim = xlim)
        grid()
        spict:::plot.col(inp$timeI[[i]], inp$obsI[[i]], pch = i, do.line = FALSE,
                         cex = 0.6, add = TRUE, add.legend = FALSE)
        if (inp$nindex > 1 & is.null(one.index)) {
            for (i in 2:inp$nindex) {
                main <- paste0("Nobs I: ", inp$nobsI[i])
                plot(inp$timeI[[i]], inp$obsI[[i]], typ = "l",
                     ylab = paste("Index", i), xlab = "Time", main = main,
                     xlim = xlim)
                grid()
                spict:::plot.col(inp$timeI[[i]], inp$obsI[[i]], pch = i,
                                 do.line = FALSE, cex = 0.6, add = TRUE, add.legend = FALSE)
            }
        }
        box(lwd = 1.5)
    }
    if (inp$nobsE) {
        main <- paste0("Nobs E: ", inp$nobsE)
        ylab <- "Effort"
        plot(inp$timeE, inp$obsE, typ = "l", ylab = ylab, xlab = "Time",
             main = main, xlim = xlim)
        grid()
        spict:::plot.col(inp$timeE, inp$obsE, do.line = FALSE, cex = 0.6,
                         add = TRUE, add.legend = FALSE)
        box(lwd = 1.5)
    }
    if (inp$logmcovflag) {
        main <- paste0("Nobs logmcovariate: ", length(inp$logmcovariate))
        ylab <- "logm covariate"
        plot(inp$time, inp$logmcovariatein, typ = "l", ylab = ylab,
             xlab = "Time", main = main, xlim = xlim, col = "blue",
             lwd = 1.5)
        grid()
        spict:::plot.col(inp$logmcovariatetime, inp$logmcovariate, do.line = FALSE,
                         cex = 0.6, add = TRUE, add.legend = FALSE)
        box(lwd = 1.5)
    }
    txt.stamp(stamp, do.flag = TRUE)
}


plotSpict.data.unc <- function(x, do.plot=NULL,
                               stamp=get.version(),
                               automfrow=TRUE){
    ## if check.inp has not been run yet
    if(!inherits(x, "spictcls")){
        x <- try(check.inp(x, verbose = FALSE, mancheck = FALSE), silent=TRUE)
        if(inherits(x, "try-error")){
            stop("Provided object 'x' needs to be an input list or a fitted spict object!")
        }
    }
    ## if fitted object
    if(inherits(x, "spictcls") && any(names(x) == "opt")){
        inp <- x$inp
        rep <- x
        isRep <- TRUE
    }else{
        inp <- check.inp(x, verbose = FALSE, mancheck = FALSE)
        rep <- NULL
        isRep <- FALSE
    }
    if(!is.null(do.plot)) automfrow <- FALSE
    if(automfrow) {
        if(any(!is.null(inp))) {
            noi <- ifelse(inherits(inp$obsI, "list"),
                          length(inp$obsI), 1)
        } else {
            noi <- 1
        }
        op <- par(mfrow=n2mfrow(noi,asp=2))
        on.exit(par(op))
    }
    ## spict::plotspict.data(dat$dataExplo$inp)
    inpin = inp
    MSY = NULL
    one.index = NULL
    qlegend = TRUE
    isrep <- ifelse(inherits(inpin, "spictcls") && "opt" %in%
                    names(inpin), 1, 0)
    if (isrep) {
        inpin <- inpin$inp
    }
    inp <- check.inp(inpin)
    nseries <- inp$nseries + as.numeric(inp$logmcovflag)
    if ("true" %in% names(inp)) {
        nseries <- nseries + 2
        if (inp$timevaryinggrowth) {
            nseries <- nseries + 1
        }
    }
    xlim <- range(inp$timeC, unlist(inp$timeI), inp$timeE)
    main <- paste0("Nobs C: ", inp$nobsC)
    ylab <- "Catch"
    ylab <- spict:::add.catchunit(ylab, inp$catchunit)
    plot(inp$timeC, inp$stdevfacC, typ = "l",
         ylab = "Catch uncertainty scaling",
         xlab = "Time",
         main = main, xlim = xlim)
    grid()
    spict:::plot.col(inp$timeC, inp$stdevfacC, do.line = FALSE, cex = 0.6,
                     add = TRUE, add.legend = qlegend)
    box(lwd = 1.5)
    if (inp$nindex > 0) {
        i <- 1
        main <- paste0("Nobs I: ", inp$nobsI[i])
        plot(inp$timeI[[i]], inp$stdevfacI[[i]], typ = "l",
             ylab = paste("Index",
                          i), xlab = "Time", main = main, xlim = xlim)
        grid()
        spict:::plot.col(inp$timeI[[i]], inp$stdevfacI[[i]], pch = i, do.line = FALSE,
                         cex = 0.6, add = TRUE, add.legend = FALSE)
        if (inp$nindex > 1 & is.null(one.index)) {
            for (i in 2:inp$nindex) {
                main <- paste0("Nobs I: ", inp$nobsI[i])
                plot(inp$timeI[[i]], inp$stdevfacI[[i]], typ = "l",
                     ylab = paste("Index", i), xlab = "Time", main = main,
                     xlim = xlim)
                grid()
                spict:::plot.col(inp$timeI[[i]], inp$stdevfacI[[i]], pch = i,
                                 do.line = FALSE, cex = 0.6, add = TRUE, add.legend = FALSE)
            }
        }
        box(lwd = 1.5)
    }
    txt.stamp(stamp, do.flag = TRUE)
}



plotSpict.priors <- function(x, do.plot=NULL,
                             stamp=get.version(),
                             automfrow=TRUE){
    ## if check.inp has not been run yet
    if(!inherits(x, "spictcls")){
        x <- try(check.inp(x, verbose = FALSE, mancheck = FALSE), silent=TRUE)
        if(inherits(x, "try-error")){
            stop("Provided object 'x' needs to be an input list or a fitted spict object!")
        }
    }
    ## if fitted object
    if(inherits(x, "spictcls") && any(names(x) == "opt")){
        inp <- x$inp
        rep <- x
        isRep <- TRUE
    }else{
        inp <- check.inp(x, verbose = FALSE, mancheck = FALSE)
        rep <- NULL
        isRep <- FALSE
    }
    useflags <- inp$priorsuseflags
    inds <- which(useflags == 1)
    ninds <- length(inds)
    if(!is.null(do.plot)) automfrow <- FALSE
    if(automfrow) {
        nopriors <- get.no.active.priors(inp)
        op <- par(mfrow=n2mfrow(nopriors))
        on.exit(par(op))
    }
    counter <- 0
    nused <- sum(useflags)
    if (ninds > 0){
        for (i in 1:ninds){
            j <- inds[i]
            priorvec <- inp$priors[[j]]
            nm <- names(inp$priors)[j]
            isGamma <- FALSE
            nmpl <- sub('log', '', nm)
            nmpl <- sub('gamma','',nmpl)
            if(isRep){
                par <- get.par(nm, rep, exp=FALSE)
            }
            if (length(grep('gamma', nm)) == 1){
                isGamma <- TRUE
                if(nm=="logngamma" && isRep) par <- get.par("logn",rep,exp=FALSE)
            }

            repriors <- c('logB', 'logF', 'logBBmsy', 'logFFmsy')
            if (nm %in% repriors){
                if(isRep) par <- par[priorvec[5], , drop=FALSE]
                nmpl <- paste0(nmpl, fd(priorvec[4]))
                if (nm == 'logB'){
                    nmpl <- spict:::add.catchunit(nmpl, inp$catchunit)
                }
            }
            if(isRep){
                nrowPar <- nrow(par)
            }else
                nrowPar <- 1
            for (rr in 1:nrowPar){
                if (nrowPar > 1){
                    nmpl <- paste0(nmpl, rr)
                }
                prvec <- priorvec
                if(is.list(priorvec)) prvec <- priorvec[[rr]]
                if(isRep){
                    mu <- ifelse(is.na(par[rr, 4]), prvec[1], par[rr, 2])
                    sd <- ifelse(is.na(par[rr, 4]), prvec[2], par[rr, 4])
                }else{
                    mu <- prvec[1]
                    sd <- prvec[2]
                }
                if(isRep){
                    if(isGamma && is.na(par[rr, 4])){
                        xmin <- 1e-12
                        xmax <- qgamma(0.99,shape=mu,rate=sd)
                    } else {
                        xmin <- mu - 3*sd
                        xmax <- mu + 3*sd
                    }
                }else{
                    xmin <- mu - 3*sd
                    xmax <- mu + 3*sd
                }
                xpr <- xpo <- seq(xmin, xmax, length=200)
                if(!isGamma) {
                    priorvals <- dnorm(xpr, prvec[1], prvec[2])
                }  else  {
                    priorvals <- dgamma(xpr, prvec[1], prvec[2])
                }

                if(isRep){
                    if (is.na(par[rr, 4])){
                        posteriorvals <- NULL
                    } else {
                        if(isGamma) xpo <- seq(mu - 3*sd, mu + 3*sd, length=200)
                        posteriorvals <- dnorm(xpo, par[rr, 2], par[rr, 4])
                    }
                }else posteriorvals <- NULL
                plot(exp(xpr), priorvals, typ='l', xlab=nmpl, ylab='Density', log='x',
                     lwd=1.5, ylim=c(0, max(priorvals, posteriorvals)*1.3))
                if(isRep){
                    if (is.na(par[rr, 4])){
                        if (!is.na(par[rr, 2])){
                            abline(v=exp(par[rr, 2]), lty=2, col=3, lwd=1.5)
                        }
                        legend('topright', legend=c('Prior', 'Post. Mean'), lty=1:2,
                               col=c(1, 3), lwd=1.5)
                    } else {
                        lines(exp(xpo), posteriorvals, col=3, lwd=1.5)
                        legend('topright', legend=c('Prior', 'Post.'), lty=1,
                               col=c(1, 3), lwd=1.5)
                    }
                }
                box(lwd=1.5)
                if (isRep && rep$opt$convergence != 0){
                    warning.stamp()
                }
                counter <- counter + 1
                if(!is.null(do.plot) && counter >= do.plot) {
                    txt.stamp(stamp)
                    return()
                }
            }
        }
        txt.stamp(stamp)
    }
}


plotSpict.sum <- function(dat, input){
    plot2(dat$results)
}

plotSpict.abs <- function(dat, input){
    par(mfrow = c(1,2))
    spict::plotspict.biomass(dat$results)
    spict::plotspict.f(dat$results, qlegend = FALSE)
}

plotSpict.prod <- function(dat, input){
    spict::plotspict.production(dat$results)
}


plotSpict.resid1 <- function(dat, input){
    spict:::plotspict.diagnostic(dat$results)
}

plotSpict.resid2 <- function(dat, input){
    spict:::plotspict.diagnostic.process(dat$results)
}


plotSpict.diag1 <- function(dat, input){
    spict:::plotspict.ci(dat$dataExplo$inp)

}

plotSpict.diag2 <- function(dat, input){
    par(mfrow = c(1,2))
    hist(dat$dataExplo$inp$obsC,
         main = "", xlab = "Catch")
    hist(unlist(dat$dataExplo$inp$obsI),
         main = "", xlab = "Indices")
}
