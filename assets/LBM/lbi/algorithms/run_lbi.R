
## mean length + columns freq per year
run_lbi <- function(data, bin.size, linf, lmat, mk){

    returnResults <- list()
    out <- tryCatch( {

        ## Parameter checks
        missing.pars <- NULL
        neg.pars <- NULL
        if(is.na(bin.size)){
            missing.pars <- c(missing.pars, "binSize")
        }else if(bin.size < 0) neg.pars <- c(neg.pars, "binSize")
        if(is.na(linf)){
            missing.pars <- c(missing.pars, "Linf")
        }else if(linf < 0) neg.pars <- c(neg.pars, "Linf")
        if(is.na(lmat)){
            missing.pars <- c(missing.pars, "Lm50")
        }else if(lmat < 0) neg.pars <- c(neg.pars, "Lm50")
        if(is.na(mk)){
            missing.pars <- c(missing.pars, "MK")
        }else if(mk < 0) neg.pars <- c(neg.pars, "MK")
        if(!is.null(missing.pars)) stop(paste0(paste(missing.pars, collapse = ", ")," missing!"))
        if(!is.null(neg.pars)) stop(paste0(paste(neg.pars, collapse = ", ")," cannot be negative!"))

        catch <- data$catch
        midL <- data$midLengths
        weight <- data$weight

        years <- as.numeric(format(data$dates, "%Y"))
        ny <- length(years)
        if(any(duplicated(years))) warning("The length frequency data is not aggregated by year. This method should be used with yearly aggregated data!")

        ## Results object
        res <- data.frame(year = years,
                          Lc = NA,
                          Lopt = linf * (3 / (3 + mk)),
                          Lmax5 = NA,
                          L25 = NA,
                          L95 = NA,
                          Lmean = NA,
                          Lmaxy = NA,
                          Pmega = NA,
                          LFeM = NA)

        ## Loop over years
        for(i in 1:ny){
            if(ny == 1){
                ci <- catch
            }else{
                ci <- catch[,i]
            }
            ## Indicators
            cumPer <- cumsum(ci) / sum(ci)
            revCumPer <- cumsum(rev(ci)) / sum(ci)
            ind <- max(which(revCumPer <= 0.05))
            weights5 <- rev(ci)
            weights5[(ind+1):length(ci)] <- 0
            weights5[ind+1] <- (0.05 - revCumPer[ind]) * sum(ci)
            res$Lmax5[i] <- stats::weighted.mean(rev(midL), weights5)
            ind <- which.max(ci)
            res$Lc[i] <- midL[which(ci[1:ind] >= 0.5 * ci[ind])[1]]
            ## res$L75[i] <- min(midL[which(cumPer >= 0.75)])
            res$L25[i] <- min(midL[which(cumPer >= 0.25)])
            ## res$Lmed[i] <- min(midL[which(cumPer >= 0.5)])
            res$L95[i] <- min(midL[which(cumPer >= 0.95)])
            ## res$L90[i] <- min(midL[which(cumPer >= 0.90)])
            ind <- midL >= res$Lc[i]
            res$Lmean[i] <- weighted.mean(midL[ind], ci[ind])
            if(any(!is.null(weight)) && any(!is.na(weight))){
                bio<- ci * weight
                res$Lmaxy[i] <- midL[bio == max(bio)]
            }
            res$Pmega[i] <- sum(ci[which(midL >= (1.1 * res$Lopt[i]))]) / sum(ci) ## ref: 0.3
            res$LFeM[i] <- (1/mk * linf + res$Lc[i] * (1 + 1))/(1/mk + 1 + 1)
        }

        ## Indicator ratios
        res$Lmax5_Linf <- res$Lmax5 / linf
        res$L95_Linf <- res$L95 / linf
        res$L25_Lm50 <- res$L25 / lmat
        res$Lc_Lm50 <- res$Lc / lmat
        res$Lmean_Lopt <- res$Lmean / res$Lopt
        res$Lmaxy_Lopt <- res$Lmaxy / res$Lopt
        res$Lmean_LFeM <- res$Lmean / res$LFeM
        ##        res$Lmean_Lm50 <- res$Lmean / res$Lm50

        ## Reorder
        res <- res[,c(1,2,5,8,10,7,3,6,4,11:12,9,13:17)]

        returnResults[['res']] <- res

    },
    error=function(cond) {
        print("There was an error here")
        message(paste0("Error!!", cond))
        errorResult = list()
        errorResult[['error']] <- gettext(cond)
        return (errorResult)
        ## Choose a return value in case of error
    },
    finally={
        print("Done")
    })
    if ('error' %in% names(out)) {
        returnResults[['error']] <- out$error
    }
    return (returnResults)
}
