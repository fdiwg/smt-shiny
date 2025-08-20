

run_spict <- function(inp) {
    set.seed(1)

    returnResults <- list()
    out <- tryCatch( {

        print(paste0("spict version: ", packageVersion("spict")))

        checks <- list()
        checks$convergence <- checks$sd_not_na <- checks$cv_k  <- checks$cv_m <- TRUE
        fit <- try(spict::fit.spict(inp), silent = TRUE)

        ## checks
        has.to.be.no.na <- c(get.par("logFm", fit, exp=TRUE)[2],
                             get.par("logFmsy", fit, exp=TRUE)[2],
                             get.par("logBmsy", fit, exp=TRUE)[2],
                             get.par("logFpFmsynotS", fit),
                             get.par("logBpBmsy", fit),
                             get.par("logFmFmsynotS", fit),
                             get.par("logBmBmsy", fit))

        ## non-convergence
        if(any(is.null(fit)) || any(is.na(fit)) ||
           inherits(fit, "try-error") ||
           is.null(fit$opt$convergence) ||
           is.na(fit$opt$convergence) ||
           anyNA(has.to.be.no.na) ||
           fit$opt$convergence != 0){
            checks$convergence <- FALSE
        }
        if(any(is.infinite(fit$sd))){
            checks$sd_not_na <- FALSE
        }
        cv <- try(get.par("logK", fit, exp = TRUE),silent = TRUE)
        if(!inherits(cv, "try-error")){
            if(inherits(cv, "matrix")){
                if(ncol(cv) >= 5){
                    if(any(cv[,5] > 10) || any(is.na(cv[,5]))){
                        checks$cv_k <- FALSE
                    }
                }else{
                    checks$cv_k <- FALSE
                }
            }
            if(inherits(cv, "numeric")){
                if(length(cv) >= 5){
                    if(any(cv[5] > 10) || any(is.na(cv[5]))){
                        checks$cv_k <- FALSE
                    }
                }else{
                    checks$cv_k <- FALSE
                }
            }
        }else{
            checks$cv_k <- FALSE
        }

        cv <- try(get.par("logm", fit, exp = TRUE),silent = TRUE)
        if(!inherits(cv, "try-error")){
            if(inherits(cv, "matrix")){
                if(ncol(cv) >= 5){
                    if(any(cv[,5] > 10) || any(is.na(cv[,5]))){
                        checks$cv_m <- FALSE
                    }
                }else{
                    checks$cv_m <- FALSE
                }
            }
            if(inherits(cv, "numeric")){
                if(length(cv) >= 5){
                    if(any(cv[5] > 10) || any(is.na(cv[5]))){
                        checks$cv_m <- FALSE
                    }
                }else{
                    checks$cv_m <- FALSE
                }
            }
        }else{
            checks$cv_m <- FALSE
        }

        if (isTRUE(checks$convergence)) {
            fit <- spict::calc.osa.resid(fit)
            fit <- spict::calc.process.resid(fit)
        }

        returnResults[['checks']] <- checks
        returnResults[['res']] <- fit
    },
    error = function(cond) {
        message(paste0("Error!! ", cond))
        errorResult = list()
        errorResult[['error']] <- gettext(cond)
        return (errorResult)
    },
    finally={
        print("Done")
    })

    if ('error' %in% names(out)) {
        returnResults[['error']] <- out$error
    }

    return (returnResults)
}
