


textSpict.explo1 <- function(spict_dat, input){

    inp <- spict_dat$dataExplo$inp

    outi <- paste0(
        "Catch observations:\n",
        "Time: ", paste(round(inp$timeC,3), collapse = ", "), "\n",
        "Catch: ", paste(round(inp$obsC,3), collapse = ", "), "\n",
        "\n",
        "Index observations:\n",
        ifelse(inp$nindex > 1, "Index 1:\n",""),
        "Time: ", paste(round(inp$timeI[[1]],3), collapse = ", "), "\n",
        "Index: ", paste(round(inp$obsI[[1]],3), collapse = ", "), "\n"
    )

        if(inp$nindex > 1) {
            for(i in 2:inp$nindex) {
                outi <- paste0(
                    outi,
                    "Index ",i,":\n",
                    "Time: ", paste(round(inp$timeI[[i]],3),
                                    collapse = ", "), "\n",
                    "Index: ", paste(round(inp$obsI[[i]],3),
                                     collapse = ", "), "\n")
            }
        }

    HTML(gsub("\n","<br/>", outi))

}



textSpict.diag1 <- function(spict_dat, input){

    file.name <- spict_dat$checks$fileName

    ## Catches
    nobsC <- spict_dat$dataExplo$inp$nobsC
    range.timeC <- range(spict_dat$dataExplo$inp$timeC)
    range.obsC <- range(spict_dat$dataExplo$inp$obsC)
    catchunit <- spict_dat$dataExplo$inp$catchunit

    ## Indices
    nindex <- spict_dat$dataExplo$inp$nindex
    nobsI <- rep(NA, nindex)
    range.timeI <- vector("list", nindex)
    range.obsI <- vector("list", nindex)
    for(i in 1:nindex){
        nobsI[i] <- length(na.omit(spict_dat$dataExplo$inp$obsI[[i]]))
        range.timeI[[i]] <- range(spict_dat$dataExplo$inp$timeI[[i]])
        range.obsI[[i]] <- range(spict_dat$dataExplo$inp$obsI[[i]])
    }

    ## TODO number of 0 -> because is set to NA, maybe warning message!
    ## TODO number of missing years
    ## TODO timeing of indices

    HTML(paste0(
        "<h4>Data set information</h4>",
        "File name: ", file.name, "<br>",
        "<br>",

        "<h4>Catch time series</h4>",
        "Number of observations: ", nobsC, "<br>",
        "Time range: ", range.timeC[1], "-", range.timeC[2], "<br>",
        "Catch range: ", range.obsC[1], "-", range.obsC[2], "<br>",
        "Catch unit: ", catchunit, "<br>",
        "<br>",

        "<h4>Rel abundance time series</h4>",
        "Number of indices: ", nindex, "<br>",

        paste0(
            sapply(1:nindex, function(i) {
                paste0(
                    "<b>Index ", i, "</b><br>",
                    "Number of observations: ", nobsI[i], "<br>",
                    "Time range: ", range.timeI[[i]][1], "-", range.timeI[[i]][2], "<br>",
                    "Abundance range: ", range.obsI[[i]][1], "-", range.obsI[[i]][2], "<br><br>"
                )
            }),
            collapse = ""
        )
    ))

}


textSpict.sum <- function(dat, input){

    res <- capture.output(summary(dat$results))

    HTML(paste(res, collapse = "<br>"))
}


textSpict.diag_res <- function(dat, input){

    rep <- dat$results
    ndigits <- 7

    res <- capture.output({
        cat(paste("Convergence: ", rep$opt$convergence, "\t  MSG: ",
                  rep$opt$message, "\n", sep = ""))

        if (rep$opt$convergence > 0) {
            cat("WARNING: Model did not obtain proper convergence! Estimates and uncertainties are most likely invalid and cannot be trusted.\n")
        }

        if (rep$opt$convergence > 0 | rep$inp$optim.method == "SANN") {
            grad <- rep$obj$gr()
            names(grad) <- names(rep$par.fixed)
            cat("Gradient at current parameter vector\t")
            cat("", paste(capture.output(grad), " \n"))
        }

        if (rep$opt$convergence > 0) {
            txtobj <- "Objective function: "
        }else {
            txtobj <- "Objective function at optimum: "
        }
        cat(paste0(txtobj, round(rep$obj$fn(), ndigits), "\n"))


        if ("sderr" %in% names(rep))
            cat("WARNING: Could not calculate standard deviations. The optimum found may be invalid. Proceed with caution.\n")

        ## cat(paste0("Euler time step (years):  1/", round(1/rep$inp$dteuler,
        ##                                                  2), " or ", round(rep$inp$dteuler, 5), "\n"))

        ## str <- paste0("Nobs C: ", rep$inp$nobsC, "\t")
        ## if (rep$inp$nindex > 0) {
        ##     str <- paste0(str, paste0(paste0(",  Nobs I", 1:rep$inp$nindex),
        ##                               ": ", rep$inp$nobsI, collapse = ""),
        ##                   "\t")
        ## }
        ## if (rep$inp$nobsE > 0)
        ##     str <- paste0(str, paste0(",  Nobs E: ", rep$inp$nobsE))
        ## cat(paste0(str, "\n"))

        ## if (rep$inp$catchunit != "") {
        ##     cat(paste("Catch/biomass unit:", rep$inp$catchunit, "\n"))
        ## }

        resout <- sumspict.fixedpars(rep, ndigits = ndigits)
        resout <- sumspict.priors(rep, ndigits)
    })

    HTML(paste(res, collapse = "<br>"))
}
