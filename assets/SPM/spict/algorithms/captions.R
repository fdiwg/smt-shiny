
captionSpict.tables <- function(dat, input, format = "datatable", type){

    switch(type,
           "estimates" = {
               tab.num <- 1
               txt <- paste0("Parameter estimates")
           },
           "refs_s" = {
               tab.num <- 2
               txt <- paste0("Stochastic reference points")
           },
           "refs_d" = {
               tab.num <- 3
               txt <- paste0("Deterministic reference points")
           },
           "states" = {
               tab.num <- 4
               txt <- paste0("Parameter estimates")
           },
           "pred" = {
               tab.num <- 5
               txt <- paste0("Parameter estimates")
           }
           )

    if(format == "datatable"){
        txt <- paste0("<p class=\"pheader_elefan\">Table ",tab.num,": ",txt, "</p>")
    }

    return(txt)
}


captionSpict.plots <- function(dat, input, format = "withFig", type){

    switch(type,
           "explo1" = {
               plot.num <- 1
               txt <- paste0("Time series of catches (in ", input$catchunit,
                             ", shown in the first panel) and relative abundance index or indices (shown in the following panel(s)).")
           },
           "explo2" = {
               plot.num <- 2
               txt <- paste0("Relative uncertainty associated with the catch time series (first panel) and with the relative abundance index or indices (remaining panels).")
           },
           "priors" = {
               plot.num <- 3
               active.priors <- names(which(sapply(dat$dataExplo$inp$priors, function(x) if(is.list(x)) any(sapply(x, function(x) x[3] == 1)) else x[3] == 1)))
               txt <- paste0("Density distributions of the activated prior(s): ",
                             paste(active.priors, collapse = ", "))
           },
           "diag1" = {
               plot.num <- 4
               txt <- paste0("Advanced spict data plot")
           },
           "diag2" = {
               plot.num <- 5
               txt <- paste0("Histogram of catches and indices")
           },
           "sum" = {
               plot.num <- 6
               txt <- paste0("Estimated relative biomass (B/Bmsy, upper left), relative fishing mortality (F/Fmsy, upper right), catch time series (lower left), and stock status in relation to reference points (Kobe plot, lower right).")
           },
           "abs" = {
               plot.num <- 7
               txt <- paste0("Estimated absolute biomass (left panel) and fishing mortality (right panel).")
           },
           "prod" = {
               plot.num <- 8
               txt <- paste0("Estimated production curve.")
           },
           "priors2" = {
               plot.num <- 9
               active.priors <- names(which(sapply(dat$dataExplo$inp$priors, function(x) if(is.list(x)) any(sapply(x, function(x) x[3] == 1)) else x[3] == 1)))
               txt <- paste0("Prior density distributions (black) and estimated posterior distributions (green) for the activated priors: ",
                             paste(active.priors, collapse = ", "))
           },
           "resid1" = {
               plot.num <- 10
               txt <- paste0("Residual diagnostics for the catch time series (first column) and index time series (remaining columns): ",
                             "Row 1: log-transformed observations; ",
                             "Row 2: one-step-ahead (OSA) residuals; ",
                             "Row 3: autocorrelation function (ACF) of residuals; ",
                             "Row 4: quantile-quantile (Q–Q) plot of residuals.")

           },
           "resid2" = {
               plot.num <- 11
               txt <- paste0("Residual diagnostics for the predicted biomass process (first column) and fishing mortality process (second column): ",
                             "Row 1: log-transformed observations; ",
                             "Row 2: one-step-ahead (OSA) residuals; ",
                             "Row 3: autocorrelation function (ACF) of residuals; ",
                             "Row 4: quantile-quantile (Q–Q) plot of residuals.")
           }
           )

    if(format == "withFig"){
        txt <- paste0("<p class=\"pheader_elefan\">Figure ",plot.num,": ",txt, "</p>")
    }else if(format == "withFigLatex"){
        txt <- paste0("<p> Figure ",plot.num,": ",txt, "</p>")
    }

    return(txt)
}
