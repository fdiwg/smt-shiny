
captionSpict.tables <- function(dat, input, format = "datatable", type){

    switch(type,
           "estimates" = {
               tab.num <- 1
               txt <- paste0(
                   "Estimated values and 95% confidence intervals for the main model parameters: ",
                   "r = intrinsic growth rate, K = carrying capacity, m = maximum sustainable yield (MSY), ",
                   "n = shape parameter of the production curve, q = catchability, ",
                   "sdb = standard deviation of biomass process errors, ",
                   "sdf = standard deviation of fishing mortality process errors, ",
                   "sdi = standard deviation of index observation errors, ",
                   "sdc = standard deviation of catch observation errors; ",
                   "and hyper-parameters: alpha = sdb/sdi, beta = sdf/sdc. ",
                   "Note that multiple catchability (q) and observation error (sdi) parameters may be estimated when multiple indices are included."
               )
           },
           "refs_s" = {
               tab.num <- 2
               txt <- paste0(
                   "Estimated stochastic reference points with 95% confidence intervals: ",
                   "biomass at maximum sustainable yield (", withMathJax("\\(B_{\\mathrm{MSY}}\\)"), "), ",
                   "fishing mortality at maximum sustainable yield (", withMathJax("\\(F_{\\mathrm{MSY}}\\)"), "), ",
                   "and the maximum sustainable yield (MSY)."
               )
           },
           "refs_d" = {
               tab.num <- 4
               txt <- paste0(
                   "Estimated deterministic reference points with 95% confidence intervals: ",
                   "biomass at maximum sustainable yield (", withMathJax("\\(B_{\\mathrm{MSY}}\\)"), "), ",
                   "fishing mortality at maximum sustainable yield (", withMathJax("\\(F_{\\mathrm{MSY}}\\)"), "), ",
                   "and the maximum sustainable yield (MSY)."
               )
           },
           "states" = {
               tab.num <- 3
               txt <- paste0(
                   "Estimated biomass (B) and fishing mortality (F) at the end of the time series, ",
                   "together with stock status indicators: relative biomass (", withMathJax("\\(B/B_{\\mathrm{MSY}}\\)"),
                   ") and relative fishing mortality (", withMathJax("\\(F/F_{\\mathrm{MSY}}\\)"), ")."
               )
           },
           "pred" = {
               tab.num <- 5
               txt <- paste0(
                   "Estimated biomass (B) and fishing mortality (F) states, and stock status ",
                   "in terms of relative biomass (", withMathJax("\\(B/B_{\\mathrm{MSY}}\\)"), ") ",
                   "and relative fishing mortality (", withMathJax("\\(F/F_{\\mathrm{MSY}}\\)"), "). ",
                   "Results are forecasted one year beyond the end of the observed time series, ",
                   "including predicted catch for the forecast year and the equilibrium biomass ",
                   "expected if fishing were to continue indefinitely. ",
                   "During the forecast, fishing mortality is assumed constant at the level estimated ",
                   "for the final year of the time series."
               )
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
                             ", shown in the first panel) and relative abundance index or indices (shown in the following panel(s)). The colour indicates the in-year timing. Missing values or zeros are omitted from the time series and further analysis.")
           },
           "explo2" = {
               plot.num <- 2
               txt <- paste0("Relative uncertainty associated with the catch time series (first panel) and with the relative abundance index or indices (remaining panels). The colour indicates the in-year timing. If uploaded data does not include information about the relative uncertainty, it is assumed to be constant over time (at 1).")
           },
           "priors" = {
               plot.num <- 3
               active.priors <- names(which(sapply(dat$dataExplo$inp$priors, function(x) if(is.list(x)) any(sapply(x, function(x) x[3] == 1)) else x[3] == 1)))
               active.priors <- gsub("log", "", active.priors)
               txt <- paste0("Density distributions of the activated prior(s): ",
                             paste(active.priors, collapse = ", "))
           },
           "diag1" = {
               plot.num <- 4
               txt <- paste0("Advanced spict data plot.")
           },
           "diag2" = {
               plot.num <- 5
               txt <- paste0("Histogram of catches and indices.")
           },
           "sum" = {
               plot.num <- 6
               txt <- paste0("Estimated relative biomass (", withMathJax("\\(B/B_{\\mathrm{MSY}}\\)"), ", upper left), relative fishing mortality (", withMathJax("\\(F/F_{\\mathrm{MSY}}\\)"), ", upper right), catches (lower left), and stock status in terms of ", withMathJax("\\(B/B_{\\mathrm{MSY}}\\)"), " and ", withMathJax("\\(F/F_{\\mathrm{MSY}}\\)"), " (Kobe plot, lower right). The blue shaded areas in the relative biomass and fishing mortality plot display the 95% confidence intervals. The circles show the index and catch observations, where the colour indicates the in-year timing and different plotting symbols are used for different index time series. The horizontal line in the catch plot shows the maximum sustainable yield (MSY) and the gray shaded area the associated 95% confidence interval. The grayish shaded area in the Kobe plot indicates the 95% confidence interval of the reference points ", withMathJax("\\(B_{\\mathrm{MSY}}\\)")," and ",withMathJax("\\(F_{\\mathrm{MSY}}\\)"),".")
           },
           "prod" = {
               plot.num <- 7
               txt <- paste0("Estimated production curve showing the theoretical surplus production (y axis) as a function of biomass relative to carrying capacity (B/K, x axis). The vertical dotted line indicates the the relative biomass where surplus production is maximized (MSY). The observed annual surplus production is plotted as blue circles conected by a line.")
           },
           "abs" = {
               plot.num <- 8
               txt <- paste0("Estimated absolute (first y axis) and relative (second y axis) biomass (", withMathJax("\\(B\\)"), " and ", withMathJax("\\(B/B_{\\mathrm{MSY}}\\)"), ", left) and fishing mortality (", withMathJax("\\(F\\)"), " and ", withMathJax("\\(F/F_{\\mathrm{MSY}}\\)"), ", right). The blue shaded areas display the 95% confidence intervals of the relative states (",withMathJax("\\(B/B_{\\mathrm{MSY}}\\)")," and ",withMathJax("\\(F/F_{\\mathrm{MSY}}\\)"),"). The dashed lines indicate the 95% confidence intervals of the absolute states (",withMathJax("\\(B\\)"),"and",withMathJax("\\(F\\)"),"). The horizontal linesshows the reference points (",withMathJax("\\(B_{\\mathrm{MSY}}\\)"), " and ", withMathJax("\\(F_{\\mathrm{MSY}}\\)"),") and the gray shaded area the associated 95% confidence interval.")
           },
           "priors2" = {
               plot.num <- 9
               active.priors <- names(which(sapply(dat$dataExplo$inp$priors, function(x) if(is.list(x)) any(sapply(x, function(x) x[3] == 1)) else x[3] == 1)))
               active.priors <- gsub("log", "", active.priors)
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
