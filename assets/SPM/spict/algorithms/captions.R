
captionSpict.tables <- function(dat, input, format = "datatable", type){

    if(format == "datatable"){
        f <- withMathJax("\\(F\\)")
        fmsy <- withMathJax("\\(F_{\\mathrm{MSY}}\\)")
        ffmsy <- withMathJax("\\(F/F_{\\mathrm{MSY}}\\)")
        b <- withMathJax("\\(B\\)")
        bmsy <- withMathJax("\\(B_{\\mathrm{MSY}}\\)")
        bbmsy <- withMathJax("\\(B/B_{\\mathrm{MSY}}\\)")
        perc <- "%"
    }else if(format == "kable"){
        f <- paste0("\\(F\\)")
        fmsy <- paste0("\\(F_{MSY}\\)")
        ffmsy <- paste0("\\(F/F_{MSY}\\)")
        b <- paste0("\\(B\\)")
        bmsy <- paste0("\\(B_{MSY}\\)")
        bbmsy <- paste0("\\(B/B_{MSY}\\)")
        perc <- "\\%"
    }else{
        f <- paste0("F")
        fmsy <- paste0("Fmsy")
        ffmsy <- paste0("F/Fmsy")
        b <- paste0("B")
        bmsy <- paste0("Bmsy")
        bbmsy <- paste0("B/Bmsy")
        perc <- "%"
    }

    switch(type,
           "estimates" = {
               tab.num <- 1
               txt <- paste0(
                   "Estimated values and 95",perc," confidence intervals for the main model parameters: ",
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
                   "Estimated stochastic reference points with 95",perc," confidence intervals: ",
                   "biomass at maximum sustainable yield (", bmsy, "), ",
                   "fishing mortality at maximum sustainable yield (", fmsy, "), ",
                   "and the maximum sustainable yield (MSY)."
               )
           },
           "refs_d" = {
               tab.num <- 4
               txt <- paste0(
                   "Estimated deterministic reference points with 95",perc," confidence intervals: ",
                   "biomass at maximum sustainable yield (", bmsy, "), ",
                   "fishing mortality at maximum sustainable yield (", fmsy, "), ",
                   "and the maximum sustainable yield (MSY)."
               )
           },
           "states" = {
               tab.num <- 3
               txt <- paste0(
                   "Estimated biomass (B) and fishing mortality (F) at the end of the time series, ",
                   "together with stock status indicators: relative biomass (", bbmsy,
                   ") and relative fishing mortality (", ffmsy, ")."
               )
           },
           "pred" = {
               tab.num <- 5
               txt <- paste0(
                   "Estimated biomass (B) and fishing mortality (F) states, and stock status ",
                   "in terms of relative biomass (", bbmsy, ") ",
                   "and relative fishing mortality (", ffmsy, "). ",
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

    if(format == "withFig"){
        f <- withMathJax("\\(F\\)")
        fmsy <- withMathJax("\\(F_{\\mathrm{MSY}}\\)")
        ffmsy <- withMathJax("\\(F/F_{\\mathrm{MSY}}\\)")
        b <- withMathJax("\\(B\\)")
        bmsy <- withMathJax("\\(B_{\\mathrm{MSY}}\\)")
        bbmsy <- withMathJax("\\(B/B_{\\mathrm{MSY}}\\)")
        perc <- "%"
    }else if(format == "withFigLatex"){
        f <- paste0("\\(F\\)")
        fmsy <- paste0("\\(F_{MSY}\\)")
        ffmsy <- paste0("\\(F/F_{MSY}\\)")
        b <- paste0("\\(B\\)")
        bmsy <- paste0("\\(B_{MSY}\\)")
        bbmsy <- paste0("\\(B/B_{MSY}\\)")
        perc <- "\\%"
    }else{
        f <- paste0("F")
        fmsy <- paste0("Fmsy")
        ffmsy <- paste0("F/Fmsy")
        b <- paste0("B")
        bmsy <- paste0("Bmsy")
        bbmsy <- paste0("B/Bmsy")
        perc <- "%"
    }

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
               txt <- paste0("Advanced spict data plot: The top row shows the observations where the horizontal dashed line in the catch plot indicates a guess of MSY. This guess comes from a linear regression between the index and the catch divided by the index (middle row, left). This regression is expected to have a negative slope. A similar plot can be made showing catch versus catch/index (middle row, right) to approximately find the optimal effort (or effort proxy). The catch vs. index observations are shown in the left panel in the bottom row, where the horizontal dashed line indicates a guess of MSY. The proportional increase in the index as a function of catch (bottom row, right) should show primarily positive increases in index at low catches and vice versa. Positive increases in index at large catches could indicate model violations.")
           },
           "diag2" = {
               plot.num <- 5
               txt <- paste0("Histogram of all catch and index observations.")
           },
           "sum" = {
               plot.num <- 6
               txt <- paste0("Estimated relative biomass (", bbmsy, ", upper left), relative fishing mortality (", ffmsy, ", upper right), catches (lower left), and stock status in terms of ", bbmsy, " and ", ffmsy, " (Kobe plot, lower right). The blue shaded areas in the relative biomass and fishing mortality plot display the 95",perc," confidence intervals. The circles show the index and catch observations, where the colour indicates the in-year timing and different plotting symbols are used for different index time series. The horizontal line in the catch plot shows the maximum sustainable yield (MSY) and the gray shaded area the associated 95",perc," confidence interval. The grayish shaded area in the Kobe plot indicates the 95",perc," confidence interval of the reference points ", bmsy," and ", fmsy,".")
           },
           "prod" = {
               plot.num <- 7
               txt <- paste0("Estimated production curve showing the theoretical surplus production (y axis) as a function of biomass relative to carrying capacity (B/K, x axis). The vertical dotted line indicates the the relative biomass where surplus production is maximized (MSY). The observed annual surplus production is plotted as blue circles conected by a line.")
           },
           "abs" = {
               plot.num <- 8
               txt <- paste0("Estimated absolute (first y axis) and relative (second y axis) biomass (", b, " and ", bbmsy, ", left) and fishing mortality (", f, " and ", ffmsy, ", right). The blue shaded areas display the 95",perc," confidence intervals of the relative states (",bbmsy," and ",ffmsy,"). The dashed lines indicate the 95",perc," confidence intervals of the absolute states (",b," and ",f,"). The horizontal lines show the reference points (",bmsy, " and ", fmsy,") and the gray shaded area the associated 95",perc," confidence interval.")
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
               txt <- paste0(
                   "Residual diagnostics for the catch time series (first column) and index time series (remaining columns): ",
                   "Row 1: log-transformed observations; ",
                   "Row 2: one-step-ahead (OSA) residuals with a test for bias — a p-value < 5% indicates that the mean residual differs significantly from zero; ",
                   "Row 3: autocorrelation function (ACF) of residuals with a Ljung–Box test — a p-value < 5% indicates significant autocorrelation; ",
                   "Row 4: quantile–quantile (Q–Q) plot of residuals with a Shapiro–Wilk test — a p-value < 5% indicates deviation from normality. ",
                   "Panel headers (and associated p-values) are colour-coded: red highlights violations of the assumptions."
               )
           },
           "resid2" = {
               plot.num <- 11
               txt <- paste0("Residual diagnostics for the biomass process (first column) and fishing mortality process (second column): ",
                             "Row 1: log-transformed observations; ",
                             "Row 2: one-step-ahead (OSA) residuals with a test for bias — a p-value < 5% indicates that the mean residual differs significantly from zero; ",
                             "Row 3: autocorrelation function (ACF) of residuals with a Ljung–Box test — a p-value < 5% indicates significant autocorrelation; ",
                             "Row 4: quantile–quantile (Q–Q) plot of residuals with a Shapiro–Wilk test — a p-value < 5% indicates deviation from normality. ",
                             "Panel headers (and associated p-values) are colour-coded: red highlights violations of the assumptions.")
           }
           )

    if(format == "withFig"){
        txt <- paste0("<p class=\"pheader_elefan\">Figure ",plot.num,": ",txt, "</p>")
    }else if(format == "withFigLatex"){
        txt <- paste0("<p> Figure ",plot.num,": ",txt, "</p>")
    }

    return(txt)
}
