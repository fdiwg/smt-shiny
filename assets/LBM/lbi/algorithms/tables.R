
tableLBI.intro <- function(lbi_dat, input, format = "dataframe"){

    if(format == "kable"){
        tmp <- data.frame(Indicator = c("L\\textsubscript{max5\\%}",
                                        "L\\textsubscript{95\\%}",
                                        "P\\textsubscript{mega}",
                                        "L\\textsubscript{25\\%}",
                                        "L\\textsubscript{c}",
                                        "L\\textsubscript{mean}",
                                        "L\\textsubscript{maxy}",
                                        "L\\textsubscript{mean}"
                                        ),
                          Description = c("Mean length of the largest 5\\% of individuals in the catch",
                                          "95\\% percentile of length distribution",
                                          "Proportion of fish larger than optimal harvest length (L\\textsubscript{opt}) + 15\\%",
                                          "25\\% percentile of the length distribution",
                                          "Length at 50\\% modal abundance",
                                          "Mean length of individuals > L\\textsubscript{c}",
                                          "Length class with maximum biomass in catch",
                                          "Mean length of individuals > L\\textsubscript{c}"
                                          ),
                          "Ref point" = c("L\\textsubscript{inf}",
                                          "L\\textsubscript{inf}",
                                          "0.3",
                                          "L\\textsubscript{mat}",
                                          "L\\textsubscript{mat}",
                                          "L\\textsubscript{opt}",
                                          "L\\textsubscript{opt}",
                                          "L\\textsubscript{F=M}"
                                          ),
                          "Indicator ratio" = c("L\\textsubscript{max5\\%}/L\\textsubscript{inf}",
                                                "L\\textsubscript{95\\%}/L\\textsubscript{inf}",
                                                "P\\textsubscript{mega}",
                                                "L\\textsubscript{25\\%}/L\\textsubscript{mat}",
                                                "L\\textsubscript{c}/L\\textsubscript{mat}",
                                                "L\\textsubscript{mean}/L\\textsubscript{opt}",
                                                "L\\textsubscript{maxy}/L\\textsubscript{opt}",
                                                "L\\textsubscript{mean}/L\\textsubscript{F=M}"
                                                ),
                          "Exp. value" = c("> 0.8",
                                           "> 0.8",
                                           "> 0.3",
                                           "> 1",
                                           "> 1",
                                           "$\\approx 1$",
                                           "$\\approx 1$",
                                           "$\\leq 1$"
                                           ),
                          Property = c("Conservation (large individuals)",
                                       "Conservation (large individuals)",
                                       "Conservation (large individuals)",
                                       "Conservation (immature)",
                                       "Conservation (immature)",
                                       "Optimal yield",
                                       "Optimal yield",
                                       "MSY"
                                       ))

    }else if(format == "datatable"){
        tmp <- data.frame(Indicator = c("L<sub>max5%</sub>",
                                        "L<sub>95%</sub>",
                                        "P<sub>mega</sub>",
                                        "L<sub>25%</sub>",
                                        "L<sub>c</sub>",
                                        "L<sub>mean</sub>",
                                        "L<sub>maxy</sub>",
                                        "L<sub>mean</sub>"
                                        ),
                          Description = c("Mean length of the largest 5% of individuals in the catch",
                                          "95% percentile of length distribution",
                                          "Proportion of fish larger than optimal harvest length (L<sub>opt</sub>) + 15%",
                                          "25% percentile of the length distribution",
                                          "Length at 50% modal abundance",
                                          "Mean length of individuals > L<sub>c</sub>",
                                          "Length class with maximum biomass in catch",
                                          "Mean length of individuals > L<sub>c</sub>"
                                          ),
                          "Ref point" = c("L<sub>inf</sub>",
                                          "L<sub>inf</sub>",
                                          "0.3",
                                          "L<sub>mat</sub>",
                                          "L<sub>mat</sub>",
                                          "L<sub>opt</sub>",
                                          "L<sub>opt</sub>",
                                          "L<sub>F=M</sub>"
                                          ),
                          "Indicator ratio" = c("L<sub>max5%</sub>/L<sub>inf</sub>",
                                                "L<sub>95%</sub>/L<sub>inf</sub>",
                                                "P<sub>mega</sub>",
                                                "L<sub>25%</sub>/L<sub>mat</sub>",
                                                "L<sub>c</sub>/L<sub>mat</sub>",
                                                "L<sub>mean</sub>/L<sub>opt</sub>",
                                                "L<sub>maxy</sub>/L<sub>opt</sub>",
                                                "L<sub>mean</sub>/L<sub>F=M</sub>"
                                                ),
                          "Exp. value" = c("> 0.8",
                                           "> 0.8",
                                           "> 0.3",
                                           "> 1",
                                           "> 1",
                                           "&asymp; 1",
                                           "&asymp; 1",
                                           "&le; 1"
                                           ),
                          Property = c("Conservation (large individuals); indicating whether number of large individuals is lower or higher than expected.",
                                       "Conservation (large individuals); indicating whether number of large individuals is lower or higher than expected.",
                                       "Conservation (large individuals); indicating whether number of large individuals is lower or higher than expected.",
                                       "Conservation (immature); indicating whether number of immature individuals is lower or higher than expected.",
                                       "Conservation (immature); indicating whether number of immature individuals is lower or higher than expected.",
                                       "Optimal yield; indicating whether the stock is likely to be exploited optimally.",
                                       "Optimal yield; indicating whether the stock is likely to be exploited optimally.",
                                       "MSY; indicating whether the yield is likely to correspond to the maximum sustainable yield (MSY)."
                                       ))
    }else{
        tmp <- NULL
    }

    labs <- c("Indicator","Description","Ref. point","Indicator ratio","Exp. value","Property")

    if(format == "dataframe"){
        colnames(tmp) <- labs
        return(tmp)
    }else if(format == "kable"){
        capti <- captionLBI.tables(lbi_dat, input, format = format,
                                   type = "intro")
        return(knitr::kable(tmp, format = "latex", col.names = labs,
                            row.names = FALSE, escape = FALSE,
                            linesep = "",
                            caption = capti) %>%
               kable_styling(font_size = 11,
                             latex_options = "HOLD_position") %>%
               column_spec(2, width = "4.5cm") %>%
               column_spec(6, width = "3.5cm")
               )
    }else if(format == "datatable"){
        return(DT::datatable(tmp, colnames = labs,
                             escape = FALSE, rownames = FALSE))
    }
}



tableLBI.data <- function(lbi_dat, input, format = "dataframe"){

    tmp <- cbind(lbi_dat$dataExplo$lfq$midLengths,
                 lbi_dat$dataExplo$lfq$catch)

    labs <- c("Mid lengths", as.character(lbi_dat$dataExplo$lfq$dates))

    ## Rounding
    tmp <- signif(tmp, digits = 3)

    ## Return
    colnames(tmp) <- labs
    rownames(tmp) <- NULL
    return(tmp)
}



tableLBI.inputPars <- function(lbi_dat, input, format = "datatable"){

    ## TODO: add number of years or year range, number of samples, etc. to table

    if(input$LBI_split_mk){
        mk <- input$LBI_M / input$LBI_K
    }else{
        mk <- input$LBI_MK
    }

    tab <- data.frame(binSize = input$LBI_binSize,
                      lengthUnit = input$LBI_lengthUnit,
                      MK = mk,
                      M = input$LBI_M,
                      K = input$LBI_K,
                      Linf = input$LBI_Linf,
                      Lm50 = input$LBI_Lm50,
                      a = input$LBI_LWa,
                      b = input$LBI_LWb)
    ind.remove <- which(is.na(tab[1,]))
    tab <- tab[,-ind.remove]

    if(format == "datatable"){
        labs <- c("Bin size",
                  "Length unit",
                  "M/K",
                  "M",
                  "K",
                  "L<sub>&#8734;</sub>",
                  "L<sub>mat</sub>",
                  "a",
                  "b")
    }else if(format == "kable"){
        labs <- c("Bin size",
                  "Length unit",
                  "M/K",
                  "M",
                  "K",
                  "L\\textsubscript{inf}",
                  "L\\textsubscript{mat}",
                  "a",
                  "b")
    }else if(format == "dataframe"){
        labs <- c("Bin size",
                  "Length unit",
                  "M/K",
                  "M",
                  "K",
                  "Linf",
                  "Lm50",
                  "a",
                  "b")
    }
    colnames(tab) <- labs[-ind.remove]

    ## Selection
    tmp <- tab
    labs <- labs[-ind.remove]

    ## Rounding
    tmp[,which(labs != "Length unit")] <- signif(tmp[,which(labs != "Length unit")],
                                                digits = 3)

    if(format == "dataframe"){
        colnames(tmp) <- labs
        return(tmp)
    }else if(format == "kable"){
        capti <- captionLBI.tables(lbi_dat, input, format = format,
                                   type = "input")
        return(knitr::kable(tmp, format = "latex", col.names = labs,
                            row.names = FALSE, escape = FALSE,
                            align=rep('c',ncol(tmp)),
                            linesep = "",
                            caption = capti) %>%
               kable_styling(font_size = 11,
                             latex_options = "HOLD_position") %>%
               column_spec(1:ncol(tmp), width = "1.2cm"))
    }else if(format == "datatable"){
        return(DT::datatable(tmp, colnames = labs,
                             escape = FALSE, rownames = FALSE,
                             options = list(dom = 't',
                                            columnDefs = list(list(
                                                className = 'dt-center',
                                                targets = 0:(ncol(tmp)-1))))))
    }
}


tableLBI.indicators <- function(lbi_dat, input, format = "datatable"){
    ind <- apply(lbi_dat$results, 2, function(x) !all(is.na(x)))
    res <- round(lbi_dat$results[,ind],2)
    refs <- c("year","Lc","Lm50","Lopt","Linf","Lmax5","L75","L25",
              "Lmed","L95","L90","Lmean","Lmaxy",
              "Pmega","PmegaRef","LFeM","Lmaxy_Lopt","L95_Linf","Lmean_LFeM",
              "Lmean_Lm50","Lmean_Lopt","Lmax5_Linf",
              "Lc_Lm50","L25_Lm50")
    if(format == "datatable"){
        labs <- c(
            "Year",
            "L<sub>c</sub>",
            "L<sub>mat</sub>",
            "L<sub>opt</sub>",
            "L<sub>&#8734;</sub>",
            "L<sub>max5%</sub>",
            "L<sub>75%</sub>",
            "L<sub>25%</sub>",
            "L<sub>med</sub>",
            "L<sub>95%</sub>",
            "L<sub>90%</sub>",
            "L<sub>mean</sub>",
            "L<sub>maxy</sub>",
            "P<sub>mega</sub>",
            "P<sub>mega</sub>Ref",
            "L<sub>F=M</sub>",
            "L<sub>maxy</sub>/L<sub>opt</sub>",
            "L<sub>95%</sub>/L<sub>&#8734;</sub>",
            "L<sub>mean</sub>/L<sub>F=M</sub>",
            "L<sub>mean</sub>/L<sub>mat</sub>",
            "L<sub>mean</sub>/L<sub>opt</sub>",
            "L<sub>max5%</sub>/L<sub>&#8734;</sub>",
            "L<sub>c</sub>/L<sub>mat</sub>",
            "L<sub>25%</sub>/L<sub>mat</sub>")
    }else if(format == "kable"){
        labs <- c("Year",
                  "L\\textsubscript{c}",
                  "L\\textsubscript{mat}",
                  "L\\textsubscript{opt}",
                  "L\\textsubscript{inf}",
                  "L\\textsubscript{max5\\%}",
                  "L\\textsubscript{75\\%}",
                  "L\\textsubscript{25\\%}",
                  "L\\textsubscript{med}",
                  "L\\textsubscript{95\\%}",
                  "L\\textsubscript{90\\%}",
                  "L\\textsubscript{mean}",
                  "L\\textsubscript{maxy}",
                  "P\\textsubscript{mega}",
                  "P\\textsubscript{mega}Ref",
                  "L\\textsubscript{F=M}",
                  "L\\textsubscript{maxy}/L\\textsubscript{opt}",
                  "L\\textsubscript{95\\%}/L\\textsubscript{inf}",
                  "L\\textsubscript{mean}/L\\textsubscript{F=M}",
                  "L\\textsubscript{mean}/L\\textsubscript{mat}",
                  "L\\textsubscript{mean}/L\\textsubscript{opt}",
                  "L\\textsubscript{max5\\%}/L\\textsubscript{inf}",
                  "L\\textsubscript{c}/L\\textsubscript{mat}",
                  "L\\textsubscript{25\\%}/L\\textsubscript{mat}")
    }else if(format == "dataframe"){
        labs <- refs
    }
    rnames <- colnames(res)
    rnames <- labs[match(rnames, refs)]
    if(is.null(input$LBI_LWa) || is.na(input$LBI_LWa) || is.null(input$LBI_LWb) || is.na(input$LBI_LWb)){
        ind <- 1:8
    }else{
        ind <- 1:9
    }


    ## Selection
    tmp <- as.data.frame(res[,ind])
    labs <- rnames[ind]

    ## Rounding
    tmp <- signif(tmp, digits = 3)

    if(format == "dataframe"){
        colnames(tmp) <- labs
        return(tmp)
    }else if(format == "kable"){
        capti <- captionLBI.tables(lbi_dat, input, format = format,
                                   type = "indicators")
        return(knitr::kable(tmp, format = "latex", col.names = labs,
                            row.names = FALSE, escape = FALSE,
                            align=rep('c',ncol(tmp)),
                            linesep = "",
                            caption = capti) %>%
               kable_styling(font_size = 11,
                             latex_options = "HOLD_position") %>%
               column_spec(1:ncol(tmp), width = "1.5cm"))
    }else if(format == "datatable"){
        return(DT::datatable(tmp, colnames = labs,
                             escape = FALSE, rownames = FALSE,
                             options = list(dom = 't',
                                            columnDefs = list(list(
                                                className = 'dt-center',
                                                targets = 0:(ncol(tmp)-1))))))
    }
}


tableLBI.ratios <- function(lbi_dat, input, format = "datatable"){
    ind <- apply(lbi_dat$results, 2, function(x) !all(is.na(x)))
    res <- round(lbi_dat$results[,ind],2)
    refs <- c("year","Lc","Lm50","Lopt","Linf","Lmax5","L75","L25",
              "Lmed","L95","L90","Lmean","Lmaxy",
              "Pmega","PmegaRef","LFeM","Lmaxy_Lopt","L95_Linf","Lmean_LFeM",
              "Lmean_Lm50","Lmean_Lopt","Lmax5_Linf",
              "Lc_Lm50","L25_Lm50")
    if(format == "datatable"){
        labs <- c(
            "Year",
            "L<sub>c</sub>",
            "L<sub>mat</sub>",
            "L<sub>opt</sub>",
            "L<sub>&#8734;</sub>",
            "L<sub>max5%</sub>",
            "L<sub>75%</sub>",
            "L<sub>25%</sub>",
            "L<sub>med</sub>",
            "L<sub>95%</sub>",
            "L<sub>90%</sub>",
            "L<sub>mean</sub>",
            "L<sub>maxy</sub>",
            "P<sub>mega</sub>",
            "P<sub>mega</sub>Ref",
            "L<sub>F=M</sub>",
            "L<sub>maxy</sub>/L<sub>opt</sub>",
            "L<sub>95%</sub>/L<sub>&#8734;</sub>",
            "L<sub>mean</sub>/L<sub>F=M</sub>",
            "L<sub>mean</sub>/L<sub>mat</sub>",
            "L<sub>mean</sub>/L<sub>opt</sub>",
            "L<sub>max5%</sub>/L<sub>&#8734;</sub>",
            "L<sub>c</sub>/L<sub>mat</sub>",
            "L<sub>25%</sub>/L<sub>mat</sub>")
    }else if(format == "kable"){
        labs <- c("Year",
                  "L\\textsubscript{c}",
                  "L\\textsubscript{mat}",
                  "L\\textsubscript{opt}",
                  "L\\textsubscript{inf}",
                  "L\\textsubscript{max5\\%}",
                  "L\\textsubscript{75\\%}",
                  "L\\textsubscript{25\\%}",
                  "L\\textsubscript{med}",
                  "L\\textsubscript{95\\%}",
                  "L\\textsubscript{90\\%}",
                  "L\\textsubscript{mean}",
                  "L\\textsubscript{maxy}",
                  "P\\textsubscript{mega}",
                  "P\\textsubscript{mega}Ref",
                  "L\\textsubscript{F=M}",
                  "L\\textsubscript{maxy}/L\\textsubscript{opt}",
                  "L\\textsubscript{95\\%}/L\\textsubscript{inf}",
                  "L\\textsubscript{mean}/L\\textsubscript{F=M}",
                  "L\\textsubscript{mean}/L\\textsubscript{mat}",
                  "L\\textsubscript{mean}/L\\textsubscript{opt}",
                  "L\\textsubscript{max5\\%}/L\\textsubscript{inf}",
                  "L\\textsubscript{c}/L\\textsubscript{mat}",
                  "L\\textsubscript{25\\%}/L\\textsubscript{mat}")
    }else if(format == "dataframe"){
        labs <- refs
    }
    rnames <- colnames(res)
    rnames <- labs[match(rnames, refs)]
    if(is.null(input$LBI_LWa) || is.na(input$LBI_LWa) || is.null(input$LBI_LWb) || is.na(input$LBI_LWb)){
        ind <- c(1,9:ncol(res))
    }else{
        ind <- c(1,10:ncol(res))
    }

    ## Selection
    tmp <- as.data.frame(res[,ind])
    labs <- rnames[ind]

    ## Rounding
    tmp <- signif(tmp, digits = 3)

    ## cols <- rgb(t(col2rgb(c("darkred","darkgreen"))/255), alpha = 0.3)
    cols <- c("#EA6249","#6CB87B")
    if(format == "dataframe"){
        colnames(tmp) <- labs
        return(tmp)
    }else if(format == "kable"){
        cols <- c("Greeni","Redi")
        capti <- captionLBI.tables(lbi_dat, input, format = format,
                                   type = "ratios")
        tmp[,2] <- kableExtra::cell_spec(tmp[,2], format = "latex", background = ifelse(tmp[,2] > 0.8, cols[1], cols[2]))
        tmp[,3] <- kableExtra::cell_spec(tmp[,3], format = "latex", background = ifelse(tmp[,3] > 0.8, cols[1], cols[2]))
        tmp[,4] <- kableExtra::cell_spec(tmp[,4], format = "latex", background = ifelse(tmp[,4] > 0.3, cols[1], cols[2]))
        tmp[,5] <- kableExtra::cell_spec(tmp[,5], format = "latex", background = ifelse(tmp[,5] > 1, cols[1], cols[2]))
        tmp[,6] <- kableExtra::cell_spec(tmp[,6], format = "latex", background = ifelse(tmp[,6] > 1, cols[1], cols[2]))
        tmp[,7] <- kableExtra::cell_spec(tmp[,7], format = "latex", background = ifelse(tmp[,7] > 1, cols[1], cols[2]))
        tmp[,8] <- kableExtra::cell_spec(tmp[,8], format = "latex", background = ifelse(tmp[,8] > 1, cols[1], cols[2]))
        if(ncol(tmp) > 8) tmp[,9] <- kableExtra::cell_spec(tmp[,9], format = "latex", background = ifelse(tmp[,9] > 1, cols[1], cols[2]))


        return(
            kableExtra::kable(tmp, format = "latex", col.names = labs,
                              booktabs = TRUE,
                              row.names = FALSE, escape = FALSE,
                              align=rep('c',ncol(tmp)),
                              linesep = "",
                              caption = capti)  %>%
            kable_styling(font_size = 11, latex_options = "HOLD_position") %>%
            column_spec(1:ncol(tmp), width = "1.7cm")
        )

    }else if(format == "datatable"){
        return(DT::datatable(tmp, colnames = labs,
                             escape = FALSE, rownames = FALSE,
                             options = list(dom = 't',
                                            columnDefs = list(list(
                                                className = 'dt-center',
                                                targets = 0:(ncol(tmp)-1))))) %>%
               formatStyle(names(tmp)[c(2,3)],
                           backgroundColor = styleInterval(0.8, c(cols[1], cols[2]))) %>%
               formatStyle(names(tmp)[c(4)],
                           backgroundColor = styleInterval(0.3, c(cols[1], cols[2]))) %>%
               formatStyle(names(tmp)[c(5:length(names(tmp)))],
                           backgroundColor = styleInterval(1, c(cols[1], cols[2]))))
    }
}
