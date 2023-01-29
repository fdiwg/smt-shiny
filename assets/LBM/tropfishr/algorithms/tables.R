tableTropFishR.data <- function(elefan_ga, input, format = "dataframe"){

    tmp <- cbind(elefan_ga$dataExplo$lfq$midLengths,
                 elefan_ga$dataExplo$lfq$catch)

    labs <- c("Mid lengths", as.character(elefan_ga$dataExplo$lfq$dates))

    ## Rounding
    tmp <- signif(tmp, digits = 3)

    ## Return
    colnames(tmp) <- labs
    rownames(tmp) <- NULL
    return(tmp)
}


tableTropFishR.input <- function(elefan_ga, input, format = "datatable"){

    ## TODO: add number of years or year range, number of samples, etc. to table

    tmp <- round(range(as.numeric(as.character(input$ELEFAN_years_selected)), na.rm = TRUE))
    if(tmp[2] == tmp[1]){
        yearRange <- tmp[1]
    }else{
        yearRange <- paste(tmp, collapse="-")
    }

    tab <- t(data.frame(yearRange = yearRange,
                        lengthUnit = input$elefan_lengthUnit,
                        binSize = input$ELEFAN_GA_binSize,
                        ma = input$ELEFAN_GA_MA,
                        addlsqrt = if(!is.null(input$ELEFAN_GA_addlsqrt)){
                                       if(input$ELEFAN_GA_addlsqrt){TRUE}else{FALSE}}else{FALSE}
                        ))

    ## Growth parameters provided/estimated
    if(!is.na(input$provide_Linf)){
        linf_range <- input$provide_Linf
    }else{
        if(is.null(input$ELEFAN_GA_Linf)){
            linf_range <- c(0.8,1.2) * round(max(elefan_ga$dataExplo$lfq$midLengths)/0.95)
        }else{
            linf_range <- range(input$ELEFAN_GA_Linf)
        }
        linf_range <- paste0(min(linf_range), " - ", max(linf_range))
    }

    if(!is.na(input$provide_K)){
        k_range <- input$provide_K
    }else{
        k_range <- range(input$ELEFAN_GA_K)
        k_range <- paste0(min(k_range), " - ", max(k_range))
    }

    if(!is.na(input$provide_t_anchor)){
        ta_range <- input$provide_t_anchor
    }else{
        ta_range <- range(input$ELEFAN_GA_t_anchor)
        ta_range <- paste0(min(ta_range), " - ", max(ta_range))
    }

    tab <- as.data.frame(rbind(tab,
                               ga_linf = linf_range,
                               ga_k = k_range,
                               ga_ta = ta_range))

    ## Seasonality
    if(!is.null(input$ELEFAN_GA_seasonalised) && input$ELEFAN_GA_seasonalised){
        tab <- as.data.frame(rbind(tab,
                                   C = paste0(min(input$ELEFAN_GA_C)," - ",max(input$ELEFAN_GA_C)),
                                   ts = paste0(min(input$ELEFAN_GA_ts)," - ",max(input$ELEFAN_GA_ts))))
    }

    tab <- as.data.frame(rbind(tab,
                               popSize = input$ELEFAN_GA_popSize,
                               maxiter = input$ELEFAN_GA_maxiter,
                               run = input$ELEFAN_GA_run,
                               pmut = input$ELEFAN_GA_pmutation,
                               pcross = input$ELEFAN_GA_pcrossover,
                               elitism = input$ELEFAN_GA_elitism,
                               sel_years_cc = input$ELEFAN_years_selected_cc,
                               a = input$LWa,
                               b = input$LWb,
                               natM = input$natM
                               ))

    if(input$natM =="Pauly's growth & temp. formula"){
        tab <- as.data.frame(rbind(tab,
                                   temp = input$temp,
                                   school = if(!is.null(input$schooling)){if(input$schooling){TRUE}else{FALSE}}else{FALSE},
                                   tmax = input$tmax))
    }

    tab <- as.data.frame(rbind(tab,
                               lm50 = input$Lm50,
                               lm75 = input$Lm75,
                               select = input$select))

    if(input$select =="Define L50 & L75"){
        tab <- as.data.frame(rbind(tab,
                                   l50user = input$l50_user,
                                   l75user = input$l75_user))
    }

    if(input$select =="Define L50 & (L75-L25)"){
        tab <- as.data.frame(rbind(tab,
                                   l50user = input$l50_user,
                                   wqsuser = input$wqs_user))
    }

    ## tab <- as.data.frame(rbind(tab,
    ##                            fSteps = input$fRangeSteps,
    ##                            fRange = paste0(input$fRangeMin, "-", input$fRangeMax),
    ##                            l50Steps = input$lcRangeSteps))

    ## if(input$select!="Estimate"){
    ##     tab <- as.data.frame(rbind(tab,
    ##                                l50Range = paste0(input$lcRangeMin, "-", input$lcRangeMax)))
    ## }

    ## Lables
    clabs <- c("Parameter/setting","Value")
    labs <- rownames(tab)
    labtmp <- c("fileName", "yearRange", "lengthUnit",
                "binSize", "ma", "addlsqrt", "ga_linf", "ga_k",
                "ga_ta", "C", "ts", "popSize", "maxiter", "run", "pmut",
                "pcross", "elitism", "a", "b", "natM",
                "select")
    ind <- which(labs %in% labtmp)
    ind2 <- which(labtmp %in% labs)
    if(format == "datatable"){
        tmp <- c("File name", "Year range", "Length unit", "Bin size",
                 "Moving average (MA)", "Addl. sqrt")
        if(!is.na(input$provide_Linf)){
            tmp <- c(tmp, "L<sub>&#8734;</sub> search space")
        }else{
            tmp <- c(tmp, "L<sub>&#8734;</sub> (fixed)")
        }
        if(!is.na(input$provide_K)){
            tmp <- c(tmp, "K search space")
        }else{
            tmp <- c(tmp, "K (fixed)")
        }
        if(!is.na(input$provide_t_anchor)){
            tmp <- c(tmp, "t<sub>a</sub> search space")
        }else{
            tmp <- c(tmp, "t<sub>a</sub> (fixed)")
        }
        tmp <- c(tmp, "C", "t<sub>s</sub> seach space", "Pop. size (GA)",
                 "Max. iter (GA)", "Run (GA)", "Prob. mutation (GA)",
                 "Prob. Crossover (GA)", "Elitism (GA)", "a", "b",
                 "M method", "Selectivity")
        labs <- replace(labs,
                        ind,
                        tmp[ind2])
    }else if(format == "kable"){
        tmp <- c("File name", "Year range", "Length unit", "Bin size",
                 "Moving average (MA)", "Addl. sqrt")
        if(!is.na(input$provide_Linf)){
            tmp <- c(tmp, "L\\textsubscript{inf} search space")
        }else{
            tmp <- c(tmp, "L\\textsubscript{inf} (fixed)")
        }
        if(!is.na(input$provide_K)){
            tmp <- c(tmp, "K search space")
        }else{
            tmp <- c(tmp, "K (fixed)")
        }
        if(!is.na(input$provide_t_anchor)){
            tmp <- c(tmp, "t\\textsubscript{a} search space")
        }else{
            tmp <- c(tmp, "t\\textsubscript{a} (fixed)")
        }
        tmp <- c(tmp, "C", "t\\textsubscript{s}  seach space", "Pop. size (GA)",
                 "Max. iter (GA)", "Run (GA)", "Prob. mutation (GA)",
                 "Prob. Crossover (GA)", "Elitism (GA)", "a", "b",
                 "M method", "Selectivity")
        labs <- replace(labs,
                        ind,
                        tmp[ind2])
    }else if(format == "dataframe"){
        tmp <- c("File name", "Year range", "Length unit", "Bin size",
                 "Moving average (MA)", "Addl. sqrt")
        if(!is.na(input$provide_Linf)){
            tmp <- c(tmp, "Linf search space")
        }else{
            tmp <- c(tmp, "Linf (fixed)")
        }
        if(!is.na(input$provide_K)){
            tmp <- c(tmp, "K search space")
        }else{
            tmp <- c(tmp, "K (fixed)")
        }
        if(!is.na(input$provide_t_anchor)){
            tmp <- c(tmp, "ta search space")
        }else{
            tmp <- c(tmp, "ta (fixed)")
        }
        tmp <- c(tmp, "C", "ts  seach space", "Pop. size (GA)",
                 "Max. iter (GA)", "Run (GA)", "Prob. mutation (GA)",
                 "Prob. Crossover (GA)", "Elitism (GA)", "a", "b",
                 "M method", "Selectivity")
        labs <- replace(labs,
                        ind,
                        tmp[ind2])
    }

    tab <- as.data.frame(cbind(labs, tab))
    rownames(tab) <- NULL

    ## Output
    if(format == "dataframe"){
        colnames(tab) <- clabs
        return(tab)
    }else if(format == "kable"){
        capti <- captionTropFishR.tables(elefan_ga, input, format = format,
                                         type = "input")
        ## tab[1,2] <- paste0("\\verb|",tab[1,2],"|")
        return(knitr::kable(tab, format = "latex", col.names = clabs,
                            row.names = FALSE, escape = FALSE,
                            align = c("l","c"),
                            linesep = "",
                            caption = capti) %>%
               kable_styling(font_size = 11,
                             latex_options = "HOLD_position") %>%
               column_spec(1:ncol(tab), width = c("5cm","5cm")))
    }else if(format == "datatable"){
        return(DT::datatable(tab, colnames = clabs,
                             escape = FALSE, rownames = FALSE,
                             options = list(dom = 't',
                                            columnDefs = list(list(
                                                className = 'dt-center',
                                                targets = 0:(ncol(tab)-1))))))
    }
}


tableTropFishR.growth <- function(elefan_ga, input, format = "datatable"){

    ## Table
    if(input$provideGP && !is.na(input$provide_Linf) && !is.na(input$provide_K) &&
       !is.na(input$provide_t_anchor)){
        tmp <- as.data.frame(c(elefan_ga$results$resGA$par, list(Rn_max = NA)))
    }else{
        tmp <- as.data.frame(c(elefan_ga$results$resGA$par,
                               list(Rn_max = elefan_ga$results$resGA$Rn_max)))
    }

    ## Lables
    labs <- colnames(tmp)
    labtmp <- c("Linf",
                "ts",
                "t_anchor","Rn_max","phiL")
    ind <- which(labs %in% labtmp)
    ind2 <- which(labtmp %in% labs)
    if(format == "datatable"){
        labs <- replace(labs,
                        ind,
                        c("L<sub>&#8734;</sub>",
                          "t<sub>s</sub>",
                          "t<sub>a</sub>","Rn","phi'")[ind2])
    }else if(format == "kable"){
        labs <- replace(labs, ind,
                        c("L\\textsubscript{inf}",
                          "t\\textsubscript{s}",
                          "t\\textsubscript{a}","Rn","phi'")[ind2])
    }else if(format == "dataframe"){
        labs <- replace(labs, ind, c("ta","Rn","phi'")[ind2])
    }

    ## Rounding
    tmp <- signif(tmp, digits = 3)

    ## Output
    if(format == "dataframe"){
        colnames(tmp) <- labs
        return(tmp)
    }else if(format == "kable"){
        capti <- captionTropFishR.tables(elefan_ga, input, format = format,
                                         type = "growth")
        return(knitr::kable(tmp, format = "latex", col.names = labs,
                            row.names = FALSE, escape = FALSE,
                            align = rep('c',ncol(tmp)),
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


tableTropFishR.mort <- function(elefan_ga, input, format = "datatable"){

    ## Table
    Z <- elefan_ga$results$resCC$Z
    M <- mean(elefan_ga$results$resM[elefan_ga$results$resCC$reg_int[1]:elefan_ga$results$resCC$reg_int[2]])
    FM <- Z - M
    E <- FM / Z
    if(input$select == "Other"){
        tmp <- TropFishR::select_ogive(list(selecType = "trawl_ogive",
                                            L50 = elefan_ga$results$L50, L75 = elefan_ga$results$L75),
                                       Lt = c(input$l1_user,input$l2_user))
        tmp <- as.data.frame(t(as.matrix(c(Z, M, FM, E,
                                           tmp[1],
                                           tmp[2]))))
        names(tmp) <- c("Z","M","F","E",paste0("Ls",input$l1_user),
                        paste0("L",input$l2_user))
    }else{
        tmp <- as.data.frame(t(as.matrix(c(Z, M, FM, E,
                                           elefan_ga$results$L50,
                                           elefan_ga$results$L75))))
        names(tmp) <- c("Z","M","F","E","Ls50","Ls75")
    }

    ## Lables
    labs <- colnames(tmp)
    labtmp <- c("Ls25",
                "Ls50",
                "Ls75",
                "Ls95")
    ind <- which(labs %in% labtmp)
    ind2 <- which(labtmp %in% labs)
    if(format == "datatable"){
        labs <- replace(labs,
                        ind,
                        c("L<sub>s25</sub>",
                          "L<sub>s50</sub>",
                          "L<sub>s75</sub>",
                          "L<sub>s95</sub>")[ind2])
    }else if(format == "kable"){
        labs <- replace(labs, ind,
                        c("L\\textsubscript{s25}",
                          "L\\textsubscript{s50}",
                          "L\\textsubscript{s75}",
                          "L\\textsubscript{s95}")[ind2])
    }

    ## Rounding
    tmp <- signif(tmp, digits = 3)

    ## Output
    if(format == "dataframe"){
        colnames(tmp) <- labs
        return(tmp)
    }else if(format == "kable"){
    capti <- captionTropFishR.tables(elefan_ga, input, format = format,
                                     type = "mort")
        return(knitr::kable(tmp, format = "latex", col.names = labs,
                            row.names = FALSE, escape = FALSE,
                            align = rep('c',ncol(tmp)),
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

tableTropFishR.refs <- function(elefan_ga, input, format = "datatable"){

    ## Table
    if(is.null(elefan_ga$results$Lm50) || is.null(elefan_ga$results$Lm75)){
        tmp <- elefan_ga$results$resYPR1$df_Es[1:3]
        names(tmp) <- c("Fmax","F0.1","F0.5")
        showNotification(
            "Note that SPR and SPR-based reference points are only estimated if maturity parameters are provided.",
            type = "message",
            duration = 30,
            closeButton = TRUE
        )
    }else{
        tmp <- elefan_ga$results$resYPR1$df_Es
        names(tmp) <- c("Fmax","F0.1","F0.5","F30","F35","F40")
    }

    ## Lables
    labs <- colnames(tmp)
    labtmp <- c("Fmax","F0.1",
                "F0.5","F30","F35","F40")
    ind <- which(labs %in% labtmp)
    ind2 <- which(labtmp %in% labs)
    if(format == "datatable"){
        labs <- replace(labs,
                        ind,
                        c("F<sub>max</sub>",
                          "F<sub>0.1</sub>",
                          "F<sub>0.5</sub>",
                          "F<sub>30</sub>",
                          "F<sub>35</sub>",
                          "F<sub>40</sub>")[ind2])
    }else if(format == "kable"){
        labs <- replace(labs, ind,
                        c("F\\textsubscript{max}",
                          "F\\textsubscript{0.1}",
                          "F\\textsubscript{0.5}",
                          "F\\textsubscript{30}",
                          "F\\textsubscript{35}",
                          "F\\textsubscript{40}")[ind2])
    }

    ## Rounding
    tmp <- signif(tmp, digits = 3)

    ## Output
    if(format == "dataframe"){
        colnames(tmp) <- labs
        return(tmp)
    }else if(format == "kable"){
    capti <- captionTropFishR.tables(elefan_ga, input, format = format,
                                     type = "refs")
        return(knitr::kable(tmp, format = "latex", col.names = labs,
                            row.names = FALSE, escape = FALSE,
                            align = rep('c',ncol(tmp)),
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



tableTropFishR.status <- function(elefan_ga, input, format = "datatable"){

    ## Table
    Z <- elefan_ga$results$resCC$Z
    M <- mean(elefan_ga$results$resM[elefan_ga$results$resCC$reg_int[1]:elefan_ga$results$resCC$reg_int[2]])
    FM <- Z - M
    if(is.null(elefan_ga$results$Lm50) || is.null(elefan_ga$results$Lm75)){
        tmp <- cbind(FM/elefan_ga$results$resYPR1$df_Es[1:3])
        names(tmp) <- c("F/Fmax","F/F0.1","F/F0.5")
    }else{
        tmp <- cbind(FM/elefan_ga$results$resYPR1$df_Es,
                     elefan_ga$results$resYPR1$currents$curr.SPR)
        names(tmp) <- c("F/Fmax","F/F0.1","F/F0.5","F/F30","F/F35","F/F40","SPR")
    }

    ## Lables
    labs <- colnames(tmp)
    labtmp <- c("F/Fmax",
                "F/F0.1",
                "F/F0.5",
                "F/F30",
                "F/F35",
                "F/F40")
    ind <- which(labs %in% labtmp)
    ind2 <- which(labtmp %in% labs)
    if(format == "datatable"){
        labs <- replace(labs,
                        ind,
                        c("F/F<sub>max</sub>",
                          "F/F<sub>0.1</sub>",
                          "F/F<sub>0.5</sub>",
                          "F/F<sub>30</sub>",
                          "F/F<sub>35</sub>",
                          "F/F<sub>40</sub>")[ind2])
    }else if(format == "kable"){
        labs <- replace(labs, ind,
                        c("F/F\\textsubscript{max}",
                          "F/F\\textsubscript{0.1}",
                          "F/F\\textsubscript{0.5}",
                          "F/F\\textsubscript{30}",
                          "F/F\\textsubscript{35}",
                          "F/F\\textsubscript{40}")[ind2])
    }

    ## Rounding
    tmp <- signif(tmp, digits = 3)

    ## Output
    if(format == "dataframe"){
        colnames(tmp) <- labs
        return(tmp)
    }else if(format == "kable"){
    capti <- captionTropFishR.tables(elefan_ga, input, format = format,
                                     type = "status")
        return(knitr::kable(tmp, format = "latex", col.names = labs,
                            row.names = FALSE, escape = FALSE,
                            align = rep('c',ncol(tmp)),
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



tableTropFishR.forOtherMethods <- function(elefan_ga, input, format = "dataframe"){

    ## Table
    k <- elefan_ga$results$resGA$par$K
    m <- mean(elefan_ga$results$resM[elefan_ga$results$resCC$reg_int[1]:elefan_ga$results$resCC$reg_int[2]])
    tmp <- data.frame(Linf = elefan_ga$results$resGA$par$Linf,
                      K = k,
                      M = m,
                      MK = m/k)

    ## Lables
    labs <- colnames(tmp)
    if(format == "datatable"){
        labs[1] <- "L<sub>&#8734;</sub>"
    }else if(format == "kable"){
        labs[1] <- "L\\textsubscript{inf}"
    }else if(format == "dataframe"){
    }

    ## Rounding
    tmp <- signif(tmp, digits = 3)

    ##
    rownames(tmp) <- NULL

    ## Output
    if(format == "dataframe"){
        colnames(tmp) <- labs
        return(tmp)
    }else if(format == "kable"){
        capti <- captionTropFishR.tables(elefan_ga, input, format = format,
                                         type = "forOtherMethods")
        return(knitr::kable(tmp, format = "latex", col.names = labs,
                            row.names = FALSE, escape = FALSE,
                            align = rep('c',ncol(tmp)),
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
