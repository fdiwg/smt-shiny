

captionTropFishR.tables <- function(elefan_ga, input, format = "datatable", type){

    switch(type,
           "input" = {
               tab.num <- 0
               txt <- paste0("Input parameters. GA refers to the Genetic Algorithm.")
           },
           "growth" = {
               tab.num <- 1
               if(format == "datatable"){
               txt <- paste0("Estimated von Bertlanffy growth parameters (",withMathJax("\\(L_\\infty, K, t_a\\)"),"), the growth performance coefficient ",
                             withMathJax("\\(phi' = \\log_{10} K + 2 \\log_{10} L_\\infty\\)"),
                             ", and the best score value (",withMathJax("\\(Rn\\)"),"). Phi' provides a metric that accounts for the correlation of ",withMathJax("\\(L_\\infty\\)")," and ",withMathJax("\\(K\\)")," to compare growth parameters among analyses or species. The Rn value is the sum of all values associated with the bins after restructuring that intersect the growth curves. The value allows comparison of the fit of estimated growth parameters to the data set for different MA values, growth paramater search spaces, or ELEFAN optimisation parameters (e.g. population size), but not for different data sets or bin sizes.")
           }else if(format == "kable"){
               txt <- paste0("Estimated von Bertlanffy growth parameters (","\\(L_\\infty, K, t_a\\)","), the growth performance coefficient ","\\(phi' = \\log_{10} K + 2 \\log_{10} L_\\infty\\)",", and the best score value (","\\(Rn\\)","). Phi' provides a metric that accounts for the correlation of ","\\(L_\\infty\\)"," and ","\\(K\\)"," to compare growth parameters among analyses or species. The Rn value is the sum of all values associated with the bins after restructuring that intersect the growth curves. The value allows comparison of the fit of estimated growth parameters to the data set for different MA values, growth paramater search spaces, or ELEFAN optimisation parameters (e.g. population size), but not for different data sets or bin sizes.")
           }
           },
           "mort" = {
               tab.num <- 2
               if(format == "datatable"){
                   vals <- paste0(withMathJax('\\(L_{s50}\\)'),", ",
                                  withMathJax('\\(L_{s75}\\)'))
               }else if(format == "kable"){
                   vals <- paste0("\\(L_{s50}\\), \\(L_{s75}\\)")
               }else{
                   vals <- paste0("Ls50, Ls75")
               }
               if(input$select == "Other"){
                   txt <- paste0("Estimated mortality rates (Z, F, M), exploitation rate (E), and estimated selectivity parameters (",
                                 paste0("Ls",input$l1_user),", ",
                                 paste0("Ls",input$l2_user),")")
               }else if(input$select == "Estimate" || (is.null(input$l50_user) && is.null(input$l75_user)) || (is.null(input$l50_user) && is.null(input$wqs_user)) ||  (is.null(input$l1_user) && is.null(input$l2_user))){
                   txt <- paste0("Estimated mortality rates (Z, F, M), exploitation rate (E), and estimated selectivity parameters (",vals,").")
               }else{
                   txt <- paste0("Estimated mortality rates (Z, F, M), exploitation rate (E), and provided selectivity parameters (",vals,").")
               }
           },
           "refs" = {
               tab.num <- 3
               if(format == "datatable"){
                   vals <- paste0(withMathJax("\\(F_{max}\\)"),", ",
                                  withMathJax("\\(F_{0.1}\\)"),", ",
                                  withMathJax("\\(F_{0.5}\\)"))
                   vals2 <- paste0(withMathJax("\\(F_{30\\%SPR}\\)"),", ",
                                   withMathJax("\\(F_{35\\%SPR}\\)"),", ",
                                   withMathJax("\\(F_{40\\%SPR}\\)"))
               }else if(format == "kable"){
                   vals <- paste0("\\(F_{max}\\), \\(F_{0.1}\\), \\(F_{0.5}\\)")
                   vals2 <- paste0("\\(F_{30\\%SPR}\\), \\(F_{35\\%SPR}\\), \\(F_{40\\%SPR}\\)")
               }else{
                   vals <- paste0("Fmax, F0.1, F0.5")
                   vals2 <- paste0("F30, F35, F40")
               }
               if(is.null(elefan_ga$results$Lm50) || is.null(elefan_ga$results$Lm75)){
                   txt <- paste0("Estimated reference points (",vals,").")
               }else{
                   txt <- paste0("Estimated reference points (",vals,") and SPR-based reference points (",vals2,").")
               }
           },"status" = {
               tab.num <- 4
               if(format == "datatable"){
                   vals <- paste0(withMathJax("\\(F_{max}\\)"),", ",
                                  withMathJax("\\(F_{0.1}\\)"),", ",
                                  withMathJax("\\(F_{0.5}\\)"))
                   vals2 <- paste0(withMathJax("\\(F_{max}\\)"),", ",
                                   withMathJax("\\(F_{0.1}\\)"),", ",
                                   withMathJax("\\(F_{0.5}\\)"),", ",
                                   withMathJax("\\(F_{30\\%SPR}\\)"),", ",
                                   withMathJax("\\(F_{35\\%SPR}\\)"),", ",
                                   withMathJax("\\(F_{40\\%SPR}\\)"))
               }else if(format == "kable"){
                   vals <- paste0("\\(F_{max}\\), \\(F_{0.1}\\), \\(F_{0.5}\\)")
                   vals2 <- paste0("\\(F_{max}\\), \\(F_{0.1}\\), \\(F_{0.5}\\), \\(F_{30\\%SPR}\\), \\(F_{35\\%SPR}\\), \\(F_{40\\%SPR}\\)")
               }else{
                   vals <- paste0("Fmax, F0.1, F0.5")
                   vals2 <- paste0("F30, F35, F40")
               }
               if(is.null(elefan_ga$results$Lm50) || is.null(elefan_ga$results$Lm75)){
                   txt <- paste0("Estimated stock status in terms of current fishing mortality (F) to reference points (",vals,").")
               }else{
                   txt <- paste0("Estimated stock status in terms of current fishing mortality (F) to reference points (",vals2,") and current Spawning Potential Ratio (SPR).")
               }
           },
           "forOtherMethods" = {
               tab.num <- 5
               txt <- paste0("Parameters estimated with TropFishR that can be used as input for the other length-based stock assessment methods (e.g. LBI and LBSPR).")
           }
           )

    if(format == "datatable"){
        txt <- paste0("<p class=\"pheader_elefan\">Table ",tab.num,": ",txt, "</p>")
    }

    return(txt)
}






captionTropFishR.plots <- function(elefan_ga, input, format = "withFig", type){

    switch(type,
           "explo" = {
               plot.num <- 1
               txt <- paste0("Uploaded length-frequency distributions per sampling time (x axis). Panel A shows the raw data, while panel B shows the restructured data. This means after subtracting the moving average (MA) of the count in each length class. The combination of bin size and MA critically affects the separation of peaks (i.e. potential cohorts) in the dataset and thus the estimation of growth parameters by ELEFAN. Blue shading indicates a high count per length bin (panel A) and a high positive value (panel B). Red shading indicates a negative value (only panel B). A good bin size value reduces noise in the data by aggregation. A good MA value leads to visually distinct peaks in particular among small length classes.")
           },
           "diag1" = {
               plot.num <- 2
               txt <- paste0("Number of samples by month.")
           },
           "diag2" = {
               plot.num <- 3
               txt <- paste0("Length frequency distributions by month.")
           },
           "diag3" = {
               plot.num <- 4
               txt <- paste0("Length frequency distributions by year.")
           },
           "growth" = {
               plot.num <- 5
               txt <- paste0("Uploaded raw (A) and restructured (B) length-frequency data with overlaid von Bertalanffy growth (VBG) curves fitted by ELEFAN with genetic algorithm. Ideally, the growth curves overlay with length bins with a high count or high positive value (blue shading) for raw (A) and restructured (B) data, respectively.")
           },
           "ga" = {
               plot.num <- 6
               txt <- paste0("Score graph of ELEFAN with genetic algorithm. Fitness value (y axis) corresponds here to the score value of ELEFAN (Rn) and in the lingo of genetic algorithm 'Generation' (x axis) refers to the iteration.  Ideally, the number of iterations (or generations) is large enough, so that there are no large jumps visible during the last iterations of the best and average score value.")
           },
           "mort" = {
               plot.num <- 7
               txt <- paste0("Estimated (total, fishing, and natural) mortality rates by length class (solid lines) and average levels for main exploited length classes (dashed lines), i.e. the length classes considered in the regression analysis of the length-converted catch curve. Note, that the natural mortality rates estimated by the empirical formulae after Then et al. (2015) and Pauly et al. (1980) are independent of length. By contrast, the formulae after Gislason et al. (2010) and Lorenzen et al. (2022) estimate length-dependent natural mortality rates based on the asymptotic length, growth coefficient, and body length of the fish.")
           },
           "cc" = {
               plot.num <- 8
               txt <- paste0("Logarithm of catch per length interval against relative age. Blue points correspond to points used in the regression analysis (blue line) of the catch curve for the estimation of total mortality (Z), which corresponds to the slope of the displayed regression line. The selection of points is automatic and based on a list of expert recommendations.")
           },"sel" = {
               plot.num <- 9
               if(input$select == "Estimate" || (is.null(input$l50_user) && is.null(input$l75_user)) || (is.null(input$l50_user) && is.null(input$wqs_user)) ||  (is.null(input$l1_user) && is.null(input$l2_user))){
                   txt <- "Estimated logistic gear selectivity used in the assessment as the probability of capture (y axis) at length (x axis). Displayed selection ogive is used for the yield per recruit analysis (YPR)."
               }else{
                   txt <- "Provided logistic gear selectivity used in the assessment as the probability of capture (y axis) at length (x axis). Displayed selection ogive is used for the yield per recruit analysis (YPR)."
               }
           },"ypr" = {
               plot.num <- 10
               refs <- paste0(withMathJax('\\(F_{30\\%SPR}\\)'),", ",
                              withMathJax('\\(F_{35\\%SPR}\\)'),", and",
                              withMathJax('\\(F_{40\\%SPR}\\)'))
               if(is.null(elefan_ga$results$Lm50) || is.null(elefan_ga$results$Lm75)){
                   txt <- paste0("Yield per recruit (A) and biomass per recruit (B) for a range of fishing mortality rates (x axis). Grey segements indicate various reference points: Fmax is defined as the fishing mortality (F) leading to the maximum yield per recruit. F0.1 corresponds to F where the slope of the yield per recruit curve is equal to 10% of the slope in the origin and poses a more conservative reference point than Fmax. F0.5 corresponds to F where the biomass per recruit is equal to 50% of the biomass per recruit without fishing.")
               }else{
                   txt <- paste0("Yield per recruit (A) and biomass per recruit (B), as well as spawning potential ratio (C) for a range of fishing mortality rates (x axis). Grey segements indicate various reference points: Fmax is defined as the fishing mortality (F) leading to the maximum yield per recruit. F0.1 corresponds to F where the slope of the yield per recruit curve is equal to 10% of the slope in the origin and poses a more conservative reference point than Fmax. F0.5 corresponds to F where the biomass per recruit is equal to 50% of the biomass per recruit without fishing. ",refs," correspond to F that leads to a SPR of 30%, 35%, and 40% respectively.")
               }
           },"iso" = {
               plot.num <- 11
               txt <- paste0("Yield (A) and biomass (B) per recruit for a range of fishing mortality rates (x axis) and gear selectivity (y axis) combinations. Colors indicate high (red) to low (blue) yield and biomass. Gear selectivity is defined by the length at 50% selectivity (Ls50). The black dot indicates current yield and biomass per recruit.")
           }
           )

    if(format == "withFig"){
        txt <- paste0("<p class=\"pheader_elefan\">Figure ",plot.num,": ",txt, "</p>")
    }else if(format == "withFigLatex"){
        txt <- paste0("<p> Figure ",plot.num,": ",txt, "</p>")
    }

    return(txt)
}
