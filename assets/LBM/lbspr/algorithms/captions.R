

captionLBSPR.tables <- function(lbspr_dat, input, format = "datatable", type){

    switch(type,
           "input" = {
               tab.num <- 0
               txt <- paste0("Input parameters.")
           },
           "res" = {
               tab.num <- 1
               if(format == "datatable"){
                   vals <- paste0(withMathJax('\\(L_{s50\\%}\\)')," and ",withMathJax('\\(L_{s95\\%}\\)'))
               }else if(format == "kable"){
                   vals <- paste0("\\(L_{s50\\%}\\) and \\(L_{s95\\%}\\)")
               }else{
                   vals <- paste0("Ls50% and Ls95%")
               }
               txt <- paste0("Estimated Spawning potential ratio (SPR) in percent, estimated selectivity parameters (",vals," in ",input$LBSPR_lengthUnit,") in the same length unit as uploaded length measurements, and the fishing mortality relative to the natural mortality (F/M) for each year included in the assessment. The uncertainty of estimated parameters is provided as the 95% confidence limits in brackets.")
           }
           )

    if(format == "datatable"){
        txt <- paste0("<p class=\"pheader_elefan\">Table ",tab.num,": ",txt, "</p>")
    }

    return(txt)
}



captionLBSPR.plots <- function(lbspr_dat, input, format = "withFig", type){

    switch(type,
           "data" = {
               plot.num <- 1
               txt <- paste0("Uploaded length-frequency distributions for each year. The bin size affects the binning of the data and thus the distributions in this graph. A good bin size is as small as possible while at the same time large enough to reduce the noise in the data.")
           },
           "pie" = {
               plot.num <- 2
               txt <- paste0("Estimated spawning potential ratio (SPR) value in relation to the SPR reference points. The estimated SPR is indicated by the dashed line with the SPR value next to the dashed line. The coloured areas indicate the SPR reference points: The red area/line indicates the proportion of SPR below the limit reference point; the green area/line indicates the proportion above the limit and below the target reference point; and the the yellow area indicates the proportion above the target reference point. Note, that when the assessment is done for multiple years, only the last year of the assessment is shown in this graph.")
           },
           "sel" = {
               plot.num <- 3
               txt <- paste0("Provided maturity information and estimated selectivity information. The curves indicate the proportion of the stock that is mature or vulnerable to the gear (y axis) at a given length (x axis).")
           },
           "ts" = {
               plot.num <- 4
               txt <- paste0("Time series plot of the estimated selectivity parameters in ",input$LBSPR_lengthUnit,", fishing mortality relative to natural mortality, and spawning potential ratio (SPR) with 95% confidence intervals. The lines correspond to the smoothed estimates over time. Note, that the graphs only show single points when the assessment spans a single year.")
           }
           )

    if(format == "withFig"){
        txt <- paste0("<p class=\"pheader_elefan\">Figure ",plot.num,": ",txt, "</p>")
    }else if(format == "withFigLatex"){
        txt <- paste0("<p> Figure ",plot.num,": ",txt, "</p>")
    }

    return(txt)
}
