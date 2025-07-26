

captionLBI.tables <- function(lbi_dat, input, format = "datatable", type){

    switch(type,
           "input" = {
               tab.num <- 0
               txt <- paste0("Input parameters.")
           },
           "intro" = {
               tab.num <- 1
               txt <- paste0("Description of all length-based indicators (LBIs).")
           },
           "indicators" = {
               tab.num <- 2
               if(format == "datatable"){
                   vals <- paste0(withMathJax("\\(L_{maxy}\\)"))
               }else if(format == "kable"){
                   vals <- paste0("\\(L_{maxy}\\)")
               }else{
                   vals <- paste0("Lmaxy")
               }
               txt <- paste0("Estimated length-based indicators in ",input$LBI_lengthUnit,". Note, that the indicator ",vals," is only estimated and included in this table if the parameters of the length-weight relationship (a and b) are specified. The indicators in this table relative to the respective reference points are used to derive the stock status in Table 2 and Figure 2.")
           },
           "ratios" = {
               tab.num <- 3
               if(format == "datatable"){
                   vals <- paste0(withMathJax('\\(L_{95\\%}/L_{\\infty}\\)'))
                   vals2 <- paste0(withMathJax('\\(L_{25\\%}/L_{mat}\\)'))
               }else if(format == "kable"){
                   vals <- paste0("\\(L_{95\\%}/L_{\\infty}\\)")
                   vals2 <- paste0("\\(L_{25\\%}/L_{mat}\\)")
               }else{
                   vals <- paste0("L95%/Linf")
                   vals2 <- paste0("L25%/Lmat")
               }
               txt <- paste0("Estimated length-based indicators relative to specific reference levels for each LBI (columns) for each (selected) year of the uploaded ",
                             "data set (rows). Based on expected values listed in the LBI ",
                             "description table in the Methods tab, the cell with each ratio is either highlighted ",
                             "green or red ",
                             "indicating that the ratio is above or below the expected value, respectively. Ideally, ",
                             "all cells are highlighted green for a given year, thus, showing no indication of ",
                             "overfishing. Note, that green and red cells in the same year do not mean that the ",
                             "indicators contradict each other, but could indicate that the stock performs ",
                             "differently regarding different properties (see LBI description table in the methods tab ",
                             "for more details on the ",
                             "properties of each indicator). For example, the indicators could indicate that the ",
                             "proportion of older larger individuals is smaller than expected (red ",
                             vals,") ",
                             "but at the same time the proportion of immature individuals is as expected ",
                             "(green; ", vals2,
                             "). Generally, the more indicators are red, the larger the likeliness of ",
                             "overexploitation.")

           }
           )

    if(format == "datatable"){
        txt <- paste0("<p class=\"pheader_elefan\">Table ",tab.num,": ",txt, "</p>")
    }

    return(txt)
}



captionLBI.plots <- function(lbi_dat, input, format = "withFig", type){

    switch(type,
           "data" = {
               plot.num <- 1
               txt <- paste0("Uploaded length-frequency distributions for each year. The bin size affects the binning of the data and thus the distributions in this graph. A good bin size is as small as possible while at the same time large enough to reduce the noise in the data.")
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
           "fit" = {
               plot.num <- 5
               if(format == "datatable"){
                   vals <- paste0(withMathJax('\\(L_{mean}/L_{opt}\\)'))
               }else if(format == "kable"){
                   vals <- paste0("\\(L_{mean}/L_{opt}\\)")
               }else{
                   vals <- paste0("Lmean/Lopt")
               }
               txt <- paste0("Estimated length-based indicators relative to reference levels as a point for each year. The expected values for each LBI ratio ",
                 "are represented as dashed horizontal lines.",
                 "Ideally, the indicator ratios are equal or close to the expected values, e.g. ",
                 vals, " is equal or close to 1. The further the points/lines are from the ",
                 "expected values of the various indicator ratios, the more evidence of overfishing.",
                 "Whenever the graph includes a greenish area the horizontal line represents a ",
                 "limit reference point, rather than a target reference point and any value above ",
                 "the limit reference point (i.e. in the greenish area) is considered desirable.")
           }
           )

    if(format == "withFig"){
        txt <- paste0("<p class=\"pheader_elefan\">Figure ",plot.num,": ",txt, "</p>")
    }else if(format == "withFigLatex"){
        txt <- paste0("<p> Figure ",plot.num,": ",txt, "</p>")
    }

    return(txt)
}
