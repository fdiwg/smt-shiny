tableSpict.data <- function(dat, input, format = "dataframe"){

    browser()

    tmp <- cbind(dat$dataExplo$inp$timeC,
                 dat$dataExplo$inp$obsC)

    labs <- c("timeC", "obsC")

    ## Rounding
    tmp <- signif(tmp, digits = 3)

    ## Return
    colnames(tmp) <- labs
    rownames(tmp) <- NULL
    return(tmp)
}


tableSpict.input <- function(dat, input, format = "datatable"){
    browser()
    return(NULL)
}


tableSpict.estimates <- function(dat, input, format = "datatable"){
    browser()
    return(NULL)
}


tableSpict.states <- function(dat, input, format = "datatable"){
    browser()
    return(NULL)
}

tableSpict.refs <- function(dat, input, format = "datatable"){
    browser()
    return(NULL)
}



tableSpict.pred <- function(dat, input, format = "datatable"){
    browser()
    return(NULL)
}
