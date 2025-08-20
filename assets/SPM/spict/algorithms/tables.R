tableSpict.data <- function(dat, input, format = "dataframe"){

    padNA <- function(x, n) {
        length(x) <- n
        x
    }

    tmp <- data.frame(
        timeC = dat$dataExplo$inp$timeC,
        obsC  = dat$dataExplo$inp$obsC,
        stdevfacC = dat$dataExplo$inp$stdevfacC
    )

    maxlen <- max(
        length(dat$dataExplo$inp$timeC),
        sapply(dat$dataExplo$inp$timeI, length)
    )


    tmp$timeC <- padNA(tmp$timeC, maxlen)
    tmp$obsC  <- padNA(tmp$obsC, maxlen)

    for (i in seq_along(dat$dataExplo$inp$timeI)) {
        tmp[[paste0("timeI", i)]] <- padNA(dat$dataExplo$inp$timeI[[i]], maxlen)
        tmp[[paste0("obsI",  i)]] <- padNA(dat$dataExplo$inp$obsI[[i]], maxlen)
        tmp[[paste0("stdevfacI",  i)]] <- padNA(dat$dataExplo$inp$stdevfacI[[i]], maxlen)
    }

    ## Return
    rownames(tmp) <- NULL
    return(tmp)
}


tableSpict.priors <- function(dat, input, format = "dataframe"){

    inp <- dat$dataExplo$inp

    active.priors <- names(which(sapply(inp$priors, function(x) if(is.list(x)) any(sapply(x, function(x) x[3] == 1)) else x[3] == 1)))

    priors <- cbind(active.priors, round(do.call(rbind, inp$priors[active.priors]),3)[,1:2])

    tmp <- priors

    labs <- c("Prior","Mean","SD")

    ## Return
    colnames(tmp) <- labs
    rownames(tmp) <- NULL
    return(tmp)
}


tableSpict.estimates <- function(dat, input, format = "datatable"){

    ## Table
    tmp <- spict:::sumspict.parest(dat$results)[,1:3]
    pars <- rownames(tmp) <- gsub(" ", "", rownames(tmp))
    tmp <- tmp[!(pars %in% c("rc","rold")),]
    qs <- grep("^q", rownames(tmp), value = TRUE)
    sdis <- grep("^sdi", rownames(tmp), value = TRUE)
    wanted <- c("r","K","m","n", qs, "sdb","sdf",sdis,"sdc","alpha","beta")
    tmp <- tmp[intersect(wanted, rownames(tmp)), , drop = FALSE]
    tmp <- signif(tmp, digits = 3)
    tmp <- data.frame(par = rownames(tmp), tmp)
    rownames(tmp) <- NULL

    ## Lables
    labs <- colnames(tmp)
    labtmp <- c("par","estimate","cilow","ciupp")
    ind <- which(labs %in% labtmp)
    ind2 <- which(labtmp %in% labs)
    if (format == "datatable") {
        labs <- replace(labs,
                        ind,
                        c("Parameter",
                          "Est",
                          "lower CI<sub>95%</sub>",
                          "upper CI<sub>95%</sub>")[ind2])
    } else if(format == "kable") {
        labs <- replace(labs, ind,
                        c("Parameter",
                          "Est",
                          "lower CI\\textsubscript{95%}",
                          "upper CI\\textsubscript{95%}")[ind2])
    }

    ## Output
    if (format == "dataframe") {
        colnames(tmp) <- labs
        return(tmp)
    } else if(format == "kable") {
        capti <- captionSpict.tables(dat, input, format = format,
                                     type = "estimates")
        return(knitr::kable(tmp, format = "latex", col.names = labs,
                            row.names = FALSE, escape = FALSE,
                            align = rep('c',ncol(tmp)),
                            linesep = "",
                            caption = capti) %>%
               kable_styling(font_size = 11,
                             latex_options = "HOLD_position") %>%
               column_spec(1:ncol(tmp), width = "1.2cm"))
    } else if(format == "datatable") {
        return(DT::datatable(tmp, colnames = labs,
                             escape = FALSE, rownames = FALSE,
                             options = list(dom = 't',
                                            columnDefs = list(list(
                                                className = 'dt-center',
                                                targets = 0:(ncol(tmp)-1))))))
    }
}


tableSpict.states <- function(dat, input, format = "datatable"){

    ## Table
    tmp <- spict:::sumspict.states(dat$results)[,1:3]
    pars <- gsub(" ", "", rownames(tmp))
    tmp <- signif(tmp, digits = 3)
    tmp <- data.frame(par = rownames(tmp), tmp)
    rownames(tmp) <- NULL

    ## Lables
    labs <- colnames(tmp)
    labtmp <- c("par","estimate","cilow","ciupp")
    ind <- which(labs %in% labtmp)
    ind2 <- which(labtmp %in% labs)
    if (format == "datatable") {
        labs <- replace(labs,
                        ind,
                        c("Parameter",
                          "Est",
                          "lower CI<sub>95%</sub>",
                          "upper CI<sub>95%</sub>")[ind2])
    } else if(format == "kable") {
        labs <- replace(labs, ind,
                        c("Parameter",
                          "Est",
                          "lower CI\\textsubscript{95%}",
                          "upper CI\\textsubscript{95%}")[ind2])
    }

    pars <- as.character(tmp[,1])
    if (format == "datatable") {
        pars <- gsub("_", "<sub>", pars)
        pars <- gsub("/", "</sub>/", pars)
        pars <- gsub("msy", "<sub>MSY", pars)
        pars <- paste0(pars, "</sub>")
    } else if(format == "kable") {
        pars <- gsub("_", "\\\\textsubscript{", pars)
        pars <- gsub("/", "}/", pars)
        pars <- gsub("msy", "\\\\textsubscript{MSY", pars)
        pars <- paste0(pars, "}")
    }
    tmp[,1] <- pars

    ## Output
    if (format == "dataframe") {
        colnames(tmp) <- labs
        return(tmp)
    } else if(format == "kable") {
        capti <- captionSpict.tables(dat, input, format = format,
                                     type = "states")
        return(knitr::kable(tmp, format = "latex", col.names = labs,
                            row.names = FALSE, escape = FALSE,
                            align = rep('c',ncol(tmp)),
                            linesep = "",
                            caption = capti) %>%
               kable_styling(font_size = 11,
                             latex_options = "HOLD_position") %>%
               column_spec(1:ncol(tmp), width = "1.2cm"))
    } else if(format == "datatable") {
        return(DT::datatable(tmp, colnames = labs,
                             escape = FALSE, rownames = FALSE,
                             options = list(dom = 't',
                                            columnDefs = list(list(
                                                className = 'dt-center',
                                                targets = 0:(ncol(tmp)-1))))))
    }
}

tableSpict.refs_s <- function(dat, input, format = "datatable"){

    ## Table
    tmp <- spict:::sumspict.srefpoints(dat$results)[,1:3]
    pars <- gsub(" ", "", rownames(tmp))
    tmp <- signif(tmp, digits = 3)
    tmp <- data.frame(par = rownames(tmp), tmp)
    rownames(tmp) <- NULL

    ## Lables
    labs <- colnames(tmp)
    labtmp <- c("par","estimate","cilow","ciupp")
    ind <- which(labs %in% labtmp)
    ind2 <- which(labtmp %in% labs)
    if (format == "datatable") {
        labs <- replace(labs,
                        ind,
                        c("Parameter",
                          "Est",
                          "lower CI<sub>95%</sub>",
                          "upper CI<sub>95%</sub>")[ind2])
    } else if(format == "kable") {
        labs <- replace(labs, ind,
                        c("Parameter",
                          "Est",
                          "lower CI\\textsubscript{95%}",
                          "upper CI\\textsubscript{95%}")[ind2])
    }
    pars <- as.character(tmp[,1])
    if (format == "datatable") {
        pars <- gsub("msy", "<sub>MSY</sub>", pars)
        pars <- gsub("s$", "<sup>s</sup>", pars)
    } else if(format == "kable") {
        pars <- gsub("msy", "\\\\textsubscript{MSY}", pars)
        pars <- gsub("s$", "\\\\textsuperscript{s}", pars)
    }
    tmp[,1] <- pars

    ## Output
    if (format == "dataframe") {
        colnames(tmp) <- labs
        return(tmp)
    } else if(format == "kable") {
        capti <- captionSpict.tables(dat, input, format = format,
                                     type = "refs_s")
        return(knitr::kable(tmp, format = "latex", col.names = labs,
                            row.names = FALSE, escape = FALSE,
                            align = rep('c',ncol(tmp)),
                            linesep = "",
                            caption = capti) %>%
               kable_styling(font_size = 11,
                             latex_options = "HOLD_position") %>%
               column_spec(1:ncol(tmp), width = "1.2cm"))
    } else if(format == "datatable") {
        return(DT::datatable(tmp, colnames = labs,
                             escape = FALSE, rownames = FALSE,
                             options = list(dom = 't',
                                            columnDefs = list(list(
                                                className = 'dt-center',
                                                targets = 0:(ncol(tmp)-1))))))
    }
}

tableSpict.refs_d <- function(dat, input, format = "datatable"){

    ## Table
    tmp <- spict:::sumspict.drefpoints(dat$results)[,1:3]
    pars <- gsub(" ", "", rownames(tmp))
    tmp <- signif(tmp, digits = 3)
    tmp <- data.frame(par = rownames(tmp), tmp)
    rownames(tmp) <- NULL

    ## Lables
    labs <- colnames(tmp)
    labtmp <- c("par","estimate","cilow","ciupp")
    ind <- which(labs %in% labtmp)
    ind2 <- which(labtmp %in% labs)
    if (format == "datatable") {
        labs <- replace(labs,
                        ind,
                        c("Parameter",
                          "Est",
                          "lower CI<sub>95%</sub>",
                          "upper CI<sub>95%</sub>")[ind2])
    } else if(format == "kable") {
        labs <- replace(labs, ind,
                        c("Parameter",
                          "Est",
                          "lower CI\\textsubscript{95%}",
                          "upper CI\\textsubscript{95%}")[ind2])
    }
    pars <- as.character(tmp[,1])
    if (format == "datatable") {
        pars <- gsub("msy", "<sub>MSY</sub>", pars)
        pars <- gsub("d$", "<sup>d</sup>", pars)
    } else if(format == "kable") {
        pars <- gsub("msy", "\\\\textsubscript{MSY}", pars)
        pars <- gsub("d$", "\\\\textsuperscript{d}", pars)
    }
    tmp[,1] <- pars

    ## Output
    if (format == "dataframe") {
        colnames(tmp) <- labs
        return(tmp)
    } else if(format == "kable") {
        capti <- captionSpict.tables(dat, input, format = format,
                                     type = "refs_d")
        return(knitr::kable(tmp, format = "latex", col.names = labs,
                            row.names = FALSE, escape = FALSE,
                            align = rep('c',ncol(tmp)),
                            linesep = "",
                            caption = capti) %>%
               kable_styling(font_size = 11,
                             latex_options = "HOLD_position") %>%
               column_spec(1:ncol(tmp), width = "1.2cm"))
    } else if(format == "datatable") {
        return(DT::datatable(tmp, colnames = labs,
                             escape = FALSE, rownames = FALSE,
                             options = list(dom = 't',
                                            columnDefs = list(list(
                                                className = 'dt-center',
                                                targets = 0:(ncol(tmp)-1))))))
    }
}



tableSpict.pred <- function(dat, input, format = "datatable"){

    ## Table
    tmp <- spict:::sumspict.predictions(dat$results)[,1:3]
    pars <- gsub(" ", "", rownames(tmp))
    tmp <- signif(tmp, digits = 3)
    tmp <- data.frame(par = rownames(tmp), tmp)
    rownames(tmp) <- NULL

    ## Lables
    labs <- colnames(tmp)
    labtmp <- c("par","estimate","cilow","ciupp")
    ind <- which(labs %in% labtmp)
    ind2 <- which(labtmp %in% labs)
    if (format == "datatable") {
        labs <- replace(labs,
                        ind,
                        c("Parameter",
                          "Est",
                          "lower CI<sub>95%</sub>",
                          "upper CI<sub>95%</sub>")[ind2])
    } else if(format == "kable") {
        labs <- replace(labs, ind,
                        c("Parameter",
                          "Est",
                          "lower CI\\textsubscript{95%}",
                          "upper CI\\textsubscript{95%}")[ind2])
    }
    pars <- as.character(tmp[,1])
    if (format == "datatable") {
        pars <- gsub("_", "<sub>", pars)
        pars <- gsub("/", "</sub>/", pars)
        pars <- gsub("msy", "<sub>MSY", pars)
        pars <- gsub(")", "</sub>)", pars)
        pars[-6] <- paste0(pars[-6], "</sub>")
    } else if(format == "kable") {
        pars <- gsub("_", "\\\\textsubscript{", pars)
        pars <- gsub("/", "}/", pars)
        pars <- gsub("msy", "\\\\textsubscript{MSY", pars)
        pars <- gsub(")", "})", pars)
        pars[-6] <- paste0(pars[-6], "}")
    }
    tmp[,1] <- pars

    ## Output
    if (format == "dataframe") {
        colnames(tmp) <- labs
        return(tmp)
    } else if(format == "kable") {
        capti <- captionSpict.tables(dat, input, format = format,
                                     type = "pred")
        return(knitr::kable(tmp, format = "latex", col.names = labs,
                            row.names = FALSE, escape = FALSE,
                            align = rep('c',ncol(tmp)),
                            linesep = "",
                            caption = capti) %>%
               kable_styling(font_size = 11,
                             latex_options = "HOLD_position") %>%
               column_spec(1:ncol(tmp), width = "1.2cm"))
    } else if(format == "datatable") {
        return(DT::datatable(tmp, colnames = labs,
                             escape = FALSE, rownames = FALSE,
                             options = list(dom = 't',
                                            columnDefs = list(list(
                                                className = 'dt-center',
                                                targets = 0:(ncol(tmp)-1))))))
    }
}
