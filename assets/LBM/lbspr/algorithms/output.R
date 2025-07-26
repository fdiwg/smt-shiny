

textLBSPR.diag1 <- function(lbspr_dat, input){

    file.name <- lbspr_dat$dataExplo$checks$fileName

    ## input data format
    format <- "not clear"
    if(lbspr_dat$dataExplo$checks$format == "wide") {
        format <- "length frequency table"
    }
    if(lbspr_dat$dataExplo$checks$format == "long") {
        format <- "raw length measurements"
    }

    ## Number of length bins
    l.diff <- diff(sort(unique(lbspr_dat$dataExplo$raw$Length)))
    l.diff.min <- min(l.diff)
    l.diff.max <- max(l.diff)

    ## Range of lengths
    l.min <- min(lbspr_dat$dataExplo$raw$Length)
    l.max <- max(lbspr_dat$dataExplo$raw$Length)

    ## Number of time points (samples)
    samp.times <- unique(lbspr_dat$dataExplo$raw$Date)
    n.samp.times <- length(samp.times)

    ## Time span (first and last sample dates)
    range.samp.times <- range(samp.times)

    ## Total number of samples
    n.samp <- sum(lbspr_dat$dataExplo$raw$Frequency)
    ## Samples by sampling time  ## TODO option for user input how can select to do this by "day", "week", "month", "year" and table adjusts
    tmp <- by(lbspr_dat$dataExplo$raw$Frequency,
              lbspr_dat$dataExplo$raw$Date, sum)
    n.samp.by.samp.time <- data.frame("Sampling time" = names(tmp),
                                      "Samples" = as.vector(tmp))

    table_html <- paste0(
        "<table border='1' cellspacing='0' cellpadding='15' style='text-align:center;'>",
        "<tr><th>Sampling time</th><th>Number of samples</th></tr>",
        paste(
            apply(n.samp.by.samp.time, 1, function(row)
                paste0("<tr><td>", paste(row, collapse = "</td><td>"), "</td></tr>")
                ),
            collapse = "\n"
        ),
        "</table>"
    )

    HTML(paste0(
        "<h4>Data set information</h4>",
        "File name: ", file.name, "<br>",
        "File format: ", format, "<br>", "<br>",
        "<h4>Sampling times</h4>",
        "Number of sampling times: ", n.samp.times, "<br>",
        "Start of sampling: ", range.samp.times[1], "<br>",
        "End of sampling: ", range.samp.times[2], "<br>", "<br>",
        ## "Sampling times: ", paste(samp.times, collapse = ", "), "<br>", "<br>",
        "<h4>Samples</h4>",
        "Number of samples: ", n.samp, "<br>", "<br>",
        table_html, "<br>", "<br>",
        "<h4>Length measurements</h4>",
        "Length range: ", paste0(round(l.min,2), " - ", round(l.max,2), " ",
                                 input$lbi_lengthUnit), "<br>",
        "Length diff range: ", paste0(round(l.diff.min,2), " - ",
                                      round(l.diff.max,2), " ",
                                      input$lbi_lengthUnit), "<br>"
    ))
}
