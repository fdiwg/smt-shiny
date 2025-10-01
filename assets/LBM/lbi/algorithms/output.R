

textLBI.diag1 <- function(lbi_dat, input){

    file.name <- lbi_dat$dataExplo$checks$fileName

    ## input data format
    format <- "not clear"
    if(lbi_dat$dataExplo$checks$format == "wide") {
        format <- "length frequency table"
    }
    if(lbi_dat$dataExplo$checks$format == "long") {
        format <- "raw length measurements"
    }

    ## Number of length bins
    l.diff <- diff(sort(unique(lbi_dat$dataExplo$raw$Length)))
    l.diff.min <- min(l.diff)
    l.diff.max <- max(l.diff)

    ## Range of lengths
    l.min <- min(lbi_dat$dataExplo$raw$Length)
    l.max <- max(lbi_dat$dataExplo$raw$Length)

    ## Number of time points (samples)
    samp.times <- unique(lbi_dat$dataExplo$raw$Date)
    n.samp.times <- length(samp.times)

    ## Time span (first and last sample dates)
    range.samp.times <- range(samp.times)

    ## Total number of samples
    n.samp <- sum(lbi_dat$dataExplo$raw$Frequency)
    ## Samples by sampling time  ## TODO option for user input how can select to do this by "day", "week", "month", "year" and table adjusts
    tmp <- by(lbi_dat$dataExplo$raw$Frequency,
              lbi_dat$dataExplo$raw$Date, sum)
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
