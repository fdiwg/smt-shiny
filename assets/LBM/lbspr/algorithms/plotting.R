

plotLBSPR.data <- function(lbspr_dat, input){
    dat <- lbspr_dat$dataExplo[['lfq']]
    ny <- length(dat$dates)
    if(ny >= 19){
        mfrow = c(4,ceiling(ny/4))
    }else if(ny >= 9){
        mfrow = c(3,ceiling(ny/3))
    }else if(ny < 9 & ny >= 4){
        mfrow = c(2,ceiling(ny/2))
    }else if(ny < 4){
        mfrow = c(1,ny)
    }
    par(mfrow = mfrow, mar = c(3,3,2,1), oma = c(3,3,1,1))
    x <- dat$midLengths
    bin.width <- diff(dat$midLengths)
    bin.lower <- dat$midLengths - c(bin.width[1], bin.width)/2
    bin.upper <- dat$midLengths + c(bin.width, bin.width[length(bin.width)])/2
    xlim <- range(0,dat$midLengths)
    ylim <- c(0,1.1) * range(dat$catch)
    for(i in 1:ny){
        if(ny == 1){
            y <- dat$catch
        }else{
            y <- dat$catch[,i]
        }
        plot(x, y, ty='n',
             xlim = xlim,
             ylim = ylim,
             yaxs="i", xaxs="i",
             xlab = "", ylab = "")
        for(j in seq(dat$midLengths)){
            polygon(
                x = c(bin.lower[j], bin.upper[j], bin.upper[j], bin.lower[j]),
                y = c(0, 0, y[j], y[j]),
                col = "grey80",
                border = "grey20", lwd = 1)
        }
        mtext(format(dat$dates[i], "%Y"), 3, 0.5, font = 2)
        mtext(paste0("Mid lengths [", input$LBSPR_lengthUnit,"]"), 1, 1, outer = TRUE)
        mtext("Frequency", 2, 1, outer = TRUE)
        box(lwd = 1.5)
    }
}

plotLBSPR.diag1 <- function(lbspr_dat, input){

    bs <- lbspr_dat$binSize
    lfq <- lfqCreate(lbspr_dat$dataExplo$raw,
                     Lname = "Length",
                     Dname = "Date",
                     Fname = "Frequency",
                     bin_size = bs)
    ## for now by month, maybe later let user choose
    new.lfq <- lfqModify(lfq,
                         years = lbspr_dat$years_selected,
                         aggregate = "month")

    if(!is.matrix(new.lfq$catch)) {
        tmp <- sum(new.lfq$catch)
    } else {
        tmp <- colSums(new.lfq$catch)
    }
    dates <- new.lfq$dates
    all.dates <- seq(min(dates), max(dates), by = "month")
    if (length(all.dates) == 1) {
        single.date <- all.dates
        all.dates <- seq(single.date %m-% months(1), single.date %m+% months(1), by = "month")
    }
    samps <- rep(0, length(all.dates))
    samps[match(dates, all.dates)] <- as.vector(tmp)
    p <- barplot(samps, ylab = "Number of samples")
    axis(1, at = p, labels = format(all.dates,"%B"))
    years <- format(all.dates, "%Y")
    years[duplicated(years)] <- NA
    axis(1, at = p, labels = years, tick = FALSE, line = 1)

}

plotLBSPR.diag2 <- function(lbspr_dat, input){

    bs <- lbspr_dat$binSize
    lfq <- lfqCreate(lbspr_dat$dataExplo$raw,
                     Lname = "Length",
                     Dname = "Date",
                     Fname = "Frequency",
                     bin_size = bs)
    new.lfq <- lfqModify(lfq,
                         years = lbspr_dat$years_selected,
                         aggregate = "month")

    ## dates <- as.Date(paste0(format(new.lfq$dates,"%Y-%m"),"-15"))
    dates <- new.lfq$dates
    all.dates <- seq(min(dates), max(dates), by = "month")
    if (length(all.dates) == 1) {
        single.date <- all.dates
        all.dates <- seq(single.date %m-% months(1), single.date %m+% months(1), by = "month")
    }
    lengths <- new.lfq$midLengths
    all.lengths <- seq(min(lengths), max(lengths), min(diff(lengths)))
    catch <- t(new.lfq$catch)
    all.catch <- matrix(0, length(all.dates), length(all.lengths))
    all.catch[as.matrix(expand.grid(match(dates, all.dates),
                                    match(round(lengths,4), round(all.lengths,4))))] <- as.vector(catch)

    layout(matrix(c(1, 2), nrow = 1), widths = c(6, 1))
    zlim <- range(all.catch, na.rm = TRUE)
    n_colors <- 12
    color_palette <- hcl.colors(n_colors, "YlOrRd", rev = TRUE)
    breaks <- seq(zlim[1], zlim[2], length.out = n_colors + 1)
    par(mar = c(5, 4, 4, 1))
    image(x = all.dates,
          y = all.lengths,
          z = all.catch,
          col = color_palette,
          breaks = breaks,
          xlab = "",
          ylab = paste0("Mid length [", input$LBSPR_lengthUnit, "]"),
          main = "")
    box(lwd = 1.5)
    par(mar = c(5, 1, 4, 4))
    legend_y <- seq(zlim[1], zlim[2], length.out = n_colors)
    legend_z <- matrix(legend_y, nrow = 1)
    image(x = 1, y = legend_y,
          z = legend_z,
          col = color_palette,
          xaxt = "n", yaxt = "n", xlab = "", ylab = "")
    axis(4, at = pretty(zlim), labels = pretty(zlim), las = 1)
    mtext("Frequency", side = 4, line = 2.5)

}


plotLBSPR.diag3 <- function(lbspr_dat, input){


    bs <- lbspr_dat$binSize
    lfq <- lfqCreate(lbspr_dat$dataExplo$raw,
                     Lname = "Length",
                     Dname = "Date",
                     Fname = "Frequency",
                     bin_size = bs)
    new.lfq <- lfqModify(lfq,
                         years = lbspr_dat$years_selected,
                         aggregate = "year")

    ## Selected freqs
    years <- format(new.lfq$dates, "%Y")
    yearly_catch <- rowsum(t(new.lfq$catch), group = years)
    yearly_catch <- t(yearly_catch)

    res <- apply(yearly_catch, 2, function(x) rep(new.lfq$midLengths,
                                                  times = x))

    ## boxplot(res)
    ny <- ncol(yearly_catch)
    par(mfrow = n2mfrow(ny, asp = 2))
    for(i in 1:ny){
        p <- barplot(yearly_catch[,i], main = colnames(yearly_catch)[i],
                     xaxt = "n")
        axis(1, at = p, labels = new.lfq$midLengths)
    }
    mtext(paste0("Mid length [", input$LBSPR_lengthUnit, "]"), 1, -1, outer = TRUE)
    mtext("Frequency", 2, -1, outer = TRUE)

}




plotLBSPR.hist <- function(lbspr_dat, input){

    modfit <- lbspr_dat$results

    if(!is.null(modfit)){
        LBSPR::plotSize(modfit)
    }
}


plotLBSPR.sel <- function(lbspr_dat, input){

    modfit <- lbspr_dat$results

    if(!is.null(modfit)){
        LBSPR::plotMat(modfit, useSmooth = TRUE, Title = NULL)
    }
}



plotLBSPR.pie <- function(lbspr_dat, input){

    modfit <- lbspr_dat$results

    if(!is.null(modfit)){
        suppressMessages({
            LBSPR::plotSPRCirc(modfit,
                               SPRTarg = input$LBSPR_sprTarg,
                               SPRLim = input$LBSPR_sprLim,
                               useSmooth = TRUE,
                               Title = FALSE, Leg = FALSE,
                               limcol = "firebrick4",
                               targcol = "forestgreen",
                               abtgcol = "goldenrod2",
                               labcol = if(modfit@Ests[nrow(modfit@Ests),"SPR"] < input$LBSPR_sprLim){
                                            "firebrick4"} else if(modfit@Ests[nrow(modfit@Ests),"SPR"] < input$LBSPR_sprTarg){"forestgreen"}else{"goldenrod2"},
                               texcex = 1,
                               labcex = 1.2)
        })
    }
}


plotLBSPR.ts <- function(lbspr_dat, input){

    modfit <- lbspr_dat$results

    if(!is.null(modfit)){
        LBSPR::plotEsts(modfit, Lwd = 2, doSmooth = TRUE, incL50 = FALSE)
    }
}
