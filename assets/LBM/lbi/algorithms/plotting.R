

plotLBI.data <- function(lbi_dat, input){
    dat <- lbi_dat$dataExplo[['lfq']]
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
        mtext(paste0("Mid lengths [", input$LBI_lengthUnit,"]"), 1, 1, outer = TRUE)
        mtext("Frequency", 2, 1, outer = TRUE)
        box(lwd = 1.5)
    }
}

plotLBI.diag1 <- function(lbi_dat, input){

    bs <- lbi_dat$binSize
    lfq <- lfqCreate(lbi_dat$dataExplo$raw,
                     Lname = "Length",
                     Dname = "Date",
                     Fname = "Frequency",
                     bin_size = bs)
    ## for now by month, maybe later let user choose
    new.lfq <- lfqModify(lfq,
                         years = lbi_dat$years_selected,
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

plotLBI.diag2 <- function(lbi_dat, input){

    bs <- lbi_dat$binSize
    lfq <- lfqCreate(lbi_dat$dataExplo$raw,
                     Lname = "Length",
                     Dname = "Date",
                     Fname = "Frequency",
                     bin_size = bs)
    new.lfq <- lfqModify(lfq,
                         years = lbi_dat$years_selected,
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
          ylab = paste0("Mid length [", input$LBI_lengthUnit, "]"),
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


plotLBI.diag3 <- function(lbi_dat, input){


    bs <- lbi_dat$binSize
    lfq <- lfqCreate(lbi_dat$dataExplo$raw,
                     Lname = "Length",
                     Dname = "Date",
                     Fname = "Frequency",
                     bin_size = bs)
    new.lfq <- lfqModify(lfq,
                         years = lbi_dat$years_selected,
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
    mtext(paste0("Mid length [", input$LBI_lengthUnit, "]"), 1, -1, outer = TRUE)
    mtext("Frequency", 2, -1, outer = TRUE)

}


plotLBI.fit <- function(lbi_dat, input){

    greenred <- function(x, index){
        ref <- if(index == "Pmega"){
                   0.3
               }else if(index %in% c("Lmax5_Linf","L95_Linf")){
                   0.8
               }else 1

        sapply(x, function(x) ifelse(x < ref,"#EA6249","#6CB87B"))
    }

    ind <- apply(lbi_dat$results, 2, function(x) !all(is.na(x)))
    res <- lbi_dat$results[,ind]
    refs <- c("Lmax5_Linf","L95_Linf","Pmega",
              "L25_Lm50","Lc_Lm50",
              "Lmean_Lopt",
              "Lmaxy_Lopt",
              "Lmean_LFeM"
              )
    labs <- c(expression(L[max5*'%']~"/"~L[infinity]),
              expression(L[95]~"/"~L[infinity]),expression(P[mega]),
              expression(L[25*'%']~"/"~L[mat]),
              expression(L[c]~"/"~L[mat]),
              expression(L[mean]~"/"~L[opt]),
              expression(L[maxy]~"/"~L[opt]),
              expression(L[mean]~"/"~L[F=M]))
    rnames <- colnames(res)
    ind <- na.omit(match(refs,rnames))
    xlim <- range(res$year) + c(-0.2,0.2)
    ylim <- c(0,max(1.1,1.05 * max(res[,ind])))
    par(mfrow = c(3,3), mar = c(3,3,2,1), oma = c(3,3,1,1))
    for(i in 1:length(ind)){
        plot(res$year, rep(1, length(res$year)),
             ty = "n",
             ylim = ylim,
             xlim = xlim,
             xlab = "", ylab = "")
        if(rnames[ind[i]] == "Pmega"){
            polygon(c(0.5,2,2,0.5) * c(xlim,rev(xlim)),
                    c(1,1,2,2) * c(0.3,0.3,max(ylim),max(ylim)),
                    border = NA, col = rgb(t(col2rgb("darkgreen")/255),alpha=0.05))
            abline(h = 0.3, lty = 2, col = "grey40", lwd = 1.5)
        }else if(rnames[ind[i]] %in% c("Lmax5_Linf","L95_Linf")){
            polygon(c(0.5,2,2,0.5) * c(xlim,rev(xlim)),
                    c(1,1,2,2) * c(0.8,0.8,max(ylim),max(ylim)),
                    border = NA, col = rgb(t(col2rgb("darkgreen")/255),alpha=0.05))
            abline(h = 0.8, lty = 2, col = "grey40", lwd = 1.5)
        }else if(rnames[ind[i]] %in% c("Lmean_LFeM")){
            polygon(c(0.5,2,2,0.5) * c(xlim,rev(xlim)),
                    c(1,1,2,2) * c(1,1,max(ylim),max(ylim)),
                    border = NA, col = rgb(t(col2rgb("darkgreen")/255),alpha=0.05))
            abline(h = 1, lty = 2, col = "grey40", lwd = 1.5)
        }else{
             abline(h = 1, lty = 2, col = "grey40", lwd = 1.5)
        }
        lines(res$year, res[,ind[i]], ty = "b", col = "grey70", lwd=1.5, cex = 1.7, pch = NA)
        points(res$year, res[,ind[i]], col = greenred(res[,ind[i]], rnames[ind[i]]), lwd=1.5, cex = 1.7)
        mtext(labs[i], 3, 0.5, font = 2)
        mtext("Year(s)", 1, 1, outer = TRUE)
        mtext("Indicator ratio", 2, 1, outer = TRUE)
        if(i == 1){
            legend("topright", legend = c("Ref"),
                   bg = "white",
                   lty=2, lwd=1.5, col = "grey40")
        }
        box(lwd=1.5)
    }
}
