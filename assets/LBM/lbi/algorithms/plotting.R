

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
    ylim <- c(0,max(1.02,max(res[,ind])))
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
