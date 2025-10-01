

plotTropFishR.data <- function(elefan_ga, input){
    par(mfrow = c(2,1), mar = c(1,4,0,1), oma = c(3,1,1,0))
    TropFishR:::plot.lfq(elefan_ga$dataExplo$lfq, Fname = "catch", date.axis = "",
                         ylab = paste0("Length classes [",input$elefan_lengthUnit,"]"))
    legend("topleft",legend=as.expression(bquote(bold("A"))),
           x.intersp = -0.3, y.intersp = 0.3, cex=1.3, bg = "white")
    TropFishR:::plot.lfq(elefan_ga$dataExplo$lfqbin, Fname = "rcounts", date.axis = "modern",
                         ylab = paste0("Length classes [",input$elefan_lengthUnit,"]"))
    legend("topleft",legend=as.expression(bquote(bold("B"))),
           x.intersp = -0.3, y.intersp = 0.3, cex=1.3, bg = "white")
    ## graphics::box(lwd = 1.5)
}


plotTropFishR.diag1 <- function(elefan_ga, input){

    bs <- elefan_ga$binSize
    lfq <- lfqCreate(elefan_ga$dataExplo$raw,
                     Lname = "Length",
                     Dname = "Date",
                     Fname = "Frequency",
                     bin_size = bs)
    ## for now by month, maybe later let user choose
    new.lfq <- lfqModify(lfq,
                         years = elefan_ga$years_selected,
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

plotTropFishR.diag2 <- function(elefan_ga, input){

    bs <- elefan_ga$binSize
    lfq <- lfqCreate(elefan_ga$dataExplo$raw,
                     Lname = "Length",
                     Dname = "Date",
                     Fname = "Frequency",
                     bin_size = bs)
    new.lfq <- lfqModify(lfq,
                         years = elefan_ga$years_selected,
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
          ylab = paste0("Mid length [", input$elefan_lengthUnit, "]"),
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


plotTropFishR.diag3 <- function(elefan_ga, input){


    bs <- elefan_ga$binSize
    lfq <- lfqCreate(elefan_ga$dataExplo$raw,
                     Lname = "Length",
                     Dname = "Date",
                     Fname = "Frequency",
                     bin_size = bs)
    new.lfq <- lfqModify(lfq,
                         years = elefan_ga$years_selected,
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
    mtext(paste0("Mid length [", input$elefan_lengthUnit, "]"), 1, -1, outer = TRUE)
    mtext("Frequency", 2, -1, outer = TRUE)

}


plotTropFishR.growth <- function(elefan_ga, input){
    par(mfrow = c(2,1), mar = c(1,4,0,1), oma = c(3,1,1,0))
    TropFishR:::plot.lfq(elefan_ga$dataExplo$lfqbin, Fname = "catch", date.axis = "",
                         ylab = paste0("Length classes [",input$elefan_lengthUnit,"]"))
    lt <- lfqFitCurves(lfq = elefan_ga$dataExplo$lfqbin,
                       par=as.list(elefan_ga$results$resGA$par),
                       draw = TRUE, lty = 1, col = "dodgerblue2", lwd=2.5)
    legend("topleft",legend=as.expression(bquote(bold("A"))),
           x.intersp = -0.3, y.intersp = 0.3, cex=1.3, bg = "white")
    TropFishR:::plot.lfq(elefan_ga$dataExplo$lfqbin, Fname = "rcounts",
                         ylab = paste0("Length classes [",input$elefan_lengthUnit,"]"))
    lt <- lfqFitCurves(lfq = elefan_ga$dataExplo$lfqbin,
                       par=as.list(elefan_ga$results$resGA$par),
                       draw = TRUE, lty = 1, col = "dodgerblue2", lwd=2.5)
    legend("topleft",legend=as.expression(bquote(bold("B"))),
           x.intersp = -0.3, y.intersp = 0.3, cex=1.3, bg= "white")
}


plotTropFishR.ga <- function(elefan_ga, input){
    par(mar=c(5,5,2,1))
    if(input$provideGP && !is.na(input$provide_Linf) && !is.na(input$provide_K) &&
       !is.na(input$provide_t_anchor)){
        plot.new()
        legend("center",legend = "ELEFAN skipped!", pch = NA, bty = "n")
    }else{
        ## GA::plot.ga()
        cex.points = 0.7
        col = c("green3", "dodgerblue3",
                adjustcolor("green3", alpha.f = 0.1))
        pch = c(16, 1)
        lty = c(1,2)
        legend = TRUE
        grid = graphics:::grid
        object <- elefan_ga$results$resGA$gafit
        is.final <- !(any(is.na(object@summary[, 1])))
        iters <- if (is.final) 1:object@iter else 1:object@maxiter
        summary <- object@summary
        ylim <- c(max(apply(summary[, c(2, 4)], 2,
                            function(x) min(range(x, na.rm = TRUE, finite = TRUE)))),
                  max(range(summary[,1], na.rm = TRUE, finite = TRUE)))
        plot(iters, summary[, 1], type = "n", ylim = ylim, xlab = "Generation",
             ylab = "Fitness value (Rn)")
        if (is.final & is.function(grid)) grid(equilogs = FALSE)
        points(iters, summary[, 1], type = ifelse(is.final, "o","p"),
               pch = pch[1], lty = lty[1], col = col[1], cex = cex.points)
        points(iters, summary[, 2], type = ifelse(is.final, "o","p"),
               pch = pch[2], lty = lty[2], col = col[2], cex = cex.points)
        if(is.final){
            polygon(c(iters, rev(iters)), c(summary[, 4], rev(summary[,1])), border = FALSE, col = col[3])
        }else{
            title(paste("Iteration", object@iter), font.main = 1)
        }
        if(is.final & legend){
            inc <- !is.na(col)
            legend("bottomright", legend = c("Best", "Mean", "Median")[inc],
                   col = col[inc], pch = c(pch, NA)[inc],
                   lty = c(lty, 1)[inc], lwd = c(1, 1, 10)[inc],
                   pt.cex = c(rep(cex.points, 2), 2)[inc], inset = 0.02)
        }
    }
}


plotTropFishR.mort <- function(elefan_ga, input){
    ## Constant rates
    meanZ <- elefan_ga$results$resCC$Z
    meanM <- mean(elefan_ga$results$resM[elefan_ga$results$resCC$reg_int[1]:elefan_ga$results$resCC$reg_int[2]])
    meanFM <- meanZ - meanM
    ## Length-dep rates
    midLengths <- elefan_ga$results$lfqbin$midLengths
    lt <- seq(min(midLengths), max(midLengths), 0.01)
    L50 <- elefan_ga$results$L50
    L75 <- elefan_ga$results$L75
    slist <- list(selecType = "trawl_ogive",
                  L50 = L50, L75 = L75)
    sest <- TropFishR::select_ogive(slist, Lt = lt)
    FM <- meanFM * sest
    if(input$natM == "Then's growth formula"){
        natM <- "Then_growth"
    }else if(input$natM == "Pauly's growth & temp. formula"){
        natM <- "Pauly_Linf"
    }else if(input$natM == "Then's max. age formula"){
        natM <- "Then_tmax"
    }else if(input$natM == "Gislason's length-based formula"){
        natM <- "Gislason2"  ## below 10cm constant
    }else if(input$natM == "Lorenzen's length-based formula"){
        natM <- "Lorenzen_2022"
    }
    flag.lb.m <- ifelse(natM %in% c("Gislason", "Gislason2","Lorenzen_2022"), TRUE, FALSE)
    Mest <- M_empirical_temp(Linf = elefan_ga$results$resGA$par$Linf,
                             K_l = elefan_ga$results$resGA$par$K,
                             Bl = as.numeric(lt),
                             method = natM,
                             schooling = input$schooling,
                             tmax = input$tmax, temp = input$temp)
    if(flag.lb.m){
        M <- Mest$Ml
    }else{
        M <- rep(as.numeric(Mest), length(lt))
    }
    Z <- FM + M
    ## Exploited lengths
    lrange_exploit <- range(midLengths[elefan_ga$results$resCC$reg_int[1]:elefan_ga$results$resCC$reg_int[2]])

    cols <- c("dodgerblue4","darkorange","darkgreen")

    par(mar=c(5,5,2,1) + 0.1)
    plot(lt, rep(0, length(lt)),
         xlim = extendrange(r = range(lt), f = 0.01),
         ylim = c(0,max(extendrange(r = range(0,FM,M,Z,meanM,meanFM,meanZ), f = 0.1))),
         xlab = "", ylab = "",
         ty = 'n')
    segments(lrange_exploit[1], meanZ, lrange_exploit[2], meanZ, col = cols[1], lwd = 2, lty = 2)
    segments(lrange_exploit[1], meanFM, lrange_exploit[2], meanFM, col = cols[2], lwd = 2, lty = 2)
    segments(lrange_exploit[1], meanM, lrange_exploit[2], meanM, col = cols[3], lwd = 2, lty = 2)
    lines(lt, Z, col = cols[1], lwd = 2)
    lines(lt, FM, col = cols[2], lwd = 2)
    lines(lt, M, col = cols[3], lwd = 2)
    mtext("Length [cm]", 1, 3)
    mtext(expression("Mortality rate ["*yr^{-1}*"]"), 2, 3)
    legend("bottomright",
           legend = c("Total","Fishing","Natural mortality"),
           ## horiz = TRUE,
           col = cols, lwd = 2,
           bg = "white")
    graphics::box(lwd = 1.5)
}


plotTropFishR.catchcurve <- function(elefan_ga, input){
    resCC <- elefan_ga$results$resCC
    ind <- resCC$reg_int[1]:resCC$reg_int[2]
    par(mar=c(5,5,2,1))
    plot(resCC$t_midL, resCC$lnC_dt, ty = 'n',
         xlab = "Relative age [years]", ylab = "ln(C/dt)",
         cex=1.4)
    points(resCC$t_midL[-ind], resCC$lnC_dt[-ind],
           col = "black", cex=1.4)
    points(resCC$t_midL[ind], resCC$lnC_dt[ind],
           col = "dodgerblue2", pch = 16, cex=1.4)
    abline(resCC$linear_mod, lwd=2.5, col = "dodgerblue2")
    graphics::box(lwd = 1.5)
}



plotTropFishR.sel <- function(elefan_ga, input){
    L50 <- elefan_ga$results$L50
    L75 <- elefan_ga$results$L75
    slist <- list(selecType = "trawl_ogive",
                  L50 = L50, L75 = L75)
    lt <- seq(0, 1.5 * max(elefan_ga$results$lfqbin$midLengths), 0.01)
    sest <- TropFishR::select_ogive(slist, Lt = lt)
    par(mar=c(5,5,2,1))
    plot(lt, sest, ty='n', lwd=2,
         xlab = "Length", ylab = "Probability of capture")
    if(input$select == "Other"){
        tmp <- TropFishR::select_ogive(slist, Lt = input$l1_user)
        segments(input$l1_user, -1, input$l1_user, tmp, lty = 2, lwd=1.5, col="grey60")
        segments(-10, tmp, input$l1_user, tmp, lty = 2, lwd=1.5, col="grey60")
    }else{
        tmp <- TropFishR::select_ogive(slist, Lt = L50)
        segments(L50, -1, L50, tmp, lty = 2, lwd=1.5, col="grey60")
        segments(-10, tmp, L50, tmp, lty = 2, lwd=1.5, col="grey60")
    }
    if(input$select == "Other"){
        tmp <- TropFishR::select_ogive(slist, Lt = input$l2_user)
        segments(input$l2_user, -1, input$l2_user, tmp, lty = 2, lwd=1.5, col="grey60")
        segments(-10, tmp, input$l2_user, tmp, lty = 2, lwd=1.5, col="grey60")
    }else{
        tmp <- TropFishR::select_ogive(slist, Lt = L75)
        segments(L75, -1, L75, tmp, lty = 3, lwd=1.5, col="grey60")
        segments(-10, tmp, L75, tmp, lty = 3, lwd=1.5, col="grey60")
    }
    lines(lt, sest, lwd=2.5, col="dodgerblue2")
    if(input$select == "Other"){
        legend("bottomright", legend = c("Selection ogive",
                                         paste0("Ls",input$l1_user),
                                         paste0("Ls",input$l2_user)),
               lty = c(1,2,3), col=c("dodgerblue2","grey60","grey60"),
               lwd=c(2,1.5,1.5))
    }else{
        legend("bottomright", legend = c("Selection ogive","Ls50","Ls75"),
               lty = c(1,2,3), col=c("dodgerblue2","grey60","grey60"),
               lwd=c(2,1.5,1.5))
    }
    graphics::box(lwd = 1.5)
}


plotTropFishR.ypr <- function(elefan_ga, input){
    resYPR <- elefan_ga$results$resYPR1
    refs <- as.numeric(resYPR$df_Es)

    if(all(is.na(resYPR$SPR)) || is.null(elefan_ga$results$Lm50) || is.null(elefan_ga$results$Lm75)){

        par(mfrow=c(2,1), mar=c(1,4,0,2), oma=c(4,1,1,0))
        ## YPR
        plot(resYPR$FM_change, resYPR$totY, ty='n',
             ylim = c(0,1.25) * range(resYPR$totY),
             xaxt = "n",
             xlab = "", ylab = "")
        tmp <- resYPR$totY[which.min(abs(resYPR$FM_change-refs[1]))]
        segments(refs[1], -10, refs[1], tmp,
                 lty=2, lwd=1.5, col="grey60")
        segments(-10, tmp, refs[1], tmp,
                 lty=2, lwd=1.5, col="grey60")
        tmp <- resYPR$totY[which.min(abs(resYPR$FM_change-refs[2]))]
        segments(refs[2], -10, refs[2], tmp,
                 lty=3, lwd=1.5, col="grey60")
        segments(-10, tmp, refs[2], tmp,
                 lty=3, lwd=1.5, col="grey60")
        lines(resYPR$FM_change, resYPR$totY, lwd=2.5,
              col="dodgerblue2")
        legend("topleft",legend=as.expression(bquote(bold("A"))),
               x.intersp = -0.3, y.intersp = 0.3, cex=1.3, bg= "white")
        legend("topright",legend=c("Fmax","F0.1"),
               title = "Reference points",
               lty = c(2,3), cex=1.1, bg= "white")
        mtext("Yield per recruit", 2, 3.5)
        ## BPR
        plot(resYPR$FM_change, resYPR$meanB, ty='n',
             ylim = c(0,1.1) * range(resYPR$meanB),
             xlab = "", ylab = "")
        tmp <- resYPR$meanB[which.min(abs(resYPR$FM_change-refs[3]))]
        segments(refs[3], -10, refs[3], tmp,
                 lty=2, lwd=1.5, col="grey60")
        segments(-10, tmp, refs[3], tmp,
                 lty=2, lwd=1.5, col="grey60")
        lines(resYPR$FM_change, resYPR$meanB, lwd=2.5,
              col="dodgerblue2")
        legend("topleft",legend=as.expression(bquote(bold("B"))),
               x.intersp = -0.3, y.intersp = 0.3, cex=1.3, bg= "white")
        legend("topright",title = "Reference points",
               legend=c("F0.5"), lty = c(2), cex=1.1, bg= "white")
        mtext("Biomass per recruit", 2, 3.5)
        mtext("Fishing mortality", 1, 3)
        graphics::box(lwd = 1.5)


    }else{

        par(mfrow=c(3,1), mar=c(1,4,0,2), oma=c(4,1,1,0))
        ## YPR
        plot(resYPR$FM_change, resYPR$totY, ty='n',
             ylim = c(0,1.25) * range(resYPR$totY),
             xaxt = "n",
             xlab = "", ylab = "")
        tmp <- resYPR$totY[which.min(abs(resYPR$FM_change-refs[1]))]
        segments(refs[1], -10, refs[1], tmp,
                 lty=2, lwd=1.5, col="grey60")
        segments(-10, tmp, refs[1], tmp,
                 lty=2, lwd=1.5, col="grey60")
        tmp <- resYPR$totY[which.min(abs(resYPR$FM_change-refs[2]))]
        segments(refs[2], -10, refs[2], tmp,
                 lty=3, lwd=1.5, col="grey60")
        segments(-10, tmp, refs[2], tmp,
                 lty=3, lwd=1.5, col="grey60")
        lines(resYPR$FM_change, resYPR$totY, lwd=2.5,
              col="dodgerblue2")
        legend("topleft",legend=as.expression(bquote(bold("A"))),
               x.intersp = -0.3, y.intersp = 0.3, cex=1.3, bg= "white")
        legend("topright",legend=c("Fmax","F0.1"),
               title = "Reference points",
               lty = c(2,3), cex=1.1, bg= "white")
        mtext("Yield per recruit", 2, 3.5)
        ## BPR
        plot(resYPR$FM_change, resYPR$meanB, ty='n',
             ylim = c(0,1.1) * range(resYPR$meanB),
             xaxt = "n",
             xlab = "", ylab = "")
        tmp <- resYPR$meanB[which.min(abs(resYPR$FM_change-refs[3]))]
        segments(refs[3], -10, refs[3], tmp,
                 lty=2, lwd=1.5, col="grey60")
        segments(-10, tmp, refs[3], tmp,
                 lty=2, lwd=1.5, col="grey60")
        lines(resYPR$FM_change, resYPR$meanB, lwd=2.5,
              col="dodgerblue2")
        legend("topleft",legend=as.expression(bquote(bold("B"))),
               x.intersp = -0.3, y.intersp = 0.3, cex=1.3, bg= "white")
        legend("topright",legend=c("F0.5"),
               title = "Reference points",
               lty = c(2), cex=1.1, bg= "white")
        mtext("Biomass per recruit", 2, 3.5)
        ## SPR
        plot(resYPR$FM_change, resYPR$SPR, ty='n',
             ylim = c(0,1.1) * range(resYPR$SPR),
             xlab = "", ylab = "")
        tmp <- resYPR$SPR[which.min(abs(resYPR$FM_change-refs[4]))]
        segments(refs[4], -10, refs[4], tmp,
                 lty=2, lwd=1.5, col="grey60")
        segments(-10, tmp, refs[4], tmp,
                 lty=2, lwd=1.5, col="grey60")
        tmp <- resYPR$SPR[which.min(abs(resYPR$FM_change-refs[5]))]
        segments(refs[5], -10, refs[5], tmp,
                 lty=3, lwd=1.5, col="grey60")
        segments(-10, tmp, refs[5], tmp,
                 lty=3, lwd=1.5, col="grey60")
        tmp <- resYPR$SPR[which.min(abs(resYPR$FM_change-refs[6]))]
        segments(refs[6], -10, refs[6], tmp,
                 lty=4, lwd=1.5, col="grey60")
        segments(-10, tmp, refs[6], tmp,
                 lty=4, lwd=1.5, col="grey60")
        lines(resYPR$FM_change, resYPR$SPR, lwd=2.5,
              col="dodgerblue2")
        legend("topleft",legend=as.expression(bquote(bold("C"))),
               x.intersp = -0.3, y.intersp = 0.3, cex=1.3, bg= "white")
        legend("topright",legend=c(expression(F[30]),expression(F[35]),expression(F[40])),
               title = "Reference points",
               lty = c(2,3,4), cex=1.1, bg= "white")
        mtext("Spawning potential ratio", 2, 3.5)
        mtext("Fishing mortality", 1, 3)
        graphics::box(lwd = 1.5)
    }
}



plotTropFishR.iso <- function(elefan_ga, input){
    par(mfrow = c(2,1), mar = c(4,4,0,1), oma = c(2,0,1,0))
    plot_predict_mod(elefan_ga$results$resYPR2,
                     type = "Isopleth", xaxis1 = "FM",
                     mark = TRUE, contour = 6, xlab="",
                     ylab1 = "")
    mtext(expression(L[50]),2,2.5)
    legend("topleft",legend=as.expression(bquote(bold("A"))),
           x.intersp = -0.3, y.intersp = 0.3, cex=1.3, bg = "white")
    plot_predict_mod(elefan_ga$results$resYPR2, type = "Isopleth",
                     xaxis1 = "FM", yaxis1 = "B_R", mark = TRUE,
                     contour = 6, xlab = "Fishing mortality",
                     ylab1 = "")
    mtext(expression(L[50]),2,2.5)
    legend("topleft",legend=as.expression(bquote(bold("B"))),
           x.intersp = -0.3, y.intersp = 0.3, cex=1.3,
           bg = "white")
}
