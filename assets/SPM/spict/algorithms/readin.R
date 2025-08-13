

validate_spict_csv <- function(file, sep = "", dec = ""){

    is_mostly_numeric <- function(df) {
        numeric_cols <- sapply(df, is.numeric)
        ## at least 50% numeric
        return(mean(numeric_cols) >= 0.5)
    }

    if(sep == "auto") {
        separators <- c(",", ";", "\t", " ")
    } else {
        separators <- sep
    }
    if(dec == "auto") {
        decimals <- c(".", ",")
    } else {
        decimals <- dec
    }
    inputData <- NULL
    check_csv <- FALSE
    check_delimiter <- FALSE

    for (sep_try in separators) {
        for (dec_try in decimals) {
            tmp <- try(read.csv(file, sep = sep_try, dec = dec_try), silent = TRUE)
            if (!inherits(tmp, "try-error") &&
                is.data.frame(tmp) && ncol(tmp) > 1 &&
                is_mostly_numeric(tmp)) {
                inputData <- tmp
                check_csv <- TRUE
                check_delimiter <- TRUE
                break
            }
        }
        if (check_csv) break
    }

    message(sprintf("Detected sep='%s', dec='%s'", sep_try, dec_try))

    if (check_csv) {
        format <- if(is.character(inputData[,1]) && colnames(inputData)[1] == "variable") "long" else "wide"
    }

    ## return checks
    checks <- list(csv = check_csv,
                   delimiter = check_delimiter,
                   format = format)
    return(list(inputData = inputData,
                checks = checks))
}


read_spict_csv <- function(csvFile, sep = "", dec = ""){

    Sys.setlocale("LC_TIME", "C")

    print (paste0("Separator is: ", sep))

    if (is.null(sep) || is.na(sep)) {
        sep <- "auto"
    }

    print (paste0("Decimal delimiter is: ", dec))

    if (is.null(dec) || is.na(dec)) {
        dec <- "auto"
    }

    ## read in and validate csv file
    dataset <- validate_spict_csv(csvFile, sep = sep, dec = dec)

    return(dataset)
}


## function to check consistency and find matching column names
## ------------------------------------------------------------------
checkDat <- function(dat, colNames){

    checks <- dat$checks
    format <- checks$format
    dat <- dat$inputData

    if (format == "wide") {
        colNamesRaw <- colnames(dat)
    } else {
        colNamesRaw <- dat[,1]
    }

    timeCInd <- which(colNames$timeC == colNamesRaw)
    obsCInd <- which(colNames$obsC == colNamesRaw)

    ## catch matched?
    if(length(timeCInd) == 0) stop(paste0("Column ",colNames$timeC," for the times of the catch observations not found in the uploaded file."))
    if(length(obsCInd) == 0) stop(paste0("Column ",colNames$obsC," for the catch observations not found in the uploaded file."))

    ## matched to too many columns?
    if(length(timeCInd) > 1 && format == "wide") stop(paste0("Several columns match to the times of the catch observations: ",paste0(colNamesRaw[timeCInd], collapse = ", "),". The column names in the uploaded file have to be unique! Please change them."))
    if(length(obsCInd) > 1 && format == "wide") stop(paste0("Several columns match to the catch observations: ",paste0(colNamesRaw[obsCInd], collapse = ", "),". The column names in the uploaded file have to be unique! Please change them."))

    timeIInd <- list()
    obsIInd <- list()
    if(length(colNames$timeI) > 0){
        for(i in 1:length(colNames$timeI)){
            timeIInd[[i]] <- which(colNames$timeI[[i]] == colNamesRaw)
        }
    }
    if(length(colNames$obsI) > 0){
        for(i in 1:length(colNames$obsI)){
            obsIInd[[i]] <- which(colNames$obsI[[i]] == colNamesRaw)
        }
    }

    timeEInd <- which(colNames$timeE == colNamesRaw)
    obsEInd <- which(colNames$obsE == colNamesRaw)

    ## index or effort matched?
    if(length(timeIInd) < 1 && length(timeEInd) < 1) stop(paste0("Neither a column for the times of the index nor effort found in the uploaded file."))
    if(length(obsIInd) < 1 && length(obsEInd) < 1) stop(paste0("Neither a column for the index nor effort observations found in the uploaded file."))

    ## matched to too many columns?
    if(length(timeEInd) > 1 && format == "wide") stop(paste0("Several columns match to the times of the effort observations: ",paste0(colNamesRaw[timeEInd], collapse = ", "),". The column names in the uploaded file have to be unique! Please change them."))
    if(length(obsEInd) > 1 && format == "wide") stop(paste0("Several columns match to the catch observations: ",paste0(colNamesRaw[obsEInd], collapse = ", "),". The column names in the uploaded file have to be unique! Please change them."))

    ## scaling of uncertainty of observations
    stdevfacCInd <- which(colNames$stdevfacC == colNamesRaw)
    stdevfacEInd <- which(colNames$stdevfacE == colNamesRaw)
    stdevfacIInd <- list()
    if(length(colNames$stdevfacI) > 0) {
        for(i in 1:length(colNames$stdevfacI)){
            if(colNames$stdevfacI[i] != "NA" &&
               colNames$stdevfacI[i] != ""){
                stdevfacIInd[[i]] <- which(colNames$stdevfacI[[i]] == colNamesRaw)
            }
        }
    }

    if (format == "wide") {
        timeC <- dat[,timeCInd]
        obsC <- dat[,obsCInd]

        if(length(timeIInd) > 0){
            timeI <- list()
            for(i in 1:length(timeIInd)){
                timeI[[i]] <- dat[,timeIInd[[i]]]
            }
        }else{
            timeI <- NA
        }
        if(length(obsIInd) > 0){
            obsI <- list()
            for(i in 1:length(obsIInd)){
                obsI[[i]] <- dat[,obsIInd[[i]]]
            }
        }else{
            obsI <- NA
        }

        if(length(timeEInd) > 0){
            timeE <- dat[,timeEInd]
        }else{
            timeE <- NA
        }
        if(length(obsEInd) > 0){
            obsE <- dat[,obsEInd]
        }else{
            obsE <- NA
        }

        if(length(stdevfacCInd) > 0){
            stdevfacC <- dat[,stdevfacCInd]
        }else{
            stdevfacC <- NA
        }

        if(length(stdevfacEInd) > 0){
            stdevfacE <- dat[,stdevfacEInd]
        }else{
            stdevfacE <- NA
        }

        if(length(stdevfacIInd) > 0){
            stdevfacI <- list()
            for(i in 1:length(stdevfacIInd)){
                if(!is.null(stdevfacIInd[[i]])) {
                    stdevfacI[[i]] <- dat[,stdevfacIInd[[i]]]
                }
            }
        }else stdevfacI <- NA


    } else {

        timeC <- dat[timeCInd,2]
        obsC <- dat[obsCInd,2]

        if(length(timeIInd) > 0){
            timeI <- list()
            for(i in 1:length(timeIInd)){
                timeI[[i]] <- dat[timeIInd[[i]],2]
            }
        }else{
            timeI <- NA
        }
        if(length(obsIInd) > 0){
            obsI <- list()
            for(i in 1:length(obsIInd)){
                obsI[[i]] <- dat[obsIInd[[i]],2]
            }
        }else{
            obsI <- NA
        }

        if(length(timeEInd) > 0){
            timeE <- dat[timeEInd,2]
        }else{
            timeE <- NA
        }
        if(length(obsEInd) > 0){
            obsE <- dat[obsEInd,2]
        }else{
            obsE <- NA
        }

        if(length(stdevfacCInd) > 0){
            stdevfacC <- dat[stdevfacCInd,2]
        }else{
            stdevfacC <- NA
        }

        if(length(stdevfacEInd) > 0){
            stdevfacE <- dat[stdevfacEInd,2]
        }else{
            stdevfacE <- NA
        }

        if(length(stdevfacIInd) > 0){
            stdevfacI <- list()
            for(i in 1:length(stdevfacIInd)){
                if(!is.null(stdevfacIInd[[i]])) {
                    stdevfacI[[i]] <- dat[stdevfacIInd[[i]],2]
                }
            }
        }else stdevfacI <- NA

    }


    ## combine to dataframe
    nc <- length(timeC)
    nis <- lapply(timeI, length)
    ne <- length(timeE)
    nsc <- length(stdevfacC)
    nsis <- lapply(stdevfacI, length)
    nse <- length(stdevfacE)
    nmax <- max(c(nc,unlist(nis),ne))

    ## checks
    ## if(length(stdevfacCInd) > 0 && nsc > nc){
    ##     warning("stdevfacC is longer than the catch observations. Cutting the variable to the same length as the catch observations.")
    ##     stdevfacC <- stdevfacC[1:nc]
    ## }
    ## if(length(stdevfacEInd) > 0 && nse > ne){
    ##     warning("stdevfacE is longer than the effort observations. Cutting the variable to the same length as the effort observations.")
    ##     stdevfacE <- stdevfacE[1:ne]
    ## }
    ## if(length(stdevfacIInd) > 0){
    ##     if(any(sapply(1:nsis,function(x) nsis[[x]] > nis[[x]]))){
    ##         warning("stdevfacI is longer than the effort observations. Cutting the variable to the same length as the effort observations.")
    ##         for(i in 1:length(stdevfacI)){
    ##             stdevfacI[[i]] <- stdevfacI[[i]][1:nsis[[i]]]
    ##         }
    ##     }
    ## }

    timeC <- c(timeC, rep(NA, nmax-nc))
    obsC <- c(obsC, rep(NA, nmax-nc))
    ret <- cbind(timeC, obsC)

    if(length(timeIInd) > 0){
        for(i in 1:length(timeIInd)){
            tmp <- c(timeI[[i]],rep(NA, nmax-nis[[i]]))
            ret <- cbind(ret, tmp)
            colnames(ret) <- c(colnames(ret)[-ncol(ret)], paste0("timeI",i))
        }
    }
    if(length(obsIInd) > 0){
        for(i in 1:length(obsIInd)){
            tmp <- c(obsI[[i]],rep(NA, nmax-nis[[i]]))
            ret <- cbind(ret, tmp)
            colnames(ret) <- c(colnames(ret)[-ncol(ret)], paste0("obsI",i))
        }
    }

    if(length(timeEInd) > 0){
        timeE <- c(timeE, rep(NA, nmax-ne))
        ret <- cbind(ret, timeE)
    }
    if(length(obsEInd) > 0){
        obsE <- c(obsE, rep(NA, nmax-ne))
        ret <- cbind(ret, obsE)
    }

    if(length(stdevfacCInd) > 0){
        stdevfacC <- c(stdevfacC, rep(NA, nmax-nc))
        ret <- cbind(ret, stdevfacC)
    }
    if(length(stdevfacIInd) > 0){
        for(i in 1:length(stdevfacIInd)){
            tmp <- c(stdevfacI[[i]], rep(NA, nmax-nis[[i]]))
            ret <- cbind(ret, tmp)
            colnames(ret) <- c(colnames(ret)[-ncol(ret)], paste0("stdevfacI",i))
        }
    }
    if(length(stdevfacEInd) > 0){
        stdevfacE <- c(stdevfacE, rep(NA, nmax-ne))
        ret <- cbind(ret, stdevfacE)
    }


    ret <- as.data.frame(ret)

    return(ret)
}


## function that makes inp from dat
## ------------------------------------------------------------------
dat2inp <- function(dat){

    ## checks
    if(!any(colnames(dat) == "timeC")) stop("Your data is missing a column with the timing of the commercial catches labelled 'timeC'!")
    if(!any(colnames(dat) == "obsC")) stop("Your data is missing a column with the commercial catch observations labelled 'obsC'!")
    if(!any(colnames(dat) == "timeI1") && !any(colnames(dat) == "timeE")) stop("Your data is missing a column with the timing of the index or effort data labelled 'timeI1' or 'timeE'!")
    if(!any(colnames(dat) == "obsI1")  && !any(colnames(dat) == "obsE")) stop("Your data is missing a column with the index or effort observations labelled 'obsI1' or 'obsE'!")

    colna <- colnames(dat)
    nmax <- nrow(dat)

    ## spict input list
    inp <- list()
    inp$timeC <- dat$timeC
    inp$obsC <- dat$obsC

    if(any(colnames(dat) == "timeI1")){
        nsurv <- length(which(sapply(strsplit(colna,"timeI"),function(x) length(x) > 1)))
        if(nsurv > 1){
            inp$timeI <- list()
            for(i in 1:nsurv){
                inp$timeI[[i]] <- as.numeric(na.omit(dat[,which(colnames(dat) == paste0("timeI",i))]))
            }
        }else{
            inp$timeI <- dat$timeI1
        }
    }

    if(any(colnames(dat) == "obsI1")){
        nsurv <- length(which(sapply(strsplit(colna,"obsI"),function(x) length(x) > 1)))
        if(nsurv > 1){
            inp$obsI <- list()
            for(i in 1:nsurv){
                inp$obsI[[i]] <- as.numeric(na.omit(dat[,which(colnames(dat) == paste0("obsI",i))]))
            }
        }else{
            inp$obsI <- dat$obsI1
        }
    }

    if(any(colnames(dat) == "timeE")){
        inp$timeE <- dat$timeE
    }
    if(any(colnames(dat) == "obsE")){
        inp$obsE <- dat$obsE
    }

    ## uncertainty scaling
    if(any(colnames(dat) == "stdevfacC")){
        inp$stdevfacC <- dat$stdevfacC
    }
    if(length(grep("stdevfacI", colnames(dat))) > 0){
        survi <- which(sapply(strsplit(colna,"stdevfacI"),function(x) length(x) > 1))
        if(is.list(inp$timeI)) {
            inp$stdevfacI <- vector("list", length(inp$timeI))
            for(i in 1:length(inp$timeI)){
                inp$stdevfacI[[i]] <- rep(1,length(inp$timeI[[i]]))
            }
            for(i in 1:length(survi)){
                inp$stdevfacI[[as.integer(strsplit(colna[survi[i]], "stdevfacI")[[1]][2])]] <- as.numeric(dat[,survi[i]])
            }
        } else {
            if(length(survi) == 1){
                inp$stdevfacI <- as.numeric(dat[,survi])
            } else stop("Two columns found for stdevfacI. That doesn't work!")
        }
    }
    if(any(colnames(dat) == "stdevfacE")){
        inp$stdevfacE <- dat$stdevfacE
    }

    inp <- try(check.inp(inp))

    return(inp)
}


## function that makes dat from inp (for data download of example data)
## ------------------------------------------------------------------
inp2dat <- function(inp){

    ## check inp
    inp <- check.inp(inp)

    ## catches
    timeC <- inp$timeC
    obsC <- inp$obsC

    ## indices
    timeI <- list()
    obsI <- list()
    if(length(inp$timeI) > 0){
        if(class(inp$timeI) == "list"){
            nsurv <- length(inp$timeI)
            for(i in 1:nsurv){
                timeI[[i]] <- inp$timeI[[i]]
                obsI[[i]] <- inp$obsI[[i]]
            }
        }else{
            nsurv <- 1
            timeI[[1]] <- inp$timeI
            obsI[[1]] <- inp$obsI
        }
    }else{
        nsurv <- 0
    }

    ## effort
    timeE <- inp$timeE
    obsE <- inp$obsE

    ## uncertainty scaling
    stdevfacC <- inp$stdevfacC
    stdevfacI <- inp$stdevfacI
    stdevfacE <- inp$stdevfacE

    nc <- length(timeC)
    nis <- lapply(timeI, length)
    ne <- length(timeE)
    nmax <- max(c(nc,unlist(nis)))

    timeC <- c(timeC, rep(NA, nmax-nc))
    obsC <- c(obsC, rep(NA, nmax-nc))
    stdevfacC <- c(stdevfacC, rep(NA, nmax-nc))
    ret <- cbind(timeC,obsC,stdevfacC)

    if(nsurv > 0){
        for(i in 1:nsurv){
            tmp <- c(timeI[[i]],rep(NA, nmax-nis[[i]]))
            ret <- cbind(ret, tmp)
            colnames(ret) <- c(colnames(ret)[-ncol(ret)], paste0("timeI",i))
            tmp <- c(obsI[[i]],rep(NA, nmax-nis[[i]]))
            ret <- cbind(ret, tmp)
            colnames(ret) <- c(colnames(ret)[-ncol(ret)], paste0("obsI",i))
            tmp <- c(stdevfacI[[i]],rep(NA, nmax-nis[[i]]))
            ret <- cbind(ret, tmp)
            colnames(ret) <- c(colnames(ret)[-ncol(ret)], paste0("stdevfacI",i))
        }
    }

    if(length(timeE) > 0){
        timeE <- c(timeE, rep(NA, nmax-ne))
        obsE <- c(obsE, rep(NA, nmax-ne))
        stdevfacE <- c(stdevfacE, rep(NA, nmax-ne))
        ret <- cbind(ret, timeE, obsE, stdevfacE)
    }

    ret <- as.data.frame(ret)

    return(ret)
}
