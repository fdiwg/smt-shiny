
library(lubridate)


check.numeric.and.min <- function(pars, can.be.zero = FALSE){

    all.pars <- c(popSize = "population size (GA parameter)",
                  pmut = "probability of mutation (GA parameter)",
                  pcross = "probability of crossover (GA parameter)",
                  elite = "elitism parameter (GA parameter)",
                  maxiter = "maxiter parameter (GA parameter)",
                  run = "run parameter (GA parameter)",
                  ma = "moving average (MA)",
                  binSize = "bin size",
                  a = "a parameter of the length-weight relationship",
                  b = "b parameter of the length-weight relationship",
                  mk = "M/K",
                  linf = "Asymptotic length (Linf)",
                  lm50 = "Length at 50% maturity (Lm50)",
                  lm95 = "Length at 95% maturity (Lm95)",
                  temp = "temperature",
                  tmax = "maximum age",
                  fRangeMin = "minimum value of the F range",
                  fRangeMax = "maximum value of the F range",
                  fRangeSteps = "the number of steps of the F range",
                  lcRangeMin = "minimum value of the selectivity range",
                  lcRangeMax = "maximum value of the selectivity range",
                  lcRangeSteps = "the number of steps of the selectivity range")

    for(i in 1:length(pars)){
        if(!is.numeric(pars[i]) || is.na(pars[i]) || is.null(pars[i])){
            stop(paste0("The ",all.pars[match(names(pars)[i],names(all.pars))],
                        " is not numeric! Please check your input! Did you use a dot as decimal separator?"))
        }
        if(can.be.zero){
            if(pars[i] < 0){
                stop(paste0("The ",all.pars[match(names(pars)[i],names(all.pars))],
                            " has to be larger than or equal to 0!"))
            }
        }else{
            if(pars[i] <= 0){
                stop(paste0("The ",all.pars[match(names(pars)[i],names(all.pars))],
                            " has to be larger than 0!"))
            }
        }

    }
}



formatTimestamp <- function(ts) {
    return(format(as.Date(as.POSIXct(ts, origin="1970-01-01")), format="%Y-%m-%d"))
}

validateLBMInputFile <- function(file, type = "freq"){

    ## read in file
    inputData <- try(read.csv(file, sep=",", dec="."), silent=TRUE)

    ## Major error, e.g. no csv file?
    check_csv <- ifelse(!inherits(inputData, "try-error"), TRUE, FALSE)

    if(check_csv){

        ## Does data include several columns and correct delimiter
        check_delimiter <- ifelse(ncol(inputData) > 1 || type == "raw", TRUE, FALSE)

        ## try if different delimiter solves the issue
        if(!check_delimiter){
            inputData <- read.csv(file, sep=";", dec=".")
            check_delimiter <- ifelse(ncol(inputData) > 1, TRUE, FALSE)
        }
        if(!check_delimiter){
            inputData <- read.csv(file, sep="\t", dec=".")
            check_delimiter <- ifelse(ncol(inputData) > 1, TRUE, FALSE)
        }

        ## Can date be read from column names?
        if(type == "freq"){
            ## Is the first column numeric? -> length classes
            check_lengths <- ifelse(is.numeric(inputData[,1]), TRUE, FALSE)
            colnams <- as.vector(colnames(inputData))
            dates <- sapply(colnams[2:length(colnams)], function(x){
                x <- ifelse(startsWith(x, 'X'), substring(x, 2), x)
                formatTimestamp(parse_date_time(x, c('ymd', 'dmy', 'mdy')))
            })
            inputData <- cbind(inputData[,1], inputData[,which(!is.na(dates))+1])
        }else if(type == "raw"){
            if(ncol(inputData) >= 2){
                suppressWarnings(tmp <- try(sapply(head(inputData[,1]), function(x)
                    formatTimestamp(parse_date_time(x, c('ymd', 'dmy', 'mdy')))),
                    silent = TRUE))
                if(inherits(tmp, "try-error") || all(is.na(tmp))){
                    suppressWarnings(tmp <- try(sapply(head(inputData[,2]), function(x)
                        formatTimestamp(parse_date_time(x, c('ymd', 'dmy', 'mdy')))),
                        silent = TRUE))
                    if(inherits(tmp, "try-error") || all(is.na(tmp))){
                        ## stop("Neither first nor second one recognised as Date!")
                    }else{
                        dates <- tmp
                        lengths <- inputData[,1]
                    }
                }else{
                    dates <- tmp
                    lengths <- inputData[,2]
                }
                dates <- inputData[,1]  ## assumes that second column is always date (if provided)

                ## try to read first column as lengths
                if(all(is.na(dates))){
                    dates <- as.Date(format(Sys.time(),"%Y-%m-%d"))
                    warning("No date found, maybe just length provided, using system date as placeholder for missing date.")
                    lengths <- inputData[,1]
                }
            }else{
                dates <- as.Date(format(Sys.time(),"%Y-%m-%d"))
                warning("No date found, maybe just length provided, using system date as placeholder for missing date.")
                lengths <- inputData[,1]
            }
            check_lengths <- ifelse(is.numeric(lengths), TRUE, FALSE)
        }
        check_dates <- ifelse(any(!is.na(dates)), TRUE, FALSE)

        ## Only use numeric columns + Are there sufficient numeric samples?
        if(type == "freq"){
            inputData <- inputData[,sapply(inputData, is.numeric)]
        }
        check_ncols <- ifelse((!is.null(ncol(inputData)) && ncol(inputData) > 2) || type == "raw", TRUE, FALSE)

    }else{
        check_delimiter <- FALSE
        check_lengths <- FALSE
        check_dates <- FALSE
        check_ncols <- FALSE
    }

    ## return checks
    checks <- list(csv=check_csv, delimiter=check_delimiter, lengths=check_lengths,
                   dates=check_dates, ncols=check_ncols)
    return(list(inputData=inputData, checks=checks))
}


validateLBMInputFile2 <- function(file, type = "freq"){

    is_mostly_numeric <- function(df) {
        numeric_cols <- sapply(df, is.numeric)
        if (type == "freq") {
            ## all have to be numeric
            return(mean(numeric_cols) == 1)
        } else {
            ## assuming one date, one length, and one optional freq column at least 50% have to be numeric
            return(mean(numeric_cols) >= 0.5)
        }
    }

    separators <- c(",", ";", "\t")
    decimals <- c(".", ",")
    inputData <- NULL
    check_csv <- FALSE
    check_delimiter <- FALSE

    for (sep_try in separators) {
        for (dec_try in decimals) {
            tmp <- try(read.csv(file, sep = sep_try, dec = dec_try), silent = TRUE)
            if (!inherits(tmp, "try-error") && is.data.frame(tmp) && ncol(tmp) > 1 && is_mostly_numeric(tmp)) {
                inputData <- tmp
                check_csv <- TRUE
                check_delimiter <- TRUE
                break
            }
        }
        if (check_csv) break
    }

    message(sprintf("Detected sep='%s', dec='%s'", sep_try, dec_try))

    if(check_csv){

        ## Can date be read from column names?
        if(type == "freq"){
            ## Is the first column numeric? -> length classes
            check_lengths <- ifelse(is.numeric(inputData[,1]), TRUE, FALSE)
            colnams <- as.vector(colnames(inputData))
            dates <- sapply(colnams[2:length(colnams)], function(x){
                x <- ifelse(startsWith(x, 'X'), substring(x, 2), x)
                formatTimestamp(parse_date_time(x, c('ymd', 'dmy', 'mdy')))
            })
            inputData <- cbind(inputData[,1], inputData[,which(!is.na(dates))+1])
        }else if(type == "raw"){
            if(ncol(inputData) >= 2){
                suppressWarnings(tmp <- try(sapply(head(inputData[,1]), function(x)
                    formatTimestamp(parse_date_time(x, c('ymd', 'dmy', 'mdy')))),
                    silent = TRUE))
                if(inherits(tmp, "try-error") || all(is.na(tmp))){
                    suppressWarnings(tmp <- try(sapply(head(inputData[,2]), function(x)
                        formatTimestamp(parse_date_time(x, c('ymd', 'dmy', 'mdy')))),
                        silent = TRUE))
                    if(inherits(tmp, "try-error") || all(is.na(tmp))){
                        ## stop("Neither first nor second one recognised as Date!")
                    }else{
                        dates <- tmp
                        lengths <- inputData[,1]
                    }
                }else{
                    dates <- tmp
                    lengths <- inputData[,2]
                }
                dates <- inputData[,1]  ## assumes that second column is always date (if provided)

                ## try to read first column as lengths
                if(all(is.na(dates))){
                    dates <- as.Date(format(Sys.time(),"%Y-%m-%d"))
                    warning("No date found, maybe just length provided, using system date as placeholder for missing date.")
                    lengths <- inputData[,1]
                }
            }else{
                dates <- as.Date(format(Sys.time(),"%Y-%m-%d"))
                warning("No date found, maybe just length provided, using system date as placeholder for missing date.")
                lengths <- inputData[,1]
            }
            check_lengths <- ifelse(is.numeric(lengths), TRUE, FALSE)
        }
        check_dates <- ifelse(any(!is.na(dates)), TRUE, FALSE)

        ## Only use numeric columns + Are there sufficient numeric samples?
        if(type == "freq"){
            inputData <- inputData[,sapply(inputData, is.numeric)]
        }
        check_ncols <- ifelse((!is.null(ncol(inputData)) && ncol(inputData) > 2) || type == "raw", TRUE, FALSE)

    }else{
        check_delimiter <- FALSE
        check_lengths <- FALSE
        check_dates <- FALSE
        check_ncols <- FALSE
    }

    ## return checks
    checks <- list(csv=check_csv, delimiter=check_delimiter, lengths=check_lengths,
                   dates=check_dates, ncols=check_ncols)
    return(list(inputData=inputData, checks=checks))
}



read_lbm_csv <- function(csvFile, format=""){

    Sys.setlocale("LC_TIME", "C")

    print (paste0("Format is: ", format))

    if (is.null(format) || is.na(format)) {
        order <- c('ymd', 'ydm', 'dmy', 'mdy')
    } else if (format == "mdy") {
        order <- c('mdy')
    } else if (format == "dmy") {
        order <- c('dmy')
    } else if (format == 'ymd') {
        order = c('ymd', 'ydm')
    } else if (format == 'ydm') {
        order = c('ydm', 'ymd')
    } else {
        order <- c('ymd', 'ydm', 'dmy', 'mdy')
    }

    ## read in and validate csv file
    dataset <- validateLBMInputFile2(csvFile, type = "freq")
    if(all(unlist(dataset$checks))){
        inputData <- dataset$inputData
        ## lfq list
        dataset$lfq <- list()

        ## sample number
        dataset$lfq$sample.no <- seq(1, nrow(as.matrix(inputData[,-1])))

        ## dates
        colnams <- colnames(inputData)[-1]
        colnams <- sapply(colnams, function(x) ifelse(startsWith(x, 'X'), substring(x, 2), x))
        dates <- sapply(colnams,
                        function(x)
                            formatTimestamp(parse_date_time(x, c('ymd', 'dmy', 'mdy'))))
        dates <- as.Date(dates, "%Y-%m-%d")
        dataset$lfq$dates <- sort(dates)

        ## length classes
        dataset$lfq$midLengths <- inputData[,1]

        ## catch matrix
        dataset$lfq$catch <- as.matrix(inputData[,-1])[,order(dates)]
        colnames(dataset$lfq$catch) <- sort(dates)
    }else{
        ## requires that first column contains dates and second length measurements
        dataset <- validateLBMInputFile2(csvFile, type = "raw")
        if(all(unlist(dataset$checks))){
            inputData <- dataset$inputData

            if(ncol(inputData) == 1){
                freqs <- 1
            }else if(ncol(inputData) == 2){
                freqs <- 1
            }else{
                freqs <- inputData[,3]
            }

            ## either first length and date or first date and second length
            suppressWarnings(tmp <- try(sapply(inputData[,1], function(x)
                formatTimestamp(parse_date_time(x, c('ymd', 'dmy', 'mdy')))),
                silent = TRUE))
            if(inherits(tmp, "try-error") || all(is.na(tmp))){
                suppressWarnings(tmp <- try(sapply(inputData[,2], function(x)
                    formatTimestamp(parse_date_time(x, c('ymd', 'dmy', 'mdy')))),
                    silent = TRUE))
                if(inherits(tmp, "try-error") || all(is.na(tmp))){
                    dates <- as.Date(format(Sys.time(),"%Y-%m-%d"))
                    lengths <- inputData[,1]
                    ## stop("Neither first nor second one recognised as Date!")
                }else{
                    dates <- tmp
                    lengths <- inputData[,1]
                }
            }else{
                dates <- tmp
                lengths <- inputData[,2]
            }

            newData <- data.frame(Length = lengths,
                                  Date = as.Date(dates, "%Y-%m-%d"),
                                  Frequency = freqs)

            ## colnames(inputData) <- c("Length","Date","Frequency")
            ## Format date
            ## dates <- sapply(inputData$Date,
            ##                 function(x)
            ##                     formatTimestamp(parse_date_time(x, c('ymd', 'dmy', 'mdy'))))
            ## inputData$Date <- as.Date(dates, "%Y-%m-%d")
            ## smallest bin size
            tmp <- diff(sort(newData$Length))
            bs <- round(max(c(0.05,
                              min(tmp[tmp > 0], na.rm = TRUE),
                              0.05*0.23*max(newData$Length, na.rm = TRUE)^0.6)),2)
            ## lfq list
            dataset$lfq <- lfqCreate(newData, Lname = "Length", Dname = "Date", Fname = "Frequency",
                                     bin_size = bs, aggregate_dates = TRUE)
        }else{
            ## If both wrong report errors from freq format
            dataset <- validateLBMInputFile2(csvFile, type = "freq")
        }
    }

    return(dataset)
}
