spictModule <- function(input, output, session) {

    ns <- session$ns

    ## Definition of reactive values
    ## ----------------------------
    spict_dat <- reactiveValues(
        dataExplo = NULL,
        results = NULL,
        colNamesORI = NULL,
        colNames = list(),
        colNamesIndex = NULL,
        nIndices = 1,
        nPriors = 3,
        timeShifts = NULL,
        robflags = NULL,
        logqPriors = NULL,
        logsdiPriors = NULL
    )

    spictUploadVreResult <- reactiveValues()

    inputSpictData <- reactiveValues()

    fileSpictState <- reactiveValues(
        upload = NULL
    )

    spictAcknowledged <- reactiveVal(FALSE)
    spictPendingRun   <- reactiveVal(FALSE)


    ## Definition of functions
    ## ----------------------------
    load.data <- reactive({
        if (is.null(input$file) || is.null(fileSpictState$upload)) {
            return(NULL)
        }

        dataset <- read_spict_csv(input$file$datapath,
                                  input$spictCSVsep,
                                  input$spictCSVdec)
        dataset$checks$fileName <- input$file$name
        checks <- dataset$checks

        print(input$file)

        if (is.null(dataset$inputData)) {
            shinyjs::disable("go_spict")
            shinyjs::disable("check_spict")
            shinyjs::disable("createSpictReport")
            shinyjs::disable("createSpictzip")
            showModal(modalDialog(
                title = "Error",
                if(!checks$csv && !checks$delimiter){
                    HTML("Several checks of the uploaded data did not pass. Are you sure that you uploaded the correct file containing a catch and index time series in either the wide or long format?<br><br> Please double-check your data set and refer to the 'Data Upload' info button (i) for more help!")
                } else if(!checks$csv){
                    "Something went wrong when reading in your data set. Did you select a CSV file (i.e. file with ending '.csv')? Click on the info icon for more information."
                }else if(!checks$delimiter){
                    "Something went wrong when reading in your data set. Please ensure that your CSV file delimiter is a comma ',' or semicolon ';'. Click on the info icon for more information."
                }else{
                    HTML("There was an unexpected error when reading in your data set. Are you sure that you uploaded the correct file containing a catch and index time series in either the wide or long format?<br><br> Please double-check your data set and refer to the 'Data Upload' info button (i) for more help!")
                },
                easyClose = TRUE,
                footer = NULL
            ))
            return (NULL)
        } else {
            if (dataset$check$format == "wide") {
                spict_dat$colNamesORI <- colnames(dataset$inputData)
            } else {
                spict_dat$colNamesORI <- unique(dataset$inputData[,1])
            }
            names(spict_dat$colNamesORI) <- spict_dat$colNamesORI
            return(dataset)
        }
    })

    spictDataExplo1 <- function(inputData, colNames) {

        if(!is.null(colNames$timeC) &&
           !is.null(colNames$obsC) &&
           all(sapply(colNames$timeI, function(x) !is.null(x))) &&
           all(sapply(colNames$timeI, function(x) x != "")) &&
           all(sapply(colNames$timeI, function(x) x != "NA")) &&
           all(sapply(colNames$obsI, function(x) !is.null(x))) &&
           all(sapply(colNames$obsI, function(x) x != "")) &&
           all(sapply(colNames$obsI, function(x) x != "NA"))) {

            dat_checked <- checkDat(inputSpictData, colNames)

            if (!all(apply(dat_checked, 2, is.numeric))) {
                stop("The columns are not numeric, is the decimal delimiter correct? Consider trying another decimal delimiter or check your input data and refer to the 'Data Upload' info button (i) for more help!.")
            }
            inpORI <- dat2inp(dat_checked)
            inp <- check.inp(inpORI)

            res <- list(dat_checked = dat_checked,
                        inpORI = inpORI,
                        inp = inp)

            return(res)
        }
    }

    spictDataExplo2 <- function(spict_dat, input) {

        inp <- spict_dat$dataExplo$inpORI

        ## shift indices
        if(!is.null(spict_dat$timeShifts)){
            for(i in 1:length(inp$timeI)){
                tmp <- as.numeric(spict_dat$timeShifts[[i]])
                if(!is.null(tmp) && !is.na(tmp) &&
                   is.numeric(tmp) &&
                   tmp >= 0 && tmp < 1){
                    inp$timeI[[i]] <- floor(inp$timeI[[i]]) + tmp
                }
            }
        }
        ## adjust time series
        inp <- shorten.inp(inp,
                           mintime = input$timerange[1],
                           maxtime = input$timerange[2])

        if(!is.null(input$lognPrior)){

            ## priors
            if(input$lognPrior){
                mu <- input$lognMu
                mu <- ifelse(input$lognLog,log(mu),mu)
                inp$priors$logn <- c(mu,input$lognSd,1)
            }else{
                mu <- input$lognMu
                mu <- ifelse(input$lognLog,log(mu),mu)
                inp$priors$logn <- c(mu,input$lognSd,0)
            }
            if(input$logAlphaPrior){
                mu <- input$logAlphaMu
                mu <- ifelse(input$logAlphaLog,log(mu),mu)
                inp$priors$logalpha <- c(mu,input$logAlphaSd,1)
            }else{
                mu <- input$logAlphaMu
                mu <- ifelse(input$logAlphaLog,log(mu),mu)
                inp$priors$logalpha <- c(input$logAlphaMu,input$logAlphaSd,0)
            }
            if(input$logBetaPrior){
                mu <- input$logBetaMu
                mu <- ifelse(input$logBetaLog,log(mu),mu)
                inp$priors$logbeta <- c(input$logBetaMu,input$logBetaSd,1)
            }else{
                mu <- input$logBetaMu
                mu <- ifelse(input$logBetaLog,log(mu),mu)
                inp$priors$logbeta <- c(input$logBetaMu,input$logBetaSd,0)
            }

            tmp <- ifelse(input$logbkfracPrior,1,0)
            mu <- input$logbkfracMu
            mu <- ifelse(input$logbkfracLog,log(mu),mu)
            inp$priors$logbkfrac <- c(mu,input$logbkfracSd,tmp)

            tmp <- ifelse(input$logKPrior,1,0)
            mu <- input$logKMu
            mu <- ifelse(input$logKLog,log(mu),mu)
            inp$priors$logK <- c(mu,input$logKSd,tmp)

            tmp <- ifelse(input$logmPrior,1,0)
            mu <- input$logmMu
            mu <- ifelse(input$logmLog,log(mu),mu)
            inp$priors$logm <- c(mu,input$logmSd,tmp)

            tmp <- ifelse(input$logrPrior,1,0)
            mu <- input$logrMu
            mu <- ifelse(input$logrLog,log(mu),mu)
            inp$priors$logr <- c(mu,input$logrSd,tmp)

            tmp <- ifelse(input$logsdbPrior,1,0)
            mu <- input$logsdbMu
            mu <- ifelse(input$logsdbLog,log(mu),mu)
            inp$priors$logsdb <- c(mu,input$logsdbSd,tmp)

            tmp <- ifelse(input$logsdfPrior,1,0)
            mu <- input$logsdfMu
            mu <- ifelse(input$logsdfLog,log(mu),mu)
            inp$priors$logsdf <- c(mu,input$logsdfSd,tmp)

            tmp <- ifelse(input$logsdcPrior,1,0)
            mu <- input$logsdcMu
            mu <- ifelse(input$logsdcLog,log(mu),mu)
            inp$priors$logsdc <- c(mu,input$logsdcSd,tmp)

            for(i in 1:spict_dat$nIndices){
                if(!is.null(spict_dat$logqPriors[[i]]$prior) &&
                   spict_dat$logqPriors[[i]]$prior == TRUE) {
                    tmp <- ifelse(spict_dat$logqPriors[[i]]$prior,1,0)
                    mu <- spict_dat$logqPriors[[i]]$mu
                    mu <- ifelse(spict_dat$logqPriors[[i]]$log,log(mu),mu)
                    inp$priors$logq[[i]] <- c(mu,spict_dat$logqPriors[[i]]$sd,tmp)
                }
            }

            for(i in 1:spict_dat$nIndices){
                if(!is.null(spict_dat$logsdiPriors[[i]]$prior) &&
                   spict_dat$logsdiPriors[[i]]$prior == TRUE) {
                    tmp <- ifelse(spict_dat$logsdiPriors[[i]]$prior,1,0)
                    mu <- spict_dat$logsdiPriors[[i]]$mu
                    mu <- ifelse(spict_dat$logsdiPriors[[i]]$log,log(mu),mu)
                    inp$priors$logsdi[[i]] <- c(mu,spict_dat$logsdiPriors[[i]]$sd,tmp)
                }
            }

        }

        ## catch unit
        inp$catchunit <- input$catchUnit

        ## robflags
        inp$robflagc <- ifelse(input$robflagc == TRUE, 1, 0)
        inp$robflagi <- rep(0, spict_dat$nIndices)
        for(i in 1:spict_dat$nIndices){
            if(!is.null(spict_dat$robflags[[i]])) {
                inp$robflagi[i] <- spict_dat$robflags[[i]]
            }
        }

        ## check validity of priors
        active.priors <- names(which(sapply(inp$priors, function(x) if(is.list(x)) any(sapply(x, function(x) x[3] == 1)) else x[3] == 1)))

        print(active.priors)

        for(i in 1:length(active.priors)) {
            prior <- inp$priors[[active.priors[i]]]
            if(is.list(prior)) {
                for(j in 1:length(active.priors[[i]])) {
                    if (any(!is.numeric(prior[[j]])) || any(is.na(prior[[j]]))) {
                        warning(paste0("Prior ",active.priors[[i]],j,
                                       " is specified as (",
                                       paste(prior[[j]], collapse = ","), "). That is not possible! Deactivating this prior for now!"))
                        inp$priors[[active.priors[i]]][[j]][3] <- 0
                    }
                }
            } else {
                if (any(!is.numeric(prior)) || any(is.na(prior))) {
                    warning(paste0("Prior ",active.priors[[i]],
                                   " is specified as (",
                                   paste(prior, collapse = ","), "). That is not possible! Deactivating this prior for now!"))
                    inp$priors[[active.priors[i]]][3] <- 0
                }
            }
        }

        inp <- check.inp(inp)

        return(list(inp = inp))
    }

    resetSpictInputValues <- function() {

        ## resetting reactive values
        spict_dat$dataExplo <- NULL
        spict_dat$results <- NULL
        spict_dat$colNames <- NULL
        spict_dat$colNamesORI <- NULL
        spict_dat$colNamesIndex <- NULL
        spict_dat$nIndices <- 1
        spict_dat$nPriors <- 3
        spict_dat$timeShifts <- NULL
        spict_dat$robflags <- NULL
        spict_dat$logqPriors <- NULL
        spict_dat$logsdiPriors <- NULL
        inputSpictData$data <- NULL
        fileSpictState$upload <- NULL
        ## spictUploadVreResult ?
        ## spictAcknowledged(FALSE)

        ## resetting UIs
        shinyjs::reset("file")
        shinyjs::reset("spictCSVsep")
        shinyjs::reset("spictCSVdec")
        shinyjs::reset("guess_cols")
        shinyjs::reset("n_indices")
        ## shinyjs::reset("dateFormat")
        shinyjs::reset("timeC_lab")
        shinyjs::reset("obsC_lab")
        shinyjs::reset("stdevC_lab")
        shinyjs::reset("timeI_selectors")
        shinyjs::reset("obsI_selectors")
        shinyjs::reset("stdevI_selectors")
        shinyjs::reset("timerange")
        shinyjs::reset("dteuler")
        shinyjs::reset("timeIshift_ui")
        shinyjs::reset("robflagi_ui")
        shinyjs::reset("logq_priors_ui")
        shinyjs::reset("logsdi_priors_ui")
        shinyjs::reset("catchUnit")
        shinyjs::reset("robflagc")
        shinyjs::reset("fig_format")
        shinyjs::reset("tab_format")

        shinyjs::reset("lognPrior")
        shinyjs::reset("lognMu")
        shinyjs::reset("lognSd")
        shinyjs::reset("lognLog")

        shinyjs::reset("logAlphaPrior")
        shinyjs::reset("logAlphaMu")
        shinyjs::reset("logAlphaSd")
        shinyjs::reset("logAlphaLog")

        shinyjs::reset("logBetaPrior")
        shinyjs::reset("logBetaMu")
        shinyjs::reset("logBetaSd")
        shinyjs::reset("logBetaLog")

        shinyjs::reset("logbkfracPrior")
        shinyjs::reset("logbkfracMu")
        shinyjs::reset("logbkfracSd")
        shinyjs::reset("logbkfracLog")

        shinyjs::reset("logKPrior")
        shinyjs::reset("logKMu")
        shinyjs::reset("logKSd")
        shinyjs::reset("logKLog")

        shinyjs::reset("logrPrior")
        shinyjs::reset("logrMu")
        shinyjs::reset("logrSd")
        shinyjs::reset("logrLog")

        shinyjs::reset("logmPrior")
        shinyjs::reset("logmMu")
        shinyjs::reset("logmSd")
        shinyjs::reset("logmLog")

        shinyjs::reset("logsdbPrior")
        shinyjs::reset("logsdbMu")
        shinyjs::reset("logsdbSd")
        shinyjs::reset("logsdbLog")

        shinyjs::reset("logsdfPrior")
        shinyjs::reset("logsdfMu")
        shinyjs::reset("logsdfSd")
        shinyjs::reset("logsdfLog")

        shinyjs::reset("logsdcPrior")
        shinyjs::reset("logsdcMu")
        shinyjs::reset("logsdcSd")
        shinyjs::reset("logsdcLog")

        ## disable buttons
        shinyjs::disable("go_spict")
        shinyjs::disable("check_spict")
        shinyjs::disable("createSpictReport")
        shinyjs::disable("createSpictzip")
    }

    run_spict_server <- function(check = FALSE) {

        show_single_modal <- function(...) {
            removeModal()
            showModal(modalDialog(...))
        }

        withCallingHandlers(
            res <- tryCatch({

                inp <- spict_dat$dataExplo$inp

                flog.info("Starting spict computation / checking")
                if (check) inp$dteuler <- 1/2
                captured_warnings <- character(0)
                res <- withCallingHandlers(
                    run_spict(inp),
                    warning = function(w) {
                        captured_warnings <<- c(captured_warnings, conditionMessage(w))
                        invokeRestart("muffleWarning")
                    }
                )

                if (isFALSE(res$checks$convergence)) {
                    wtxt <- ""
                    if (length(captured_warnings)) {
                        uw <- unique(captured_warnings)
                        if (length(uw) > 6) uw <- c(uw[1:5], "... (more warnings truncated)")
                        wtxt <- paste0(
                            "<br><br>Warnings captured during fitting:<br>• ",
                            paste(uw, collapse = "<br>• ")
                        )
                    }
                    show_single_modal(
                        title = "Warning",
                        HTML(paste0(
                            "The model did not converge and one-step-ahead residuals could not be estimated! ",
                            "Please interpret the results with caution and try to tune the model. ",
                            "Guidelines for model tuning can be found in the good practice paper by Kokkalis et al. (2024).",
                            wtxt
                        )),
                        easyClose = TRUE,
                        footer = modalButton("Close")
                    )
                } else if (isFALSE(res$checks$sd_not_na)) {
                    show_single_modal(
                        title = "Warning",
                        "Some of the variance parameters could not be estimated (NaN). This usually indicates problems with model convergence. Please interpret the results with caution and try to tune the model so that all parameters and their uncertainties can be estimated. Guidelines for model tuning can be found in the good practice paper by Kokkalis et al. (2024).",
                        easyClose = TRUE,
                        footer = modalButton("Close")
                    )
                } else if (isFALSE(res$checks$cv_m)) {
                    show_single_modal(
                        title = "Warning",
                        "The coefficient of variation of the parameter m is larger than 10. This large uncertainty for this key parameter might indicate a lack of contrast in the data. Please interpret the results with caution. Guidelines for model tuning can be found in the good practice paper by Kokkalis et al. (2024).",
                        easyClose = TRUE,
                        footer = modalButton("Close")
                    )
                } else if (isFALSE(res$checks$cv_k)) {
                    show_single_modal(
                        title = "Warning",
                        "The coefficient of variation of the parameter K is larger than 10. This large uncertainty for this key parameter might indicate a lack of contrast in the data. Please interpret the results with caution. Guidelines for model tuning can be found in the good practice paper by Kokkalis et al. (2024).",
                        easyClose = TRUE,
                        footer = modalButton("Close")
                    )
                } else if (length(captured_warnings)) {
                    shiny::showNotification("Model fit completed with warnings; check logs.", type = "warning", duration = 10)
                }

                js$hideComputing(); js$enableAllButtons()

                if(check) return(TRUE)

                spict_dat$results <- res$res

                if (!is.null(session$userData$sessionMode()) &&
                    session$userData$sessionMode() == "GCUBE") {
                    flog.info("Uploading spict report to i-Marine workspace")
                    reportFileName <- paste(tempdir(),"/","spict_report_",
                                            format(Sys.time(), "%Y%m%d_%H%M_%s"),".pdf",
                                            sep="")
                    createSpictPDFReport(reportFileName, spict_dat, input, output)
                    spictUploadVreResult$res <- FALSE

                    basePath <- paste0("/Home/",session$userData$sessionUsername(),"/Workspace/")
                    SH_MANAGER <- session$userData$storagehubManager()

                    tryCatch({
                        uploadToIMarineFolder(SH_MANAGER, reportFileName, basePath, uploadFolderName)
                        spictUploadVreResult$res <- TRUE
                    }, error = function(err) {
                        flog.error("Error uploading the spict report to the i-Marine Workspace: %s", err)
                        spictUploadVreResult$res <- FALSE
                    })
                }

                return(TRUE)

            }, error = function(cond) {
                flog.error("Error in SPiCT: %s ", cond)
                removeModal()
                showModal(modalDialog(
                    title = "Error",
                    cond$message,
                    easyClose = TRUE,
                    footer = modalButton("Close")
                ))
                FALSE
            }, finally = {
                js$hideComputing()
                js$enableAllButtons()
                if (!is.null(spict_dat$results)) {
                    shinyjs::enable("createSpictReport")
                    shinyjs::enable("createSpictzip")
                }
            }),
            warning = function(w) {
                invokeRestart("muffleWarning")
            }
        )
    }


    check_spict_server <- function() {

        is_bad <- function(x) {
            is.null(x) || (is.character(x) && (identical(x, "") || identical(toupper(x), "NA"))) || (length(x) == 1 && is.na(x))
        }
        fmt_val <- function(x) {
            if (is.null(x)) return("NULL")
            if (length(x) == 0) return("length 0")
            if (length(x) == 1 && is.na(x)) return("NA")
            if (is.character(x) && identical(toupper(x), "NA")) return("\"NA\"")
            paste0(capture.output(str(x, give.head = FALSE, vec.len = 1)), collapse = " ")
        }

        withCallingHandlers({
            problems <- character(0)

            colNames <- list(
                timeC = input$timeC_lab,
                obsC  = input$obsC_lab,
                stdevfacC = input$stdevC_lab,
                timeI = spict_dat$colNamesIndex$time,
                obsI  = spict_dat$colNamesIndex$obs,
                stdevfacI  = spict_dat$colNamesIndex$stdev
            )
            inp <- spict_dat$dataExplo$inp     # may be NULL

            flog.info("Checking for spict")

            ## Input mapping ----------------------------
            if (is.null(colNames[["timeC"]]) || is_bad(colNames[["timeC"]])) {
                problems <- c(problems, "Times (catch) not assigned.")
            }
            if (is.null(colNames[["obsC"]]) || is_bad(colNames[["obsC"]])) {
                problems <- c(problems, "Observations (catch) not assigned.")
            }
            x <- colNames[["timeI"]]
            if (is.null(x) || length(x) == 0) {
                problems <- c(problems, "Times (index) not assigned.")
            } else {
                bad_idx <- which(vapply(x, is_bad, logical(1)))
                if (length(bad_idx)) {
                    problems <- c(problems, sprintf("Times (index%s) not assigned.", paste(bad_idx, collapse = ", ")))
                }
            }
            x <- colNames[["obsI"]]
            if (is.null(x) || length(x) == 0) {
                problems <- c(problems, "Observations (index) not assigned.")
            } else {
                bad_idx <- which(vapply(x, is_bad, logical(1)))
                if (length(bad_idx)) {
                    problems <- c(problems, sprintf("Observations (index%s) not assigned.", paste(bad_idx, collapse = ", ")))
                }
            }
            if (!is.null(colNames$timeI) && !is.null(colNames$obsI)) {
                if (length(colNames$timeI) != length(colNames$obsI)) {
                    problems <- c(problems, sprintf(
                                                "Length mismatch: Times (index) has %d elements, Observations (index) has %d.",
                                                length(colNames$timeI), length(colNames$obsI)
                                            ))
                }
            }

            if (length(problems)) {
                flog.error("SPiCT check found issues:\n%s", paste("-", problems, collapse = "\n"))
                showModal(modalDialog(
                    title = "Input mapping issues",
                    shiny::tags$div(
                                    "Please fix the following:",
                                    shiny::tags$ul(lapply(problems, shiny::tags$li))
                                ),
                    easyClose = TRUE,
                    footer = NULL
                ))
                js$hideComputing(); js$enableAllButtons()
                return(FALSE)
            }

            js$hideComputing(); js$enableAllButtons()
            return(TRUE)
        },
        warning = function(w) {
            showModal(modalDialog(
                title = "Warning",
                conditionMessage(w),
                easyClose = TRUE,
                footer = NULL
            ))
            invokeRestart("muffleWarning")
        })
    }

    indexColumns <- reactive({
        req(input$n_indices)
        n <- input$n_indices

        time_cols <- sapply(1:n, function(i) input[[paste0("timeI", i)]])
        obs_cols  <- sapply(1:n, function(i) input[[paste0("obsI", i)]])
        stdev_cols  <- sapply(1:n, function(i) input[[paste0("stdevI", i)]])

        list(time = time_cols, obs = obs_cols, stdev = stdev_cols)
    })

    robflags <- reactive({
        req(input$n_indices)
        n <- input$n_indices

        flags <- lapply(1:n, function(i) {
            list(
                flag = input[[paste0("robflagi", i)]]
            )
        })

        names(flags) <- paste0("robflagi", 1:n)
        flags
    })

    timeShifts <- reactive({
        req(input$n_indices)
        n <- input$n_indices

        res <- lapply(1:n, function(i) {
            list(
                shift = input[[paste0("timeIshift", i)]]
            )
        })

        names(res) <- paste0("timeIshift", 1:n)
        res
    })


    logqPriors <- reactive({
        req(input$n_indices)
        n <- input$n_indices

        priors <- lapply(1:n, function(i) {
            list(
                prior = input[[paste0("logqPrior", i)]],
                mu    = input[[paste0("logqMu", i)]],
                sd    = input[[paste0("logqSd", i)]],
                log   = input[[paste0("logqLog", i)]]
            )
        })

        names(priors) <- paste0("q", 1:n)
        priors
    })

    logsdiPriors <- reactive({
        req(input$n_indices)
        n <- input$n_indices

        priors <- lapply(1:n, function(i) {
            list(
                prior = input[[paste0("logsdiPrior", i)]],
                mu    = input[[paste0("logsdiMu", i)]],
                sd    = input[[paste0("logsdiSd", i)]],
                log   = input[[paste0("logsdiLog", i)]]
            )
        })

        names(priors) <- paste0("sdi", 1:n)
        priors
    })




    ## Input-dependent UIs
    ## ----------------------------
    ## Catches
    output$timeC_lab <- renderUI({
        ind <- which("timeC" == spict_dat$colNamesORI)
        if(input$guess_cols) {
            if (length(ind) == 1) {
                selected <- spict_dat$colNamesORI[ind]
            }else {
                selected <- ""
                if (!is.null(input$file) && !is.null(fileSpictState$upload)) {
                    showNotification(
                        "Times of catches ('timeC') not found! Please assign manually!",
                        type = "error",
                        duration = 20,
                        closeButton = TRUE
                    )
                }
            }
        } else {
            selected <- NULL
        }
        selectInput(ns("timeC_lab"),
                    tagList("Times (catch)",
                            actionButton(ns("info_timeC_lab"),
                                         tags$i(class = "fas fa-info",
                                                style="font-size: 8px"),
                                         class="infoBubbleButton")),
                    choices = c("Choose one" = "",
                                spict_dat$colNamesORI),
                    selected = selected)
    })
    output$obsC_lab <- renderUI({
        ind <- which("obsC" == spict_dat$colNamesORI)
        if(input$guess_cols) {
            if (length(ind) == 1) {
                selected <- spict_dat$colNamesORI[ind]
            }else {
                selected <- ""
                if (!is.null(input$file) && !is.null(fileSpictState$upload)) {
                    showNotification(
                        "Catches ('obsC') not found! Please assign manually!",
                        type = "error",
                        duration = 20,
                        closeButton = TRUE
                    )
                }
            }
        } else {
            selected <- NULL
        }
        selectInput(ns("obsC_lab"),
                    tagList("Observations (catch)",
                            actionButton(ns("info_obsC_lab"),
                                         tags$i(class = "fas fa-info",
                                                style="font-size: 8px"),
                                         class="infoBubbleButton")),
                    choices = c("Choose one" = "",
                                spict_dat$colNamesORI),
                    selected = selected)
    })

    output$stdevC_lab <- renderUI({
        ind <- which("stdevfacC" == spict_dat$colNamesORI)
        if(input$guess_cols) {
            if (length(ind) == 1) {
                selected <- spict_dat$colNamesORI[ind]
            }else {
                selected <- ""
            }
        } else {
            selected <- NULL
        }
        choices <- c("None" = "",
                     spict_dat$colNamesORI,
                     "None" = NA)
        selectInput(ns("stdevC_lab"),
                    tagList("Rel uncertainty (catch; optional)",
                            actionButton(ns("info_stdevC_lab"),
                                         tags$i(class = "fas fa-info",
                                                style="font-size: 8px"),
                                         class="infoBubbleButton")),
                    choices = choices,
                    selected = selected)
    })

    ## Indices
    output$timeI_selectors <- renderUI({
        req(input$n_indices)
        n <- input$n_indices

        lapply(1:n, function(i) {
            selected <- isolate(input[[paste0("timeI", i)]])
            if (input$guess_cols) {
                ind <- c(which(paste0("timeI", i) == spict_dat$colNamesORI),
                         which("timeI" == spict_dat$colNamesORI))[1]
                if (length(ind) == 1 && !is.na(ind)) {
                    selected <- spict_dat$colNamesORI[ind]
                } else {
                    selected <- ""
                    if (!is.null(input$file) && !is.null(fileSpictState$upload)) {
                        if(n > 1) {
                            showNotification(
                                paste0("Times of index ",i," ('timeI",i,"') not found! Please assign manually!"),
                                type = "error",
                                duration = 20,
                                closeButton = TRUE
                            )
                        } else {
                            showNotification(
                                paste0("Times of index ('timeI') not found! Please assign manually!"),
                                type = "error",
                                duration = 20,
                                closeButton = TRUE
                            )
                        }
                    }
                }
            } else {
                selected <- NULL
            }

            labi <- if (i == 1) {
                        tagList(
                            paste0("Times (index", if (n > 1) paste0(" ", i) else "", ")"),
                            actionButton(ns("info_timeI_lab"),
                                         tags$i(class = "fas fa-info",
                                                style="font-size: 8px"),
                                         class="infoBubbleButton")
                        )
                    } else {
                        paste0("Times (index", if (n > 1) paste0(" ", i) else "", ")")
                    }

            selectInput(
                ns(paste0("timeI", i)),
                label = labi,
                choices = c("Choose one" = "", spict_dat$colNamesORI),
                selected = selected
            )
        })
    })


    output$obsI_selectors <- renderUI({
        req(input$n_indices)
        n <- input$n_indices

        lapply(1:n, function(i) {
            selected <- isolate(input[[paste0("obsI", i)]])
            if (input$guess_cols) {
                ind <- c(which(paste0("obsI", i) == spict_dat$colNamesORI),
                         which("obsI" == spict_dat$colNamesORI))[1]
                if (length(ind) == 1 && !is.na(ind)) {
                    selected <- spict_dat$colNamesORI[ind]
                } else {
                    selected <- ""
                    if (!is.null(input$file) && !is.null(fileSpictState$upload)) {
                        if(n > 1) {
                            showNotification(
                                paste0("Index ",i," ('obsI",i,"') not found! Please assign manually!"),
                                type = "error",
                                duration = 20,
                                closeButton = TRUE
                            )
                        } else {
                            showNotification(
                                paste0("Index ('obsI') not found! Please assign manually!"),
                                type = "error",
                                duration = 20,
                                closeButton = TRUE
                            )
                        }
                    }
                }
            } else {
                selected <- NULL
            }

            labi <- if (i == 1) {
                        tagList(
                            paste0("Observations (index", if (n > 1) paste0(" ", i) else "", ")"),
                            actionButton(ns("info_obsI_lab"),
                                         tags$i(class = "fas fa-info",
                                                style="font-size: 8px"),
                                         class="infoBubbleButton")
                        )
                    } else {
                        paste0("Observations (index", if (n > 1) paste0(" ", i) else "", ")")
                    }


            selectInput(ns(paste0("obsI", i)),
                        label = labi,
                        choices = c("Choose one" = "", spict_dat$colNamesORI),
                        selected = selected)
        })
    })

    output$stdevI_selectors <- renderUI({
        req(input$n_indices)
        n <- input$n_indices

        lapply(1:n, function(i) {
            selected <- isolate(input[[paste0("stdevI", i)]])
            if (input$guess_cols) {
                ind <- c(which(paste0("stdevfacI", i) == spict_dat$colNamesORI),
                         which("stdevfacI" == spict_dat$colNamesORI))[1]
                if (length(ind) == 1) {
                    selected <- spict_dat$colNamesORI[ind]
                } else {
                    selected <- ""
                }
            } else {
                selected <- NULL
            }

            labi <- if (i == 1) {
                        tagList(
                            paste0("Rel. uncertainty (index", if (n > 1) paste0(" ", i) else "", "; optional)"),
                            actionButton(ns("info_stdevI_lab"),
                                         tags$i(class = "fas fa-info",
                                                style="font-size: 8px"),
                                         class="infoBubbleButton")
                        )
                    } else {
                        paste0("Rel. uncertainty (index", if (n > 1) paste0(" ", i) else "", "; optional)")
                    }


            choices <- c("None" = "", spict_dat$colNamesORI,
                         "None" = NA)
            selectInput(ns(paste0("stdevI", i)),
                        label = labi,
                        choices = choices,
                        selected = selected)
        })
    })


    output$timeIshift_ui <- renderUI({
        req(spict_dat$nIndices)
        n <- spict_dat$nIndices

        lapply(1:n, function(i) {
            value <- isolate(input[[paste0("timeIshift", i)]])
            if (is.null(value)) value <- 0.0

            labi <- if (i == 1) {
                        tagList(
                            paste0("Time of year (index", if (n > 1) paste0(" ", i) else "", ")"),
                            actionButton(ns("info_timeI_shift"),
                                         tags$i(class = "fas fa-info",
                                                style="font-size: 8px"),
                                         class="infoBubbleButton")
                        )
                    } else {
                        paste0("Time of year (index", if (n > 1) paste0(" ", i) else "", ")")
                    }


            numericInput(
                ns(paste0("timeIshift", i)),
                label = labi,
                value = value,
                min = 0, max = 0.9999, step = 0.01
            )
        })
    })


    output$robflagi_ui <- renderUI({
        req(spict_dat$nIndices)
        n <- spict_dat$nIndices

        lapply(1:n, function(i) {
            value <- isolate(input[[paste0("robflagi", i)]])
            if (is.null(value)) value <- FALSE

            labi <- if (i == 1) {
                        tagList(
                            paste0("Robust flag (index", if (n > 1) paste0(" ", i) else "", ")"),
                            actionButton(ns("info_robflagi"),
                                         tags$i(class = "fas fa-info",
                                                style="font-size: 8px"),
                                         class="infoBubbleButton")
                        )
                    } else {
                        paste0("Robust flag (index", if (n > 1) paste0(" ", i) else "", ")")
                    }


            checkboxInput(ns(paste0("robflagi", i)),
                          label = labi,
                          value = value)
        })
    })


    output$logq_priors_ui <- renderUI({
        req(spict_dat$dataExplo$inp$timeI)
        n <- length(spict_dat$dataExplo$inp$timeI)

        lapply(1:n, function(i) {
            suffix <- as.character(i)
            val <- isolate(input[[paste0("logqPrior", suffix)]])
            checkbox <- if (is.null(val)) FALSE else val
            val <- isolate(input[[paste0("logqMu", suffix)]])
            mu <- if (is.null(val)) NA else val
            val <- isolate(input[[paste0("logqSd", suffix)]])
            sd <- if (is.null(val)) NA else val
            val <- isolate(input[[paste0("logqLog", suffix)]])
            log <- if (is.null(val)) FALSE else val

            fluidRow(
                column(2, paste0("log(q", if (n > 1) suffix else "", ")")),
                column(2, checkboxInput(ns(paste0("logqPrior", suffix)), label = NULL, value = checkbox)),
                column(3, numericInput(ns(paste0("logqMu", suffix)), label = NULL, value = mu)),
                column(3, numericInput(ns(paste0("logqSd", suffix)), label = NULL, value = sd)),
                column(2, checkboxInput(ns(paste0("logqLog", suffix)), label = NULL, value = log))
            )
        })
    })


    output$logsdi_priors_ui <- renderUI({
        req(spict_dat$dataExplo$inp$timeI)
        n <- length(spict_dat$dataExplo$inp$timeI)

        lapply(1:n, function(i) {
            suffix <- as.character(i)
            val <- isolate(input[[paste0("logsdiPrior", suffix)]])
            checkbox <- if (is.null(val)) FALSE else val
            val <- isolate(input[[paste0("logsdiMu", suffix)]])
            mu <- if (is.null(val)) NA else val
            val <- isolate(input[[paste0("logsdiSd", suffix)]])
            sd <- if (is.null(val)) NA else val
            val <- isolate(input[[paste0("logsdiLog", suffix)]])
            log <- if (is.null(val)) FALSE else val

            fluidRow(
                column(2, paste0("log(sdi", if (n > 1) suffix else "", ")")),
                column(2, checkboxInput(ns(paste0("logsdiPrior", suffix)), label = NULL, value = checkbox)),
                column(3, numericInput(ns(paste0("logsdiMu", suffix)), label = NULL, value = mu)),
                column(3, numericInput(ns(paste0("logsdiSd", suffix)), label = NULL, value = sd)),
                column(2, checkboxInput(ns(paste0("logsdiLog", suffix)), label = NULL, value = log))
            )
        })
    })



    ## Interactive UIs & Reactive values
    ## ----------------------------
    observeEvent(input$n_indices, {
        spict_dat$nIndices <- input$n_indices
    })

    observe({
        req(indexColumns())
        spict_dat$colNamesIndex <- indexColumns()
    })

    observe({
        req(timeShifts())
        spict_dat$timeShifts <- timeShifts()
    })

    observe({
        req(robflags())
        spict_dat$robflagi <- robflags()
    })

    observeEvent(
        list(input$file, input$spictCSVsep, input$spictCSVdec),
        {
        fileSpictState$upload <- 'uploaded'
        ## reset a few things
        spict_dat$dataExplo <- NULL
        spict_dat$results <- NULL
        spict_dat$colNames <- NULL
        spict_dat$colNamesORI <- NULL
        spict_dat$colNamesIndex <- NULL
        ## spictAcknowledged(FALSE)
        shinyjs::reset("timerange")
        shinyjs::disable("createSpictReport")
        shinyjs::disable("createSpictzip")
        tmp <- load.data()
        inputSpictData$inputData <- tmp$inputData
        inputSpictData$checks <- tmp$checks
        shinyjs::enable("check_spict")
        shinyjs::enable("go_spict")
    })

    observeEvent({
        inputSpictData
        inputSpictData$inputData
        inputSpictData$checks
        input$timeC_lab
        input$obsC_lab
        input$stdevC_lab
        spict_dat$colNamesIndex$time
        spict_dat$colNamesIndex$obs
        spict_dat$colNamesIndex$stdev
    }, {
        req(inputSpictData$inputData,
            input$timeC_lab,
            input$obsC_lab,
            spict_dat$colNamesIndex$time,
            spict_dat$colNamesIndex$obs)

        print(spict_dat$colNamesIndex)

        withCallingHandlers(
            tryCatch({
                colNames <- list(
                    timeC = input$timeC_lab,
                    obsC  = input$obsC_lab,
                    stdevfacC = input$stdevC_lab,
                    timeI = spict_dat$colNamesIndex$time,
                    obsI  = spict_dat$colNamesIndex$obs,
                    stdevfacI  = spict_dat$colNamesIndex$stdev
                )

                tmp1 <- spictDataExplo1(inputSpictData$inputData, colNames)

                if (!is.null(tmp1)) {
                    spict_dat$dataExplo <- list(
                        dat = tmp1$dat_checked,
                        inpORI = tmp1$inpORI,
                        inp = tmp1$inp
                    )
                    spict_dat$nPriors <- spict::get.no.active.priors(spict_dat$dataExplo$inp)
                    spict_dat$nIndices <- ifelse(inherits(spict_dat$dataExplo$inp$obsI, "list"),
                                                 length(spict_dat$dataExplo$inp$obsI), 1)
                    spict_dat$checks <- inputSpictData$checks
                }

                ## update time slider
                tr <- spict_dat$dataExplo$inpORI$timerange
                tmin <- floor(tr[1])
                tmax <- ceiling(tr[2])

                current <- tr
                ## preserve current selection if possible
                ## current <- input$timerange
                ## if (is.null(current)) {
                ##     current <- tr
                ## } else {
                ##     current <- pmax(current, tmin)
                ##     current <- pmin(current, tmax)
                ## }

                updateSliderInput(session,
                                  inputId = "timerange",
                                  min = tmin,
                                  max = tmax,
                                  value = current)

                shinyjs::enable("check_spict")
                shinyjs::enable("go_spict")

            },
            error = function(e) {
                shinyjs::disable("go_spict")
                shinyjs::disable("check_spict")
                shinyjs::disable("createSpictReport")
                shinyjs::disable("createSpictzip")
                showModal(modalDialog(
                    title = "Error",
                    e$message,
                    easyClose = TRUE,
                    footer = NULL
                ))
            }),
            warning = function(w) {
                showModal(modalDialog(
                    title = "Warning",
                    w$message,
                    easyClose = TRUE,
                    footer = NULL
                ))
                ## showNotification(
                ##     w$message,
                ##     type = "error",
                ##     duration = 20,
                ##     closeButton = TRUE
                ## )
                invokeRestart("muffleWarning")
            }
        )
    })


    observe({
        req(logqPriors())
        spict_dat$logqPriors <- logqPriors()
    })

    observe({
        req(logsdiPriors())
        spict_dat$logsdiPriors <- logsdiPriors()
    })


    observeEvent({
        spict_dat$timeShifts
        input$timerange
        input$catchUnit

        input$lognPrior; input$lognMu; input$lognSd; input$lognLog
        input$logAlphaPrior; input$logAlphaMu; input$logAlphaSd; input$logAlphaLog
        input$logBetaPrior; input$logBetaMu; input$logBetaSd; input$logBetaLog
        input$logbkfracPrior; input$logbkfracMu; input$logbkfracSd; input$logbkfracLog
        input$logKPrior; input$logKMu; input$logKSd; input$logKLog
        input$logrPrior; input$logrMu; input$logrSd; input$logrLog
        input$logmPrior; input$logmMu; input$logmSd; input$logmLog

        spict_dat$logqPriors

        input$logsdbPrior; input$logsdbMu; input$logsdbSd; input$logsdbLog
        input$logsdfPrior; input$logsdfMu; input$logsdfSd; input$logsdfLog
        input$logsdcPrior; input$logsdcMu; input$logsdcSd; input$logsdcLog

        spict_dat$logsdiPriors
    }, {
        ## Defensive check: ensure input and data lengths match
        req(length(spict_dat$dataExplo$inpORI$timeI) == input$n_indices)
        req(length(spict_dat$timeShifts) >= input$n_indices)
        req(spict_dat$dataExplo$inp, input$timerange)

        withCallingHandlers(
            tryCatch({
                tmp2 <- spictDataExplo2(spict_dat, input)
                spict_dat$dataExplo$inp <- tmp2$inp
                spict_dat$nPriors <- spict::get.no.active.priors(spict_dat$dataExplo$inp)
                spict_dat$nIndices <- ifelse(inherits(spict_dat$dataExplo$inp$obsI, "list"),
                                             length(spict_dat$dataExplo$inp$obsI), 1)
                shinyjs::enable("go_spict")
                shinyjs::enable("check_spict")
            }, error = function(e) {
                shinyjs::disable("go_spict")
                shinyjs::disable("createSpictReport")
                shinyjs::disable("createSpictzip")
                showModal(modalDialog(title = "Error", e$message, easyClose = TRUE))
            }),
            warning = function(w) {
                showModal(modalDialog(
                    title = "Warning",
                    w$message,
                    easyClose = TRUE,
                    footer = NULL
                ))
                invokeRestart("muffleWarning")
            }
        )
    })



    ## Action buttons
    ## ----------------------------

    observeEvent(input$check_spict, {
        req(inputSpictData$inputData)

        js$showComputing()
        js$disableAllButtons()

        ok <- check_spict_server()
        if (!ok) return(invisible(NULL))

        ok <- run_spict_server(check = TRUE)
        if (!ok) return(invisible(NULL))

        showNotification(
            "No errors during check run, ready to run assessment!",
            type = "message",
            duration = 60,
            closeButton = TRUE
        )
    })

    observeEvent(input$go_spict, {
        req(inputSpictData$inputData)

        ok <- check_spict_server()
        if (!ok) return(invisible(NULL))

        if(!spictAcknowledged()) {

            spictPendingRun(TRUE)

            showModal(modalDialog(
                title = "Acknowledge model assumptions",
                bsCollapse(id = ns("assumptions"), open = NULL,
                           bsCollapsePanel("▶ Click to show/hide",
                                           HTML(spictAssumptionsHTML()))
                           ),
                tags$p(HTML("See the <a href='https://elearning.fao.org/course/view.php?id=502' target='_blank'>FAO eLearning module</a> for more information.")),
                footer = tagList(
                    modalButton("Cancel"),
                    actionButton(ns("spict_ack"), "I Acknowledge", class = "btn-success")
                ),
                easyClose = FALSE
            ))

            return(invisible(NULL))
        }

        ok <- run_spict_server()
        if (!ok) return(invisible(NULL))
    })

    observeEvent(input$spict_ack, {
        req(spict_dat$dataExplo$inp)
        removeModal()
        spictAcknowledged(TRUE)

        js$showComputing()
        js$disableAllButtons()

        if (spictPendingRun()) {
            spictPendingRun(FALSE)
            ok <- run_spict_server()
            if (!ok) return(invisible(NULL))
        }
    })

    observeEvent(input$reset_spict, {
        fileSpictState$upload <- NULL
        resetSpictInputValues()
    })


    ## Information windows -----------------------
    observeEvent(input$workflowConsiderations, {
        showModal(modalDialog(
            title = "Workflow Considerations - SPiCT",
            HTML(getWorkflowConsiderationTextForSpict()),
            easyClose = TRUE,
            size = "l"
        ))
    }, ignoreInit = TRUE)

    observeEvent(input$dataConsiderations, {
        showModal(modalDialog(
            title = "Data Loading and Formatting Considerations - SPiCT",
            HTML(getDataConsiderationTextForSpict()),
            easyClose = TRUE,
            size = "l"
        ))
    }, ignoreInit = TRUE)

    observeEvent(input$dataConsiderations2, {
        showModal(modalDialog(
            title = "Data Considerations - SPiCT",
            HTML(getDataConsiderationTextForSpict()),
            easyClose = TRUE,
            size = "l"
        ))
    }, ignoreInit = TRUE)

    observeEvent(input$methodConsiderations, {
        showModal(modalDialog(
            title = "Methodological Considerations - SPiCT",
            HTML(getMethodConsiderationTextForSpict()),
            easyClose = TRUE,
            size = "l"
        ))
    }, ignoreInit = TRUE)

    observeEvent(input$methodConsiderations2, {
        showModal(modalDialog(
            title = "Methodological Considerations - SPiCT",
            HTML(getMethodConsiderationTextForSpict()),
            easyClose = TRUE,
            size = "l"
        ))
    }, ignoreInit = TRUE)

    observeEvent(input$resultConsiderations, {
        showModal(modalDialog(
            title = "Results Considerations - SPiCT",
            HTML(getResultConsiderationTextForSpict()),
            easyClose = TRUE,
            size = "l"
        ))
    }, ignoreInit = TRUE)

    observeEvent(input$resultConsiderations2, {
        showModal(modalDialog(
            title = "Results Considerations - SPiCT",
            HTML(getResultConsiderationTextForSpict()),
            easyClose = TRUE,
            size = "l"
        ))
    }, ignoreInit = TRUE)

    observeEvent(input$info_csv_sep, {
        showModal(modalDialog(
            title = "CSV file field separator",
            HTML("<p>Choose the csv file field separator. By default, the app will try to guess the field separator automatically by trying various options. This field allows you to select a specific field separator or to enter your own. In order to enter your own separator, just press backspace to delete the current value in the input field and type the separator code, e.g. '.' for a point, or '\t' for tab separated fields (the single quotation marks are not needed in the input field).</p>"),
            easyClose = TRUE,
            size = "l"
        ))
    }, ignoreInit = TRUE)

    observeEvent(input$info_csv_dec, {
        showModal(modalDialog(
            title = "CSV file decimal separator",
            HTML("<p>Choose the csv file decimal separator. By default, the app will try to guess the decimal separator automatically by trying various options. This field allows you to select a specific decimal separator or to enter your own. In order to enter your own separator, just press backspace to delete the current value in the input field and type the separator code, e.g. '.' for a point, or ' ' for a whitespace as decimal separator (the single quotation marks are not needed in the input field).</p>"),
            easyClose = TRUE,
            size = "l"
        ))
    }, ignoreInit = TRUE)

    observeEvent(input$info_col_auto, {
        showModal(modalDialog(
            title = "Automatically assign columns",
            HTML("<p>If you click this checkbox, the tool will try to assign the respective columns (or variables in the long format) to the specific input variables (time of catch, catch observations, time of index, and index observations) automatically. This only works if the uploaded data contains the expected names, e.g. timeC, obsI, etc. This works well for the example data set.</p>"),
            easyClose = TRUE,
            size = "l"
        ))
    }, ignoreInit = TRUE)

    observeEvent(input$info_n_indices, {
        showModal(modalDialog(
            title = "Number of indices",
            HTML("<p>Select the number of indices (relative abundunces) in the input data. The number of input fields for information regarding the index / indices will change with the number selected here.</p>"),
            easyClose = TRUE,
            size = "l"
        ))
    }, ignoreInit = TRUE)

    observeEvent(input$info_timeC_lab, {
        showModal(modalDialog(
            title = "Column with times of catch observations",
            HTML("<p>Choose the name of the column (or variable in the long format) in your data set from the drop-down list that corresponds to the times of the catch observations. If there is no drop-down list, make sure to upload a data set in one of the two data formats.</p>"),
            easyClose = TRUE,
            size = "l"
        ))
    }, ignoreInit = TRUE)

    observeEvent(input$info_obsC_lab, {
        showModal(modalDialog(
            title = "Column with catch observations",
            HTML("<p>Choose the column (or variable in the long format) in your data set from the drop-down list that corresponds to the catch observations. If there is no drop-down list, make sure to upload a data set in one of the two data formats.</p>"),
            easyClose = TRUE,
            size = "l"
        ))
    }, ignoreInit = TRUE)

    observeEvent(input$info_stdevC_lab, {
        showModal(modalDialog(
            title = "Optional column with relative uncertainty of catch observations",
            HTML("<p>Option to choose the column (or variable in the long format) in your data set from the drop-down list that corresponds to the relative uncertainty weighting of the catch observations. If there is no drop-down list, make sure to upload a data set in one of the two data formats.<br><br> This input field can also be left empty and equal uncertainty weighting for all catch observations is assumed.</p>"),
            easyClose = TRUE,
            size = "l"
        ))
    }, ignoreInit = TRUE)

    observeEvent(input$info_timeI_lab, {
        showModal(modalDialog(
            title = "Column with times of index observations",
            HTML("<p>Choose the column (or variable in the long format) in your data set from the drop-down list that corresponds to the times of the index observations. If there is no drop-down list, make sure to upload a data set in one of the two data formats.<br><br> If the number of indices is increased, one additional input field for each additional index will appear below.</p>"),
            easyClose = TRUE,
            size = "l"
        ))
    }, ignoreInit = TRUE)

    observeEvent(input$info_obsI_lab, {
        showModal(modalDialog(
            title = "Column with index observations",
            HTML("<p>Choose the column (or variable in the long format) in your data set from the drop-down list that corresponds to the index observations. If there is no drop-down list, make sure to upload a data set in one of the two data formats.<br><br> If the number of indices is increased, one additional input field for each additional index will appear below.</p>"),
            easyClose = TRUE,
            size = "l"
        ))
    }, ignoreInit = TRUE)

    observeEvent(input$info_stdevI_lab, {
        showModal(modalDialog(
            title = "Optional column with relative uncertainty of index observations",
            HTML("<p>Option to choose the column (or variable in the long format) in your data set that corresponds to the relative uncertainty weighting of the index observations from the drop-down list. If there is no drop-down list, make sure to upload a data set in one of the two data formats.<br><br> This input field can also be left empty and equal uncertainty weighting for observations of one index is assumed.<br><br> If the number of indices is increased, one additional input field for each additional index will appear below.</p>"),
            easyClose = TRUE,
            size = "l"
        ))
    }, ignoreInit = TRUE)

    observeEvent(input$info_timerange, {
        showModal(modalDialog(
            title = "Time range for assessment",
            HTML("<p>Select start and end time of the input data that should be considered for the assessment. Observations outside of the selected period will be omitted from further analysis.</p>"),
            easyClose = TRUE,
            size = "l"
        ))
    }, ignoreInit = TRUE)

    observeEvent(input$info_timeI_shift, {
        showModal(modalDialog(
            title = "Time of the year as fraction of index time series",
            HTML("<p>The index observations are assumed to correspond to specific points in time, such as 1. April 2020, 2021, etc. rather than intervals as for the catch observations. This field allows to define the time of year which correspond to the index as a fraction. For example, 0.25 would correspond to the 1st of April of every year.<br><br> Note, that this option only allows to adjust all observations for a given index. If the time of the year varies between observations (e.g. 2020.29, 2021.45), the column with the index time in the input data can be modified to include the time of the year information (e.g. 2020.29, 2021.45) </p>"),
            easyClose = TRUE,
            size = "l"
        ))
    }, ignoreInit = TRUE)

    observeEvent(input$info_dteuler, {
        showModal(modalDialog(
            title = "Time step of the Euler discretization",
            HTML("<p>Euler discretization time step, i.e. the number of time steps per year used to approximate the solution of the differential equations.<br><br> The default and recommended setting is at least 16 time steps per year (1/16). Finer steps (1/16–1/64) improve accuracy but can substantially increase computation time and memory use. For exploratory assessments, coarser steps such as 1/4 or 1/8 may be sufficient.</p>"),
easyClose = TRUE,
size = "l"
))
    }, ignoreInit = TRUE)

    observeEvent(input$info_catchunit, {
        showModal(modalDialog(
            title = "Catch unit",
            HTML("<p>Set the unit of the catches (to be displayed in the graphs), e.g. '000 t, t, kg. </p>"),
            easyClose = TRUE,
            size = "l"
        ))
    }, ignoreInit = TRUE)

    observeEvent(input$info_robflagi, {
        showModal(modalDialog(
            title = "Robust flag for Index",
            HTML("<p>Option to use the robust estimation for the index.<br><br> The presence of extreme observations may inflate estimates of observation noise and increase the general uncertainty of the fit. To reduce this effect it is possible to apply a robust estimation scheme, which is less sensitive to extreme observations.<br><br> Robust estimation is implemented using a mixture of light-tailed and a heavy-tailed Gaussian distribution as described in Pedersen and Berg (2017). This entails two additional parameters (pp and robfac) that require estimation. This may not always be possible given the increased model complexity.</p>"),
            easyClose = TRUE,
            size = "l"
        ))
    }, ignoreInit = TRUE)

    observeEvent(input$info_robflagc, {
        showModal(modalDialog(
            title = "Robust flag for catches",
            HTML("<p>Option to use the robust estimation for the catches.<br><br> The presence of extreme observations may inflate estimates of observation noise and increase the general uncertainty of the fit. To reduce this effect it is possible to apply a robust estimation scheme, which is less sensitive to extreme observations.<br><br> Robust estimation is implemented using a mixture of light-tailed and a heavy-tailed Gaussian distribution as described in Pedersen and Berg (2017). This entails two additional parameters (pp and robfac) that require estimation. This may not always be possible given the increased model complexity.</p>"),
            easyClose = TRUE,
            size = "l"
        ))
    }, ignoreInit = TRUE)

    observeEvent(input$info_fig_format, {
        showModal(modalDialog(
            title = "Format of archived figures",
            HTML("<p>Choose the format of the figures in the zip archive that can be downloaded from the drop-down list.</p>"),
            easyClose = TRUE,
            size = "l"
        ))
    }, ignoreInit = TRUE)

    observeEvent(input$info_tab_format, {
        showModal(modalDialog(
            title = "Format of archived tables",
            HTML("<p>Choose the format of the tables in the zip archive that can be downloaded from the drop-down list.</p>"),
            easyClose = TRUE,
            size = "l"
        ))
    }, ignoreInit = TRUE)


    observeEvent(input$info_config_priors, {
        showModal(modalDialog(
            title = "Configure priors",
            HTML("<p>By default 3 vague priors are activated in SPiCT, for the shape of the production curve (logn) and for the nosie ratios logalpha = logsdi - logsdb and logbeta = logsdc - logsdf. The reason for these default priors is that available data quantity and quality are often insufficient to estimate the shape of the production curve or disentangle process and observation noise. These priors can be de-activated by unclicking the box in the 'Activate' column below. Their mean and standard deviation (SD) can be changed in the columns below.<br><br> In addition, a range of other priors can be defined. In order to define a prior, the respective prior has to be activated and a mean value (on normal or log scale, dependent if the checkbox in the column 'Mean on log scale' is ticked and SD has to be provided in the respective columns. Once all information is provided and the prior is activated, a plot with the prior should appear in the right panel.<br> Priors should only be used if auxiliary information is available and can affect assessment results and uncertainty.</p>"),
            easyClose = TRUE,
            size = "l"
        ))
    }, ignoreInit = TRUE)

    observeEvent(input$infoAssessment, {
        showModal(modalDialog(
            title = "Check, Assessment, Reset & Report",
            HTML("<p>The <b>'Run Check'</b> allows to check if the uploaded data and provided information corresponds the expectations before running the main assessment. While the main assessment can take a few minutes to run, depending on the input data and settings, the check is performed quicker and can identify issues in the data or settings. The check does not produce results (figures or tables), but a notification in the middle of the screen will inform you whether the check was successful. <br> <br> <b>'Run Assessment'</b> performs the main assessment and should yield plenty of figures and tables in the result section upon successful completion. The run may take several minutes and depends on the uploaded data and settings. Run time with the sample dataset and default settings is 1-2 minutes. <br> <br> <b>'Reset'</b> removes all results, the uploaded dataset, and resets all settings to default values. <br> <br> After successful completion of the main assessment, the button <b>'Download Report'</b> allows you to download a pdf document with all results. This report is also automatically uploaded to your private workspace. The button <b>'Download Results (zip)'</b> allows you download all figures and tables as a zip archive.</p>"),
            easyClose = TRUE,
            size = "l"
        ))
    }, ignoreInit = TRUE)


    ## Tour --------------------------------
    observeEvent(input$tour_general, {

        steps <- list(
            list(element = NA,
                 intro = paste0("This tour takes you through the main steps of the data-limited stock assessment with SPiCT.<br><br>",
                                "Click 'Next' to continue.")),
            list(element = paste0("#", ns("file_wrapper")),
                 intro = "As a first step, you need to upload data. You can do that by clicking on 'Browse' and select a csv file on your computer."),
            list(element = paste0("#", ns("dataConsiderations2")),
                 intro = "If you do not have your own file and want to use an example fiel or if you are interested in more information about the data type and format, click on the small the information button here. <br><br>Note these information buttons (indicated by 'i') throughout the whole app."),
            list(element = paste0("#", ns("spictCSVsep"),
                                  " + .selectize-control"),
                 intro = "By default, the app will try to recognize the field separator, but you can also specify it here by either choosing from the list or by pressing backspace and enter any separator."),
            list(element = paste0("#", ns("spictCSVdec"),
                                  " + .selectize-control"),
                 intro = "Similarly, by default, the app will try to recognize the decimal separator, but you can also specify it here by either choosing from the list or by pressing backspace and enter any separator."),
            list(element = paste0("#", ns("guess_cols")),
                 intro = "If you tick this checkbox, the SMT tool will try to assign the columns (or variables in the long data format) to the input variables automatically. This only works if the column (or variable) names are the same as in the example data files: timeC, obsC, timeI and obsI (or timeI1, timeI2, and obsI1, and obsI2, for multiple indices)."),
            list(element = paste0("#", ns("timeC_lab"),
                                  " + .selectize-control"),
                 intro = "If the autmatic assignment does not work or if manual assignment is preferred, these input fields allow to select the columns (or variables for the long data format) for the specific input time series from a drop-down list.<br><br> This field asks you for example to select the column that contains the information about the times of the catch observations."),
            list(element = paste0("#", ns("stdevC_lab"),
                                  " + .selectize-control"),
                 intro = "If information about the relative uncertainty scaling of the catch observations is available and included in the input data, the respective column (or variable) can be selected here.<br><br> If for example the uncertainty of the catches were likely more uncertain before a specific sampling program, this information could be provided in this variable.<br><br> For an example, see column 'stdevfacC' in the albacore example data set."),
            list(element = paste0("#", ns("obsI1"),
                                  " + .selectize-control"),
                 intro = "This field asks you to select the column for the index observations.<br><br> Note, that by default only one index can be selected, but another settings allows you to provide multiple indices."),
            list(element = paste0("#", ns("n_indices")),
                 intro = "The number of indices can be changed here."),
            list(element = paste0("#", ns("box_settings")),
                 intro = "After you chose your data set and it was uploaded successfully (no error messages), you can explore your data and adjust settings in this box."),
            list(element = "#settings_spict ul.nav.nav-tabs",
                 intro = "There are multiple tabs that allow you to adjust various aspects of the assessment method."),
            list(element = paste0("#", ns("tab1")),
                 intro = "For example, the first tab allows you to visually inspect the uploaded data and set the time range for the assessment or time of year for the index/indices.<br><br> Remember the small information buttons ('i') next to the labels, if you need more information about these settings.")
        )

        current_tab <- input$settings
        ## If tab 1 selected
        if (!is.null(current_tab) && current_tab == "data") {
            steps <- append(steps,
                            list(
                                list(element = paste0("#", ns("plot_explo1_ui")),
                                     intro = paste0("A time series plot will be shown here when the data was uploaded successfully."))
                            ))
        }

        steps <- append(steps,
                        list(
                            list(element = paste0("#", ns("tab2")),
                                 intro = "The second tab contains information about priors and allows you to change default priors and add additional priors."),
                            list(element = paste0("#", ns("tab3")),
                                 intro = "The last tab contains summary statistics and diagnostics of the uploaded data."),
                            list(element = paste0("#", ns("check_spict")),
                                 intro = "As running SPiCT can take one minute or longer depending on the size of the data set, you can first run a check. This would inform you if the required information is available and in the required format."),
                            list(element = paste0("#", ns("go_spict")),
                                 intro = "If the check is successful, you can run the data-limited stock assessment by clicking here.<br><br>Note, that a pop-up window will ask you if you are aware of and acknowledge the model assumptions."),
                            list(element = paste0("#", ns("reset_spict")),
                                 intro = "This button allows you to reset all settings.<br><br>Note, that this also removes your input data."),
                            list(element = paste0("#", ns("createSpictReport")),
                                 intro = "This button creates and downloads an automatic assessment report with information about your data, settings and results."),
                            list(element = paste0("#", ns("createSpictzip")),
                                 intro = "You can also download all graphs and tables in a zip archive by clicking here.")))

        if (!is.null(current_tab) && current_tab == "data") {
            steps <- append(steps,
                            list(
                                list(element = paste0("#", ns("fig_format"),
                                                      " + .selectize-control"),
                                     intro = paste0("You can select the format of the figures in the zip archive here.")),
                                list(element = paste0("#", ns("tab_format"),
                                                      " + .selectize-control"),
                                     intro = paste0("And the format of the tables here."))
                            ))
        }

        steps <- append(steps,
                        list(
                            list(element = paste0("#", ns("tour_res")),
                                 intro = "The results tour might be helpful to get an overview over the results.<br><br> Note, that the tour only makes sense after SPiCT was run successfully."),
                            list(element = NA,
                                 intro = "This concludes the SPiCT tour.<br><br>Remember the information buttons ('i') that might be helpful when uploading data, adjusting settings or interpreting results."),
                            list(element = paste0("#", ns("info_wrapper")),
                                 intro = "These buttons offer another helpful option to get detailed information on the workflow, data, methods, and results."),
                            list(element = NA,
                                 intro = "For more information, you might also consider to visit the <a href='https://elearning.fao.org/course/view.php?id=502' target='_blank'>FAO e-learning course</a>.")))


        later::later(function() {
            introjs(session, options = list(steps = steps))
        }, delay = 0.5)
    }, ignoreInit = TRUE)


    ## Results tour
    observeEvent(input$tour_res, {

        if(is.null(spict_dat$results)) {

            steps <- list(
                list(element = NA,
                     intro = "No results found. This tour only works if you run the assessment."),
                list(element = paste0("#", ns("go_spict")),
                     intro = "Make sure you uploaded your data and run the assessment by clicking here."),
                list(element = NA,
                     intro = "Start this tour again after you see some tables and graphs below."))

        } else {

            current_tab <- input$results

            if (!is.null(current_tab) && current_tab == "res1") {
                steps <- list(
                    list(element = NA,
                         intro = "This is a tour that takes you through the main results of the data-limited stock assessment with SPiCT.")
                )
                steps <- append(steps,
                                list(list(element = paste0("#", ns("plot_sum")),
                                          intro = paste0("This figure shows an overview of the main assessment results across four panels: time series of estimated ", withMathJax("\\(B/B_{\\mathrm{MSY}}\\)"), " and ", withMathJax("\\(F/F_{\\mathrm{MSY}}\\)")," with 95% confidence intervals (blue), including relative abundance observations (colour indicates in-year timing; symbols distinguish index series), estimated and observed catches with the MSY reference line and its 95% confidence interval (grey), and a Kobe plot summarizing status with uncertainty around ", withMathJax("\\(B_{\\mathrm{MSY}}\\)")," and ", withMathJax("\\(F_{\\mathrm{MSY}}\\)"),".")),
                                     list(element = paste0("#", ns("table_est")),
                                          intro = paste("This table includes the estimated parameter values with 95% confidence intervals. Key parameters include the intrinsic growth rate (r), carrying capacity (K), maximum sustainable yield (MSY, m), and the shape parameter of the production curve (n). Additional parameters are catchability (q), process error standard deviations for biomass (sdb) and fishing mortality (sdf), and observation error standard deviations for indices (sdi) and catches (sdc). Hyper-parameters α (sdb/sdi) and β (sdf/sdc) are also reported. When multiple indices are used, separate q and sdi estimates are provided.")),
                                     list(element = paste0("#", ns("table_refs_s")),
                                          intro = paste("includes the estimated stochastic reference ",
                                                        "points with 95% confidence intervals: ",
                                                        "biomass at maximum sustainable yield ",
                                                        withMathJax("\\(B_{\\mathrm{MSY}}\\)"),
                                                        "fishing mortality at maximum sustainable yield ",
                                                        withMathJax("\\(F_{\\mathrm{MSY}}\\)"),
                                                        "and the maximum sustainable yield (MSY).")),
                                     list(element = paste0("#", ns("table_states")),
                                          intro = paste("This table includes the estimated biomass (B) and ",
                                                        "fishing mortality (F) at the end of the time series, ",
                                                        "together with stock status indicators: ",
                                                        "relative biomass ",
                                                        withMathJax("\\(B/B_{\\mathrm{MSY}}\\)"),
                                                        "and relative fishing mortality ",
                                                        withMathJax("\\(F/F_{\\mathrm{MSY}}\\)"),".")),
                                     list(element = paste0("#", ns("plot_prod")),
                                          intro = paste("This figure shows the estimated production curve showing the theoretical surplus production (y axis) as a function of biomass relative to carrying capacity (B/K, x axis). The vertical dotted line indicates the the relative biomass where surplus production is maximized (MSY). The observed annual surplus production is plotted as blue circles conected by a line."))))
            } else if (!is.null(current_tab) && current_tab == "res2") {
                steps <- list(
                    list(element = NA,
                         intro = "This is a tour that takes you through the additional results of the data-limited stock assessment with SPiCT.")
                )
                steps <- append(steps,
                                list(
                                    list(element = paste0("#", ns("plot_abs")),
                                         intro = paste0("This figure estimated absolute (first y axis) and relative (second y axis) biomass (", withMathJax("\\(B\\)"), " and ", withMathJax("\\(B/B_{\\mathrm{MSY}}\\)"), ", left) and fishing mortality (", withMathJax("\\(F\\)"), " and ", withMathJax("\\(F/F_{\\mathrm{MSY}}\\)"), ", right). The blue shaded areas display the 95% confidence intervals of the relative states (",withMathJax("\\(B/B_{\\mathrm{MSY}}\\)")," and ",withMathJax("\\(F/F_{\\mathrm{MSY}}\\)"),"). The dashed lines indicate the 95% confidence intervals of the absolute states (",withMathJax("\\(B\\)"),"and",withMathJax("\\(F\\)"),"). The horizontal lines show the reference points (",withMathJax("\\(B_{\\mathrm{MSY}}\\)"), " and ", withMathJax("\\(F_{\\mathrm{MSY}}\\)"),") and the gray shaded area the associated 95% confidence interval.")),
                                    list(element = paste0("#", ns("table_refs_d")),
                                         intro = paste0("This table includes the estimated ",
                                                        " deterministic reference points with ",
                                                        " 95% confidence intervals: ",
                                                        "biomass at maximum sustainable yield ",
                                                        withMathJax("\\(B_{\\mathrm{MSY}}\\)"),
                                                        "fishing mortality at maximum sustainable yield ",
                                                        withMathJax("\\(F_{\\mathrm{MSY}}\\)"),
                                                        "and the maximum sustainable yield (MSY).")),
                                    list(element = paste0("#", ns("table_pred")),
                                         intro = paste0("This table includes the estimated biomass (B) and fishing mortality (F) states, and stock status ",
                       "in terms of relative biomass ", withMathJax("\\(B/B_{\\mathrm{MSY}}\\)"),
                       "and relative fishing mortality ", withMathJax("\\(F/F_{\\mathrm{MSY}}\\)"),".",
                       "Results are forecasted one year beyond the end of the observed time series, ",
                       "including predicted catch for the forecast year and the equilibrium biomass ",
                       "expected if fishing were to continue indefinitely. ",
                       "During the forecast, fishing mortality is assumed constant at the level estimated ",
                       "for the final year of the time series."))
                       ))
            } else if (!is.null(current_tab) && current_tab == "res3") {
                steps <- list(
                    list(element = NA,
                         intro = "This is a tour that takes you through the diagnostics of the data-limited stock assessment with SPiCT.")
                )
                steps <- append(steps,
                                list(
                                    list(element = paste0("#", ns("plot_priors2_ui")),
                                         intro = paste0("This figure shows the prior density distributions and estimated posterior distributions for the activated priors.")),
                                    list(element = paste0("#", ns("plot_resid1")),
                                         intro = paste0("This figure shows the residual diagnostics for the catch time series (first column) and index time series (remaining columns): ",
                                                        "Row 1: log-transformed observations; ",
                                                        "Row 2: one-step-ahead (OSA) residuals; ",
                                                        "Row 3: autocorrelation function (ACF) of residuals; ",
                                                        "Row 4: quantile-quantile (Q–Q) plot of residuals.")),
                                    list(element = paste0("#", ns("plot_resid2")),
                                         intro = paste0("This figure shows the residual diagnostics for the predicted biomass process (first column) and fishing mortality process (second column): ",
                                                        "Row 1: log-transformed observations; ",
                                                        "Row 2: one-step-ahead (OSA) residuals; ",
                                                        "Row 3: autocorrelation function (ACF) of residuals; ",
                                                        "Row 4: quantile-quantile (Q–Q) plot of residuals."))))
                }

            steps <- append(steps,
                            list(
                                list(element = "#results_spict ul.nav.nav-tabs",
                                     intro = "This concludes the results tour for this tab.<br><br>Note that there are multiple tabs that contain different results and the tour only takes you through the open tab. So, if you want information about plots and tables in another tab, please select that tab and restart this tour.")))


        }

                            later::later(function() {
                                introjs(session, options = list(steps = steps))
                            }, delay = 0.5)
    }, ignoreInit = TRUE)



    ## Plots
    ## --------------------------
    plot_explo_height <- reactive({
        req(spict_dat$dataExplo$inp)
        noi <- spict_dat$nIndices + 1
        dims <- preferred_mfrow(noi)
        if(noi == 1) {
            total_height <- 300
        } else if(noi == 2) {
            total_height <- 400
        } else {
            dims <- preferred_mfrow(noi)
            n_rows <- dims[1]
            row_height <- 300
            total_height <- n_rows * row_height
        }
        return(total_height)
    })

    output$plot_explo1 <- renderPlot({
        req(spict_dat$dataExplo$inp)
        inp <- spict_dat$dataExplo$inp
        if(any(!sapply(spict_dat$dataExplo$inp$obsI,is.null))) {
            noi <- ifelse(inherits(spict_dat$dataExplo$inp$obsI, "list"),
                          length(spict_dat$dataExplo$inp$obsI), 1)
        } else {
            noi <- 1
        }
        par(mfrow = preferred_mfrow(noi+1))
        plotSpict.data(inp, automfrow = FALSE, do.plot=noi)
    },
    height = function() plot_explo_height(),
    res = 100)

    output$plot_explo1_ui <- renderUI({
        req(plot_explo_height())
        plotOutput(ns("plot_explo1"), height = paste0(plot_explo_height(), "px"))
    })

    output$title_explo1 <- renderText({
        req(spict_dat$dataExplo$inp)
        captionSpict.plots(spict_dat, input, format = "withFig", type = "explo1")
    })

    output$plot_explo2 <- renderPlot({
        req(spict_dat$dataExplo$inp)
        inp <- spict_dat$dataExplo$inp
        if(any(!sapply(spict_dat$dataExplo$inp$obsI,is.null))) {
            noi <- ifelse(inherits(spict_dat$dataExplo$inp$obsI, "list"),
                          length(spict_dat$dataExplo$inp$obsI), 1)
        } else {
            noi <- 1
        }
        par(mfrow = preferred_mfrow(noi+1))
        plotSpict.data.unc(inp, automfrow = FALSE, do.plot=noi)
    },
    height = function() plot_explo_height(),
    res = 100)

    output$plot_explo2_ui <- renderUI({
        req(plot_explo_height())
        plotOutput(ns("plot_explo2"), height = paste0(plot_explo_height(), "px"))
    })

    output$title_explo2 <- renderText({
        req(spict_dat$dataExplo$inp)
        captionSpict.plots(spict_dat, input, format = "withFig", type = "explo2")
        })

    plot_priors_height <- reactive({
        req(spict_dat$dataExplo$inp, spict_dat$nPriors)
        nopriors <- spict_dat$nPriors
        if(nopriors == 1) {
            total_height <- 500
        } else if(nopriors == 2) {
            total_height <- 400
        } else {
            dims <- preferred_mfrow(nopriors)
            n_rows <- dims[1]
            row_height <- 300
            total_height <- n_rows * row_height
        }
        return(total_height)
    })

    output$plot_priors <- renderPlot({
        req(spict_dat$dataExplo$inp)
        inp <- spict_dat$dataExplo$inp
        nopriors <- spict::get.no.active.priors(inp)
        par(mfrow = preferred_mfrow(nopriors))
        plotSpict.priors(inp, automfrow = FALSE, do.plot=nopriors)
    },
        height = function() plot_priors_height(),
        res = 100)

    output$plot_priors_ui <- renderUI({
        req(plot_priors_height())
        plotOutput(ns("plot_priors"), height = paste0(plot_priors_height(), "px"))
    })

    output$title_priors <- renderText({
        req(spict_dat$dataExplo$inp)
        captionSpict.plots(spict_dat, input, format = "withFig", type = "priors")
    })

    output$plot_diag1 <- renderPlot({
        req(spict_dat$dataExplo$inp)
        plotSpict.diag1(spict_dat, input)
    },
    width = "auto",
    height = "auto",
    res = 100)
    output$title_diag1 <- renderText({
        req(spict_dat$dataExplo$inp)
        captionSpict.plots(spict_dat, input, format = "withFig", type = "diag1")
    })

    output$plot_diag2 <- renderPlot({
        req(spict_dat$dataExplo$inp)
        plotSpict.diag2(spict_dat, input)
    },
    width = "auto",
    height = "auto",
    res = 100)
    output$title_diag2 <- renderText({
        req(spict_dat$dataExplo$inp)
        captionSpict.plots(spict_dat, input, format = "withFig", type = "diag2")
    })

    output$plot_diag3 <- renderPlot({
        req(spict_dat$dataExplo$inp)
        plotSpict.diag3(spict_dat, input)
    },
    width = "auto",
    height = "auto",
    res = 100)
    output$title_diag3 <- renderText({
        req(spict_dat$dataExplo$inp)
        captionSpict.plots(spict_dat, input, format = "withFig", type = "diag3")
    })


    output$plot_sum <- renderPlot({
        req(spict_dat$results)
        plotSpict.sum(spict_dat, input)
    },
    width = "auto",
    height = "auto",
    res = 100)
    output$title_sum <- renderText({
        req(spict_dat$results)
        captionSpict.plots(spict_dat, input, format = "withFig", type = "sum")
    })

    output$plot_abs <- renderPlot({
        req(spict_dat$results)
        plotSpict.abs(spict_dat, input)
    },
    width = "auto",
    height = "auto",
    res = 100)
    output$title_abs <- renderText({
        req(spict_dat$results)
        captionSpict.plots(spict_dat, input, format = "withFig", type = "abs")
    })

    output$plot_prod <- renderPlot({
        req(spict_dat$results)
        plotSpict.prod(spict_dat, input)
    },
    width = "auto",
    height = "auto",
    res = 100)
    output$title_prod <- renderText({
        req(spict_dat$results)
        captionSpict.plots(spict_dat, input, format = "withFig", type = "prod")
    })


    output$plot_priors2 <- renderPlot({
        req(spict_dat$results)
        nopriors <- spict_dat$nPriors
        par(mfrow = preferred_mfrow(nopriors))
        plotSpict.priors(spict_dat$results, automfrow = FALSE, do.plot=nopriors)
    },
    height = function() plot_priors_height(),
    res = 100)

    output$plot_priors2_ui <- renderUI({
        req(plot_priors_height())
        plotOutput(ns("plot_priors2"), height = paste0(plot_priors_height(), "px"))
    })

    output$title_priors2 <- renderText({
        req(spict_dat$results)
        captionSpict.plots(spict_dat, input, format = "withFig", type = "priors2")
    })


    output$plot_resid1 <- renderPlot({
        req(spict_dat$results)
        plotSpict.resid1(spict_dat)
    },
    width = "auto",
    height = "auto",
    res = 100)
    output$title_resid1 <- renderText({
        req(spict_dat$results)
        captionSpict.plots(spict_dat, input, format = "withFig", type = "resid1")
    })

    output$plot_resid2 <- renderPlot({
        req(spict_dat$results)
        plotSpict.resid2(spict_dat)
    },
    width = "auto",
    height = "auto",
    res = 100)
    output$title_resid2 <- renderText({
        req(spict_dat$results)
        captionSpict.plots(spict_dat, input, format = "withFig", type = "resid2")
    })



    ## Text
    ## --------------------------
    output$text_explo1 <- renderUI({
        req(spict_dat$dataExplo$inp)
        textSpict.explo1(spict_dat, input)
    })

    output$text_diag1 <- renderUI({
        req(spict_dat$dataExplo$inp)
        textSpict.diag1(spict_dat, input)
    })

    output$text_diag_res <- renderUI({
        req(spict_dat$results)
        textSpict.diag_res(spict_dat, input)
    })

    output$text_sum <- renderUI({
        req(spict_dat$results)
        textSpict.sum(spict_dat, input)
    })



    ## Tables
    ## --------------------------
    output$table_est <- renderDataTable({
        req(spict_dat$results)
        tableSpict.estimates(spict_dat, input, format = "datatable")
    })
    output$title_table_est <- renderText({
        req(spict_dat$results)
        captionSpict.tables(spict_dat, input, format = "datatable",
                            type = "estimates")
    })

    output$table_refs_s <- renderDataTable({
        req(spict_dat$results)
        tableSpict.refs_s(spict_dat, input, format = "datatable")
    })
    output$title_table_refs_s <- renderText({
        req(spict_dat$results)
        captionSpict.tables(spict_dat, input, format = "datatable",
                            type = "refs_s")
    })

    output$table_refs_d <- renderDataTable({
        req(spict_dat$results)
        tableSpict.refs_d(spict_dat, input, format = "datatable")
    })
    output$title_table_refs_d <- renderText({
        req(spict_dat$results)
        captionSpict.tables(spict_dat, input, format = "datatable",
                            type = "refs_d")
    })

    output$table_states <- renderDataTable({
        req(spict_dat$results)
        tableSpict.states(spict_dat, input, format = "datatable")
    })
    output$title_table_states <- renderText({
        req(spict_dat$results)
        captionSpict.tables(spict_dat, input, format = "datatable",
                            type = "states")
    })

    output$table_pred <- renderDataTable({
        req(spict_dat$results)
        tableSpict.pred(spict_dat, input, format = "datatable")
    })
    output$title_table_pred <- renderText({
        req(spict_dat$results)
        captionSpict.tables(spict_dat, input, format = "datatable",
                            type = "pred")
    })



    ## Files
    ## --------------------------
    output$createSpictReport <- downloadHandler(
        filename = paste("spict_report_",
                         format(Sys.time(),
                                "%Y%m%d_%H%M_%s"),".pdf",sep=""),
        content = function(file) {
            createSpictPDFReport(file, spict_dat, input, output)
        }
    )

    output$createSpictzip <- downloadHandler(
        filename = paste("spict_results_",
                         format(Sys.time(), "%Y%m%d_%H%M_%s"),".zip",sep=""),
        content = function(file) {
            makeContentSpictzip(file, spict_dat, input, output)
        },
        contentType = "application/zip"
    )

    output$spictTitle <- renderText({
        session$userData$page("spict")
        text <- "<span><h3><b>Data-limited stock assessment with SPiCT</b></h3></span>"
        text
    })


}
