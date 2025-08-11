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
                if(!checks$csv){
                    "Something went wrong when reading in your data set. Did you select a CSV file (i.e. file with ending '.csv')? Click on the info icon for more information."
                }else if(!checks$delimiter){
                    "Something went wrong when reading in your data set. Please ensure that your CSV file delimiter is a comma ',' or semicolon ';'. Click on the info icon for more information."
                }else{
                    "There was an unexpected error when reading in your data set. Please double-check your data set and refer to the info button for more help. "
                },
                easyClose = TRUE,
                footer = NULL
            ))
            return (NULL)
        } else {
            spict_dat$colNamesORI <- colnames(dataset$inputData)
            names(spict_dat$colNamesORI) <- spict_dat$colNamesORI
            return(dataset)
        }
    })

    spictDataExplo1 <- function(inputData, colNames) {

        ## TODO check for multiple surveys
        ## TODO informative errors if multiple surveys but only 1 time and 2 obs selected or something similar!
        ## TODO check that times are increasing?!
        ## TODO option to provide time as dates?

        ## TODO account for colNames$stdevfacC can be ""
        ## TODO account for colNames$stdevfacI can be list("","")

        if(!is.null(colNames$timeC) &&
           !is.null(colNames$obsC) &&
           all(sapply(colNames$timeI, function(x) !is.null(x))) &&
           all(sapply(colNames$timeI, function(x) x != "")) &&
           all(sapply(colNames$timeI, function(x) x != "NA")) &&
           all(sapply(colNames$obsI, function(x) !is.null(x))) &&
           all(sapply(colNames$obsI, function(x) x != "")) &&
           all(sapply(colNames$obsI, function(x) x != "NA"))) {

            dat_checked <- checkDat(inputSpictData$inputData, colNames)
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
        spictAcknowledged(FALSE)

        ## resetting UIs
        shinyjs::reset("file")
        ## shinyjs::reset("dateFormat")
        shinyjs::reset("timeC_lab")
        shinyjs::reset("obsC_lab")
        shinyjs::reset("timeI_selectors")
        shinyjs::reset("obsI_selectors")
        shinyjs::reset("stdevI_selectors")
        shinyjs::reset("timerange")
        shinyjs::reset("dteuler")

        ## disable buttons
        shinyjs::disable("go_spict")
        shinyjs::disable("check_spict")
        shinyjs::disable("createSpictReport")
        shinyjs::disable("createSpictzip")
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
            selected <- if (length(ind) == 1) spict_dat$colNamesORI[ind] else ""
        } else {
            selected <- NULL
        }
        selectInput(ns("timeC_lab"),
                    tagList("Time (catch)",
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
            selected <- if (length(ind) == 1) spict_dat$colNamesORI[ind] else ""
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
            selected <- if (length(ind) == 1) spict_dat$colNamesORI[ind] else ""
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
                selected <- if (length(ind) == 1) spict_dat$colNamesORI[ind] else ""
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
                selected <- if (length(ind) == 1) spict_dat$colNamesORI[ind] else ""
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
                selected <- if (length(ind) == 1) spict_dat$colNamesORI[ind] else ""
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
            checkbox <- isolate(input[[paste0("logqPrior", suffix)]]) %||% FALSE
            mu <- isolate(input[[paste0("logqMu", suffix)]]) %||% NA
            sd <- isolate(input[[paste0("logqSd", suffix)]]) %||% NA
            log <- isolate(input[[paste0("logqLog", suffix)]]) %||% FALSE

            fluidRow(
                column(2, paste0("log(q", if (n > 1) suffix else "", ")")),
                column(2, checkboxInput(ns(paste0("logqPrior", suffix)), label = NULL, value = checkbox)),
                column(2, numericInput(ns(paste0("logqMu", suffix)), label = NULL, value = mu)),
                column(2, numericInput(ns(paste0("logqSd", suffix)), label = NULL, value = sd)),
                column(4, checkboxInput(ns(paste0("logqLog", suffix)), label = NULL, value = log))
            )
        })
    })


    output$logsdi_priors_ui <- renderUI({
        req(spict_dat$dataExplo$inp$timeI)
        n <- length(spict_dat$dataExplo$inp$timeI)

        lapply(1:n, function(i) {
            suffix <- as.character(i)
            checkbox <- isolate(input[[paste0("logsdiPrior", suffix)]]) %||% FALSE
            mu <- isolate(input[[paste0("logsdiMu", suffix)]]) %||% NA
            sd <- isolate(input[[paste0("logsdiSd", suffix)]]) %||% NA
            log <- isolate(input[[paste0("logsdiLog", suffix)]]) %||% FALSE

            fluidRow(
                column(2, paste0("log(sdi", if (n > 1) suffix else "", ")")),
                column(2, checkboxInput(ns(paste0("logsdiPrior", suffix)), label = NULL, value = checkbox)),
                column(2, numericInput(ns(paste0("logsdiMu", suffix)), label = NULL, value = mu)),
                column(2, numericInput(ns(paste0("logsdiSd", suffix)), label = NULL, value = sd)),
                column(4, checkboxInput(ns(paste0("logsdiLog", suffix)), label = NULL, value = log))
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

    observeEvent(spict_dat$dataExplo$inpORI, {
        req(spict_dat$dataExplo$inpORI)

        tr <- spict_dat$dataExplo$inpORI$timerange
        tmin <- floor(tr[1])
        tmax <- ceiling(spict_dat$dataExplo$inpORI$lastCatchObs)

        ## TODO test this with last year no catch, but index + TODO can timerange not be larger than lastCatchObs? i.e. value > max?

        ## preserve current selection if possible
        current <- input$timerange
        if (is.null(current)) {
            current <- tr
        } else {
            current <- pmax(current, tmin)
            current <- pmin(current, tmax)
        }

        updateSliderInput(session,
                          inputId = "timerange",
                          min = tmin,
                          max = tmax,
                          value = current)
    })

    observe({
        req(timeShifts())
        spict_dat$timeShifts <- timeShifts()
    })

    observe({
        req(robflags())
        spict_dat$robflagi <- robflags()
    })

    observeEvent(input$file, {
        fileSpictState$upload <- 'uploaded'
        tmp <- load.data()
        inputSpictData$inputData <- tmp$inputData
        inputSpictData$checks <- tmp$checks
        shinyjs::enable("check_spict")
    })

    observeEvent({
        inputSpictData$inputData
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

            if(!is.null(tmp1)) {
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

            shinyjs::enable("check_spict")

        }, error = function(e) {
            shinyjs::disable("go_spict")
            shinyjs::disable("check_spict")
            shinyjs::disable("createSpictReport")
            shinyjs::disable("createSpictzip")
            showModal(modalDialog(title = "Error",
                                  e$message,
                                  easyClose = TRUE,
                                  footer = NULL))
        })
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
        })
    })



    ## Action buttons
    ## ----------------------------

    observeEvent(input$check_spict, {
        req(spict_dat$dataExplo$inp)

        removeModal()
        spictAcknowledged(TRUE)

        js$showComputing()
        js$disableAllButtons()

        res <- tryCatch({

            inp <- spict_dat$dataExplo$inp

            ## TODO checks

            ## Estimation
            flog.info("check for spit fit")
            show_modal_spinner(
                text = "Checking SPiCT",
                spin = "circle",            # or any from https://glin.github.io/shinybusy/articles/custom_spinner.html
                color = "#112446"
            )
            inp$dteuler <- 1/2
            res <- run_spict(inp)
            remove_modal_spinner()

            js$hideComputing()
            js$enableAllButtons()

        }, error = function(cond) {
            flog.error("Error in SPiCT: %s ",cond)
            showModal(modalDialog(
                title = "Error",
                cond$message,
                easyClose = TRUE,
                footer = NULL
            ))
            return(NULL)
        },
        ## warning = function(cond) {
        ##     showModal(modalDialog(
        ##         title = "Warning",
        ##         cond$message,
        ##         easyClose = TRUE,
        ##         footer = NULL
        ##     ))
        ##     return(NULL)
        ## },
        finally = {
            js$hideComputing()
            js$enableAllButtons()
        })
    })

    observeEvent(input$go_spict, {
        req(spict_dat$dataExplo$inp)

        ## TODO implement tests

        res <- tryCatch({

            spictAcknowledged(FALSE)

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

        }, error = function(cond) {
            flog.error("Error in SPiCT: %s ",cond)
            showModal(modalDialog(
                title = "Error",
                cond$message,
                easyClose = TRUE,
                footer = NULL
            ))
        })
    })

    observeEvent(input$spict_ack, {
        req(spict_dat$dataExplo$inp)
        removeModal()
        spictAcknowledged(TRUE)

        js$showComputing()
        js$disableAllButtons()

        res <- tryCatch({

            inp <- spict_dat$dataExplo$inp

            ## TODO checks

            ## Estimation
            flog.info("Starting spict computation")
            show_modal_spinner(
                text = "Running SPiCT",
                spin = "circle",            # or any from https://glin.github.io/shinybusy/articles/custom_spinner.html
                color = "#112446"
            )
            res <- run_spict(inp)
            remove_modal_spinner()

            js$hideComputing()
            js$enableAllButtons()

            spict_dat$results <- res$res

            if (!is.null(session$userData$sessionMode()) &&
                session$userData$sessionMode() == "GCUBE") {
                flog.info("Uploading spict report to i-Marine workspace")
                reportFileName <- paste(tempdir(),"/","ElefanGA_report_",
                                        format(Sys.time(), "%Y%m%d_%H%M_%s"),".pdf",
                                        sep="")
                                        #createElefanGaPDFReport(reportFileName,spict_dat,input)
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
                }, finally = {})
            }

        }, error = function(cond) {
            flog.error("Error in SPiCT: %s ",cond)
            showModal(modalDialog(
                title = "Error",
                cond$message,
                easyClose = TRUE,
                footer = NULL
            ))
            return(NULL)
        },
        ## warning = function(cond) {
        ##     showModal(modalDialog(
        ##         title = "Warning",
        ##         cond$message,
        ##         easyClose = TRUE,
        ##         footer = NULL
        ##     ))
        ##     return(NULL)
        ## },
        finally = {
            js$hideComputing()
            js$enableAllButtons()
            if (!is.null(spict_dat$results)) {
                shinyjs::enable("createSpictReport")
                shinyjs::enable("createSpictzip")
            }
        })

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

    observeEvent(input$info_col_auto, {
        showModal(modalDialog(
            title = "Automatically assign columns",
            HTML("<p>TODO </p>"),
            easyClose = TRUE,
            size = "l"
        ))
    }, ignoreInit = TRUE)

    observeEvent(input$info_n_indices, {
        showModal(modalDialog(
            title = "Number of indices",
            HTML("<p>TODO </p>"),
            easyClose = TRUE,
            size = "l"
        ))
    }, ignoreInit = TRUE)

    observeEvent(input$info_timeC_lab, {
        showModal(modalDialog(
            title = "Column with times of catch observations",
            HTML("<p>Please assign the columns of your data to the required SPiCT input data. SPiCT requires a vector with catch observations and their times, as well as either index observations and their times or effort observations and their times. Press 'Update data' when all columns are assigned. It is possible to select multiple columns representing different index observations (e.g. different survey fleets) and their times. </p>"),
            easyClose = TRUE,
            size = "l"
        ))
    }, ignoreInit = TRUE)

    observeEvent(input$info_obsC_lab, {
        showModal(modalDialog(
            title = "Column with catch observations",
            HTML("<p>TODO </p>"),
            easyClose = TRUE,
            size = "l"
        ))
    }, ignoreInit = TRUE)

    observeEvent(input$info_stdevC_lab, {
        showModal(modalDialog(
            title = "Optional column with relative uncertainty of catch observations",
            HTML("<p>TODO </p>"),
            easyClose = TRUE,
            size = "l"
        ))
    }, ignoreInit = TRUE)

    observeEvent(input$info_timeI_lab, {
        showModal(modalDialog(
            title = "Column with times of index observations",
            HTML("<p>TODO </p>"),
            easyClose = TRUE,
            size = "l"
        ))
    }, ignoreInit = TRUE)

    observeEvent(input$info_obsI_lab, {
        showModal(modalDialog(
            title = "Column with index observations",
            HTML("<p>TODO </p>"),
            easyClose = TRUE,
            size = "l"
        ))
    }, ignoreInit = TRUE)

    observeEvent(input$info_stdevI_lab, {
        showModal(modalDialog(
            title = "Optional column with relative uncertainty of index observations",
            HTML("<p>TODO </p>"),
            easyClose = TRUE,
            size = "l"
        ))
    }, ignoreInit = TRUE)

    observeEvent(input$info_timerange, {
        showModal(modalDialog(
            title = "Time range for assessment",
            HTML("<p>TODO </p>"),
            easyClose = TRUE,
            size = "l"
        ))
    }, ignoreInit = TRUE)

    observeEvent(input$info_timeI_shift, {
        showModal(modalDialog(
            title = "Time of the year as fraction of index time series",
            HTML("<p>TODO </p>"),
            easyClose = TRUE,
            size = "l"
        ))
    }, ignoreInit = TRUE)

    observeEvent(input$info_dteuler, {
        showModal(modalDialog(
            title = "Time step of the Euler discretization",
            HTML("<p>Euler discretisation time step, i.e. how many time steps per year should be used. </p>"),
            easyClose = TRUE,
            size = "l"
        ))
    }, ignoreInit = TRUE)

    observeEvent(input$info_catchunit, {
        showModal(modalDialog(
            title = "Catch unit",
            HTML("<p>Set the unit of the catches (to be displayed in the graphs), e.g. '000 t. </p>"),
            easyClose = TRUE,
            size = "l"
        ))
    }, ignoreInit = TRUE)

    observeEvent(input$info_robflagi, {
        showModal(modalDialog(
            title = "Robust flag for Index",
            HTML("<p>TODO </p>"),
            easyClose = TRUE,
            size = "l"
        ))
    }, ignoreInit = TRUE)

    observeEvent(input$info_robflagc, {
        showModal(modalDialog(
            title = "Robust flag for catches",
            HTML("<p>Should the robust estimation for catches be used? </p>"),
            easyClose = TRUE,
            size = "l"
        ))
    }, ignoreInit = TRUE)

    observeEvent(input$info_fig_format, {
        showModal(modalDialog(
            title = "Format of archived figures",
            HTML("<p>TODO </p>"),
            easyClose = TRUE,
            size = "l"
        ))
    }, ignoreInit = TRUE)

    observeEvent(input$info_tab_format, {
        showModal(modalDialog(
            title = "Format of archived tables",
            HTML("<p>TODO </p>"),
            easyClose = TRUE,
            size = "l"
        ))
    }, ignoreInit = TRUE)


    observeEvent(input$info_config_priors, {
        showModal(modalDialog(
            title = "Configure priors",
            HTML("<p>TODO </p>"),
            easyClose = TRUE,
            size = "l"
        ))
    }, ignoreInit = TRUE)

    observeEvent(input$infoAssessment, {
        showModal(modalDialog(
            title = "Check, Assessment, Reset & Report",
            HTML("<p>It is recommended to run a quick check by pressing <b>'Run Check'</b> before running the main assessment.
                             While the main assessment can take a few minutes to run, depending on the settings of the ELEFAN optimation routine and the sample size of the dataset,
                             the check is performed in a matter of seconds and can identify issues in the data or settings. The check does not produce results (figures or tables),
                             but a notification in the middle of the screen will inform you whether the check was successful. <br> <br>

                             <b>'Run Assessment'</b> performs the main assessment and should yield plenty of figures and tables in the result section upon successful completion.
                             The run may take several minutes and depends on the size of the dataset, aggregation, bin size, and parameter search space. Run time with the sample
                             dataset and default settings is 2-4 minutes. <br> <br> <b>'Reset'</b> removes all results, the uploaded dataset, and resets all settings to default values. <br> <br>

                             After successful completion of the main assessment, an additional button <b>'Download Report'</b> allows you to download a pdf document with all results.
                             This report is also automatically uploaded to your private workspace.</p>"),
            easyClose = TRUE,
            size = "l"
        ))
    }, ignoreInit = TRUE)



    ## Plots
    ## --------------------------

    ## Data exploration
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


    output$text_sum <- renderUI({
        req(spict_dat$results)
        textSpict.sum(spict_dat, input)
    })

    output$fit <- renderPrint({
        req(spict_dat$results)
        summary(spict_dat$results)
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
