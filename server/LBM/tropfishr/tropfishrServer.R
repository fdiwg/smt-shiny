elefanGaModule <- function(input, output, session) {

    ns <- session$ns

    ## Definition of reactive values
    ## ----------------------------
    elefan_ga <- reactiveValues(
        dataExplo = NULL,
        results = NULL,
        years_selected = NULL,
        binSize = NULL
    )

    elefanGaUploadVreResult <- reactiveValues()

    inputElefanGaData <- reactiveValues()

    fileGaState <- reactiveValues(
        upload = NULL
    )

    elefanAcknowledged <- reactiveVal(FALSE)
    elefanPendingRun   <- reactiveVal(FALSE)


    ## Definition of functions
    ## ----------------------------
    elefanGaFileData <- function() {
        if (is.null(input$fileGa) || is.null(fileGaState$upload)) return(NULL)

        ds <- withCallingHandlers(
            tryCatch({
                x <- read_lbm_csv(
                    input$fileGa$datapath,
                    input$elefanGaDateFormat,
                    input$elefanGaCSVsep,
                    input$elefanGaCSVdec
                )
                x$checks$fileName <- input$fileGa$name
                x
            }, error = function(e) {
                shinyjs::disable("go_ga")
                shinyjs::disable("check_ga")
                shinyjs::disable("createElefanGAReport")
                shinyjs::disable("createElefanGAzip")
                showModal(modalDialog(title = "Error",
                                      HTML(paste0("There was an unexpected error when reading in your data set. <br><br>Did you upload the correct data set? Does your data set fulfill the data and format requirements of this method? Please double-check your data set, have a look at the example data sets, and refer to the info button for more help.<br><br> The specific error message was: <br>", e$message)),
                                      easyClose = TRUE))
                NULL
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

        if (is.null(ds)) return(NULL)

        if (is.null(ds$inputData)){
            shinyjs::disable("go_ga")
            shinyjs::disable("check_ga")
            shinyjs::disable("createElefanGAReport")
            shinyjs::disable("createElefanGAzip")
            showModal(modalDialog(title = "Error",
                                  HTML(paste0("There was an unexpected error when reading in your data set. <br><br>Did you upload the correct data set? Does your data set fulfill the data and format requirements of this method? Please double-check your data set, have a look at the example data sets, and refer to the info button for more help.")),
                                  easyClose = TRUE))
            return(NULL)
        }

        checks <- if (is.null(ds$checks)) list() else ds$checks

        ok <- TRUE
        msg <- NULL

        if (!isTRUE(checks$csv)) {
            ok <- FALSE
            msg <- "Something went wrong when reading your dataset. Did you select a CSV file ('.csv')? Click the info icon for more details on data formats."
        } else if (!isTRUE(checks$delimiter)) {
            ok <- FALSE
            msg <- "The CSV delimiter wasn’t recognized. Please ensure the separator is a comma ',' or a semicolon ';'."
        } else if (is.null(ds$lfq)) {
            ok <- FALSE
            msg <- "No valid length–frequency table detected. Ensure the first column contains numeric length classes and that the decimal separator is a dot '.'."
        } else if (!isTRUE(checks$lengths)) {
            ok <- FALSE
            msg <- "The length-class column is not numeric or improperly formatted. Please verify the first column and decimal separator."
        } else if (!isTRUE(checks$dates)) {
            ok <- FALSE
            msg <- "Date columns not detected. Column names must indicate sampling dates (e.g. '2020-08-21' or '21.08.2020'); R may prefix with 'X'."
        } else if (!isTRUE(checks$ncols)) {
            ok <- FALSE
            msg <- "Insufficient numeric sampling columns. Provide at least two columns of counts per length class for given sampling dates."
        }

        if (!ok) {
            shinyjs::disable("go_ga")
            shinyjs::disable("check_ga")
            shinyjs::disable("createElefanGAReport")
            shinyjs::disable("createElefanGAzip")
            showModal(modalDialog(title = "Error", msg, easyClose = TRUE, footer = NULL))
            return(NULL)
        }

        shinyjs::enable("go_ga")
        shinyjs::enable("check_ga")
        return(list(
            lfq    = ds$lfq,
            raw    = ds$raw,
            checks = ds$checks
        ))
    }

    elefanGaDataExplo1 <- reactive({
        req(inputElefanGaData$data)
        req(elefan_ga$years_selected)
        req(elefan_ga$binSize)

        res <- tryCatch({
            elefan_ga$dataExplo <- list()
            years <- elefan_ga$years_selected
            if(is.null(years)){
                stop("No year selected for the analysis. Please select at least one year of uploaded data set.")
            }
            ## if(input$ELEFAN_agg == "year" &&
            ##    length(unique(format(inputElefanGaData$data$dates,"%Y"))) == 1){
            ##     stop("Aggregation 'year' can only be used when the dataset spans 2 or more years. Please use a finer temporal resolution (aggregation 'month' or 'quarter').")
            ## }
            agg <- input$ELEFAN_agg
            if(agg == "none") agg <- NA
            dat <- inputElefanGaData$data
            class(dat) <- "lfq"

            binSize <- elefan_ga$binSize
            if(!is.numeric(binSize)){
                stop("The bin size is not numeric! Please check your input!")
            }
            if(binSize <= 0){
                stop("The bin size has to be larger than 0!")
            }
            if(binSize > 30){
                warning("Be aware that your bin size is unexpectedly large (>30)! If your length measurements are in mm, make sure to choose 'mm' in the length unit box!")
            }
            if(binSize < min(diff(dat$midLengths))){
                stop("The specified bin size is smaller than the resolution in the uploaded dataset! Please set the bin size equal to ", min(diff(dat$midLengths))," or higher!")
            }

            lfq <- lfqModify(dat,
                             bin_size = binSize,
                             years = years,
                             ## plus_group = input$ELEFAN_GA_PLUS_GROUP,
                             aggregate = agg)

            ## Leave in original unit!
            ## ## Account for length unit
            ## if(input$elefan_lengthUnit == "mm"){
            ##     lfq$midLengths <- lfq$midLengths * 10
            ## }else if(input$elefan_lengthUnit == "in"){
            ##     lfq$midLengths <- lfq$midLengths * 2.54
            ## }

            return(lfq)

        }, error = function(cond) {
            shinyjs::disable("go_ga")
            shinyjs::disable("check_ga")
            shinyjs::disable("createElefanGAReport")
            shinyjs::disable("createElefanGAzip")
            showModal(modalDialog(
                title = "Error",
                cond$message,
                easyClose = TRUE,
                footer = NULL
            ))
            return(NULL)
        }
        ## , warning = function(cond) {
        ##       showModal(modalDialog(
        ##           title = "Warning",
        ##           cond$message,
        ##           easyClose = TRUE,
        ##           footer = NULL
        ##       ))
        ##       return(NULL)
        ##   }
        )

        return(res)
    })

    elefanGaDataExplo2 <- reactive({
        req(elefan_ga$dataExplo$lfq)

        res <- tryCatch({

            ma <- input$ELEFAN_GA_MA
            if(!is.numeric(ma)){
                stop("The moving average (MA) is not numeric! Please check your input!")
            }
            if(ma < 1){
                stop("The moving average (MA) cannot be smaller than 1!")
            }
            if(ma > 25){
                warning("Be aware that your moving average (MA) is unexpectedly large (>25)! You might consider to increase the bin size and use a lower MA.")
            }
            if(ma %% 1 != 0){
                stop("The moving average (MA) cannot have any decimal places!")
            }
            if((ma / 2) %% 1 == 0){
                stop("The moving average (MA) has to be an odd number (1,3,5,7,...)!")
            }

            if(!is.matrix(elefan_ga$dataExplo$lfq$catch)) {
                stop("The catch matrix in the length frequency data set is numeric implying a single sampling time. This could be caused if the data in fact only contains a single sampling time or if the aggregation level is too coarse, for example, if the data was aggregated over a full year. ELEFAN requires more than one length frequency sample, ideally corresponding to different seasons/months in the same year. Please check your data.")
            }

            lfqbin <- lfqRestructure(elefan_ga$dataExplo$lfq,
                                     MA = ma,
                                     addl.sqrt = input$ELEFAN_GA_addlsqrt)
            return(lfqbin)

        }, error = function(cond) {
            shinyjs::disable("go_ga")
            shinyjs::disable("check_ga")
            shinyjs::disable("createElefanGAReport")
            shinyjs::disable("createElefanGAzip")
            showModal(modalDialog(
                title = "Error",
                cond$message,
                easyClose = TRUE,
                footer = NULL
            ))
            return(NULL)
        }
      ## , warning = function(cond) {
      ##       showModal(modalDialog(
      ##           title = "Warning",
      ##           cond$message,
      ##           easyClose = TRUE,
      ##           footer = NULL
      ##       ))
      ##       return(NULL)
      ## }
      )

        return(res)
    })

    resetElefanGaInputValues <- function() {
        ## resetting UIs
        shinyjs::reset("fileGa")
        shinyjs::reset("elefanGaCSVsep")
        shinyjs::reset("elefanGaCSVdec")
        shinyjs::reset("elefanGaDateFormat")
        shinyjs::reset("ELEFAN_years_selected")
        shinyjs::reset("ELEFAN_agg")
        shinyjs::reset("ELEFAN_GA_binSize")
        shinyjs::reset("ELEFAN_GA_MA")
        ##        shinyjs::reset("ELEFAN_GA_PLUS_GROUP")
        shinyjs::reset("ELEFAN_GA_addl.sqrt")
        shinyjs::reset("ELEFAN_GA_Linf")
        shinyjs::reset("ELEFAN_GA_K")
        shinyjs::reset("ELEFAN_GA_t_anchor")
        shinyjs::reset("ELEFAN_GA_seasonalised")
        shinyjs::reset("ELEFAN_GA_C")
        shinyjs::reset("ELEFAN_GA_ts")
        shinyjs::reset("ELEFAN_GA_popSize")
        shinyjs::reset("ELEFAN_GA_maxiter")
        shinyjs::reset("ELEFAN_GA_run")
        shinyjs::reset("ELEFAN_GA_pmutation")
        shinyjs::reset("ELEFAN_GA_pcrossover")
        shinyjs::reset("ELEFAN_GA_elitism")
        shinyjs::reset("LWa")
        shinyjs::reset("LWb")
        shinyjs::reset("natM")
        shinyjs::reset("temp")
        shinyjs::reset("schooling")
        shinyjs::reset("tmax")
        shinyjs::reset("ELEFAN_years_selected_cc")
        shinyjs::reset("selectMat")
        shinyjs::reset("lm50_user")
        shinyjs::reset("lm75_user")
        shinyjs::reset("wqsm_user")
        shinyjs::reset("per_lm1_user")
        shinyjs::reset("lm1_user")
        shinyjs::reset("per_lm2_user")
        shinyjs::reset("lm2_user")
        shinyjs::reset("select")
        shinyjs::reset("l50_user")
        shinyjs::reset("l75_user")
        shinyjs::reset("wqs_user")
        shinyjs::reset("per_l1_user")
        shinyjs::reset("l1_user")
        shinyjs::reset("per_l2_user")
        shinyjs::reset("l2_user")
        shinyjs::reset("fRangeSteps")
        shinyjs::reset("fRangeMin")
        shinyjs::reset("fRangeMax")
        shinyjs::reset("lcRangeSteps")
        shinyjs::reset("lcRangeMin")
        shinyjs::reset("lcRangeMax")

        ## disable buttons
        shinyjs::disable("go_ga")
        shinyjs::disable("check_ga")
        shinyjs::disable("createElefanGAReport")
        shinyjs::disable("createElefanGAzip")

        ## resetting reactive values
        elefan_ga$dataExplo <- NULL
        elefan_ga$results <- NULL
        elefan_ga$years_selected <- NULL
        elefan_ga$binSize <- NULL
        inputElefanGaData$data <- NULL
        fileGaState$upload <- NULL
        ## elefanGaUploadVreResult ?
        ## elefanAcknowledged(FALSE)
    }


    run_check <- function() {

        res <- tryCatch({

            ## needed because of renderUI in hidden tabs
            if(is.null(input$ELEFAN_GA_Linf)){
                linf_range <- c(0.8,1.2) * round(max(inputElefanGaData$data$midLengths)/0.95)
            }else{
                linf_range <- range(input$ELEFAN_GA_Linf)
            }
            k_range <- c(min(input$ELEFAN_GA_K), max(input$ELEFAN_GA_K))
            ta_range <- c(min(input$ELEFAN_GA_t_anchor),
                          max(input$ELEFAN_GA_t_anchor))
            c_range <- c(min(input$ELEFAN_GA_C), max(input$ELEFAN_GA_C))
            ts_range <- c(min(input$ELEFAN_GA_ts), max(input$ELEFAN_GA_ts))

            if(is.null(input$yearsCC)){
                yearsCC <- unique(format(inputElefanGaData$data$dates,"%Y"))
            }else{
                yearsCC <- input$ELEFAN_years_selected_cc
            }


            ## Double-check that lower bounds are met
            popSize <- input$ELEFAN_GA_popSize
            if(popSize < 50) popSize <- 50
            maxiter <- input$ELEFAN_GA_maxiter
            if(maxiter < 20) maxiter <- 20
            pmutation <- input$ELEFAN_GA_pmutation
            if(pmutation < 0.1) pmutation <- 0.1
            run <- input$ELEFAN_GA_run
            if(run < 10) run <- 10
            pcrossover <- input$ELEFAN_GA_pcrossover
            if(pcrossover < 0.1) pcrossover <- 0.1
            elitism <- input$ELEFAN_GA_elitism
            if(elitism < 1) elitism <- 1

            ## Fix parameters
            if(!is.na(input$provide_Linf)){
                linf_range <- c(input$provide_Linf,input$provide_Linf)
            }
            if(!is.na(input$provide_K)){
                k_range <- c(input$provide_K,input$provide_K)
            }
            if(!is.na(input$provide_t_anchor)){
                ta_range <- c(input$provide_t_anchor,input$provide_t_anchor)
            }
            if(!is.na(input$provide_C)){
                c_range <- c(input$provide_C,input$provide_C)
            }
            if(!is.na(input$provide_ts)){
                ts_range <- c(input$provide_ts,input$provide_ts)
            }

            ## Parameter/Input checks
            check.numeric.and.min(c(popSize = popSize,
                                    maxiter = maxiter,
                                    run = run,
                                    pmut = pmutation,
                                    pcross = pcrossover,
                                    elite = elitism,
                                    ma = input$ELEFAN_GA_MA,
                                    binSize = input$ELEFAN_GA_binSize,
                                    a = input$LWa,
                                    b = input$LWb,
                                    fRangeMax = input$fRangeMax),
                                  can.be.zero = FALSE)
            check.numeric.and.min(c(fRangeMin = input$fRangeMin),
                                  can.be.zero = TRUE)
            if(!is.na(input$provide_Linf)){
                if(!is.numeric(linf_range)){
                    stop(paste0("The provided Linf value is not numeric! Please check your input!"))
                }
                if(linf_range <= 0){
                    stop(paste0("The provided Linf value has to be larger than 0!"))
                }
            }
            if(!is.na(input$provide_K)){
                if(!is.numeric(k_range)){
                    stop(paste0("The provided K value is not numeric! Please check your input!"))
                }
                if(k_range <= 0){
                    stop(paste0("The provided K value has to be larger than 0!"))
                }
            }
            if(!is.na(input$provide_t_anchor)){
                if(!is.numeric(ta_range)){
                    stop(paste0("The provided time anchor (ta) is not numeric! Please check your input!"))
                }
                if(ta_range <= 0){
                    stop(paste0("The provided time anchor (ta) has to be larger than 0!"))
                }
            }

        }, error = function(cond) {
            flog.error("Error in TropFishR: %s ",cond)
            showModal(modalDialog(
                title = "Error",
                cond$message,
                easyClose = TRUE,
                footer = NULL
            ))
            return(invisible(NULL))
        })
    }

    run_elefan_server <- function() {

        res <- tryCatch({

            ## needed because of renderUI in hidden tabs
            if(is.null(input$ELEFAN_GA_Linf)){
                linf_range <- c(0.8,1.2) * round(max(inputElefanGaData$data$midLengths)/0.95)
            }else{
                linf_range <- range(input$ELEFAN_GA_Linf)
            }
            k_range <- c(min(input$ELEFAN_GA_K), max(input$ELEFAN_GA_K))
            ta_range <- c(min(input$ELEFAN_GA_t_anchor),
                          max(input$ELEFAN_GA_t_anchor))
            c_range <- c(min(input$ELEFAN_GA_C), max(input$ELEFAN_GA_C))
            ts_range <- c(min(input$ELEFAN_GA_ts), max(input$ELEFAN_GA_ts))

            if(is.null(input$yearsCC)){
                yearsCC <- unique(format(inputElefanGaData$data$dates,"%Y"))
            }else{
                yearsCC <- input$ELEFAN_years_selected_cc
            }


            ## Double-check that lower bounds are met
            popSize <- input$ELEFAN_GA_popSize
            if(popSize < 50) popSize <- 50
            maxiter <- input$ELEFAN_GA_maxiter
            if(maxiter < 20) maxiter <- 20
            pmutation <- input$ELEFAN_GA_pmutation
            if(pmutation < 0.1) pmutation <- 0.1
            run <- input$ELEFAN_GA_run
            if(run < 10) run <- 10
            pcrossover <- input$ELEFAN_GA_pcrossover
            if(pcrossover < 0.1) pcrossover <- 0.1
            elitism <- input$ELEFAN_GA_elitism
            if(elitism < 1) elitism <- 1

            ## Fix parameters
            if(!is.na(input$provide_Linf)){
                linf_range <- c(input$provide_Linf,input$provide_Linf)
            }
            if(!is.na(input$provide_K)){
                k_range <- c(input$provide_K,input$provide_K)
            }
            if(!is.na(input$provide_t_anchor)){
                ta_range <- c(input$provide_t_anchor,input$provide_t_anchor)
            }
            if(!is.na(input$provide_C)){
                c_range <- c(input$provide_C,input$provide_C)
            }
            if(!is.na(input$provide_ts)){
                ts_range <- c(input$provide_ts,input$provide_ts)
            }

            ## Parameter/Input checks
            check.numeric.and.min(c(popSize = popSize,
                                    maxiter = maxiter,
                                    run = run,
                                    pmut = pmutation,
                                    pcross = pcrossover,
                                    elite = elitism,
                                    ma = input$ELEFAN_GA_MA,
                                    binSize = input$ELEFAN_GA_binSize,
                                    a = input$LWa,
                                    b = input$LWb,
                                    fRangeMax = input$fRangeMax),
                                  can.be.zero = FALSE)
            check.numeric.and.min(c(fRangeMin = input$fRangeMin),
                                  can.be.zero = TRUE)
            if(!is.na(input$provide_Linf)){
                if(!is.numeric(linf_range)){
                    stop(paste0("The provided Linf value is not numeric! Please check your input!"))
                }
                if(linf_range <= 0){
                    stop(paste0("The provided Linf value has to be larger than 0!"))
                }
            }
            if(!is.na(input$provide_K)){
                if(!is.numeric(k_range)){
                    stop(paste0("The provided K value is not numeric! Please check your input!"))
                }
                if(k_range <= 0){
                    stop(paste0("The provided K value has to be larger than 0!"))
                }
            }
            if(!is.na(input$provide_t_anchor)){
                if(!is.numeric(ta_range)){
                    stop(paste0("The provided time anchor (ta) is not numeric! Please check your input!"))
                }
                if(ta_range <= 0){
                    stop(paste0("The provided time anchor (ta) has to be larger than 0!"))
                }
            }

            ## Estimation
            flog.info("Starting Elegan GA computation")
            res <- run_tropfishr(x = inputElefanGaData$data,
                                 binSize =  input$ELEFAN_GA_binSize,
                                 seasonalised = input$ELEFAN_GA_seasonalised,
                                 low_par = list(Linf = linf_range[1], K = k_range[1],
                                                t_anchor = ta_range[1],
                                                C = c_range[1],
                                                ts = ts_range[1]),
                                 up_par = list(Linf = linf_range[2], K = k_range[2],
                                               t_anchor = ta_range[2],
                                               C = c_range[2],
                                               ts = ts_range[2]),
                                 popSize = popSize, maxiter = maxiter,
                                 run = run, pmutation = pmutation,
                                 pcrossover = pcrossover,
                                 elitism = elitism,
                                 MA = input$ELEFAN_GA_MA,
                                 addl.sqrt = input$ELEFAN_GA_addlsqrt,
                                 years = input$ELEFAN_years_selected,
                                 agg = input$ELEFAN_agg,
                                 binSizeCC = input$ELEFAN_GA_binSize,
                                 yearsCC = yearsCC,
                                 LWa = input$LWa,
                                 LWb = input$LWb,
                                 natM_method = input$natM,
                                 temp = input$temp,
                                 cor_schooling = input$schooling,
                                 tmax = input$tmax,
                                 select_method = input$select,
                                 l50_user = input$l50_user,
                                 l75_user = input$l75_user,
                                 wqs_user = input$wqs_user,
                                 per_l1 = input$per_l1_user,
                                 l1 = input$l1_user,
                                 per_l2 = input$per_l2_user,
                                 l2 = input$l2_user,
                                 fRangeSteps = input$fRangeSteps,
                                 fRangeMin = input$fRangeMin,
                                 fRangeMax = input$fRangeMax,
                                 lcRangeSteps = input$lcRangeSteps,
                                 lcRangeMin = input$lcRangeMin,
                                 lcRangeMax = input$lcRangeMax,
                                 mat_method = input$selectMat,
                                 Lm50 = input$lm50_user,
                                 Lm75 = input$lm75_user,
                                 wqsm = input$wqsm_user,
                                 per_lm1 = input$per_lm1_user,
                                 lm1 = input$lm1_user,
                                 per_lm2 = input$per_lm2_user,
                                 lm2 = input$lm2_user,
                                 skip_elefan = input$provideGP,
                                 provide_linf = input$provide_Linf,
                                 provide_k = input$provide_K,
                                 provide_ta = input$provide_t_anchor
                                 )

            js$hideComputing()
            js$enableAllButtons()

            elefan_ga$results <- res
            session$userData$fishingMortality$FcurrGA <-
                round(elefan_ga$results$resYPR1$currents[4]$curr.F, 2)

            if (!is.null(session$userData$sessionMode()) &&
                session$userData$sessionMode() == "GCUBE") {
                flog.info("Uploading Elefan GA report to i-Marine workspace")
                reportFileName <- paste(tempdir(),"/","ElefanGA_report_",
                                        format(Sys.time(), "%Y%m%d_%H%M_%s"),".pdf",
                                        sep="")
                                        #createElefanGaPDFReport(reportFileName,elefan_ga,input)
                createElefanGaPDFReport(reportFileName, elefan_ga, input, output)
                elefanGaUploadVreResult$res <- FALSE

                basePath <- paste0("/Home/",session$userData$sessionUsername(),"/Workspace/")

                SH_MANAGER <- session$userData$storagehubManager()

                tryCatch({
                    uploadToIMarineFolder(SH_MANAGER, reportFileName, basePath, uploadFolderName)
                    elefanGaUploadVreResult$res <- TRUE
                }, error = function(err) {
                    flog.error("Error uploading Elefan GA report to the i-Marine Workspace: %s", err)
                    elefanGaUploadVreResult$res <- FALSE
                }, finally = {})
            }

        }, error = function(cond) {
            flog.error("Error in TropFishR: %s ",cond)
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
            if (!is.null(elefan_ga$results)) {
                shinyjs::enable("createElefanGAReport")
                shinyjs::enable("createElefanGAzip")
            }
        })
    }


    ## Input-dependent UIs
    ## ----------------------------
    output$ELEFAN_years_selected_out <- renderUI({
        if(is.null(inputElefanGaData$data)){
            allyears <- NULL
    }else{
        allyears <- try(unique(format(inputElefanGaData$data$dates,"%Y")),silent=TRUE)
        if(inherits(allyears,"try-error")) allyears <- NULL
    }
    selectInput(ns("ELEFAN_years_selected"), "",
                choices = allyears, selected = allyears,
                multiple = TRUE,
                width = "100%")
})

output$ELEFAN_binSize_out <- renderUI({
    if(is.null(inputElefanGaData$data)){
        binSize <- 2
        maxL <- 10
    }else{
        binSize <- try(min(diff(inputElefanGaData$data$midLengths)),silent=TRUE)
        maxL <- try(max(inputElefanGaData$data$midLengths),silent=TRUE)
        if(inherits(binSize,"try-error")){
            binSize <- 2
            maxL <- 10
        }else{
            binSize <- round(0.23 * maxL^0.6, 1)
            if(binSize == 0) binSize <- 0.1
        }
    }
    numericInput(ns("ELEFAN_GA_binSize"), "",
                 binSize, min = 0.1, max = maxL, step=0.1,
                 width ='100%')
})

output$ELEFAN_GA_Linf_out <- renderUI({
    if(is.null(inputElefanGaData$data)){
        maxL <- 100
    }else{
        maxL <- try(round(max(inputElefanGaData$data$midLengths)/0.95),silent=TRUE)
        if(inherits(maxL,"try-error")){
            maxL <- 100
        }
    }
    min <- 0.25 * maxL
    max <- 1.75 * maxL
    sel <- c(0.8,1.2) * maxL
    sliderInput(ns("ELEFAN_GA_Linf"),"",
                value=sel, min = min, max = max, step=1)
})

output$ELEFAN_years_selected_cc_out <- renderUI({
    if(is.null(inputElefanGaData$data)){
        allyears <- NULL
    }else{
        allyears <- try(unique(format(inputElefanGaData$data$dates,"%Y")),silent=TRUE)
        if(inherits(allyears,"try-error")) allyears <- NULL
    }
    selectInput(ns("ELEFAN_years_selected_cc"), "",
                choices = allyears, selected = allyears,
                multiple = TRUE,
                width = "70%")
})


## Interactive UIs & Reactive values
## ----------------------------

    observe({
        if(!input$ELEFAN_GA_seasonalised){
            js$removeBox2("box_elefan_ga_seasonPar")
            js$removeBox2("box_provide_gp_sea")
        }else{
            js$showBox2("box_elefan_ga_seasonPar")
            js$showBox2("box_provide_gp_sea")
        }
    })

    observe({
        if(!input$provideGP){
            js$removeBox2("box_provide_gp")
            js$removeBox2("box_provide_gp_sea")
        }else{
            js$showBox2("box_provide_gp")
        }
    })

    observeEvent(input$ELEFAN_years_selected, {
        elefan_ga$years_selected <- input$ELEFAN_years_selected
    })

    observeEvent(input$ELEFAN_GA_binSize, {
        elefan_ga$binSize <- input$ELEFAN_GA_binSize
    })

    observe({
        if(input$natM %in% c("Then's growth formula",
                             "Gislason's length-based formula",
                             "Lorenzen's length-based formula")){
            shinyjs::hide("ui_natM_pauly", asis = TRUE)
            shinyjs::hide("ui_natM_then_tmax", asis = TRUE)
        }else if(input$natM == "Then's max. age formula"){
            shinyjs::show("ui_natM_then_tmax", asis = TRUE)
            shinyjs::hide("ui_natM_pauly", asis = TRUE)
        }else if(input$natM == "Pauly's growth & temp. formula"){
            shinyjs::show("ui_natM_pauly", asis = TRUE)
            shinyjs::hide("ui_natM_then_tmax", asis = TRUE)
        }
    })

    observe({
        if(input$selectMat == "No maturity"){
            shinyjs::hide("ui_lm50", asis = TRUE)
            shinyjs::hide("ui_lm75", asis = TRUE)
            shinyjs::hide("ui_wqsm", asis = TRUE)
            shinyjs::hide("ui_per_lm1", asis = TRUE)
            shinyjs::hide("ui_lm1", asis = TRUE)
            shinyjs::hide("ui_per_lm2", asis = TRUE)
            shinyjs::hide("ui_lm2", asis = TRUE)
        }else if(input$selectMat == "Define Lm50 & Lm75"){
            shinyjs::show("ui_lm50", asis = TRUE)
            shinyjs::show("ui_lm75", asis = TRUE)
            shinyjs::hide("ui_wqsm", asis = TRUE)
            shinyjs::hide("ui_per_lm1", asis = TRUE)
            shinyjs::hide("ui_lm1", asis = TRUE)
            shinyjs::hide("ui_per_lm2", asis = TRUE)
            shinyjs::hide("ui_lm2", asis = TRUE)
        }else if(input$selectMat == "Define Lm50 & (Lm75-Lm25)"){
            shinyjs::show("ui_lm50", asis = TRUE)
            shinyjs::hide("ui_lm75", asis = TRUE)
            shinyjs::show("ui_wqsm", asis = TRUE)
            shinyjs::hide("ui_per_lm1", asis = TRUE)
            shinyjs::hide("ui_lm1", asis = TRUE)
            shinyjs::hide("ui_per_lm2", asis = TRUE)
            shinyjs::hide("ui_lm2", asis = TRUE)
        }else if(input$selectMat == "Other"){
            shinyjs::hide("ui_lm50", asis = TRUE)
            shinyjs::hide("ui_lm75", asis = TRUE)
            shinyjs::hide("ui_wqsm", asis = TRUE)
            shinyjs::show("ui_per_lm1", asis = TRUE)
            shinyjs::show("ui_lm1", asis = TRUE)
            shinyjs::show("ui_per_lm2", asis = TRUE)
            shinyjs::show("ui_lm2", asis = TRUE)
        }
    })

    observe({
        if(input$select == "Estimate"){
            shinyjs::hide("ui_l50", asis = TRUE)
            shinyjs::hide("ui_l75", asis = TRUE)
            shinyjs::hide("ui_wqs", asis = TRUE)
            shinyjs::hide("ui_lcMin", asis=TRUE)
            shinyjs::hide("ui_lcMax", asis=TRUE)
            shinyjs::hide("ui_per_l1", asis = TRUE)
            shinyjs::hide("ui_l1", asis = TRUE)
            shinyjs::hide("ui_per_l2", asis = TRUE)
            shinyjs::hide("ui_l2", asis = TRUE)
        }else if(input$select == "Define Ls50 & Ls75"){
            shinyjs::show("ui_l50", asis = TRUE)
            shinyjs::hide("ui_wqs", asis = TRUE)
            shinyjs::show("ui_l75", asis = TRUE)
            shinyjs::show("ui_lcMin", asis=TRUE)
            shinyjs::show("ui_lcMax", asis=TRUE)
            shinyjs::hide("ui_per_l1", asis = TRUE)
            shinyjs::hide("ui_l1", asis = TRUE)
            shinyjs::hide("ui_per_l2", asis = TRUE)
            shinyjs::hide("ui_l2", asis = TRUE)
        }else if(input$select == "Define Ls50 & (Ls75 - Ls25)"){
            shinyjs::show("ui_l50", asis = TRUE)
            shinyjs::hide("ui_l75", asis = TRUE)
            shinyjs::show("ui_wqs", asis = TRUE)
            shinyjs::show("ui_lcMin", asis=TRUE)
            shinyjs::show("ui_lcMax", asis=TRUE)
            shinyjs::hide("ui_per_l1", asis = TRUE)
            shinyjs::hide("ui_l1", asis = TRUE)
            shinyjs::hide("ui_per_l2", asis = TRUE)
            shinyjs::hide("ui_l2", asis = TRUE)
        }else if(input$select == "Other"){
            shinyjs::hide("ui_l50", asis = TRUE)
            shinyjs::hide("ui_l75", asis = TRUE)
        shinyjs::hide("ui_wqs", asis = TRUE)
        shinyjs::show("ui_lcMin", asis=TRUE)
        shinyjs::show("ui_lcMax", asis=TRUE)
        shinyjs::show("ui_per_l1", asis = TRUE)
        shinyjs::show("ui_l1", asis = TRUE)
        shinyjs::show("ui_per_l2", asis = TRUE)
        shinyjs::show("ui_l2", asis = TRUE)
    }
})

observeEvent(input$fileGa, {
    fileGaState$upload <- 'uploaded'
    tmp <- elefanGaFileData()
    inputElefanGaData$data <- tmp$lfq
    inputElefanGaData$raw <- tmp$raw
    inputElefanGaData$checks <- tmp$checks
    ## bin size
    if(is.null(inputElefanGaData$data)){
        binSize <- 2
        maxL <- 10
    }else{
        binSize <- try(min(diff(inputElefanGaData$data$midLengths)),silent=TRUE)
        maxL <- try(max(inputElefanGaData$data$midLengths),silent=TRUE)
        if(inherits(binSize,"try-error")){
            binSize <- 2
            maxL <- 10
        }else{
            binSize <- round(0.23 * maxL^0.6, 1)
            if(binSize == 0) binSize <- 0.1
        }
    }
    elefan_ga$binSize <- binSize
    ## years selected
    if(is.null(inputElefanGaData$data)){
        allyears <- NULL
    }else{
        allyears <- try(unique(format(inputElefanGaData$data$dates,"%Y")),silent=TRUE)
        if(inherits(allyears,"try-error")) allyears <- NULL
    }
    elefan_ga$years_selected <- allyears
    ## linf
    if(is.null(inputElefanGaData$data)){
        maxL <- 100
    }else{
        maxL <- try(round(max(inputElefanGaData$data$midLengths)/0.95),silent=TRUE)
        if(inherits(maxL,"try-error")){
            maxL <- 100
        }
    }
    min <- 0.25 * maxL
    max <- 1.75 * maxL
    sel <- c(0.8,1.2) * maxL
    updateSliderInput(session,
                      inputId = "ELEFAN_GA_Linf",
                      min = min,
                      max = max,
                      value = sel,
                      step = 1)
})

    observeEvent(input$elefanGaDateFormat, {
        req(input$fileGa)

        tmp <- elefanGaFileData()
        inputElefanGaData$data <- tmp$lfq
        inputElefanGaData$raw <- tmp$raw
        inputElefanGaData$checks <- tmp$checks
        ## bin size
        if(is.null(inputElefanGaData$data)){
            binSize <- 2
            maxL <- 10
        }else{
            binSize <- try(min(diff(inputElefanGaData$data$midLengths)),silent=TRUE)
            maxL <- try(max(inputElefanGaData$data$midLengths),silent=TRUE)
            if(inherits(binSize,"try-error")){
                binSize <- 2
                maxL <- 10
            }else{
                binSize <- round(0.23 * maxL^0.6, 1)
                if(binSize == 0) binSize <- 0.1
            }
        }
        elefan_ga$binSize <- binSize
        ## years selected
        if(is.null(inputElefanGaData$data)){
            allyears <- NULL
        }else{
            allyears <- try(unique(format(inputElefanGaData$data$dates,"%Y")),silent=TRUE)
            if(inherits(allyears,"try-error")) allyears <- NULL
        }
        elefan_ga$years_selected <- allyears
        ## linf
        if(is.null(inputElefanGaData$data)){
            maxL <- 100
        }else{
            maxL <- try(round(max(inputElefanGaData$data$midLengths)/0.95),silent=TRUE)
            if(inherits(maxL,"try-error")){
                maxL <- 100
            }
        }
        min <- 0.25 * maxL
        max <- 1.75 * maxL
        sel <- c(0.8,1.2) * maxL
        updateSliderInput(session,
                          inputId = "ELEFAN_GA_Linf",
                          min = min,
                          max = max,
                          value = sel,
                          step = 1)
    })

    observeEvent(input$elefanGaCSVdec, {
        req(input$fileGa)
        tmp <- elefanGaFileData()
        inputElefanGaData$data <- tmp$lfq
        inputElefanGaData$raw <- tmp$raw
        inputElefanGaData$checks <- tmp$checks
        ## bin size
        if(is.null(inputElefanGaData$data)){
            binSize <- 2
            maxL <- 10
        }else{
            binSize <- try(min(diff(inputElefanGaData$data$midLengths)),silent=TRUE)
            maxL <- try(max(inputElefanGaData$data$midLengths),silent=TRUE)
            if(inherits(binSize,"try-error")){
                binSize <- 2
                maxL <- 10
            }else{
                binSize <- round(0.23 * maxL^0.6, 1)
                if(binSize == 0) binSize <- 0.1
            }
        }
        elefan_ga$binSize <- binSize
        ## years selected
        if(is.null(inputElefanGaData$data)){
            allyears <- NULL
        }else{
            allyears <- try(unique(format(inputElefanGaData$data$dates,"%Y")),silent=TRUE)
            if(inherits(allyears,"try-error")) allyears <- NULL
        }
        elefan_ga$years_selected <- allyears
        ## linf
        if(is.null(inputElefanGaData$data)){
            maxL <- 100
        }else{
            maxL <- try(round(max(inputElefanGaData$data$midLengths)/0.95),silent=TRUE)
            if(inherits(maxL,"try-error")){
                maxL <- 100
            }
        }
        min <- 0.25 * maxL
        max <- 1.75 * maxL
        sel <- c(0.8,1.2) * maxL
        updateSliderInput(session,
                          inputId = "ELEFAN_GA_Linf",
                          min = min,
                          max = max,
                          value = sel,
                          step = 1)
    })

    observeEvent(input$elefanGaCSVsep, {
        req(input$fileGa)
        tmp <- elefanGaFileData()
        inputElefanGaData$data <- tmp$lfq
        inputElefanGaData$raw <- tmp$raw
        inputElefanGaData$checks <- tmp$checks
        ## bin size
        if(is.null(inputElefanGaData$data)){
            binSize <- 2
            maxL <- 10
        }else{
            binSize <- try(min(diff(inputElefanGaData$data$midLengths)),silent=TRUE)
            maxL <- try(max(inputElefanGaData$data$midLengths),silent=TRUE)
            if(inherits(binSize,"try-error")){
                binSize <- 2
                maxL <- 10
            }else{
                binSize <- round(0.23 * maxL^0.6, 1)
                if(binSize == 0) binSize <- 0.1
            }
        }
        elefan_ga$binSize <- binSize
        ## years selected
        if(is.null(inputElefanGaData$data)){
            allyears <- NULL
        }else{
            allyears <- try(unique(format(inputElefanGaData$data$dates,"%Y")),silent=TRUE)
            if(inherits(allyears,"try-error")) allyears <- NULL
        }
        elefan_ga$years_selected <- allyears
        ## linf
        if(is.null(inputElefanGaData$data)){
            maxL <- 100
        }else{
            maxL <- try(round(max(inputElefanGaData$data$midLengths)/0.95),silent=TRUE)
            if(inherits(maxL,"try-error")){
                maxL <- 100
            }
        }
        min <- 0.25 * maxL
        max <- 1.75 * maxL
        sel <- c(0.8,1.2) * maxL
        updateSliderInput(session,
                          inputId = "ELEFAN_GA_Linf",
                          min = min,
                          max = max,
                          value = sel,
                          step = 1)
    })




    ## Action buttons
    ## ----------------------------
    observeEvent(input$check_ga, {

        js$showComputing()
        js$disableAllButtons()

        res <- tryCatch({

            ## needed because of renderUI in hidden tabs
            if(is.null(input$ELEFAN_GA_Linf)){
                linf_range <- c(0.8,1.2) * round(max(inputElefanGaData$data$midLengths)/0.95)
            }else{
                linf_range <- range(input$ELEFAN_GA_Linf)
            }
            k_range <- c(min(input$ELEFAN_GA_K), max(input$ELEFAN_GA_K))
            ta_range <- c(min(input$ELEFAN_GA_t_anchor),
                          max(input$ELEFAN_GA_t_anchor))
            c_range <- c(min(input$ELEFAN_GA_C), max(input$ELEFAN_GA_C))
            ts_range <- c(min(input$ELEFAN_GA_ts), max(input$ELEFAN_GA_ts))

            if(is.null(input$yearsCC)){
                yearsCC <- unique(format(inputElefanGaData$data$dates,"%Y"))
            }else{
                yearsCC <- input$ELEFAN_years_selected_cc
            }

            ## Fix parameters
            if(!is.na(input$provide_Linf)){
                linf_range <- c(input$provide_Linf,input$provide_Linf)
            }
            if(!is.na(input$provide_K)){
                k_range <- c(input$provide_K,input$provide_K)
            }
            if(!is.na(input$provide_t_anchor)){
                ta_range <- c(input$provide_t_anchor,input$provide_t_anchor)
            }
            if(!is.na(input$provide_C)){
                c_range <- c(input$provide_C,input$provide_C)
            }
            if(!is.na(input$provide_ts)){
                ts_range <- c(input$provide_ts,input$provide_ts)
            }

            ## Speed up
            popSize <- input$ELEFAN_GA_popSize
            if(popSize > 10) popSize <- 10
            maxiter <- input$ELEFAN_GA_maxiter
            if(maxiter > 5) maxiter <- 5
            run <- input$ELEFAN_GA_run
            if(run > 5) run <- 5

            ## Parameter/Input checks
            check.numeric.and.min(c(popSize = popSize,
                                    maxiter = maxiter,
                                    run = run,
                                    pmut = input$ELEFAN_GA_pmutation,
                                    pcross = input$ELEFAN_GA_pcrossover,
                                    elite = input$ELEFAN_GA_elitism,
                                    ma = input$ELEFAN_GA_MA,
                                    binSize = input$ELEFAN_GA_binSize,
                                    a = input$LWa,
                                    b = input$LWb,
                                    fRangeMax = input$fRangeMax),
                                  can.be.zero = FALSE)
            check.numeric.and.min(c(fRangeMin = input$fRangeMin),
                                  can.be.zero = TRUE)
            if(!is.na(input$provide_Linf)){
                if(!is.numeric(linf_range)){
                    stop(paste0("The provided Linf value is not numeric! Please check your input!"))
                }
                if(linf_range <= 0){
                    stop(paste0("The provided Linf value has to be larger than 0!"))
                }
            }
            if(!is.na(input$provide_K)){
                if(!is.numeric(k_range)){
                    stop(paste0("The provided K value is not numeric! Please check your input!"))
                }
                if(k_range <= 0){
                    stop(paste0("The provided K value has to be larger than 0!"))
                }
            }
            if(!is.na(input$provide_t_anchor)){
                if(!is.numeric(ta_range)){
                    stop(paste0("The provided time anchor (ta) is not numeric! Please check your input!"))
                }
                if(ta_range <= 0){
                    stop(paste0("The provided time anchor (ta) has to be larger than 0!"))
                }
            }

            ## Estimation check
            flog.info("Starting Elegan GA computation (check)")
            res <- run_tropfishr(x = inputElefanGaData$data,
                                 binSize = input$ELEFAN_GA_binSize,
                                 seasonalised = input$ELEFAN_GA_seasonalised,
                                 low_par = list(Linf = linf_range[1], K = k_range[1],
                                                t_anchor = ta_range[1],
                                                C = c_range[1],
                                                ts = ts_range[1]),
                                 up_par = list(Linf = linf_range[2], K = k_range[2],
                                               t_anchor = ta_range[2],
                                               C = c_range[2],
                                               ts = ts_range[2]),
                                 pmutation = input$ELEFAN_GA_pmutation,
                                 pcrossover = input$ELEFAN_GA_pcrossover,
                                 elitism = input$ELEFAN_GA_elitism,
                                 MA = input$ELEFAN_GA_MA,
                                 addl.sqrt = input$ELEFAN_GA_addlsqrt,
                                 years = input$ELEFAN_years_selected,
                                 agg = input$ELEFAN_agg,
                                 binSizeCC = input$ELEFAN_GA_binSize,
                                 yearsCC = yearsCC,
                                 LWa = input$LWa,
                                 LWb = input$LWb,
                                 natM_method = input$natM,
                                 temp = input$temp,
                                 cor_schooling = input$schooling,
                                 tmax = input$tmax,
                                 select_method = input$select,
                                 l50_user = input$l50_user,
                                 l75_user = input$l75_user,
                                 wqs_user = input$wqs_user,
                                 per_l1 = input$per_l1_user,
                                 l1 = input$l1_user,
                                 per_l2 = input$per_l2_user,
                                 l2 = input$l2_user,
                                 fRangeMin = input$fRangeMin,
                                 fRangeMax = input$fRangeMax,
                                 lcRangeMin = input$lcRangeMin,
                                 lcRangeMax = input$lcRangeMax,
                                 mat_method = input$selectMat,
                                 Lm50 = input$lm50_user,
                                 Lm75 = input$lm75_user,
                                 wqsm = input$wqsm_user,
                                 per_lm1 = input$per_lm1_user,
                                 lm1 = input$lm1_user,
                                 per_lm2 = input$per_lm2_user,
                                 lm2 = input$lm2_user,
                                 ## speed up for checking:
                                 popSize = popSize,
                                 maxiter = maxiter,
                                 run = run,
                                 fRangeSteps = 5,
                                 lcRangeSteps = 5,
                                 progressMessages = c("Checking ELEFAN","Checking YPR"),
                                 skip_elefan = input$provideGP,
                                 provide_linf = input$provide_Linf,
                                 provide_k = input$provide_K,
                                 provide_ta = input$provide_t_anchor
                                 )

            js$hideComputing()
            js$enableAllButtons()

        }, error = function(cond) {
            shinyjs::disable("go_ga")
            shinyjs::disable("createElefanGAReport")
            shinyjs::disable("createElefanGAzip")
            showModal(modalDialog(
                title = "Error",
                cond$message,
                easyClose = TRUE,
                footer = NULL
            ))
            return(NULL)
        },
      ##  warning = function(cond) {
      ##       showModal(modalDialog(
      ##           title = "Warning",
      ##           cond$message,
      ##           easyClose = TRUE,
      ##           footer = NULL
      ##       ))
      ##       return(NULL)
      ##   },
        finally = {
            js$hideComputing()
            js$enableAllButtons()
        })

        showNotification(
            "No errors during check run, ready to run assessment!",
            type = "message",
            duration = 60,
            closeButton = TRUE
        )
    })


    observeEvent(input$go_ga, {
        req(inputElefanGaData$data)

        res <- tryCatch({

            run_check()

            if(!elefanAcknowledged()) {

                elefanPendingRun(TRUE)

                showModal(modalDialog(
                    title = "Acknowledge model assumptions",
                    bsCollapse(id = ns("assumptions"), open = NULL,
                               bsCollapsePanel("▶ Click to show/hide",
                                               HTML(tropfishrAssumptionsHTML()))
                               ),
                    tags$p(HTML("See the <a href='https://elearning.fao.org/course/view.php?id=502' target='_blank'>FAO eLearning module</a> for more information.")),
                    footer = tagList(
                        modalButton("Cancel"),
                        actionButton(ns("elefan_ack"), "I Acknowledge", class = "btn-success")
                    ),
                    easyClose = FALSE
                ))

                return(invisible(NULL))
            }

            run_elefan_server()

        }, error = function(cond) {
            flog.error("Error in TropFishR: %s ",cond)
            showModal(modalDialog(
                title = "Error",
                cond$message,
                easyClose = TRUE,
                footer = NULL
            ))
        })
    })

    observeEvent(input$elefan_ack, {
        req(inputElefanGaData$data)
        removeModal()
        elefanAcknowledged(TRUE)

        js$showComputing()
        js$disableAllButtons()

        if (isTRUE(elefanPendingRun())) {
            elefanPendingRun(FALSE)
            run_elefan_server()
        }
    })

    observeEvent(input$reset_ga, {
        fileGaState$upload <- NULL
        resetElefanGaInputValues()
    })


    ## Information windows -----------------------
    observeEvent(input$workflowConsiderations, {
        showModal(modalDialog(
            title = "Workflow Considerations - TropFishR",
            HTML(getWorkflowConsiderationTextForElefan()),
            easyClose = TRUE,
            size = "l"
        ))
    }, ignoreInit = TRUE)

    observeEvent(input$dataConsiderations, {
        showModal(modalDialog(
            title = "Data Loading and Formatting Considerations - TropFishR",
            HTML(gsub("%%ELEFAN%%", "ELEFAN_GA",
                      getDataConsiderationTextForElefan())),
            easyClose = TRUE,
            size = "l"
        ))
    }, ignoreInit = TRUE)

    observeEvent(input$dataConsiderations2, {
        showModal(modalDialog(
            title = "Data Considerations - TropFishR",
            HTML(gsub("%%ELEFAN%%", "ELEFAN_GA",
                      getDataConsiderationTextForElefan())),
            easyClose = TRUE,
            size = "l"
        ))
    }, ignoreInit = TRUE)

    observeEvent(input$methodConsiderations, {
        showModal(modalDialog(
            title = "Methodological Considerations - TropFishR",
            HTML(gsub("%%ELEFAN%%", "ELEFAN_GA",
                      getMethodConsiderationTextForElefan())),
            easyClose = TRUE,
            size = "l"
        ))
    }, ignoreInit = TRUE)

    observeEvent(input$methodConsiderations2, {
        showModal(modalDialog(
            title = "Methodological Considerations - TropFishR",
            HTML(gsub("%%ELEFAN%%", "ELEFAN_GA",
                      getMethodConsiderationTextForElefan())),
            easyClose = TRUE,
            size = "l"
        ))
    }, ignoreInit = TRUE)

    observeEvent(input$resultConsiderations, {
        showModal(modalDialog(
            title = "Results Considerations - TropFishR",
            HTML(gsub("%%ELEFAN%%", "ELEFAN_GA",
                      getResultConsiderationTextForElefan())),
            easyClose = TRUE,
            size = "l"
        ))
    }, ignoreInit = TRUE)

    observeEvent(input$resultConsiderations2, {
        showModal(modalDialog(
            title = "Results Considerations - TropFishR",
            HTML(gsub("%%ELEFAN%%", "ELEFAN_GA",
                      getResultConsiderationTextForElefan())),
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

    observeEvent(input$info_csv_date, {
        showModal(modalDialog(
            title = "CSV date format",
            HTML("<p>Choose the csv file date format. By default, the app will try to guess the date format automatically by trying various combinations. This field allows you to select a specific decimal separator or to enter your own. In order to enter your own separator, just press backspace to delete the current value in the input field and type the date format in the required format, e.g. '%Y-%m-%d' for year-month-day separated by hyphen or '%d.%m.%y' by day.month.year separated by dots and where the year is given as the two last digits (e.g. 99 for 1999).</p>"),
            easyClose = TRUE,
            size = "l"
        ))
    }, ignoreInit = TRUE)

    observeEvent(input$infoYearSel, {
        showModal(modalDialog(
            title = "Selected years",
            HTML("<p>Select all or a range of years in the uploaded data set to be included in the analysis. <br><br> In theory, the longer the period covered by the data set, the better. However, data sets covering a long period with monthly aggregated data, might lead to a long run time and make the assumption that the growth parameters did not change over this period. In this case, you could consider to choose only the most recent years or choosing a quarterly aggregation of the data (see info button to 'Aggregate data by').</p>"),
            easyClose = TRUE,
            size = "l"
        ))
    }, ignoreInit = TRUE)

    observeEvent(input$infoAGG, {
        showModal(modalDialog(
            title = "Data aggregation",
            HTML("<p>Define whether the aggregation of the dataset should be kept ('none' is the default), or if the dataset should be aggregated by 'month', 'quarter', and 'year'. <br><br> Note that if 'month' is chosen, the data is assigned to the middle of respective sampling times (i.e. day 15 of each month). <br><br> In theory, the longer the period covered by the data set, the better. However, data sets covering a long period with monthly aggregated data, might lead to a long run time and might make the assumption that the growth parameters did not change over this period. Choosing only the most recent years or changing the aggregation 'quarter' and 'year' can be helpful to decrease computation time. <br><br> Note that a coarser (i.e. quarterly or yearly) aggregation reduces the information content of the data set. </p>"),
            easyClose = TRUE,
            size = "l"
        ))
    }, ignoreInit = TRUE)

    observeEvent(input$infoBS, {
        showModal(modalDialog(
            title = "Bin Size",
            HTML(paste0("<p>The bin size corresponds to the length interval over which the length frequency data are aggregated, for example 2 cm. <br><br> The combination of bin size and moving average (MA) critically affects the separation of peaks (i.e. potential cohorts) in the dataset and thus the estimation of growth parameters by ELEFAN. The bin size should be defined before defining the MA value. Ideally, the bin size is as small as possible, but large enough so that adjacent bins with high and low counts correspond to potential cohorts rather than noise. Wang et al. (2020) recommended defining the bin size dependent on the maximum length by ",withMathJax("\\(0.23 L_{max}^{0.6}\\)")," (default).</p>")),
            easyClose = TRUE,
            size = "l"
        ))
    }, ignoreInit = TRUE)

    observeEvent(input$infoMA, {
        showModal(modalDialog(
            title = "Moving average (MA)",
            HTML(paste0("<p>The moving average (", withMathJax("\\(MA\\)"), ") is a statistical operation that calculates a series of averages of different subsets of the full data set and is used in the restructuring of the length frequency data. The value indicates the number of length classes to be used in the calculation of the averages and must be a positive odd number (e.g. 5 or 7). <br><br>The combination of bin size and MA critically affects the separation of peaks (i.e. potential cohorts) in the dataset and, thus, the estimation of growth parameters by ELEFAN. Ideally, the MA value should be defined after defining the bin size and should lead to visually distinct peaks, particularly among small length classes. One option for the MA value is to set it equal to the number of length classes (bins) that potentially correspond to the youngest cohort.</p>")),
            easyClose = TRUE,
            size = "l"
        ))
    }, ignoreInit = TRUE)

    observeEvent(input$infoAT, {
        showModal(modalDialog(
            title = "Additional squareroot transformation",
            HTML(paste0("The additional squareroot transformation reduces the influence of low frequency values (Brey et al. 1988) and is defined by ",
                        withMathJax("\\(F_i = F_i / sqrt(1+2/F_i)\\)"),", where ",withMathJax("\\(F_i\\)")," is the frequency after the application of the moving average (MA) for length class i. This additional transformation might be useful if length frequency data includes many low values (Brey et al. 1988)." )),
            easyClose = TRUE,
            size = "l"
        ))
    }, ignoreInit = TRUE)

    observeEvent(input$info_searchSpace, {
        showModal(modalDialog(
            title = "Search space for growth parameters",
            HTML(paste0("<p>ELEFAN uses the Genetic Algorithm (GA) to find the set of von Bertalanffy growth (VBG) growth parameters (",withMathJax("\\(L_\\infty\\)"),", ",withMathJax("\\(K\\)"),",",withMathJax("\\(t_a\\)"),") which describe the growth curve that best fits the uploaded data set. In this tab, you can define the search space for each growth parameter. Note that the algorithm only searches within the defined parameter range. Thus, it is recommended to define a wider, rather than narrower, range. <br><br> By default, a reasonable range is defined for all parameters based on the input data uploaded. </p>")),
            easyClose = TRUE,
            size = "l"
        ))
    }, ignoreInit = TRUE)

    observeEvent(input$infolinf, {
        showModal(modalDialog(
            title = withMathJax("\\(L_\\infty\\)"),
            p(withMathJax("\\(L_\\infty\\)")," defines the asymptotic length of the von Bertalanffy growth (VBG) function. The default range is dependent upon the uploaded data set and defined as +/- 20% around the rough estimate of ",withMathJax("\\(L_\\infty = L_\\max/0.95\\)"),". Note that the maximum possible range is limited to +/- 75% of this estimate."),
            easyClose = TRUE,
            size = "l"
        ))
    }, ignoreInit = TRUE)

    observeEvent(input$infok, {
        showModal(modalDialog(
            title = withMathJax("\\(K\\)"),
            HTML(paste0("<p>The growth coefficient (", withMathJax("\\(K\\)"), ") of the von Bertalanffy growth (VBG) function determines the slope of the growth curve: a low ",withMathJax("\\(K\\)")," defines a slow-growing species and a high ",withMathJax("\\(K\\)")," defines a fast growing species. <br><br>If no prior knowledge about this life-history parameter is available, it is recommended to define a wide search space from 0.01 to 2-4. If prior information is available, a narrower range can be considered and would reduce the run time of the assessment.</p>")),
            easyClose = TRUE,
            size = "l"
        ))
    }, ignoreInit = TRUE)


    observeEvent(input$infotanchor, {
        showModal(modalDialog(
            title = withMathJax("\\(t_{a}\\)"),
            HTML(paste0(withMathJax("\\(t_{a}\\)"), " is the time point anchoring the growth curves in the year-length coordinate system, corresponding to the peak spawning month. In other words, it corresponds to the fraction of the year where yearly repeating growth curves cross length equal to zero; for example a value of 0.25 refers to April 1st of any year. Values for this field are between 0 and 1.")),
            easyClose = TRUE,
            size = "l"
        ))
    }, ignoreInit = TRUE)

    observeEvent(input$infoSeason, {
        showModal(modalDialog(
            title = "Seasonal model",
            p("This checkbox allows to use the seasonal model (or seasonalised von Bertalanffy growth (VBG) curve) which enables the calculation of the seasonal growth parameters (",
              withMathJax("\\(C\\)"), " and ", withMathJax("\\(t_{s}\\)"), "). The use of the seasonalised VBG is recommend if strong seasonality in the growth of the fish is expected, for instance, due to strong seasonal temperature differences in temperate regions. Note, that the estimation of the two additional parameters of the seasonalised VBG might increase the data requirements and, thus, uncertainty if the cohort signals in the data are poor." ),
            easyClose = TRUE,
            size = "l"
        ))
    }, ignoreInit = TRUE)

    observeEvent(input$infoC, {
        showModal(modalDialog(
            title = withMathJax("\\(C\\)"),
            p("The amplitude of the oscillation (", withMathJax("\\(C\\)"), ") of the seasonalised von Bertalanffy growth (VBG) curve. The higher the value of C, the more pronounced the seasonal oscillations are. C = 0 implies that there is no seasonality in the growth rate. If C = 1, the growth rate becomes zero at the winter point (",withMathJax("\\(WP = 0.5 - t_s\\)"),"). Values of C>1 would imply that the individuals shrink in length. Possible values for C are between 0 and 1, which is also equal to the default search space for this parameter. Generally, it is not recommended to decrease the search space for this parameter."),
            easyClose = TRUE,
            size = "l"
        ))
    }, ignoreInit = TRUE)

    observeEvent(input$infots, {
        showModal(modalDialog(
            title = withMathJax("\\(t_s\\)"),
            p("The summer point (", withMathJax("\\(t_{s}\\)"), ") of the seasonalised von Bertalanffy growth (VBG) curve defines the point in time where the seasonally varying growth rate is the largest represented by the fraction of the calendar year, e.g. 0.25 corresponds to April 1st. Values for this field are between 0 and 1, which is also equal to the default search space for this parameter. Generally, it is not recommended to decrease the search space for this parameter."),
            easyClose = TRUE,
            size = "l"
        ))
    }, ignoreInit = TRUE)

    observeEvent(input$provideGP, {
        showModal(modalDialog(
            title = "Provide growth parameters",
            p("It is possible to circumvent the estimation of growth parameters and use specified values instead. For this options, check the box before 'Provide growth parameters?' and enter any growth parameters in the appearing input fields. Note, that not all parameters (",withMathJax("\\(L_\\infty\\)"),", ",
              withMathJax("\\(K\\)"),", ",
              withMathJax("\\(t_a\\)"),") need to be specified. If only one or two parameters are specified, these parameters will be fixed and ELEFAN will be used to estimate missing parameters. If all three paramters are specified, the method will skip ELEFAN alltogether. "),
            easyClose = TRUE,
            size = "l"
        ))
    }, ignoreInit = TRUE)

    observeEvent(input$info_GA, {
        showModal(modalDialog(
            title = "ELEFAN's genetic algorithm",
            HTML(paste0("<p>Genetic algorithms (GAs) are stochastic search algorithms inspired by the basic principles of biological evolution and natural selection.  GAs simulate the evolution of living organisms, where the fittest individuals dominate over the weaker ones, by mimicking the biological mechanisms of evolution, such as selection, crossover and mutation.<br><br> Changing default parameters can have a substantial effect on the optimization process and, thus, on estimated growth parameters. Therefore, please apply caution when changing these parameters. In fact, values should only be increased from the default, though please note that this will increase the run time of the assessment.</p>")),
            easyClose = TRUE,
            size = "l"
        ))
    }, ignoreInit = TRUE)


    observeEvent(input$infoPopSize, {
        showModal(modalDialog(
            title = "Population size",
            HTML("Size of the inital population for the genetic algorithm. In theory, the higher the population size the better, however, large population size increase virtual memory demands substantially. Minimum is 50 and maximum 1000."),
            easyClose = TRUE,
            size = "l"
        ))
    }, ignoreInit = TRUE)


    observeEvent(input$infoMaxIter, {
        showModal(modalDialog(
            title = "Maximum number of iterations",
            HTML("Maximum number of iterations to run before the GA search is halted. Note that this parameter might affect the run time significantly. Minimum is 20 and maximum 1000."),
            easyClose = TRUE,
            size = "l"
        ))
    }, ignoreInit = TRUE)


    observeEvent(input$infoMaxRuns, {
        showModal(modalDialog(
            title = "Maximum number of runs",
            p("Number of consecutive generations without any improvement in the best fitness value before the GA is stopped. Note that this parameter might affect the run time significantly. Minimum is 10 and maximum 1000."),
            easyClose = TRUE,
            size = "l"
        ))
    }, ignoreInit = TRUE)


    observeEvent(input$infoPmut, {
        showModal(modalDialog(
            title = "Probability of mutation",
            HTML("The probability of mutation in a parent chromosome is used to maintain genetic diversity from one generation to the next. Usually mutation occurs with a small probability and is set to 0.2 by default. If it is set too high, the search will turn into a primitive random search. The probability has to be between 0 and 1."),
            easyClose = TRUE,
            size = "l"
        ))
    }, ignoreInit = TRUE)


    observeEvent(input$infoPcross, {
        showModal(modalDialog(
            title = "Probability of crossover",
            HTML("The probability of crossover between pairs of chromosomes is used to combine the genetic information of two parents to generate offspring. Typically this is a large value and is set to 0.8 by default. The probability has to be between 0 and 1."),
            easyClose = TRUE,
            size = "l"
        ))
    }, ignoreInit = TRUE)


    observeEvent(input$infoElite, {
        showModal(modalDialog(
            title = "Degree of elitism",
            HTML("Number of individuals of the parent generation with the best fitness values that survive to the next generation without changes. By default, the top 5% of individuals will survive at each iteration. Minimum is 1 and maximum 100."),
            easyClose = TRUE,
            size = "l"
        ))
    }, ignoreInit = TRUE)


    observeEvent(input$infoPred, {
        showModal(modalDialog(
            title = "Prediction range",
            HTML("<p>The prediction range determines the fishing mortality rates and length at 50% selectivity (L50) values which are used in the yield per recruit model. The model estimates yield per recruit (YPR), biomass per recruit (BPR), and spawning potential ratio (SPR; if maturity parameters are provided) for each combination of fishing mortality and L50 value. Thus, the prediction ranges (F and L50) affect the axes of Figures 7 and 8. <br> <br> The range for fishing mortality can be defined by the number of 'Steps' between the minimum ('Min') and maximum ('Max') mortality rate. <br> <br> If the selectivity is estimated (default), only the number of 'Steps' can be changed for the L50 range. If the selectivity parameters are provided (e.g. L50 and L75), the minimum ('Min') and maximum ('Max') of the L50 range can be changed.</p>"),
            easyClose = TRUE,
            size = "l"
        ))
    }, ignoreInit = TRUE)

    observeEvent(input$infoLengthWeight, {
        showModal(modalDialog(
            title = "Length-weight relationship",
            HTML(paste0("<p>The estimation of the yield and biomass per recruit requires information about the average weight per length class, which can be estimated with the length-weight relationship. A common assumption is the allometric relationship ",withMathJax("\\(W = a L^{b}\\)"),", with the constant <i>a</i> in ",
                        withMathJax("\\( g/cm^{3}\\)"), " and the exponent ",
                        withMathJax("\\( b\\)")," being unitless. <br><br>Ideally, the parameters are estimated based on length and weight measurements of the stock under study. Alternatively, information about the length-weight relationship of the species under study can be found on <a href='http://www.fishbase.org/search.php' target='blank_'> FishBase</a> or <a href='https://www.sealifebase.ca' target='blank_'> SeaLifeBase</a>  for invertebrates. By default the parameters are set to ",
                        withMathJax("\\(a = 0.01 g/cm^{3}\\)")," and ",
                        withMathJax("\\(b = 3\\)"),".",
                        "</p>")),
            easyClose = TRUE,
            size = "l"
        ))
    }, ignoreInit = TRUE)


    observeEvent(input$infoAdjData, {
        showModal(modalDialog(
            title = "Adjust data (other settings)",
            HTML(paste0("<p> Select the year(s) for the estimation of the stock status. The mortality rates estimated by the catch curve correspond to all years selected. If several years are selected, the samples for selected years are combined and the estimated rates correspond to the average values over all years selected. </p>")),
            easyClose = TRUE,
            size = "l"
        ))
    }, ignoreInit = TRUE)


    observeEvent(input$infoMat, {
        showModal(modalDialog(
            title = "Maturity (optional)",
            HTML("<p>If available, maturity information about your species can be provided and allows estimation of the current Spawning Potential Ratio (SPR) and SPR-related reference points. The model assumes a logistic maturity ogive. <br><br>Maturity information can be provided (i) in terms of the length at 50% and 75% maturity ('Define Lm50 & Lm75'), (ii) in terms of the length at 50% maturity and the maturation width, which is the difference between the length at 75% maturity and 25% maturity ('Define Lm50 & (Lm75-Lm25)'), or (iii) in terms of two specified lengths (LmX1 and LmX2) at specified probabilities of maturity (mX1 and mX2) ('Other'). <br><br>Ideally, maturity information is collected directly from the stock under study e.g. by determining the maturation states of the gonads. Alternatively, you may find maturity information about your species on <a href='http://www.fishbase.org/search.php' target='blank_'> FishBase</a> or <a href='https://www.sealifebase.ca' target='blank_'> SeaLifeBase</a>  for invertebrates.</p>"),
            easyClose = TRUE,
            size = "l"
        ))
    }, ignoreInit = TRUE)


    observeEvent(input$infoSelect, {
        showModal(modalDialog(
            title = "Gear selectivity",
            HTML("<p>The specifics of how fish are caught by fisheries and thus the probability of capture for fish of various length classes are dependent on the fishing gear, which is referred to as gear selectivity. Find more information about some examples of fishing gear selectivity <a href='http://www.fao.org/3/X7788E/X7788E00.htm' target='blank_'> here </a>. <br><br>TropFishR assumes a logistic gear selection ogive and allows the estimation of gear selectivity by means of the catch curve ('Estimate' is the default).  Alternatively, the gear selectivity can be defined (i) in terms of the length at 50% and 75% selection ('Define L50 & L75'), (ii) in terms of the length at 50% selection and the selection width, which is the difference between the length at 75% selection and 25% selection ('Define L50 & (L75-L25)'), or (iii) in terms of two specified lengths (LX1 and LX2) at specified probabilities of selection (X1 and X2) ('Other').  <br> <br> Note that estimated and specified selectivity corresponds to a logistic curve (trawl-like selectivity).</p>"),
            easyClose = TRUE,
            size = "l"
        ))
    }, ignoreInit = TRUE)


    observeEvent(input$infoNatM, {
        showModal(modalDialog(
            title = "Natural mortality",
            HTML("<p>The natural mortality rate (M) is required to estimate the fishing mortality (F) from the total mortality (Z) estimated by the catch curve (F = Z - M). The natural mortality is estimated by an empirical formula based on estimated growth parameters. The options are: <br> - Then's growth formula (<a href='https://doi.org/10.1093/icesjms/fsu136' target='_blank'>Then et al. 2015</a>), <br> - Pauly's growth and temperature formula (<a href='https://doi.org/10.1093/icesjms/39.2.175' target='_blank'>Pauly 1980</a>), <br> - Then's maximum age formula (<a href='https://doi.org/10.1093/icesjms/fsu136' target='_blank'>Then et al. 2015</a>); <br> - Gislason's length-based formula (<a href='https://doi.org/10.1111/j.1467-2979.2009.00350.x' target='_blank'>Gislason et al. 2010</a>); and <br> - Lorenzen's length-based formula (<a href='https://doi.org/10.1016/j.fishres.2022.106327' target='_blank'>Lorenzen et al. 2020</a>); <br><br> While the first option does not require any additional information, the second requires the average annual sea surface temperature (SST) in degrees Celsius and allows corrections for schooling fish (multiplication by 0.8). The third option requires an estimate of the maximum age of the fish.<br><br>Please see the Natural Mortality estimator page in the Supporting Tools menu for more information. The last two options estimate a length-dependent natural mortality, i.e. the natural mortality rate depends on the body size of the fish. Note, that for Gislason's method, length classes below 10cm are assumed to have a constant natural mortality rate correspoding to a body length of 10cm, as the meta study by Gislason et al. (2010) does not include many data points below 10cm. </p>"),
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



    ## Tour --------------------------------
    observeEvent(input$tour_general, {

        steps <- list(
            list(element = NA,
                 intro = paste0("This tour takes you through the main steps of the length-based stock assessment with TropFishR.<br><br>",
                                "Click 'Next' to continue.")),
            list(element = paste0("#", ns("file_wrapper")),
                 intro = "As a first step, you need to upload data. You can do that by clicking on 'Browse' and select a csv file on your computer."),
            list(element = paste0("#", ns("dataConsiderations2")),
                 intro = "If you do not have your own file and want to use an example fiel or if you are interested in more information about the data type and format, click on the small the information button here. <br><br>Note these information buttons (indicated by 'i') throughout the whole app."),
            list(element = paste0("#", ns("elefanGaCSVsep"),
                                  " + .selectize-control"),
                 intro = "By default, the app will try to recognize the field separator, but you can also specify it here by either choosing from the list or by pressing backspace and enter any separator."),
            list(element = paste0("#", ns("elefanGaCSVdec"),
                                  " + .selectize-control"),
                 intro = "Similarly, by default, the app will try to recognize the decimal separator, but you can also specify it here by either choosing from the list or by pressing backspace and enter any separator."),
            list(element = paste0("#", ns("elefanGaDateFormat"),
                                  " + .selectize-control"),
                 intro = "By default, the app will try to recognize the date format, but you can also specify it here."),
            list(element = paste0("#", ns("elefan_lengthUnit"),
                                  " + .selectize-control"),
                 intro = "By default, the app assumes that your length measurements are in centimeter (cm), but you can choose other length unit here. Currently, millimeter (mm) and inches (in) are implemented."),
            list(element = paste0("#", ns("box_settings")),
                 intro = "After you chose your data set and it was uploaded successfully (no error messages), you can explore your data and adjust settings in this box."),
            list(element = "#settings_elefan ul.nav.nav-tabs",
                 intro = "There are multiple tabs that allow you to adjust various aspects of the assessment method."),
            list(element = paste0("#", ns("tab1")),
                 intro = "For example, the first tab allows you to visually inspect the uploaded data and set important parameters, such as the bin size or moving average.<br><br> Remember the small information buttons ('i') next to the labels, if you need more information about these parameters.")
        )

        current_tab <- input$settings
        ## If tab 1 selected
        if (!is.null(current_tab) && current_tab == "data") {
            steps <- append(steps,
                            list(
                                list(element = paste0("#", ns("plot_explo1")),
                                     intro = paste0("A length frequency plot will be shown here when the data was uploaded successfully."))
                            ))
        }

        steps <- append(steps,
                        list(
                            list(element = paste0("#", ns("tab2")),
                             intro = "For the TropFishR assessment, default values and ranges are set for all parameters and you could theoretically, just go ahead and run the assement.<br><br> However, it is highly recommended to check and adjust the default settings in the other tabs, such as the search space for the genetic algorithm used to find the best parameter estimates for ELEFAN."),
                        list(element = paste0("#", ns("tab5")),
                             intro = "The last tab contains summary statistics and diagnostics of the uploaded data."),
                        list(element = paste0("#", ns("check_ga")),
                             intro = "As running the length-based stock assessment with TropFishR can take one minute or longer depending on the number of samples, you can first run a check. This would inform you if the required information is available and in the required format."),
                        list(element = paste0("#", ns("go_ga")),
                                  intro = "If the check is successful, you can run the length-based stock assessment by clicking here.<br><br>Note, that a pop-up window will ask you if you are aware and acknowledge the model assumptions."),
                        list(element = paste0("#", ns("reset_ga")),
                                  intro = "This button allows you to reset all settings.<br><br>Note, that this also removes your input data."),
                        list(element = paste0("#", ns("createElefanGAReport")),
                             intro = "This button creates and downloads an automatic assessment report with information about your data, settings and results."),
                        list(element = paste0("#", ns("createElefanGAzip")),
                             intro = "You can also download all graphs and tables in a zip archive by clicking here.")))

        if (!is.null(current_tab) && current_tab == "data") {
            steps <- append(steps,
                            list(
                                list(element = paste0("#", ns("fig_format_ga"),
                                                      " + .selectize-control"),
                                     intro = paste0("You can select the format of the figures in the zip archive here.")),
                                list(element = paste0("#", ns("tab_format_ga"),
                                                      " + .selectize-control"),
                                     intro = paste0("And the format of the tables here."))
                            ))
        }

        steps <- append(steps,
                        list(
                            list(element = paste0("#", ns("tour_res")),
                                 intro = "The results tour might be helpful to get an overview over the results.<br><br> Note, that the tour only makes sense after TropFishR was run successfully."),
                            list(element = NA,
                                 intro = "This concludes the TropFishR tour.<br><br>Remember the information buttons ('i') that might be helpful when uploading data, adjusting settings or interpreting results."),
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

        steps <- list(
            list(element = NA,
                 intro = "This is a tour that takes you through the results of the length-based stock assessment with TropFishR.")
        )

        if(is.null(elefan_ga$results)) {

            steps <- append(steps,
                            list(
                                list(element = NA,
                                     intro = "No results found. This tour only works if you run the assessment."),
                                list(element = paste0("#", ns("go_ga")),
                                     intro = "Make sure you uploaded your data and run the assessment by clicking here."),
                                list(element = NA,
                                     intro = "Start this tour again after you see some tables and graphs below.")))

        } else {

            steps <- append(steps,
                            list(
                                list(element = paste0("#", ns("plot_growthCurves")),
                                     intro = "This figure displays the estimated growth curves overlaid on the raw (A) and restructured (B) length frequency data. It provides a useful visual indication of how well the estimated growth parameters fit the data. Ideally, the blue growth curves should pass through the main peaks in the length frequency distributions, reflecting a good alignment between the model and the observed data."),
                                list(element = paste0("#", ns("table_growth")),
                                     intro = "The estimated growth parameters are shown in this table."),
                                list(element = paste0("#", ns("plot_elefanFit")),
                                     intro = "This graph illustrates the parameter estimation process of the genetic algorithm. It shows how the best and average fitness scores evolved over successive generations. Ideally, the number of generations is sufficient to allow the algorithm to converge, meaning that no large fluctuations in the best or average scores are observed in the final iterations."),
                                list(element = paste0("#", ns("plot_mort")),
                                     intro = "This graph shows the estimated natural (green), fishing (orange), and total mortality (blue) by length. It highlights that the method assumes a constant natural mortality value by length and a sigmoidal shape of fishing and total mortality."),
                                list(element = paste0("#", ns("plot_catchCurve")),
                                     intro = "The catch curve plot shows the length classes which were used for the estimation of total mortality (blue points) and how well the regression line fits through the points."),
                                list(element = paste0("#", ns("plot_select")),
                                     intro = "This graph shows the selection ogive as the probability of capture as a function of length as estimated based on the length-converted catch curve."),
                                list(element = paste0("#", ns("table_mort")),
                                     intro = "This table contains the estimated mortality rates (Z, M, F), exploitation rate (E = F/Z), and the selection parameters."),
                                list(element = paste0("#", ns("table_refs")),
                                     intro = "This table shows the estimated reference points of the yield-per-recruit analysis. Fmax is the fishing mortality maximizing the yield, F0.1 is the fishing mortality corresponding to 10% of the slope of the origin, and F0.5 is the fishing mortality reducing the biomass by 50%.<br><br> Note, that if maturity parameters are provided, this table also includes the SPR-related reference points (F30, F35, F40)."),
                                list(element = paste0("#", ns("plot_ypr")),
                                     intro = "This graph contains the yield per recruit (A) and biomass per recruit (B) for different fishing mortality rates.<br><br>If maturity parameters are provided panel C contains the spawning potential ratio (SPR) for the fishing mortality values."),
                                list(element = paste0("#", ns("plot_ypr_iso")),
                                     intro = "This plot shows the yield per recriut (A) and biomass per recruit (B) for different fishing mortality values (x axis) and and gear selectivity(y axis) combinations. The current value is indicated by the dashed lines and a circle."),
                                list(element = paste0("#", ns("table_stockstatus")),
                                     intro = "This table summarizes the current stock status in terms of fishing mortality (F) relative to reference points.<br><br> If the maturity parameters are specified, it also includes SPR-related reference points and the current spawning potential ratio (SPR)."),
                                list(element = paste0("#", ns("table_forOtherMethods")),
                                     intro = "This table summarizes the main biological parameters which might be used as input to the length-based indicators (LBI) and length-based spawning potential ratio method (LBSPR) in the other tabs of this app."),
                                list(
                                    element = paste0("#",ns("resultConsiderations")),
                                    intro = paste0(
                                        "This concludes the tour through the TropFishR results.<br><br> For more and detailed information, click on this button and refer to the figure and table captions."))))

        }

        later::later(function() {
            introjs(session, options = list(steps = steps))
        }, delay = 0.5)
    }, ignoreInit = TRUE)



    ## Plots
    ## --------------------------

    ## Data exploration
    ## --------------------------
    output$plot_explo1 <- renderPlot({
        req(inputElefanGaData$data, input$ELEFAN_years_selected)
        elefan_ga$dataExplo[['lfq']] <- elefanGaDataExplo1()
        elefan_ga$dataExplo[['lfqbin']] <- elefanGaDataExplo2()
        if(is.matrix(elefan_ga$dataExplo$lfq$catch)) {
            plotTropFishR.data(elefan_ga, input)
        } else {
            NULL
        }
    },
    width = "auto",
    height = "auto",
    res = 100)
    output$title_explo1 <- renderText({
        req(inputElefanGaData$data, input$ELEFAN_years_selected)
        captionTropFishR.plots(elefan_ga, input, format = "withFig", type = "explo")
    })

    ## Data diagnostics
    ## --------------------------
    output$plot_diag1 <- renderPlot({
        req(inputElefanGaData$data, input$ELEFAN_years_selected)
        elefan_ga$dataExplo[['lfq']] <- elefanGaDataExplo1()
        elefan_ga$dataExplo[['lfqbin']] <- elefanGaDataExplo2()
        elefan_ga$dataExplo[['raw']] <- inputElefanGaData$raw
        elefan_ga$dataExplo[['checks']] <- inputElefanGaData$checks
        plotTropFishR.diag1(elefan_ga, input)
    },
    width = "auto",
    height = "auto",
    res = 100)
    output$title_diag1 <- renderText({
        req(inputElefanGaData$data, input$ELEFAN_years_selected)
        captionTropFishR.plots(elefan_ga, input, format = "withFig", type = "diag1")
    })

    output$plot_diag2 <- renderPlot({
        req(inputElefanGaData$data, input$ELEFAN_years_selected)
        elefan_ga$dataExplo[['lfq']] <- elefanGaDataExplo1()
        elefan_ga$dataExplo[['lfqbin']] <- elefanGaDataExplo2()
        elefan_ga$dataExplo[['raw']] <- inputElefanGaData$raw
        elefan_ga$dataExplo[['checks']] <- inputElefanGaData$checks
        plotTropFishR.diag2(elefan_ga, input)
    },
    width = "auto",
    height = "auto",
    res = 100)
    output$title_diag2 <- renderText({
        req(inputElefanGaData$data, input$ELEFAN_years_selected)
        captionTropFishR.plots(elefan_ga, input, format = "withFig", type = "diag2")
    })

    output$plot_diag3 <- renderPlot({
        req(inputElefanGaData$data, input$ELEFAN_years_selected)
        elefan_ga$dataExplo[['lfq']] <- elefanGaDataExplo1()
        elefan_ga$dataExplo[['lfqbin']] <- elefanGaDataExplo2()
        elefan_ga$dataExplo[['raw']] <- inputElefanGaData$raw
        elefan_ga$dataExplo[['checks']] <- inputElefanGaData$checks
        plotTropFishR.diag3(elefan_ga, input)
    },
    width = "auto",
    height = "auto",
    res = 100)
    output$title_diag3 <- renderText({
        req(inputElefanGaData$data, input$ELEFAN_years_selected)
        captionTropFishR.plots(elefan_ga, input, format = "withFig", type = "diag3")
    })

    ## Growth
    ## --------------------------
    output$plot_growthCurves <- renderPlot({
        req(elefan_ga$results)
        plotTropFishR.growth(elefan_ga, input)
    },
    width = "auto",
    height = "auto",
    res = 100)
    output$title_growthCurves <- renderText({
        req(elefan_ga$results)
        captionTropFishR.plots(elefan_ga, input, format = "withFig", type = "growth")
    })

    ## ELEFAN fit
    ## --------------------------
    output$plot_elefanFit <- renderPlot({
        req(elefan_ga$results)
        plotTropFishR.ga(elefan_ga, input)
    },
    width = "auto",
    height = "auto",
    res = 100)
    output$title_elefanFit <- renderText({
        req(elefan_ga$results)
        captionTropFishR.plots(elefan_ga, input, format = "withFig", type = "ga")
    })

    ## Mortality
    ## --------------------------
    output$plot_mort <- renderPlot({
        req(elefan_ga$results)
        plotTropFishR.mort(elefan_ga, input)
    },
    width = "auto",
    height = "auto",
    res = 100)
    output$title_mort <- renderText({
        req(elefan_ga$results)
        captionTropFishR.plots(elefan_ga, input, format = "withFig", type = "mort")
    })

    ## Catch curve
    ## --------------------------
    output$plot_catchCurve <- renderPlot({
        req(elefan_ga$results)
        plotTropFishR.catchcurve(elefan_ga, input)
    },
    width = "auto",
    height = "auto",
    res = 100)
    output$title_catchCurve <- renderText({
        req(elefan_ga$results)
        captionTropFishR.plots(elefan_ga, input, format = "withFig", type = "cc")
    })

    ## Selectivity
    ## --------------------------
    output$plot_select <- renderPlot({
        req(elefan_ga$results)
        plotTropFishR.sel(elefan_ga, input)
    },
    width = "auto",
    height = "auto",
    res = 100)
    output$title_select <- renderText({
        req(elefan_ga$results)
        captionTropFishR.plots(elefan_ga, input, format = "withFig", type = "sel")
    })

    ## YPR
    ## --------------------------
    output$plot_ypr <- renderPlot({
        req(elefan_ga$results)
        plotTropFishR.ypr(elefan_ga, input)
    },
    width = "auto",
    height = "auto",
    res = 100)
    output$title_ypr <- renderText({
        req(elefan_ga$results)
        captionTropFishR.plots(elefan_ga, input, format = "withFig", type = "ypr")
    })

    ## YPR-Isopleth
    ## --------------------------
    output$plot_ypr_iso <- renderPlot({
        req(elefan_ga$results)
        plotTropFishR.iso(elefan_ga, input)
    },
    width = "auto",
    height = "auto",
    res = 100)
    output$title_ypr_iso <- renderText({
        req(elefan_ga$results)
        captionTropFishR.plots(elefan_ga, input, format = "withFig", type = "iso")
    })


    ## Tables
    ## --------------------------

    ## Growth parameter
    ## --------------------------
    output$table_growth <- renderDataTable({
        req(elefan_ga$results)
        tableTropFishR.growth(elefan_ga, input, format = "datatable")
    })
    output$title_table_growth <- renderText({
        req(elefan_ga$results)
        captionTropFishR.tables(elefan_ga, input, format = "datatable",
                                type = "growth")
    })

    ## Mortality rates
    ## --------------------------
    output$table_mort <- renderDataTable({
        req(elefan_ga$results)
        tableTropFishR.mort(elefan_ga, input, format = "datatable")
    })
    output$title_table_mort <- renderText({
        req(elefan_ga$results)
        captionTropFishR.tables(elefan_ga, input, format = "datatable",
                                type = "mort")
    })

    ## Reference points
    ## --------------------------
    output$table_refs <- renderDataTable({
        req(elefan_ga$results)
        tableTropFishR.refs(elefan_ga, input, format = "datatable")
    })
    output$title_table_refs <- renderText({
        req(elefan_ga$results)
        captionTropFishR.tables(elefan_ga, input, format = "datatable",
                                type = "refs")
    })

    ## Stock status
    ## --------------------------
    output$table_stockstatus <- renderDataTable({
        req(elefan_ga$results)
        tableTropFishR.status(elefan_ga, input, format = "datatable")
    })
    output$title_table_stockstatus <- renderText({
        req(elefan_ga$results)
        captionTropFishR.tables(elefan_ga, input, format = "datatable",
                                type = "status")
    })

    ## Parameters for other values
    ## --------------------------
    output$table_forOtherMethods <- renderDataTable({
        req(elefan_ga$results)
        tableTropFishR.forOtherMethods(elefan_ga, input, format = "datatable")
    })
    output$title_table_forOtherMethods <- renderText({
        req(elefan_ga$results)
        captionTropFishR.tables(elefan_ga, input, format = "datatable",
                                type = "forOtherMethods")
    })


    ## Text
    ## --------------------------

    ## Data diagnostics
    ## --------------------------
    output$text_diag1 <- renderUI({
        req(inputElefanGaData$data, input$ELEFAN_years_selected)
        elefan_ga$dataExplo[['lfq']] <- elefanGaDataExplo1()
        elefan_ga$dataExplo[['lfqbin']] <- elefanGaDataExplo2()
        elefan_ga$dataExplo[['raw']] <- inputElefanGaData$raw
        elefan_ga$dataExplo[['checks']] <- inputElefanGaData$checks
        textTropFishR.diag1(elefan_ga, input)
    })


    ## Files
    ## --------------------------
    output$createElefanGAReport <- downloadHandler(
        filename = paste("ElefanGA_report_",
                         format(Sys.time(),
                                "%Y%m%d_%H%M_%s"),".pdf",sep=""),
        content = function(file) {
            createElefanGaPDFReport(file, elefan_ga, input, output)
        }
    )

    output$createElefanGAzip <- downloadHandler(
        filename = paste("TropFishR_results_",
                         format(Sys.time(), "%Y%m%d_%H%M_%s"),".zip",sep=""),
        content = function(file) {
            makeContentElefanGAzip(file, elefan_ga, input, output)
        },
        contentType = "application/zip"
    )

    output$elefanGaTitle <- renderText({
        session$userData$page("elefan-ga")
        text <- "<span><h3><b>Length-based stock assessment with TropFishR</b></h3></span>"
        text
    })


}
