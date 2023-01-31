elefanGaModule <- function(input, output, session) {

    ns <- session$ns

    ## Definition of reactive values
    ## ----------------------------
    elefan_ga <- reactiveValues(
        dataExplo = NULL,
        results = NULL
    )

    elefanGaUploadVreResult <- reactiveValues()

    inputElefanGaData <- reactiveValues()

    fileGaState <- reactiveValues(
        upload = NULL
    )


    ## Definition of functions
    ## ----------------------------
    elefanGaFileData <- reactive({
        if (is.null(input$fileGa) || is.null(fileGaState$upload)) {
            return(NA)
        }

        dataset <- read_lbm_csv(input$fileGa$datapath, input$elefanGaDateFormat)
        checks <- dataset$checks

        print(input$fileGa)

        if (is.null(dataset$lfq)) {
            shinyjs::disable("go_ga")
            shinyjs::disable("check_ga")
            showModal(modalDialog(
                title = "Error",
                if(!checks$csv){
                    "Something went wrong when reading in your data set. Did you select a CSV file (i.e. file with ending '.csv')? Click on the info icon for more information."
                }else if(!checks$delimiter){
                    "Something went wrong when reading in your data set. Please ensure that your CSV file delimiter is a comma ',' or semicolon ';'. Click on the info icon for more information."
                }else if(!checks$lengths){
                    "The column with length classes is not in the right format or not numeric. Please ensure that the first column of uploaded data set includes the length classes and is numeric. Furthermore, please make sure that the decimal separator is a dot '.', by selecting '.' when saving the csv file or by changing your language settings in your program (e.g. Excel). Click on the info icon for more information."
                }else if(!checks$dates){
                    "Does your data set include colums with the number of individuals per length class for a given sampling date? The name of these columns need to indicate the sampling date (e.g. '21.08.2020' or '2020-08-21'). The dates might start with the letter 'X' (e.g. 'X2020-08-21'). Click on the info icon for more information."
                }else if(!checks$ncols){
                    "Uploaded data set does not include enough numeric samples. Does your data set include at least two columns with numeric values representing the catches per length class for a given sampling date? Click on the info icon for more information."
                }else{
                    "There was an unexpected error when reading in your data set. Please double-check your data set and refer to the info button for more help. "
                },
                easyClose = TRUE,
                footer = NULL
            ))
            return (NULL)
        } else {
            shinyjs::enable("go_ga")
            shinyjs::enable("check_ga")
            return (dataset$lfq)
        }
    })

    elefanGaDataExplo1 <- reactive({
        req(inputElefanGaData$data)

        res <- tryCatch({
            elefan_ga$dataExplo <- list()
            years <- input$ELEFAN_years_selected
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

            binSize <- input$ELEFAN_GA_binSize
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
                             ##                         plus_group = input$ELEFAN_GA_PLUS_GROUP,
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

            lfqbin <- lfqRestructure(elefan_ga$dataExplo$lfq,
                                     MA = ma,
                                     addl.sqrt = input$ELEFAN_GA_addlsqrt)
            return(lfqbin)

        }, error = function(cond) {
            shinyjs::disable("go_ga")
            shinyjs::disable("check_ga")
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

        ## resetting reactive values
        elefan_ga$dataExplo <- NULL
        elefan_ga$results <- NULL
        inputElefanGaData$data <- NULL
        fileGaState$upload <- NULL
        ## elefanGaUploadVreResult ?
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
        }else if(input$select == "Define L50 & L75"){
            shinyjs::show("ui_l50", asis = TRUE)
            shinyjs::hide("ui_wqs", asis = TRUE)
            shinyjs::show("ui_l75", asis = TRUE)
            shinyjs::show("ui_lcMin", asis=TRUE)
            shinyjs::show("ui_lcMax", asis=TRUE)
            shinyjs::hide("ui_per_l1", asis = TRUE)
            shinyjs::hide("ui_l1", asis = TRUE)
            shinyjs::hide("ui_per_l2", asis = TRUE)
            shinyjs::hide("ui_l2", asis = TRUE)
        }else if(input$select == "Define L50 & (L75-L25)"){
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
        inputElefanGaData$data <- elefanGaFileData()
    })

    observeEvent(input$elefanGaDateFormat, {
        inputElefanGaData$data <- elefanGaFileData()
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

            if (!is.null(session$userData$sessionMode()) && session$userData$sessionMode()=="GCUBE") {
                flog.info("Uploading Elefan GA report to i-Marine workspace")
                reportFileName <- paste(tempdir(),"/","ElefanGA_report_",format(Sys.time(), "%Y%m%d_%H%M_%s"),".pdf",sep="")
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
        })

    })


    observeEvent(input$reset_ga, {
        fileGaState$upload <- NULL
        resetElefanGaInputValues()
    })


    ## Plots
    ## --------------------------

    ## Data exploration
    ## --------------------------
    output$plot_explo1 <- renderPlot({
        req(inputElefanGaData$data, input$ELEFAN_years_selected)
        elefan_ga$dataExplo[['lfq']] <- elefanGaDataExplo1()
        elefan_ga$dataExplo[['lfqbin']] <- elefanGaDataExplo2()
        plotTropFishR.data(elefan_ga, input)
    },
    width = "auto",
    height = "auto",
    res = 100)
    output$title_explo1 <- renderText({
        req(inputElefanGaData$data, input$ELEFAN_years_selected)
        captionTropFishR.plots(elefan_ga, input, format = "withFig", type = "explo")
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



    output$downloadReport_ga <- renderUI({
        req(elefan_ga$results)
        downloadButton(session$ns('createElefanGAReport'), 'Download Report')
    })

    output$downloadzip_ga <- renderUI({
        req(elefan_ga$results)
        downloadButton(session$ns('createElefanGAzip'), 'Download Results (zip)')
    })


    output$zip_fig_format_ga <- renderUI({
        req(elefan_ga$results)
        selectInput(session$ns("fig_format_ga"),
                    "Format of archived figures",
                    choices = c("pdf","png","jpeg","tiff","bmp","ps"),
                    selected = "pdf",
                    multiple = FALSE,
                    width = "60%")
    })



    output$ElefanGaVREUpload <- renderText({
        text <- ""
        req(elefan_ga$results)
        if (!is.null(session$userData$sessionMode()) && session$userData$sessionMode() == "GCUBE") {
            if (isTRUE(elefanGaUploadVreResult$res)) {
                text <- paste0(text, VREUploadText)
            }
        }
        text
    })

    output$createElefanGAReport <- downloadHandler(
        filename = paste("ElefanGA_report_",format(Sys.time(), "%Y%m%d_%H%M_%s"),".pdf",sep=""),
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


    output$elefanGAWorkflowConsiderationsText <- renderText({
        text <- getWorkflowConsiderationTextForElefan()
        text
    })

    output$elefanGADataConsiderationsText <- renderText({
        text <- gsub("%%ELEFAN%%", "ELEFAN_GA", getDataConsiderationTextForElefan())
        text
    })
    output$elefanGADataConsiderationsText2 <- renderText({
        text <- gsub("%%ELEFAN%%", "ELEFAN_GA", getDataConsiderationTextForElefan())
        text
    })

    output$elefanGAmethodConsiderationsText <- renderText({
        text <- gsub("%%ELEFAN%%", "ELEFAN_GA", getMethodConsiderationTextForElefan())
        text
    })
    output$elefanGAmethodConsiderationsText2 <- renderText({
        text <- gsub("%%ELEFAN%%", "ELEFAN_GA", getMethodConsiderationTextForElefan())
        text
    })

    output$elefanGAresultConsiderationsText <- renderText({
        text <- gsub("%%ELEFAN%%", "ELEFAN_GA", getResultConsiderationTextForElefan())
        text
    })
    output$elefanGAresultConsiderationsText2 <- renderText({
        text <- gsub("%%ELEFAN%%", "ELEFAN_GA", getResultConsiderationTextForElefan())
        text
    })


    output$elefanGaTitle <- renderText({
        session$userData$page("elefan-ga")
        text <- "<span><h3><b>Length-based stock assessment with TropFishR</b></h3></span>"
        text
    })


}
