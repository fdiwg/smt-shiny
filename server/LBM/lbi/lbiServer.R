lbiModule <- function(input, output, session) {

    ns <- session$ns


    ## Definition of reactive values
    ## ----------------------------
    lbi_dat <- reactiveValues(
        dataExplo = NULL,
        results = NULL,
        years_selected = NULL,
        binSize = NULL
    )
    lbiUploadVreResult <- reactiveValues()

    inputLBIData <- reactiveValues()

    fileLBIState <- reactiveValues(
        upload = NULL
    )

    lbiAcknowledged <- reactiveVal(FALSE)
    lbiPendingRun   <- reactiveVal(FALSE)

    ## Definition of functions
    ## ----------------------------
    lbiFileData <- function(){

        if (is.null(input$fileLBI) || is.null(fileLBIState$upload)) {
            return(NULL)
        }

        withCallingHandlers(
            dataset <- tryCatch({
                read_lbm_csv(input$fileLBI$datapath,
                             input$lbiDateFormat,
                                        input$lbiCSVsep,
                                        input$lbiCSVdec)

                dataset$checks$fileName <- input$fileLBI$name

            }, error = function(e) {
                shinyjs::disable("go_ga")
                shinyjs::disable("check_ga")
                shinyjs::disable("createElefanGAReport")
                shinyjs::disable("createElefanGAzip")
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

        checks <- dataset$checks

        if (is.null(dataset$lfq)) {
            shinyjs::disable("go_lbi")
            shinyjs::disable("createLBIReport")
            shinyjs::disable("createLBIzip")
            errMessage <- if(!is.null(checks) && !checks$csv){
                              "Something went wrong when reading in your data set. Did you select a CSV file (i.e. file with ending '.csv')? Click on the info icon for more information."
                          }else if(!is.null(checks) && !checks$delimiter){
                              "Something went wrong when reading in your data set. Please ensure that your CSV file delimiter is a comma ',' or semicolon ';'. Click on the info icon for more information."
                          }else if(!is.null(checks) && !checks$lengths){
                              "The column with length classes is not in the right format or not numeric. Please ensure that the first column of uploaded data set includes the length classes and is numeric. Furthermore, please make sure that the decimal separator is a dot '.', by selecting '.' when saving the csv file or by changing your language settings in your program (e.g. Excel). Click on the info icon for more information."
                          }else if(!is.null(checks) && !checks$dates){
                              "Does your data set include colums with the number of individuals per length class for a given sampling date? The name of these columns need to indicate the sampling date (e.g. '21.08.2020' or '2020-08-21'). The dates might start with the letter 'X' (e.g. 'X2020-08-21'). Click on the info icon for more information."
                          }else if(!is.null(checks) && !checks$ncols){
                              "Uploaded data set does not include enough numeric samples. Does your data set include at least two columns with numeric values representing the catches per length class for a given sampling date? Click on the info icon for more information."
                          }else{
                              "There was an unexpected error when reading in your data set. Did you upload the correct data set? Does your data set fulfill the data and format requirements of this method? Please double-check your data set, have a look at the example data sets, and refer to the info button for more help."
                          }
            print(errMessage)
            showModal(modalDialog(
                title = "Error",
                errMessage,
                easyClose = TRUE,
                footer = NULL
            ))
            return (NULL)
        } else {
            shinyjs::enable("go_lbi")
            res <- list(lfq = dataset$lfq,
                        raw = dataset$raw,
                        checks = dataset$checks)
            return (res)
        }
    }

    lbiDataExplo1 <- reactive({
        req(inputLBIData$data)
        req(lbi_dat$years_selected)
        req(lbi_dat$binSize)

        lbi_dat$dataExplo <- list()
        years <- lbi_dat$years_selected
        if(is.null(years)){
            stop("No year selected for the analysis. Please select at least one year of uploaded data set.")
        }
        dat <- inputLBIData$data
        class(dat) <- "lfq"

        binSize <- lbi_dat$binSize
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

        suppressWarnings({lfq <- lfqModify(dat,
                                           bin_size = binSize,
                                           years = years,
                         aggregate = "year")})

        ## Leave in original unit!
        ## Account for length unit
        ## if(input$lbi_lengthUnit == "mm"){
        ##     lfq$midLengths <- lfq$midLengths * 10
        ## }else if(input$lbi_lengthUnit == "in"){
        ##     lfq$midLengths <- lfq$midLengths * 2.54
        ## }

        return(lfq)
    })

    resetLBIInputValues <- function() {
        ## resetting UIs
        shinyjs::reset("fileLBI")
        shinyjs::reset("lbiCSVsep")
        shinyjs::reset("lbiCSVdec")
        shinyjs::reset("lbiDateFormat")
        shinyjs::reset("LBI_years_selected")
        shinyjs::reset("LBI_binSize")
        shinyjs::reset("LBI_Linf")
        shinyjs::reset("LBI_Lm50")
        shinyjs::reset("LBI_MK")
        shinyjs::reset("LBI_split_mk")
        shinyjs::reset("LBI_M")
        shinyjs::reset("LBI_K")
        shinyjs::reset("LBI_LWa")
        shinyjs::reset("LBI_LWb")

        ## disable buttons
        shinyjs::disable("go_lbi")
        shinyjs::disable("createLBIReport")
        shinyjs::disable("createLBIzip")

        ## resetting reactive values
        lbi_dat$dataExplo <- NULL
        lbi_dat$results <- NULL
        lbi_dat$years_selected <- NULL
        lbi_dat$binSize <- NULL
        inputLBIData$data <- NULL
        fileLBIState$upload <- NULL
        ## lbiUploadVreResult ?
        ## lbiAcknowledged(FALSE)
    }


    run_lbi_server <- function() {

        result = tryCatch({

            ## no weight at length -> no Lmaxy and Lmaxy_Lopt
            ## TODO: but how to enter weight in SMT?
            ## ADD: weight-at-length input
            lbi_dat$dataExplo[['lfq']]$weight <- input$LBI_LWa * lbi_dat$dataExplo[['lfq']]$midLengths ^ input$LBI_LWb

            if(input$LBI_split_mk){
                mk <- input$LBI_M / input$LBI_K
            }else{
                mk <- input$LBI_MK
            }

            ## Warnings
            years <- as.numeric(format(lbi_dat$dataExplo[['lfq']]$dates, "%Y"))
            if(any(duplicated(years))){
                showModal(modalDialog(
                    title = "Warning",
                    HTML("The length frequency data is not aggregated by year. This method should be used with yearly aggregated data!<hr/>")))
            }

            ## Parameter/Input checks
            check.numeric.and.min(c(binSize = input$LBI_binSize,
                                    linf = input$LBI_Linf,
                                    lm50 = input$LBI_Lm50,
                                    mk = mk
                                    ),
                                  can.be.zero = FALSE)

            ## LBIs
            flog.info("Starting LBI computation")
            res <- run_lbi(data = lbi_dat$dataExplo[['lfq']],
                           bin.size = input$LBI_binSize,
                           linf = input$LBI_Linf,
                           lmat = input$LBI_Lm50,
                           mk = mk
                           )

            js$hideComputing()
            js$enableAllButtons()

            lbi_dat$results <- res$res

            if (!is.null(session$userData$sessionMode()) && session$userData$sessionMode()=="GCUBE") {
                flog.info("Uploading LBI report to i-Marine workspace")
                reportFileName <- paste(tempdir(),"/","LBI_report_",
                                        format(Sys.time(), "%Y%m%d_%H%M_%s"),".pdf",sep="")
                                        #createLBIPDFReport(reportFileName,lbi_dat,input)
                createLBIPDFReport(reportFileName, lbi_dat, input, output)
                lbiUploadVreResult$res <- FALSE

                basePath <- paste0("/Home/",session$userData$sessionUsername(),"/Workspace/")

                SH_MANAGER <- session$userData$storagehubManager()

                tryCatch({
                    uploadToIMarineFolder(SH_MANAGER, reportFileName, basePath, uploadFolderName)
                    lbiUploadVreResult$res <- TRUE
                }, error = function(err) {
                    flog.error("Error uploading LBI report to the i-Marine Workspace: %s", err)
                    lbiUploadVreResult$res <- FALSE
                }, finally = {})
            }

        }, error = function(cond) {
            flog.error("Error in LBI: %s ",cond)
            showModal(modalDialog(
                title = "Error",
                cond$message,
                easyClose = TRUE,
                footer = NULL
            ))
            return(NULL)
        },
        finally = {
            js$hideComputing()
            js$enableAllButtons()
            if (!is.null(lbi_dat$results)) {
                shinyjs::enable("createLBIReport")
                shinyjs::enable("createLBIzip")
            }
        })
    }


    ## Input-dependent UIs
    ## ----------------------------
    output$LBI_years_selected_out <- renderUI({
        if(is.null(inputLBIData$data)){
            allyears <- NULL
        }else{
            allyears <- try(unique(format(inputLBIData$data$dates,"%Y")),silent=TRUE)
            if(inherits(allyears,"try-error")) allyears <- NULL
        }
        selectInput(ns("LBI_years_selected"), "",
                    choices = allyears, selected = allyears,
                    multiple = TRUE,
                    width = "100%")
    })

    output$LBI_binSize_out <- renderUI({
        if(is.null(inputLBIData$data)){
            binSize <- 2
            maxL <- 10
        }else{
            binSize <- try(min(diff(inputLBIData$data$midLengths)),silent=TRUE)
            maxL <- try(max(inputLBIData$data$midLengths),silent=TRUE)
            if(inherits(binSize,"try-error")){
                binSize <- 2
                maxL <- 10
            }else{
                binSize <- round(0.23 * maxL^0.6, 1)
                if(binSize == 0) binSize <- 0.1
            }
        }
        numericInput(ns("LBI_binSize"), "",
                     binSize, min = 0.1, max = maxL, step=0.1,
                     width ='100%')
    })

    output$LBI_MK_out <- renderUI({
        if(!input$LBI_split_mk || is.null(input$LBI_M) || is.na(input$LBI_M) ||
           is.null(input$LBI_K) || is.na(input$LBI_K)){
            val <- NULL
        }else{
            val <- round(input$LBI_M / input$LBI_K, 5)
        }
        numericInput(ns("LBI_MK"),
                     label = "",
                     min = 0.0001,
                     max = 10,
                     value = NULL,
                     step = 0.01)
    })

    output$LBI_Linf_out <- renderUI({
        if(is.null(inputLBIData$data)){
            maxL <- 100
        }else{
            maxL <- try(round(max(inputLBIData$data$midLengths)/0.95),silent=TRUE)
            if(inherits(maxL,"try-error")){
                maxL <- 100
            }
        }
        min <- 0
        max <- 2 * maxL
        numericInput(ns("LBI_Linf"),"",
                     value = NULL,
                     min = min, max = max, step=0.1)
    })


    output$LBI_Lm50_out <- renderUI({
        if(is.null(inputLBIData$data)){
            maxL <- 100
        }else{
            maxL <- try(round(max(inputLBIData$data$midLengths)/0.95),silent=TRUE)
            if(inherits(maxL,"try-error")){
                maxL <- 100
            }
        }
        min <- 0
        max <- 2 * maxL
        numericInput(ns("LBI_Lm50"),"",
                     value = NULL,
                     min = min, max = max, step=0.1)
    })




    ## Interactive UIs & Reactive values
    ## ----------------------------
    observe({
        if(!input$LBI_split_mk){
            js$removeBox2("box_lbi_split_mk")
        }else{
            js$showBox2("box_lbi_split_mk")
        }
    })

    observeEvent(input$LBI_years_selected, {
        lbi_dat$years_selected <- input$LBI_years_selected
    })

    observeEvent(input$LBI_binSize, {
        lbi_dat$binSize <- input$LBI_binSize
    })

    observeEvent(input$fileLBI, {
        fileLBIState$upload <- 'uploaded'
        tmp <- lbiFileData()
        inputLBIData$data <- tmp$lfq
        inputLBIData$raw <- tmp$raw
        inputLBIData$checks <- tmp$checks
        ## bin size
        if(is.null(inputLBIData$data)){
            binSize <- 2
            maxL <- 10
        }else{
            binSize <- try(min(diff(inputLBIData$data$midLengths)),silent=TRUE)
            maxL <- try(max(inputLBIData$data$midLengths),silent=TRUE)
            if(inherits(binSize,"try-error")){
                binSize <- 2
                maxL <- 10
            }else{
                binSize <- round(0.23 * maxL^0.6, 1)
                if(binSize == 0) binSize <- 0.1
            }
        }
        lbi_dat$binSize <- binSize
        ## years selected
        if(is.null(inputLBIData$data)){
            allyears <- NULL
        }else{
            allyears <- try(unique(format(inputLBIData$data$dates,"%Y")),silent=TRUE)
            if(inherits(allyears,"try-error")) allyears <- NULL
        }
        lbi_dat$years_selected <- allyears
    })

    observeEvent(input$lbiDateFormat, {
        req(input$fileLBI)
        tmp <- lbiFileData()
        inputLBIData$data <- tmp$lfq
        inputLBIData$raw <- tmp$raw
        inputLBIData$checks <- tmp$checks
        ## bin size
        if(is.null(inputLBIData$data)){
            binSize <- 2
            maxL <- 10
        }else{
            binSize <- try(min(diff(inputLBIData$data$midLengths)),silent=TRUE)
            maxL <- try(max(inputLBIData$data$midLengths),silent=TRUE)
            if(inherits(binSize,"try-error")){
                binSize <- 2
                maxL <- 10
            }else{
                binSize <- round(0.23 * maxL^0.6, 1)
                if(binSize == 0) binSize <- 0.1
            }
        }
        lbi_dat$binSize <- binSize
        ## years selected
        if(is.null(inputLBIData$data)){
            allyears <- NULL
        }else{
            allyears <- try(unique(format(inputLBIData$data$dates,"%Y")),silent=TRUE)
            if(inherits(allyears,"try-error")) allyears <- NULL
        }
        lbi_dat$years_selected <- allyears
    })

    observeEvent(input$lbiCSVsep, {
        req(input$fileLBI)
        tmp <- lbiFileData()
        inputLBIData$data <- tmp$lfq
        inputLBIData$raw <- tmp$raw
        inputLBIData$checks <- tmp$checks
        ## bin size
        if(is.null(inputLBIData$data)){
            binSize <- 2
            maxL <- 10
        }else{
            binSize <- try(min(diff(inputLBIData$data$midLengths)),silent=TRUE)
            maxL <- try(max(inputLBIData$data$midLengths),silent=TRUE)
            if(inherits(binSize,"try-error")){
                binSize <- 2
                maxL <- 10
            }else{
                binSize <- round(0.23 * maxL^0.6, 1)
                if(binSize == 0) binSize <- 0.1
            }
        }
        lbi_dat$binSize <- binSize
        ## years selected
        if(is.null(inputLBIData$data)){
            allyears <- NULL
        }else{
            allyears <- try(unique(format(inputLBIData$data$dates,"%Y")),silent=TRUE)
            if(inherits(allyears,"try-error")) allyears <- NULL
        }
        lbi_dat$years_selected <- allyears
    })

    observeEvent(input$lbiCSVdec, {
        req(input$fileLBI)
        tmp <- lbiFileData()
        inputLBIData$data <- tmp$lfq
        inputLBIData$raw <- tmp$raw
        inputLBIData$checks <- tmp$checks
        ## bin size
        if(is.null(inputLBIData$data)){
            binSize <- 2
            maxL <- 10
        }else{
            binSize <- try(min(diff(inputLBIData$data$midLengths)),silent=TRUE)
            maxL <- try(max(inputLBIData$data$midLengths),silent=TRUE)
            if(inherits(binSize,"try-error")){
                binSize <- 2
                maxL <- 10
            }else{
                binSize <- round(0.23 * maxL^0.6, 1)
                if(binSize == 0) binSize <- 0.1
            }
        }
        lbi_dat$binSize <- binSize
        ## years selected
        if(is.null(inputLBIData$data)){
            allyears <- NULL
        }else{
            allyears <- try(unique(format(inputLBIData$data$dates,"%Y")),silent=TRUE)
            if(inherits(allyears,"try-error")) allyears <- NULL
        }
        lbi_dat$years_selected <- allyears
    })


    ## Action buttons
    observeEvent(input$go_lbi, {
        req(inputLBIData$data)

        result <- tryCatch({

            ## no weight at length -> no Lmaxy and Lmaxy_Lopt
            ## TODO: but how to enter weight in SMT?
            ## ADD: weight-at-length input
            lbi_dat$dataExplo[['lfq']]$weight <- input$LBI_LWa * lbi_dat$dataExplo[['lfq']]$midLengths ^ input$LBI_LWb

            if(input$LBI_split_mk){
                mk <- input$LBI_M / input$LBI_K
            }else{
                mk <- input$LBI_MK
            }

            ## Warnings
            years <- as.numeric(format(lbi_dat$dataExplo[['lfq']]$dates, "%Y"))
            if(any(duplicated(years))){
                showModal(modalDialog(
                    title = "Warning",
                    HTML("The length frequency data is not aggregated by year. This method should be used with yearly aggregated data!<hr/>")))
            }

            ## Parameter/Input checks
            check.numeric.and.min(c(binSize = input$LBI_binSize,
                                    linf = input$LBI_Linf,
                                    lm50 = input$LBI_Lm50,
                                    mk = mk
                                    ),
                                  can.be.zero = FALSE)

            if(!lbiAcknowledged()) {

                lbiPendingRun(TRUE)

                showModal(modalDialog(
                    title = "Acknowledge model assumptions",
                    bsCollapse(id = ns("assumptions"), open = NULL,
                               bsCollapsePanel("▶ Click to show/hide",
                                               HTML(lbiAssumptionsHTML()))
                               ),
                    tags$p(HTML("See the <a href='https://elearning.fao.org/course/view.php?id=502' target='_blank'>FAO eLearning module</a> for more information.")),
                    footer = tagList(
                        modalButton("Cancel"),
                        actionButton(ns("lbi_ack"), "I Acknowledge", class = "btn-success")
                    ),
                    easyClose = FALSE
                ))

                return(invisible(NULL))
            }

            run_lbi_server()

        }, error = function(e) {
            showModal(modalDialog(
                title = "Input Error",
                e$message,
                easyClose = TRUE,
                footer = NULL
            ))
        })
    })

    observeEvent(input$lbi_ack, {
        req(inputLBIData$data)
        removeModal()
        lbiAcknowledged(TRUE)

        js$showComputing()
        js$disableAllButtons()

        if (isTRUE(lbiPendingRun())) {
            lbiPendingRun(FALSE)
            run_lbi_server()
        }
    })

    observeEvent(input$reset_lbi, {
        fileLBIState$upload <- NULL
        resetLBIInputValues()
    })


    ## Plots
    ## --------------------------

    ## Data exploration
    ## --------------------------
    output$plot_explo1 <- renderPlot({
        req(inputLBIData$data, input$LBI_years_selected)
        lbi_dat$dataExplo[['lfq']] <- lbiDataExplo1()
        plotLBI.data(lbi_dat, input)
    },
    width = "auto",
    height = "auto",
    res = 100)
    output$title_explo1 <- renderText({
        req(inputLBIData$data, input$LBI_years_selected)
        captionLBI.plots(lbi_dat, input, format = "withFig", type = "data")
    })


    ## Data diagnostics
    ## --------------------------
    output$plot_diag1 <- renderPlot({
        req(inputLBIData$data, input$LBI_years_selected)
        lbi_dat$dataExplo[['lfq']] <- lbiDataExplo1()
        lbi_dat$dataExplo[['raw']] <- inputLBIData$raw
        lbi_dat$dataExplo[['checks']] <- inputLBIData$checks
        plotLBI.diag1(lbi_dat, input)
    },
    width = "auto",
    height = "auto",
    res = 100)
    output$title_diag1 <- renderText({
        req(inputLBIData$data, input$LBI_years_selected)
        captionLBI.plots(lbi_dat, input, format = "withFig", type = "diag1")
    })

    output$plot_diag2 <- renderPlot({
        req(inputLBIData$data, input$LBI_years_selected)
        lbi_dat$dataExplo[['lfq']] <- lbiDataExplo1()
        lbi_dat$dataExplo[['raw']] <- inputLBIData$raw
        lbi_dat$dataExplo[['checks']] <- inputLBIData$checks
        plotLBI.diag2(lbi_dat, input)
    },
    width = "auto",
    height = "auto",
    res = 100)
    output$title_diag2 <- renderText({
        req(inputLBIData$data, input$LBI_years_selected)
        captionLBI.plots(lbi_dat, input, format = "withFig", type = "diag2")
    })

    output$plot_diag3 <- renderPlot({
        req(inputLBIData$data, input$LBI_years_selected)
        lbi_dat$dataExplo[['lfq']] <- lbiDataExplo1()
        lbi_dat$dataExplo[['raw']] <- inputLBIData$raw
        lbi_dat$dataExplo[['checks']] <- inputLBIData$checks
        plotLBI.diag3(lbi_dat, input)
    },
    width = "auto",
    height = "auto",
    res = 100)
    output$title_diag3 <- renderText({
        req(inputLBIData$data, input$LBI_years_selected)
        captionLBI.plots(lbi_dat, input, format = "withFig", type = "diag3")
    })


    ## Fit
    ## --------------------------
    output$plot_lbiFit <- renderPlot({
        req(lbi_dat$results)
        plotLBI.fit(lbi_dat, input)
    },
    width = "auto",
    height = "auto",
    res = 120)
    output$title_lbiFit <- renderText({
        req(lbi_dat$results)
        captionLBI.plots(lbi_dat, input, format = "withFig", type = "fit")
    })


    ## Tables
    ## --------------------------

    ## Indicators
    ## --------------------------
    output$table_intro_lbi <- renderDataTable({
        req(lbi_dat$results)
        tableLBI.intro(lbi_dat, input, format = "datatable")
    })
    output$title_table_intro_lbi <- renderText({
        req(lbi_dat$results)
        captionLBI.tables(lbi_dat, input, format = "datatable",
                          type = "intro")
    })

    ## Indicators
    ## --------------------------
    output$table_lbi <- renderDataTable({
        req(lbi_dat$results)
        tableLBI.indicators(lbi_dat, input, format = "datatable")
    })
    output$title_table_lbi <- renderText({
        req(lbi_dat$results)
        captionLBI.tables(lbi_dat, input, format = "datatable",
                          type = "indicators")
    })

    ## Ratios
    ## --------------------------
    output$table2_lbi <- renderDataTable({
        req(lbi_dat$results)
        tableLBI.ratios(lbi_dat, input, format = "datatable")
    })
    output$title_table2_lbi <- renderText({
        req(lbi_dat$results)
        captionLBI.tables(lbi_dat, input, format = "datatable",
                          type = "ratios")
    })


    ## Text
    ## --------------------------

    ## Data diagnostics
    ## --------------------------
    output$text_diag1 <- renderUI({
        req(inputLBIData$data, input$LBI_years_selected)
        lbi_dat$dataExplo[['lfq']] <- lbiDataExplo1()
        lbi_dat$dataExplo[['raw']] <- inputLBIData$raw
        lbi_dat$dataExplo[['checks']] <- inputLBIData$checks
        textLBI.diag1(lbi_dat, input)
    })


    ## Information windows -----------------------
    observeEvent(input$workflowConsiderations, {
        showModal(modalDialog(
            title = "Workflow Considerations - LBI",
            HTML(getWorkflowConsiderationTextForLBI()),
            easyClose = TRUE,
            size = "l"
        ))
    }, ignoreInit = TRUE)

    observeEvent(input$dataConsiderations, {
        showModal(modalDialog(
            title = "Data Loading and Formatting Considerations - LBI",
            HTML(getDataConsiderationTextForLBI()),
            easyClose = TRUE,
            size = "l"
        ))
    }, ignoreInit = TRUE)

    observeEvent(input$dataConsiderations2, {
        showModal(modalDialog(
            title = "Data Considerations - LBI",
            HTML(getDataConsiderationTextForLBI()),
            easyClose = TRUE,
            size = "l"
        ))
    }, ignoreInit = TRUE)

    observeEvent(input$methodConsiderations, {
        showModal(modalDialog(
            title = "Methodological Considerations - LBI",
            HTML(getMethodConsiderationTextForLBI()),
            easyClose = TRUE,
            size = "l"
        ))
    }, ignoreInit = TRUE)

    observeEvent(input$methodConsiderations2, {
        showModal(modalDialog(
            title = "Methodological Considerations - LBI",
            HTML(getMethodConsiderationTextForLBI()),
            easyClose = TRUE,
            size = "l"
        ))
    }, ignoreInit = TRUE)

    observeEvent(input$resultConsiderations, {
        showModal(modalDialog(
            title = "Results Considerations - LBI",
            HTML(getResultConsiderationTextForLBI()),
            easyClose = TRUE,
            size = "l"
        ))
    }, ignoreInit = TRUE)

    observeEvent(input$resultConsiderations2, {
        showModal(modalDialog(
            title = "Results Considerations - LBI",
            HTML(getResultConsiderationTextForLBI()),
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
            HTML("<p>Select all or a range of years in the uploaded data set to be ",
                 "included in the analysis. ",
                 "<br><br> The indicators are estimated for every year separately.</p>"),
            easyClose = TRUE,
            size = "l"
        ))
    }, ignoreInit = TRUE)

    observeEvent(input$infoBS, {
        showModal(modalDialog(
            title = "Bin Size",
            HTML(paste0("<p>The bin size corresponds to the length interval over ",
                        "which the length frequency ",
                        "data are aggregated, for example 2 cm. <br><br> Ideally, ",
                        "the bin size is as small as ",
                        "possible, but large enough to reduce noise. By default the ",
                        "bin size is set dependent ",
                        "on the maximum length: ",
                        withMathJax("\\(0.23 L_{max}^{0.6}\\)"),
                        " (Wang et al. 2020).</p>")),
            easyClose = TRUE,
            size = "l"
            ))
    }, ignoreInit = TRUE)


    observeEvent(input$infoLinf, {
        showModal(modalDialog(
            title = withMathJax("\\(L_\\infty\\)"),
            HTML(paste0("<p>The asymptotic length (",
                        withMathJax("\\L_{\\infty}\\)"),
                        ") refers to the asymptote of the von Bertlanffy growth function, ",
                        "thus describing the theoretical maximum length at which growth is ",
                        "zero. (Refer to the 'Von Bertalanffy growth function' ",
                        "tab for more information). This parameter is required for the ",
                        "estimation of the indicators and ",
                        " can be estimated from monthly length-frequency data with TropFishR ",
                        "(see side menu on the left) or extracted from literature (e.g. ",
                        "<a href='http://www.fishbase.org/search.php' ",
                        "target='blank_'> FishBase</a> or <a href='https://www.sealifebase.ca' ",
                        "target='blank_'> SeaLifeBase</a> for invertebrates",
                        ").</p>")),
            easyClose = TRUE,
            size = "l"
        ))
    }, ignoreInit = TRUE)


    observeEvent(input$infoLm50, {
        showModal(modalDialog(
            title = withMathJax("\\(L_{m50}\\)"),
            HTML(paste0("<p>The length at 50% maturity (",withMathJax("\\L_{m50}\\)"),") ",
                        "is an important life history parameter and defines the length ",
                        "at which 50% of the population reached maturity. This parameter ",
                        "has to be estimated externally (outside SMT) or extracted from ",
                        "literature (e.g. ",
                        "<a href='http://www.fishbase.org/search.php' ",
                        "target='blank_'> FishBase</a> or <a href='https://www.sealifebase.ca' ",
                        "target='blank_'> SeaLifeBase</a> for invertebrates",
                        ").</p>")),
            easyClose = TRUE,
            size = "l"
        ))
    }, ignoreInit = TRUE)


    observeEvent(input$infoMK, {
        showModal(modalDialog(
            title = withMathJax("\\(M/K\\)"),
            HTML(paste0("<p>M/K is the ratio of the two life history parameters M, the ",
                        "instantaneous natural ",
                        "mortality rate and K, the von Bertalanffy growth constant. These two ",
                        "parameters can be estimated from monthly length-frequency data with ",
                        "TropFishR (see side menu on the left) or extracted from literature (",
                        "e.g. ",
                        "<a href='http://www.fishbase.org/search.php' ",
                        "target='blank_'> FishBase</a> or <a href='https://www.sealifebase.ca' ",
                        "target='blank_'> SeaLifeBase</a> for invertebrates",
                        "). <br> The M and K values can also be entered",
                        "separately by clicking the checkbox after 'or enter M and K?'",
                        "<br> Some scientists might argue that the M/K ratio ",
                        "does not vary a lot and a default value of 1.5 can be assumed. ",
                        " However, other references argue that this life history 'invariant' ",
                        "shows in fact a wide distribution between stocks. Please refer to the ",
                        "references in the method introduction page for more information on this ",
                        "topic.",
                        "</p>"))           ,
            easyClose = TRUE,
            size = "l"
        ))
    }, ignoreInit = TRUE)


    observeEvent(input$infoM, {
        showModal(modalDialog(
            title = withMathJax("\\(M\\)"),
            HTML(paste0("<p>M is the ",
                        "instantaneous natural ",
                        "mortality rate and can be estimated from monthly length-frequency data with ",
                        "TropFishR (see side menu on the left) or extracted from literature (",
                        "e.g. ",
                        "<a href='http://www.fishbase.org/search.php' ",
                        "target='blank_'> FishBase</a> or <a href='https://www.sealifebase.ca' ",
                        "target='blank_'> SeaLifeBase</a> for invertebrates",
                        ").",
                        "</p>")),
            easyClose = TRUE,
            size = "l"
        ))
    }, ignoreInit = TRUE)

    observeEvent(input$infoK, {
        showModal(modalDialog(
            title = withMathJax("\\(K\\)"),
            HTML(paste0("<p>K is the von Bertalanffy growth constant and can be estimated",
                        "from monthly length-frequency data with ",
                        "TropFishR (see side menu on the left) or extracted from literature (",
                        "e.g. ",
                        "<a href='http://www.fishbase.org/search.php' ",
                        "target='blank_'> FishBase</a> or <a href='https://www.sealifebase.ca' ",
                        "target='blank_'> SeaLifeBase</a> for invertebrates",
                        ").",
                        "</p>")),
            easyClose = TRUE,
            size = "l"
        ))
    }, ignoreInit = TRUE)

    observeEvent(input$infoLengthWeight, {
        showModal(modalDialog(
            title = "Length-weight relationship",
            HTML(paste0("<p>The calculation of the ", withMathJax("\\(L_{maxy}\\)")," indicator requires ",
                        "the average weight per length class, which can be estimated with the length-weight ",
                        "relationship. A common assumption is the allometric relationship ",
                        withMathJax("\\(W = a L^{b}\\)"),", with the constant <i>a</i> in ",
                        withMathJax("\\( g/cm^{3}\\)"), " and the exponent ",
                        withMathJax("\\( b\\)")," being unitless. <br><br>Ideally, the parameters are ",
                        "estimated based on length and weight measurements of the stock under study. ",
                        "Alternatively, information about the length-weight relationship of the species ",
                        "under study can be found on <a href='http://www.fishbase.org/search.php' ",
                        "target='blank_'> FishBase</a> or <a href='https://www.sealifebase.ca' ",
                        "target='blank_'> SeaLifeBase</a> for invertebrates. This information is optional ",
                        "and if
missing ",
"the ",withMathJax("\\(L_{maxy}\\)")," indicator is not calculated.",
"</p>")),
easyClose = TRUE,
size = "l"
        ))
    }, ignoreInit = TRUE)

    observeEvent(input$infoAssessment, {
        showModal(modalDialog(
            title = "Assessment, Reset & Report",
            HTML("<p><b>'Run Assessment'</b> estimates the length-based indicators and produces ",
                 "two tables and a figure in the results section upon successful completion. ",
                 "<br> <br> <b>'Reset'</b> removes all results, the uploaded dataset, ",
                 "and resets all settings to default values. <br> <br> After successful calculation ",
                 "of the indicators, two additional buttons <b>'Download Report'</b> and ",
                 "<b>'Download Results (zip)'</b> allow you to download an automated assessment ",
                 "report as a pdf document ",
                 "and a zip archive with all results, respectively. ",
                 "The report is also automatically ",
                 "uploaded to your private workspace.</p>"
                 ),
            easyClose = TRUE,
            size = "l"
        ))
    }, ignoreInit = TRUE)


    ## Tour --------------------------------
    observeEvent(input$tour_general, {

        steps <- list(
            list(element = NA,
                 intro = paste0("This tour takes you through the main steps of the calculation of the length-based indicators (LBIs).<br><br>",
                                "Click 'Next' to continue.")),
            list(element = paste0("#", ns("file_wrapper")),
                 intro = "As a first step, you need to upload data. You can do that by clicking on 'Browse' and select a csv file on your computer."),
            list(element = paste0("#", ns("dataConsiderations2")),
                 intro = "If you do not have your own file and want to use an example fiel or if you are interested in more information about the data type and format, click on the small the information button here. <br><br>Note these information buttons (indicated by 'i') throughout the whole app."),
            list(element = paste0("#", ns("lbiCSVsep"),
                                  " + .selectize-control"),
                 intro = "By default, the app will try to recognize the field separator, but you can also specify it here by either choosing from the list or by pressing backspace and enter any separator."),
            list(element = paste0("#", ns("lbiCSVdec"),
                                  " + .selectize-control"),
                 intro = "Similarly, by default, the app will try to recognize the decimal separator, but you can also specify it here by either choosing from the list or by pressing backspace and enter any separator."),
            list(element = paste0("#", ns("lbiDateFormat"),
                                  " + .selectize-control"),
                 intro = "By default, the app will try to recognize the date format, but you can also specify it here."),
            list(element = paste0("#", ns("lbi_lengthUnit"),
                                  " + .selectize-control"),
                 intro = "By default, the app assumes that your length measurements are in centimeter (cm), but you can choose other length unit here. Currently, millimeter (mm) and inches (in) are implemented."),
            list(element = paste0("#", ns("box_settings")),
                 intro = "After you chose your data set and it was uploaded successfully (no error messages), you can explore your data and adjust settings in this box."),
            list(element = "#settings_lbi ul.nav.nav-tabs",
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
                                 intro = "The last tab contains summary statistics and diagnostics of the uploaded data."),
                            list(element = paste0("#", ns("go_lbi")),
                                 intro = "If the check is successful, you can run the length-based stock assessment by clicking here.<br><br>Note, that a pop-up window will ask you if you are aware and acknowledge the model assumptions."),
                            list(element = paste0("#", ns("reset_lbi")),
                                 intro = "This button allows you to reset all settings.<br><br>Note, that this also removes your input data."),
                            list(element = paste0("#", ns("createLBIReport")),
                                 intro = "This button creates and downloads an automatic assessment report with information about your data, settings and results."),
                            list(element = paste0("#", ns("createLBIzip")),
                                 intro = "You can also download all graphs and tables in a zip archive by clicking here.")))

        if (!is.null(current_tab) && current_tab == "data") {
            steps <- append(steps,
                            list(
                                list(element = paste0("#", ns("fig_format_lbi"),
                                                      " + .selectize-control"),
                                     intro = paste0("You can select the format of the figures in the zip archive here.")),
                                list(element = paste0("#", ns("tab_format_lbi"),
                                                      " + .selectize-control"),
                                     intro = paste0("And the format of the tables here."))
                            ))
        }

        steps <- append(steps,
                        list(
                            list(element = paste0("#", ns("tour_res")),
                                 intro = "The results tour might be helpful to get an overview over the results.<br><br> Note, that the tour only makes sense after LBIs were successfully calculated."),
                            list(element = NA,
                                 intro = "This concludes the LBI tour.<br><br>Remember the information buttons ('i') that might be helpful when uploading data, adjusting settings or interpreting results."),
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
                 intro = "This is a tour that takes you through the results of the length-based indicators (LBIs).")
        )

        if(is.null(lbi_dat$results)) {

            steps <- append(steps,
                            list(
                                list(element = NA,
                                     intro = "No results found. This tour only works if you run the assessment."),
                                list(element = paste0("#", ns("go_lbi")),
                                     intro = "Make sure you uploaded your data and run the assessment by clicking here."),
                                list(element = NA,
                                     intro = "Start this tour again after you see some tables and graphs below.")))

        } else {

            steps <- append(steps,
                            list(
                                list(element = paste0("#", ns("table_intro_lbi")),
                                     intro = "This table gives an overview over all length-based indicators and reference points that were calculated in this assessment."),
                                list(element = paste0("#", ns("table_lbi")),
                                     intro = "This table shows all estimated length-based indicators."),
                                list(element = paste0("#", ns("table2_lbi")),
                                     intro = "This table shows the LBIs relative to their reference points. Cells that are red suggest that the indicator is below its reference points indicating overfishing."),
                                list(element = paste0("#", ns("plot_lbiFit")),
                                     intro = "This graph shows the indicators relative to their reference points. If the uploaded data contains multiple years, the plots show the temporal trend of the indicators."),
                                list(
                                    element = paste0("#",ns("resultConsiderations")),
                                    intro = paste0(
                                        "This concludes the tour through the LBI results.<br><br> For more and detailed information, click on this button and refer to the figure and table captions."))))

        }

        later::later(function() {
            introjs(session, options = list(steps = steps))
        }, delay = 0.5)
    }, ignoreInit = TRUE)



    ## Report & Co
    ## --------------------------
    output$createLBIReport <- downloadHandler(
        filename = paste("LBI_report_",format(Sys.time(), "%Y%m%d_%H%M_%s"),".pdf",sep=""),
        content = function(file) {
            createLBIPDFReport(file, lbi_dat, input, output)
        }
    )

    output$createLBIzip <- downloadHandler(
        filename = paste("LBI_results_",format(Sys.time(), "%Y%m%d_%H%M_%s"),".zip",sep=""),
        content = function(file) {
            makeContentLBIzip(file, lbi_dat, input, output)
        },
        contentType = "application/zip"
    )

    output$lbiTitle <- renderText({
        session$userData$page("lbi")
        text <- "<span><h3><b>Length-based indicators (LBIs)</b></h3></span>"
        text
    })

}
