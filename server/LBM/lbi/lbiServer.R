lbiModule <- function(input, output, session) {

    ns <- session$ns


    ## Definition of reactive values
    ## ----------------------------
    lbi_dat <- reactiveValues(
        dataExplo = NULL,
        results = NULL
    )
    lbiUploadVreResult <- reactiveValues()

    inputLBIData <- reactiveValues()

    fileLBIState <- reactiveValues(
        upload = NULL
    )

    ## Definition of functions
    ## ----------------------------
    lbiFileData <- reactive({
        if (is.null(input$fileLBI) || is.null(fileLBIState$upload)) {
            return(NA)
        }

        input$fileLBI$datapath
        input$lbiDateFormat

        dataset <- read_lbm_csv(input$fileLBI$datapath, input$lbiDateFormat)
        checks <- dataset$checks

        print(input$fileLBI)

        if (is.null(dataset$lfq)) {
            shinyjs::disable("go_lbi")
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
            shinyjs::enable("go_lbi")
            return (dataset$lfq)
        }
    })

    lbiDataExplo1 <- reactive({
        req(inputLBIData$data)

        lbi_dat$dataExplo <- list()
        years <- input$LBI_years_selected
        if(is.null(years)){
            stop("No year selected for the analysis. Please select at least one year of uploaded data set.")
        }
        dat <- inputLBIData$data
        class(dat) <- "lfq"

        binSize <- input$LBI_binSize
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

        ## HERE:
        browser()
        ## CHECK: force to "year"? what happens if already yearly agg?

        lfq <- lfqModify(dat,
                         bin_size = binSize,
                         years = years,
                         aggregate = "year")

        ## Account for length unit
        if(input$LBI_lengthUnit == "mm"){
            lfq$midLengths <- lfq$midLengths * 10
        }else if(input$LBI_lengthUnit == "in"){
            lfq$midLengths <- lfq$midLengths * 2.54
        }

        return(lfq)
    })

    resetLBIInputValues <- function() {
        ## resetting UIs
        shinyjs::reset("fileLBI")
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

        ## resetting reactive values
        lbi_dat$dataExplo <- NULL
        lbi_dat$results <- NULL
        inputLBIData$data <- NULL
        fileLBIState$upload <- NULL
        ## lbiUploadVreResult ?
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

    observeEvent(input$fileLBI, {
        fileLBIState$upload <- 'uploaded'
        inputLBIData$data <- lbiFileData()
    })

    observeEvent(input$lbiDateFormat, {
        inputLBIData$data <- lbiFileData()
    })



    ## Action buttons
    observeEvent(input$go_lbi, {
        req(inputLBIData$data)

        js$showComputing()
        js$disableAllButtons()

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
        })
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
        captionLBI.plots(lbi_data, input, format = "withFig", type = "data")
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
        captionLBI.plots(lbi_data, input, format = "withFig", type = "fit")
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
        captionLBI.tables(lbi_data, input, format = "datatable",
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
        captionLBI.tables(lbi_data, input, format = "datatable",
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
        captionLBI.tables(lbi_data, input, format = "datatable",
                                type = "ratios")
    })




    ## Report & Co
    ## --------------------------
    output$downloadReport_lbi <- renderUI({
        req(lbi_dat$results)
        downloadButton(session$ns('createLBIReport'), 'Download Report')
    })

    output$downloadzip_lbi <- renderUI({
        req(lbi_dat$results)
        downloadButton(session$ns('createLBIzip'), 'Download Results (zip archive)')
    })

    output$lbiVREUpload <- renderText({
        text <- ""
        req(lbi_dat$results)
        if (!is.null(session$userData$sessionMode()) && session$userData$sessionMode() == "GCUBE") {
            if (isTRUE(lbiUploadVreResult$res)) {
                text <- paste0(text, VREUploadText)
            }
        }
        text
    })

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

    output$lbiWorkflowConsiderationsText <- renderText({
        text <- getWorkflowConsiderationTextForLBI()
        text
    })

    output$lbiDataConsiderationsText <- renderText({
        text <- getDataConsiderationTextForLBI()
        text
    })

    output$lbiDataConsiderationsText2 <- renderText({
        text <- getDataConsiderationTextForLBI()
        text
    })

    output$lbiMethodConsiderationsText <- renderText({
        text <- getMethodConsiderationTextForLBI()
        text
    })

    output$lbiMethodConsiderationsText2 <- renderText({
        text <- getMethodConsiderationTextForLBI()
        text
    })

    output$lbiResultConsiderationsText <- renderText({
        text <- getResultConsiderationTextForLBI()
        text
    })

    output$lbiResultConsiderationsText2 <- renderText({
        text <- getResultConsiderationTextForLBI()
        text
    })


}
