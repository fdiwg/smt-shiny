lbsprModule <- function(input, output, session) {

    ns <- session$ns


    ## Definition of reactive values
    ## ----------------------------
    lbspr_dat <- reactiveValues(
        dataExplo = NULL,
        results = NULL
    )
    lbsprUploadVreResult <- reactiveValues()

    inputLBSPRData <- reactiveValues()

    fileLBSPRState <- reactiveValues(
        upload = NULL
    )

    ## Definition of functions
    ## ----------------------------
    lbsprFileData <- reactive({
        if (is.null(input$fileLBSPR) || is.null(fileLBSPRState$upload)) {
            return(NA)
        }

        input$fileLBSPR$datapath
        input$lbsprDateFormat

        dataset <- read_lbm_csv(input$fileLBSPR$datapath, input$lbsprDateFormat)
        checks <- dataset$checks

        print(input$fileLBSPR)

        if (is.null(dataset$lfq)) {
            shinyjs::disable("go_lbspr")
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
            shinyjs::enable("go_lbspr")
            return (dataset$lfq)
        }
    })

    lbsprDataExplo1 <- reactive({
        req(inputLBSPRData$data)

        res <- tryCatch({
            lbspr_dat$dataExplo <- list()
            years <- input$LBSPR_years_selected
            if(is.null(years)){
                stop("No year selected for the analysis. Please select at least one year of uploaded data set.")
            }
            dat <- inputLBSPRData$data
            class(dat) <- "lfq"

            binSize <- input$LBSPR_binSize
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
                             aggregate = "year")
            return(lfq)

        }, error = function(cond) {
            shinyjs::disable("go_lbspr")
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

    resetLBSPRInputValues <- function() {
        ## resetting UIs
        shinyjs::reset("fileLBSPR")
        shinyjs::reset("lbsprDateFormat")
        shinyjs::reset("LBSPR_years_selected")
        shinyjs::reset("LBSPR_binSize")
        shinyjs::reset("LBSPR_Linf")
        shinyjs::reset("LBSPR_Lm50")
        shinyjs::reset("LBSPR_Lm95")
        shinyjs::reset("LBSPR_MK")
        shinyjs::reset("LBSPR_split_mk")
        shinyjs::reset("LBSPR_M")
        shinyjs::reset("LBSPR_K")
        shinyjs::reset("LBSPR_LWa")
        shinyjs::reset("LBSPR_LWb")

        ## disable buttons
        shinyjs::disable("go_lbspr")

        ## resetting reactive values
        lbspr_dat$dataExplo <- NULL
        lbspr_dat$results <- NULL
        inputLBSPRData$data <- NULL
        fileLBSPRState$upload <- NULL
        ## lbsprUploadVreResult ?
    }



    ## Input-dependent UIs
    ## ----------------------------
    output$LBSPR_years_selected_out <- renderUI({
        if(is.null(inputLBSPRData$data)){
            allyears <- NULL
        }else{
            allyears <- try(unique(format(inputLBSPRData$data$dates,"%Y")),silent=TRUE)
            if(inherits(allyears,"try-error")) allyears <- NULL
        }
        selectInput(ns("LBSPR_years_selected"), "",
                    choices = allyears, selected = allyears,
                    multiple = TRUE,
                    width = "100%")
    })

    output$LBSPR_binSize_out <- renderUI({
        if(is.null(inputLBSPRData$data)){
            binSize <- 2
            maxL <- 10
        }else{
            binSize <- try(min(diff(inputLBSPRData$data$midLengths)),silent=TRUE)
            maxL <- try(max(inputLBSPRData$data$midLengths),silent=TRUE)
            if(inherits(binSize,"try-error")){
                binSize <- 2
                maxL <- 10
            }else{
                binSize <- round(0.23 * maxL^0.6, 1)
                if(binSize == 0) binSize <- 0.1
            }
        }
        numericInput(ns("LBSPR_binSize"), "",
                     binSize, min = 0.1, max = maxL, step=0.1,
                     width ='80%')
    })

    output$LBSPR_MK_out <- renderUI({
        if(!input$LBSPR_split_mk || is.null(input$LBSPR_M) || is.na(input$LBSPR_M) ||
           is.null(input$LBSPR_K) || is.na(input$LBSPR_K)){
            val <- NULL
        }else{
            val <- round(input$LBSPR_M / input$LBSPR_K, 5)
        }
        numericInput(ns("LBSPR_MK"),
                     label = "",
                     min = 0.0001,
                     max = 10,
                     value = NULL,
                     step = 0.01,
                     width = "80%")
    })

    output$LBSPR_Linf_out <- renderUI({
        if(is.null(inputLBSPRData$data)){
            maxL <- 100
        }else{
            maxL <- try(round(max(inputLBSPRData$data$midLengths)/0.95),silent=TRUE)
            if(inherits(maxL,"try-error")){
                maxL <- 100
            }
        }
        min <- 0
        max <- 2 * maxL
        numericInput(ns("LBSPR_Linf"),"",
                     value = NULL,
                     min = min, max = max, step=0.1,
                     width = "80%")
    })


    output$LBSPR_Lm50_out <- renderUI({
        if(is.null(inputLBSPRData$data)){
            maxL <- 100
        }else{
            maxL <- try(round(max(inputLBSPRData$data$midLengths)/0.95),silent=TRUE)
            if(inherits(maxL,"try-error")){
                maxL <- 100
            }
        }
        min <- 0
        max <- 2 * maxL
        numericInput(ns("LBSPR_Lm50"),"",
                     value = NULL,
                     min = min, max = max, step=0.1,
                     width = "80%")
    })

    output$LBSPR_Lm95_out <- renderUI({
        if(is.null(inputLBSPRData$data)){
            maxL <- 100
        }else{
            maxL <- try(round(max(inputLBSPRData$data$midLengths)/0.95),silent=TRUE)
            if(inherits(maxL,"try-error")){
                maxL <- 100
            }
        }
        min <- 0
        max <- 2 * maxL
        numericInput(ns("LBSPR_Lm95"),"",
                     value = NULL,
                     min = min, max = max, step=0.1,
                     width = "80%")
    })




    ## Interactive UIs & Reactive values
    ## ----------------------------
    observe({
        if(!input$LBSPR_split_mk){
            js$removeBox2("box_lbspr_split_mk")
        }else{
            js$showBox2("box_lbspr_split_mk")
        }
    })

    observeEvent(input$fileLBSPR, {
        fileLBSPRState$upload <- 'uploaded'
        inputLBSPRData$data <- lbsprFileData()
    })

    observeEvent(input$lbsprDateFormat, {
        inputLBSPRData$data <- lbsprFileData()
    })



    ## Action buttons
    observeEvent(input$go_lbspr, {
        req(inputLBSPRData$data)

        js$showComputing()
        js$disableAllButtons()

        result = tryCatch({

            if(input$LBSPR_split_mk){
                mk <- input$LBSPR_M / input$LBSPR_K
                m <- input$LBSPR_M
                k <- input$LBSPR_K
            }else{
                mk <- input$LBSPR_MK
                m <- -999
                k <- -999
            }

            ## Warnings
            years <- as.numeric(format(lbspr_dat$dataExplo[['lfq']]$dates, "%Y"))
            if(any(duplicated(years))){
                showModal(modalDialog(
                    title = "Warning",
                    HTML("The length frequency data is not aggregated by year. This method should be used with yearly aggregated data!<hr/>")))
            }

            ## Parameter/Input checks
            check.numeric.and.min(c(binSize = input$LBSPR_binSize,
                                    linf = input$LBSPR_Linf,
                                    lm50 = input$LBSPR_Lm50,
                                    lm95 = input$LBSPR_Lm95,
                                    mk = mk,
                                    a = input$LBSPR_LWa,
                                    b = input$LBSPR_LWb
                                    ),
                                  can.be.zero = FALSE)

            flog.info("Starting LBSPR computation")

            if(!session$userData$withtoken){

                res <- run_lbspr(data = lbspr_dat$dataExplo[['lfq']],
                               bin.size = input$LBSPR_binSize,
                               linf = input$LBSPR_Linf,
                               lm50 = input$LBSPR_Lm50,
                               lm95 = input$LBSPR_Lm95,
                               mk = mk,
                               lwa = input$LBSPR_LWa,
                               lwb = input$LBSPR_LWb,
                               lunit = input$LBSPR_lengthUnit
                               )

            }else{

                ## NEW:
                dffile <- paste(tempdir(),"/","lbspr_data_",format(Sys.time(), "%Y%m%d_%H%M_%s"),".csv",sep="")
                dffile <- gsub(" ", "_", dffile)

                tmp <- lbspr_dat$dataExplo$lfq$catch
                tmp <- cbind(lbspr_dat$dataExplo$lfq$midLengths, tmp)
                colnames(tmp) <- c("midLengths", as.character(lbspr_dat$dataExplo$lfq$dates))

                write.csv(tmp, file = dffile, quote = FALSE,
                          eol = "\n", row.names = FALSE,  fileEncoding = "UTF-8")

                ## Convert input file to string
                body <- readChar(dffile, file.info(dffile)$size)
                body <- gsub("\r\n", "\n", body)
                body <- gsub("\n$", "", body)
                print(body)

                ## write.table(body, file = "../inputFile.txt")

                ## Start WPS session
                WPS <- session$userData$sessionWps()

                ## Send the request
                exec <- WPS$execute(
                                identifier ="org.gcube.dataanalysis.wps.statisticalmanager.synchserver.mappedclasses.transducerers.LBSPR",
                                status=TRUE,
                                dataInputs = list(
                                    binSize = WPSLiteralData$new(value = input$LBSPR_binSize),
                                    linf = WPSLiteralData$new(value = input$LBSPR_Linf),
                                    mk = WPSLiteralData$new(value = mk),
                                    m = WPSLiteralData$new(value = m),
                                    k = WPSLiteralData$new(value = k),
                                    lm50 = WPSLiteralData$new(value = input$LBSPR_Lm50),
                                    lm95 = WPSLiteralData$new(value = input$LBSPR_Lm95),
                                    lwa = WPSLiteralData$new(value = input$LBSPR_LWa),
                                    lwb = WPSLiteralData$new(value = input$LBSPR_LWb),
                                    sprLim = WPSLiteralData$new(value = input$LBSPR_sprLim),
                                    sprTarg = WPSLiteralData$new(value = input$LBSPR_sprTarg),
                                    lengthUnit = WPSLiteralData$new(value = input$LBSPR_lengthUnit),
                                    fig_format = WPSLiteralData$new(value = input$fig_format_lbspr),
                                    tab_format = WPSLiteralData$new(value = input$tab_format_lbspr),
                                    inputFile = WPSComplexData$new(value = body, mimeType = "application/d4science")
                                )
                            )

                Status <- exec$getStatus()$getValue()
                print(Status)
                out <- exec$getProcessOutputs()[[1]]$getData()$getFeatures()
                print(out)

                browser()
            }

            js$hideComputing()
            js$enableAllButtons()

            lbspr_dat$results <- res$res

            if (!is.null(session$userData$sessionMode()) && session$userData$sessionMode()=="GCUBE") {
                flog.info("Uploading LBSPR report to i-Marine workspace")
                reportFileName <- paste(tempdir(),"/","LBSPR_report_",
                                        format(Sys.time(), "%Y%m%d_%H%M_%s"),".pdf",sep="")
                #createLBSPRPDFReport(reportFileName,lbspr_dat,input)
                createLBSPRPDFReport(reportFileName, lbspr_dat, input, output)
                lbsprUploadVreResult$res <- FALSE

                basePath <- paste0("/Home/",session$userData$sessionUsername(),"/Workspace/")

                SH_MANAGER <- session$userData$storagehubManager()

                tryCatch({
                    uploadToIMarineFolder(SH_MANAGER, reportFileName, basePath, uploadFolderName)
                    lbsprUploadVreResult$res <- TRUE
                }, error = function(err) {
                    flog.error("Error uploading LBSPR report to the i-Marine Workspace: %s", err)
                    lbsprUploadVreResult$res <- FALSE
                }, finally = {})
            }

        }, error = function(cond) {
            flog.error("Error in LBSPR: %s ",cond)
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


    observeEvent(input$reset_lbspr, {
        fileLBSPRState$upload <- NULL
        resetLBSPRInputValues()
    })



    ## Data exploration plots
    ## --------------------------
    output$plot_explo1 <- renderPlot({
        req(inputLBSPRData$data, input$LBSPR_years_selected)
        lbspr_dat$dataExplo[['lfq']] <- lbsprDataExplo1()
        plotLBSPR.data(lbspr_dat, input)
    },
    width = "auto",
    height = "auto",
    res = 100)
    output$title_explo1 <- renderText({
        req(inputLBSPRData$data, input$LBSPR_years_selected)
        captionLBSPR.plots(lbspr_dat, input, format = "withFig",
                                type = "data")
    })



    ## LBSPR table
    ## --------------------------
    output$table_lbspr <- renderDataTable({
        req(lbspr_dat$results)
        tableLBSPR.results(lbspr_dat, input, format = "datatable")
    })
    output$title_table_lbspr <- renderText({
        req(lbspr_dat$results)
        captionLBSPR.tables(lbspr_dat, input, format = "datatable",
                            type = "res")
    })


    ## LBSPR fit plots
    ## --------------------------
    output$plot_lbsprPie <- renderPlot({
        req(lbspr_dat$results)
        plotLBSPR.pie(lbspr_dat, input)
    },
    width = "auto",
    height = "auto",
    res = 120)
    output$title_lbsprPie <- renderText({
        req(lbspr_dat$results)
        captionLBSPR.plots(lbspr_dat, input, format = "withFig",
                                type = "pie")
    })

    output$plot_lbsprSel <- renderPlot({
        req(lbspr_dat$results)
        plotLBSPR.sel(lbspr_dat, input)
    },
    width = "auto",
    height = "auto",
    res = 120)
    output$title_lbsprSel <- renderText({
        req(lbspr_dat$results)
        captionLBSPR.plots(lbspr_dat, input, format = "withFig",
                                type = "sel")
    })

    output$plot_lbsprTimeSeries <- renderPlot({
        req(lbspr_dat$results)
        plotLBSPR.ts(lbspr_dat, input)
    },
    width = "auto",
    height = "auto",
    res = 120)
    output$title_lbsprTimeSeries <- renderText({
        req(lbspr_dat$results)
        captionLBSPR.plots(lbspr_dat, input, format = "withFig",
                                type = "ts")
    })


    ## Report & Co
    ## --------------------------
    output$downloadReport_lbspr <- renderUI({
        req(lbspr_dat$results)
        downloadButton(session$ns('createLBSPRReport'), 'Download Report')
    })

    output$downloadzip_lbspr <- renderUI({
        req(lbspr_dat$results)
        downloadButton(session$ns('createLBSPRzip'), 'Download Results (zip archive)')
    })

    output$zip_fig_format_lbspr <- renderUI({
        req(lbspr_dat$results)
        selectInput(session$ns("fig_format_lbspr"),
                    "Format of archived figures",
                    choices = c("pdf","png","jpeg","tiff","bmp","ps"),
                    selected = "pdf",
                    multiple = FALSE,
                    width = "100%")
    })


    output$lbsprVREUpload <- renderText({
        text <- ""
        req(lbspr_dat$results)
        if (!is.null(session$userData$sessionMode()) && session$userData$sessionMode() == "GCUBE") {
            if (isTRUE(lbsprUploadVreResult$res)) {
                text <- paste0(text, VREUploadText)
            }
        }
        text
    })

    output$createLBSPRReport <- downloadHandler(
        filename = paste("LBSPR_report_",format(Sys.time(), "%Y%m%d_%H%M_%s"),".pdf",sep=""),
        content = function(file) {
            createLBSPRPDFReport(file, lbspr_dat, input, output)
        }
    )

    output$createLBSPRzip <- downloadHandler(
        filename = paste("LBSPR_results_",format(Sys.time(), "%Y%m%d_%H%M_%s"),".zip",sep=""),
        content = function(file) {
            makeContentLBSPRzip(file, lbspr_dat, input, output)
        },
        contentType = "application/zip"
    )

    output$lbsprTitle <- renderText({
        session$userData$page("lbspr")
        text <- "<span><h3><b>Length-based spawning potential ratio (LBSPR)</b></h3></span>"
        text
    })

    output$lbsprWorkflowConsiderationsText <- renderText({
        text <- getWorkflowConsiderationTextForLBSPR()
        text
    })

    output$lbsprDataConsiderationsText <- renderText({
        text <- getDataConsiderationTextForLBSPR()
        text
    })

    output$lbsprDataConsiderationsText2 <- renderText({
        text <- getDataConsiderationTextForLBSPR()
        text
    })

    output$lbsprMethodConsiderationsText <- renderText({
        text <- getMethodConsiderationTextForLBSPR()
        text
    })

    output$lbsprMethodConsiderationsText2 <- renderText({
        text <- getMethodConsiderationTextForLBSPR()
        text
    })

    output$lbsprResultConsiderationsText <- renderText({
        text <- getResultConsiderationTextForLBSPR()
        text
    })

    output$lbsprResultConsiderationsText2 <- renderText({
        text <- getResultConsiderationTextForLBSPR()
        text
    })


}
