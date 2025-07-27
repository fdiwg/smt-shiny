lbsprModule <- function(input, output, session) {

    ns <- session$ns


    ## Definition of reactive values
    ## ----------------------------
    lbspr_dat <- reactiveValues(
        dataExplo = NULL,
        results = NULL,
        years_selected = NULL,
        binSize = NULL
    )
    lbsprUploadVreResult <- reactiveValues()

    inputLBSPRData <- reactiveValues()

    fileLBSPRState <- reactiveValues(
        upload = NULL
    )

    lbsprAcknowledged <- reactiveVal(FALSE)

    ## Definition of functions
    ## ----------------------------
    lbsprFileData <- reactive({
        if (is.null(input$fileLBSPR) || is.null(fileLBSPRState$upload)) {
            return(NULL)
        }

        dataset <- read_lbm_csv(input$fileLBSPR$datapath, input$lbsprDateFormat)
        dataset$checks$fileName <- input$fileLBSPR$name
        checks <- dataset$checks

        print(input$fileLBSPR)

        if (is.null(dataset$lfq)) {
            shinyjs::disable("go_lbspr")
            shinyjs::disable("createLBSPRReport")
            shinyjs::disable("createLBSPRzip")
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
            res <- list(lfq = dataset$lfq,
                        raw = dataset$raw,
                        checks = dataset$checks)
            return(res)
        }
    })

    lbsprDataExplo1 <- reactive({
        req(inputLBSPRData$data)
        req(lbspr_dat$years_selected)
        req(lbspr_dat$binSize)

        res <- tryCatch({
            lbspr_dat$dataExplo <- list()
            years <- lbspr_dat$years_selected
            if(is.null(years)){
                stop("No year selected for the analysis. Please select at least one year of uploaded data set.")
            }
            dat <- inputLBSPRData$data
            class(dat) <- "lfq"

            binSize <- lbspr_dat$binSize
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
            shinyjs::disable("createLBSPRReport")
            shinyjs::disable("createLBSPRzip")
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
        shinyjs::disable("createLBSPRReport")
        shinyjs::disable("createLBSPRzip")

        ## resetting reactive values
        lbspr_dat$dataExplo <- NULL
        lbspr_dat$results <- NULL
        lbspr_dat$years_selected <- NULL
        lbspr_dat$binSize <- NULL
        inputLBSPRData$data <- NULL
        fileLBSPRState$upload <- NULL
        ## lbsprUploadVreResult ?
        lbsprAcknowledged(FALSE)
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

    observeEvent(input$LBSPR_years_selected, {
        lbspr_dat$years_selected <- input$LBSPR_years_selected
    })

    observeEvent(input$LBSPR_binSize, {
        lbspr_dat$binSize <- input$LBSPR_binSize
    })

    observeEvent(input$fileLBSPR, {
        fileLBSPRState$upload <- 'uploaded'
        tmp <- lbsprFileData()
        inputLBSPRData$data <- tmp$lfq
        inputLBSPRData$raw <- tmp$raw
        inputLBSPRData$checks <- tmp$checks
        ## bin size
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
        lbspr_dat$binSize <- binSize
        ## years selected
        if(is.null(inputLBSPRData$data)){
            allyears <- NULL
        }else{
            allyears <- try(unique(format(inputLBSPRData$data$dates,"%Y")),silent=TRUE)
            if(inherits(allyears,"try-error")) allyears <- NULL
        }
        lbspr_dat$years_selected <- allyears
    })

    observeEvent(input$lbsprDateFormat, {
        tmp <- lbsprFileData()
        inputLBSPRData$data <- tmp$lfq
        inputLBSPRData$raw <- tmp$raw
        inputLBSPRData$checks <- tmp$checks
        ## bin size
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
        lbspr_dat$binSize <- binSize
        ## years selected
        if(is.null(inputLBSPRData$data)){
            allyears <- NULL
        }else{
            allyears <- try(unique(format(inputLBSPRData$data$dates,"%Y")),silent=TRUE)
            if(inherits(allyears,"try-error")) allyears <- NULL
        }
        lbspr_dat$years_selected <- allyears
    })



    ## Action buttons
    observeEvent(input$go_lbspr, {
        tryCatch({
            req(inputLBSPRData$data)

            if (input$LBSPR_split_mk) {
                mk <- input$LBSPR_M / input$LBSPR_K
            } else {
                mk <- input$LBSPR_MK
            }

            check.numeric.and.min(c(
                binSize = input$LBSPR_binSize,
                linf = input$LBSPR_Linf,
                lm50 = input$LBSPR_Lm50,
                lm95 = input$LBSPR_Lm95,
                mk = mk,
                a = input$LBSPR_LWa,
                b = input$LBSPR_LWb
            ), can.be.zero = FALSE)

            lbsprAcknowledged(FALSE)

            showModal(modalDialog(
                title = "Acknowledge model assumptions",
                bsCollapse(id = ns("assumptions"), open = NULL,
                           bsCollapsePanel("▶ Click to show/hide",
                                           HTML(lbsprAssumptionsHTML()))
                           ),
                tags$p(HTML("See the <a href='https://elearning.fao.org/course/view.php?id=502' target='_blank'>FAO eLearning module</a> for more information.")),
                footer = tagList(
                    modalButton("Cancel"),
                    actionButton(ns("lbspr_ack"), "I Acknowledge", class = "btn-success")
                ),
                easyClose = FALSE
            ))

        }, error = function(e) {
            showModal(modalDialog(
                title = "Input Error",
                e$message,
                easyClose = TRUE,
                footer = NULL
            ))
        })
    })


    observeEvent(input$lbspr_ack, {
        req(inputLBSPRData$data)
        removeModal()
        lbsprAcknowledged(TRUE)

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

            ## if(input$LBSPR_Linf > max(lbspr_dat$dataExplo[['lfq']]$midLengths)){
            ##     stop(paste0("The specified asymptotic length (Linf = ",input$LBSPR_Linf,") is smaller than the largest length class (",max(lbspr_dat$dataExplo[['lfq']]$midLengths),"). This is not possible for LBSPR! Please consider using another Linf value."))
            ## }

            flog.info("Starting LBSPR computation")


            ## COMMENT: this could be removed, but allows to run LBSPR without WPS
            ## if(!session$userData$withtoken){

            ##     res <- run_lbspr(data = lbspr_dat$dataExplo[['lfq']],
            ##                    bin.size = input$LBSPR_binSize,
            ##                    linf = input$LBSPR_Linf,
            ##                    lm50 = input$LBSPR_Lm50,
            ##                    lm95 = input$LBSPR_Lm95,
            ##                    mk = mk,
            ##                    lwa = input$LBSPR_LWa,
            ##                    lwb = input$LBSPR_LWb,
            ##                    lunit = input$lbspr_lengthUnit
            ##                    )

            ## }else{

            temp.dir <- tempdir()
            dffile <- paste(temp.dir,"/","lbspr_data_",format(Sys.time(), "%Y%m%d_%H%M_%s"),".csv",sep="")
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

            ## Start WPS session
            WPS <- session$userData$sessionWps()

            ## Send the request
            exec <- WPS$execute(
                            identifier ="org.gcube.dataanalysis.wps.statisticalmanager.synchserver.mappedclasses.transducerers.LBSPR",
                            status=TRUE,
                            dataInputs = list(
                                binSize = WPSLiteralData$new(value = as.double(input$LBSPR_binSize)),
                                linf = WPSLiteralData$new(value = as.double(input$LBSPR_Linf)),
                                mk = WPSLiteralData$new(value = as.double(mk)),
                                m = WPSLiteralData$new(value = as.double(m)),
                                k = WPSLiteralData$new(value = as.double(k)),
                                lm50 = WPSLiteralData$new(value = as.double(input$LBSPR_Lm50)),
                                lm95 = WPSLiteralData$new(value = as.double(input$LBSPR_Lm95)),
                                lwa = WPSLiteralData$new(value = as.double(input$LBSPR_LWa)),
                                lwb = WPSLiteralData$new(value = as.double(input$LBSPR_LWb)),
                                sprLim = WPSLiteralData$new(value = as.double(input$LBSPR_sprLim)),
                                sprTarg = WPSLiteralData$new(value = as.double(input$LBSPR_sprTarg)),
                                lengthUnit = WPSLiteralData$new(value = as.character(input$lbspr_lengthUnit)),
                                fig_format = WPSLiteralData$new(value = as.character(input$fig_format_lbspr)),
                                tab_format = WPSLiteralData$new(value = as.character(input$tab_format_lbspr)),
                                inputFile = WPSComplexData$new(value = body, mimeType = "application/d4science")
                            )
                        )

            Status <- exec$getStatus()$getValue()
            print(Status)

            out <- exec$getProcessOutputs()[[1]]$getData()$getFeatures()

            res.dir <- paste(temp.dir,"/","lbspr_res_",format(Sys.time(), "%Y%m%d_%H%M_%s"),sep="")

            ## log of run: out$Data[1] (txt)

            temp <- tempfile()
            download.file(out$Data[2], temp)
            load(unz(temp,"res/LBSPR_res.RData"))
            unlink(temp)

            file.remove(dffile)
            options(warn=0)

            if(Status == "ProcessSucceeded"){
                flog.warn("LBSPR SUCCESS")
                print("LBSPR SUCCESS")
            }else{
                flog.warn("LBSPR FAIL")
                print("LBSPR FAIL")
                stop("WPS call failed.")
            }
            ## } ## HERE:

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
            if (!is.null(lbspr_dat$results)) {
                shinyjs::enable("createLBSPRReport")
                shinyjs::enable("createLBSPRzip")
            }
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


    ## Data diagnostics
    ## --------------------------
    output$plot_diag1 <- renderPlot({
        req(inputLBSPRData$data, input$LBSPR_years_selected)
        lbspr_dat$dataExplo[['lfq']] <- lbsprDataExplo1()
        lbspr_dat$dataExplo[['raw']] <- inputLBSPRData$raw
        lbspr_dat$dataExplo[['checks']] <- inputLBSPRData$checks
        plotLBSPR.diag1(lbspr_dat, input)
    },
    width = "auto",
    height = "auto",
    res = 100)
    output$title_diag1 <- renderText({
        req(inputLBSPRData$data, input$LBSPR_years_selected)
        captionLBSPR.plots(lbspr_dat, input, format = "withFig", type = "diag1")
    })

    output$plot_diag2 <- renderPlot({
        req(inputLBSPRData$data, input$LBSPR_years_selected)
        lbspr_dat$dataExplo[['lfq']] <- lbsprDataExplo1()
        lbspr_dat$dataExplo[['raw']] <- inputLBSPRData$raw
        lbspr_dat$dataExplo[['checks']] <- inputLBSPRData$checks
        plotLBSPR.diag2(lbspr_dat, input)
    },
    width = "auto",
    height = "auto",
    res = 100)
    output$title_diag2 <- renderText({
        req(inputLBSPRData$data, input$LBSPR_years_selected)
        captionLBSPR.plots(lbspr_dat, input, format = "withFig", type = "diag2")
    })

    output$plot_diag3 <- renderPlot({
        req(inputLBSPRData$data, input$LBSPR_years_selected)
        lbspr_dat$dataExplo[['lfq']] <- lbsprDataExplo1()
        lbspr_dat$dataExplo[['raw']] <- inputLBSPRData$raw
        lbspr_dat$dataExplo[['checks']] <- inputLBSPRData$checks
        plotLBSPR.diag3(lbspr_dat, input)
    },
    width = "auto",
    height = "auto",
    res = 100)
    output$title_diag3 <- renderText({
        req(inputLBSPRData$data, input$LBSPR_years_selected)
        captionLBSPR.plots(lbspr_dat, input, format = "withFig", type = "diag3")
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


    ## Text
    ## --------------------------

    ## Data diagnostics
    ## --------------------------
    output$text_diag1 <- renderUI({
        req(inputLBSPRData$data, input$LBSPR_years_selected)
        lbspr_dat$dataExplo[['lfq']] <- lbsprDataExplo1()
        lbspr_dat$dataExplo[['raw']] <- inputLBSPRData$raw
        lbspr_dat$dataExplo[['checks']] <- inputLBSPRData$checks
        textLBSPR.diag1(lbspr_dat, input)
    })

    ## Information windows -----------------------
    observeEvent(input$workflowConsiderations, {
        showModal(modalDialog(
            title = "Workflow Considerations - LBSPR",
            HTML(getWorkflowConsiderationTextForLBSPR()),
            easyClose = TRUE,
            size = "l"
        ))
    }, ignoreInit = TRUE)

    observeEvent(input$dataConsiderations, {
        showModal(modalDialog(
            title = "Data Loading and Formatting Considerations - LBSPR",
            HTML(getDataConsiderationTextForLBSPR()),
            easyClose = TRUE,
            size = "l"
        ))
    }, ignoreInit = TRUE)

    observeEvent(input$dataConsiderations2, {
        showModal(modalDialog(
            title = "Data Considerations - LBSPR",
            HTML(getDataConsiderationTextForLBSPR()),
            easyClose = TRUE,
            size = "l"
        ))
    }, ignoreInit = TRUE)

    observeEvent(input$methodConsiderations, {
        showModal(modalDialog(
            title = "Methodological Considerations - LBSPR",
            HTML(getMethodConsiderationTextForLBSPR()),
            easyClose = TRUE,
            size = "l"
        ))
    }, ignoreInit = TRUE)

    observeEvent(input$methodConsiderations2, {
        showModal(modalDialog(
            title = "Methodological Considerations - LBSPR",
            HTML(getMethodConsiderationTextForLBSPR()),
            easyClose = TRUE,
            size = "l"
        ))
    }, ignoreInit = TRUE)

    observeEvent(input$resultConsiderations, {
        showModal(modalDialog(
            title = "Results Considerations - LBSPR",
            HTML(getResultConsiderationTextForLBSPR()),
            easyClose = TRUE,
            size = "l"
        ))
    }, ignoreInit = TRUE)

    observeEvent(input$resultConsiderations2, {
        showModal(modalDialog(
            title = "Results Considerations - LBSPR",
            HTML(getResultConsiderationTextForLBSPR()),
            easyClose = TRUE,
            size = "l"
        ))
    }, ignoreInit = TRUE)

    observeEvent(input$infoYearSel, {
        showModal(modalDialog(
            title = "Selected years",
            HTML("<p>Select all or a range of years in the uploaded data set to be ",
                 "included in the analysis. ",
                 "<br><br> The method estimates SPR and fishing mortality for every year.</p>"),
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
                        "on the maximum length: ", withMathJax("\\(0.23 L_{max}^{0.6}\\)"),
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
                        "tab for more information). This parameter is required for LBSPR ",
                        " and ",
                        " can be estimated from monthly length-frequency data with TropFishR ",
                        "(see side menu on the left) or extracted from literature (e.g.",
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
                        "target='blank_'> SeaLifeBase</a>  for invertebrates. This information is optional ",
                        "and if missing ",
                        "the ",withMathJax("\\(L_{maxy}\\)")," indicator is not calculated.",
                        "</p>")),
            easyClose = TRUE,
            size = "l"
        ))
    }, ignoreInit = TRUE)


    observeEvent(input$infoMaturity, {
        showModal(modalDialog(
            title = "Maturity information",
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

    observeEvent(input$infoRefs, {
        showModal(modalDialog(
            title = "SPR reference points",
            HTML(paste0("<p> The stock status in terms of the spawning potential ",
                        "ratio (SPR) is defined relative to the limit and reference ",
                        "target points (SPR limit and SPR target, respectively). ",
                        "While an SPR below the limit reference point indicates ",
                        "overexploitation, a level above the target reference point ",
                        "would suggest underexploitation. ",
                        "Various levels have been suggested as reference levels (see ",
                        "e.g. Brooks et al. 2010, Clark 2002). ",
                        "The default levels are set to 0.2 and 0.4 for the limit ",
                        "and target reference points, respectively.",
                        " </p>")),
            easyClose = TRUE,
            size = "l"
        ))
    }, ignoreInit = TRUE)


    observeEvent(input$infoAssessment, {
        showModal(modalDialog(
            title = "Assessment, Reset & Report",
            HTML("<p><b>'Run Assessment'</b> runs the LBSPR method and produces ",
                 "a table and three figures in the results section upon successful completion. ",
                 "<br> <br> <b>'Reset'</b> removes all results, the uploaded dataset, ",
                 "and resets all settings to default values. <br> <br> After successful completion ",
                 "of the method, two additional buttons <b>'Download Report'</b> and ",
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
                 intro = paste0("This tour takes you through the main steps of the length-based spawning potential ratio method (LBSPR).<br><br>",
                                "Click 'Next' to continue.")),
            list(element = paste0("#", ns("file_wrapper")),
                 intro = "As a first step, you need to upload data. You can do that by clicking on 'Browse' and select a csv file on your computer."),
            list(element = paste0("#", ns("dataConsiderations2")),
                 intro = "If you do not have your own file and want to use an example fiel or if you are interested in more information about the data type and format, click on the small the information button here. <br><br>Note these information buttons (indicated by 'i') throughout the whole app."),
            list(element = paste0("#", ns("lbsprDateFormat"),
                                  " + .selectize-control"),
                 intro = "By default, the app will try to recognize the date format, but you can also specify it here."),
            list(element = paste0("#", ns("lbspr_lengthUnit"),
                                  " + .selectize-control"),
                 intro = "By default, the app assumes that your length measurements are in centimeter (cm), but you can choose other length unit here. Currently, millimeter (mm) and inces (in) are implemented."),
            list(element = paste0("#", ns("box_settings")),
                 intro = "After you chose your data set and it was uploaded successfully (no error messages), you can explore your data and adjust settings in this box."),
            list(element = "#settings_lbspr ul.nav.nav-tabs",
                 intro = "There are multiple tabs that allow you to adjust various aspects of the assessment method."),
            list(element = paste0("#", ns("tab1")),
                 intro = "For example, the first tab allows you to visually inspect the uploaded data and set important parameters, such as the bin size or moving average.<br><br> Remeber the small information buttons ('i') next to the labels, if you need more information about these parameters.")
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
                            list(element = paste0("#", ns("go_lbspr")),
                                 intro = "If the check is successful, you can run the length-based stock assessment by clicking here.<br><br>Note, that a pop-up window will ask you if you are aware and acknowledge the model assumptions."),
                            list(element = paste0("#", ns("reset_lbspr")),
                                 intro = "This button allows you to reset all settings.<br><br>Note, that this also removes your input data."),
                            list(element = paste0("#", ns("createLBSPRReport")),
                                 intro = "This button creates and downloads an automatic assessment report with information about your data, settings and results."),
                            list(element = paste0("#", ns("createLBSPRzip")),
                                 intro = "You can also download all graphs and tables in a zip archive by clicking here.")))

        if (!is.null(current_tab) && current_tab == "data") {
            steps <- append(steps,
                            list(
                                list(element = paste0("#", ns("fig_format_lbspr"),
                                                      " + .selectize-control"),
                                     intro = paste0("You can select the format of the figures in the zip archive here.")),
                                list(element = paste0("#", ns("tab_format_lbspr"),
                                                      " + .selectize-control"),
                                     intro = paste0("And the format of the tables here."))
                            ))
        }

        steps <- append(steps,
                        list(
                            list(element = paste0("#", ns("tour_res")),
                                 intro = "The results tour might be helpful to get an overview over the results.<br><br> Note, that the tour only makes sense after LBSPR was run successfully."),
                            list(element = NA,
                                 intro = "This concludes the LBSPR tour.<br><br>Remeber the information buttons ('i') that might be helpful when uploading data, adjusting settings or interpreting results."),
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
                 intro = "This is a tour that takes you through the results of the length-based spawning potential ratio method (LBSPR).")
        )

        if(is.null(lbi_dat$results)) {

            steps <- append(steps,
                            list(
                                list(element = NA,
                                     intro = "No results found. This tour only works if you run the assessment."),
                                list(element = paste0("#", ns("go_lbspr")),
                                     intro = "Make sure you uploaded your data and run the assessment by clicking here."),
                                list(element = NA,
                                     intro = "Start this tour again after you see some tables and graphs below.")))

        } else {

            steps <- append(steps,
                            list(
                                list(element = paste0("#", ns("table_lbspr")),
                                     intro = "This table shows the estimated spawning potential ratio (SPR), selectivity parameters, and fishing mortality relative to natural mortality (F/M)."),
                                list(element = paste0("#", ns("plot_lbsprPie")),
                                     intro = "This figure shows the estimated SPR relative to specified reference points."),
                                list(element = paste0("#", ns("plot_lbsprSel")),
                                     intro = "This graph shows the estimated selectivity ogives (per year if data set spans multiple years) and the specified maturity ogive."),
                                list(element = paste0("#", ns("plot_lbsprTimeSeries")),
                                     intro = "This graph shows the selectivity, F/M, and SPR per year and if the data set spans multiple years, a smothed trend line."),
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
    output$createLBSPRReport <- downloadHandler(
        filename = paste("LBSPR_report_",
                         format(Sys.time(), "%Y%m%d_%H%M_%s"),".pdf",sep=""),
        content = function(file) {
            createLBSPRPDFReport(file, lbspr_dat, input, output)
        }
    )

    output$createLBSPRzip <- downloadHandler(
        filename = paste("LBSPR_results_",
                         format(Sys.time(), "%Y%m%d_%H%M_%s"),".zip",sep=""),
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

}
