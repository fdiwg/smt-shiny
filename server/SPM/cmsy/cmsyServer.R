cmsyModule <- function(input, output, session) {
  
  ns <- session$ns
  
  cmsy <- reactiveValues()
  cmsy$exploPlot <- FALSE
  cmsyUploadVreResult <- reactiveValues()
  
  fileContents <- reactiveValues()
  filePath <- reactiveValues()
  
  basicValidation <- function (a) {
    #if (is.null(colnames(a[1])) || is.na(colnames(a[1]))) return (FALSE)
    #if (tolower(colnames(a[1])) == 'stock') return(TRUE)
    validInputColumns<-c("stock","yr","ct","bt")
    if(ncol(a)==1){return ("delimiter error")
    } else if (length(setdiff(tolower(names(a[1:4])),validInputColumns))!=0) {return ("colname error")
    } else if (!is.numeric(a$ct)) {return ("not point")
    } else if ((max(as.numeric(a$yr))-min(as.numeric(a$yr)))<15) {return("under 15")
    } else {return(a)}
  }
  
  
  cmsyFileData <- reactive({
    inFileCmsy <- input$fileCmsy
    if (is.null(inFileCmsy)) {
      return (NULL)
    }
    contents <- read.csv(inFileCmsy$datapath)
    contents <- basicValidation(contents)
    if (is.null(contents)) {
      return (NULL)
    }else{print("First calculate")
      calculateAndUpdateYear(contents,sort(unique(contents$Stock))[1])
      }
    return (contents)
  })
  
  
  observeEvent(input$fileCmsy, {
    filePath$datapath <- input$fileCmsy$datapath
    contents <- cmsyFileData()
    if (!is.data.frame(contents)) {
      shinyjs::disable("go_cmsy")
      showModal(modalDialog(
        title = "Error",
        if(is.null(contents)){"Input file seems invalid"
          }else if(contents=="delimiter error"){"Please ensure that your .csv file delimiter is a comma ','" 
          }else if(contents=="not point"){"Please ensure your separate decimals using points ‘.’ or you don't have non numeric value"
          }else if(contents=="colname error"){
            text<-"Please ensure your columns names exactly match the guidelines, i.e."
            text<-paste0(text, "<ul>")
            text <- paste0(text, "<li>Stock</li>")
            text <- paste0(text, "<li>yr</li>")
            text <- paste0(text, "<li>ct</li>")
            text <- paste0(text, "<li>bt</li>")
            text <- paste0(text, "</ul>")
            HTML(text)
          }else if(contents=="under 15"){"Catch time series must be at least 15 years"
          } else{"Input file seems invalid"},
        easyClose = TRUE,
        footer = NULL
      ))
      fileContents$data <- NULL
      flog.error("Input file for CMSY %s seems invalid", filePath$datapath)
    } else {
      shinyjs::enable("go_cmsy")
      fileContents$data <- contents
      cmsy$exploPlot <- TRUE
      flog.info("Input file for CMSY %s seems valid", filePath$datapath)
    }
  })
  
  output$stockSelector <- renderUI({
    contents <- cmsyFileData()
    if (!is.null(contents)) {
    tags$div(
           selectInput(session$ns("stock"), "Select a stock", sort(unique(contents$Stock)))
    )
    }
  })
  

  calculateAndUpdateYear<-function(contents,stock) {
    
    if (is.null(contents)) {
      "Test"
      return(NULL)
    }
    
    maxYear <- 0
    minYear <- 0
    i<-0
    for (line in rownames(contents)) {
      if (contents[line, "Stock"] == stock) {
        if (i == 0) {
          maxYear <- contents[line, "yr"]
          minYear <- contents[line, "yr"]
        } else {
          if (maxYear < contents[line, "yr"]) {
            maxYear <- contents[line, "yr"]
          }
          if (minYear > contents[line, "yr"]) {
            minYear <- contents[line, "yr"]
          }
        }
        i <- i + 1
      }
    }
    print(minYear)
    print(maxYear)
    #updateTextInput(session, "minOfYear", value=as.integer(minYear))
    #updateTextInput(session, "maxOfYear", value=as.integer(maxYear))
    
    # updateTextInput(session, "startYear", value=as.integer(minYear))
    # updateTextInput(session, "endYear", value=as.integer(maxYear))
    updateSliderInput(session, "rangeYear", value=c(as.integer(minYear),as.integer(maxYear)))
    updateSearchInput(session, "qrangeYear", value=c(as.integer(minYear),as.integer(maxYear)))
    
    if (minYear < 1960) {
      updateSliderInput(session,"stb",value=c(0.5,0.9))
    } else {
      updateSliderInput(session,"stb",value=c(0.2,0.6))
    }
  }
  
  observeEvent(input$stock, {
    print("calculate")
    calculateAndUpdateYear(fileContents$data,input$stock)
  })
  

  output$CMSY_years_selected_out <- renderUI({
    contents <- cmsyFileData()
    if(is.null(contents)){
      maxY <- 2030
      minY <- 1990
    }else{
      minY <- try(round(min(contents$yr)),silent=TRUE)
      maxY <- try(round(max(contents$yr)),silent=TRUE)
      
      if(inherits(maxY,"try-error")){
        maxY <- 2030
        minY <- 1990
      }
    }
    sliderInput(ns("CMSY_years_selected"),"", value=c(minY,maxY), min = minY, max = maxY, step=1,sep='')
    
    } )
  
  output$CMSY_years_q_out <- renderUI({
    contents <- cmsyFileData()
    if(is.null(contents)){
      maxY <- 2030
      minY <- 1990
    }else{
      minY <- try(min(input$CMSY_years_selected),silent=TRUE)
      maxY <- try(max(input$CMSY_years_selected),silent=TRUE)
      
      if(inherits(maxY,"try-error")){
        maxY <- 2030
        minY <- 1990
      }
    }
    sliderInput(ns("CMSY_years_q"),"", value=c(minY,maxY), min = minY, max = maxY, step=1,sep='')
    
  } )
  
  observe({
    contents <- cmsyFileData()
    if(is.null(contents)){
      maxY <- 2030
      minY <- 1990
    }else{
      minY <- try(min(input$CMSY_years_selected),silent=TRUE)
      maxY <- try(max(input$CMSY_years_selected),silent=TRUE)
      
      if(inherits(maxY,"try-error")){
        maxY <- 2030
        minY <- 1990
      }
    }
  updateNumericInput(session,"int.yr", value=maxY-5, min = minY, max = maxY, step=1)
  })
  
  observe({
    if(!input$cmsy_checkbox_intb){
      js$removeBox2("box_cmsy_intb")
    }else{
      js$showBox2("box_cmsy_intb")
    }
  })
  
  # observeEvent(input$cmsy_checkbox_comparison, shinyjs::disable("cmsy_checkbox_comparison"))
  
  # observe({
  #   if(!input$cmsy_checkbox_comparison){
  #     js$removeBox2("fish_mort_ref_pts")
  #     js$removeBox2("biomass_ref_points")
  #   }else{
  #     js$showBox2("fish_mort_ref_pts")
  #     js$showBox2("biomass_ref_points")
  #     
  #   }
  # })
  
  
  observeEvent(input$go_cmsy, {

    query <- parseQueryString(session$clientData$url_search)
    
    if (is.null(fileContents$data)) {
      return (NULL)
    }
    yr <- fileContents$data$yr
    
    minYr <- NULL
    maxYr <- NULL
    for (y in yr) {
      if (is.null(minYr) || y < minYr) {
        minYr <- y
      }
      if (is.null(maxYr) || y > maxYr) {
        maxYr <- y
      }
    }
    
    if ((maxYr-minYr)<=15) {
      offset = maxYr-minYr
      showModal(modalDialog(
        title = "Error",
        paste0("The input time-series must cover at least 15 years in length, the provided one covers ", offset, " years"),
        easyClose = TRUE,
        footer = NULL
      ))
      return(NULL)
    } else {
      results = tryCatch({
        js$showComputing()
        #templateFileDlmTools <- paste0(getwd(), "/assets/cmsy/cmsyFastTemplate.xml")
        templateFileDlmTools <- paste0(getwd(), "/assets/cmsy/cmsyLegacyTemplate.xml")
        
        a <- fileContents$data
        
        group_ = ""
        name_ = ""
        en_name_ = ""
        scientific_name_ = ""
        sub_region_ = ""
        region_ = ""
        for (line in rownames(a)) {
          if (a[line, "Stock"] == input$stock) {
            name_ = a[line, "name"]
            group_ = a[line, "group"]
            en_name_ = a[line, "english_name"]
            scientific_name_ = a[line, "scientific_name"]
            region_ = a[line, "region"]
            sub_region_ = a[line, "subregion"]
            break
          }
        }
        
        cmsy$fast <- list()
        js$disableAllButtons()
        flog.info("Starting CMSY computation")

        ret <- runCmsy(region=region_,
                       subregion=toString(sub_region_),
                       stock=input$stock,
                       group=toString(group_),
                       name=toString(name_),
                       englishName=toString(en_name_),
                       scientificName=toString(scientific_name_),
                       source="-",
                       minOfYear=unique(min(cmsyFileData()$yr)),
                       maxOfYear=unique(max(cmsyFileData()$yr)),
                       startYear=min(input$CMSY_years_selected),
                       endYear=max(input$CMSY_years_selected)-1,
                       flim="NA",
                       fpa="NA",
                       blim="NA",
                       bpa="NA",
                       bmsy="NA",
                       fmsy="NA",
                       msy="NA",
                       msyBTrigger="NA",
                       b40="NA",
                       m="NA",
                       fofl="NA",
                       last_f="NA",
                       resiliance="Medium",
                       r.low=min(input$resiliance),
                       r.hi=max(input$resiliance),
                       stb.low=min(input$stb),
                       stb.hi=max(input$stb),
                       int.yr=if(input$cmsy_checkbox_intb){input$int.yr}else{"NA"},
                       intb.low=if(input$cmsy_checkbox_intb){min(input$intb)}else{"NA"},
                       intb.hi=if(input$cmsy_checkbox_intb){max(input$intb)}else{"NA"},
                       endb.low=min(input$endb),
                       endb.hi=max(input$endb),
                       q.start=min(input$CMSY_years_q),
                       q.end=max(input$CMSY_years_q),
                       btype=input$btype,
                       force.cmsy=FALSE,
                       comments=input$comments,
                       inputCsvFile=filePath$datapath,
                       WPS=session$userData$sessionWps())
                    
        
        js$enableAllButtons()
        js$hideComputing()
        js$showBox("box_cmsy_results")
        
        print("Create a temp dir for CMSY")
        cmsy_temp_dir <- tempdir()
        print(cmsy_temp_dir)
        
        invisible(lapply(1:nrow(ret), function(i){
          row <- ret[i,]
          row_out <- switch(row$Description,
            "Log of the computation" = {
              print(sprintf("Report computation logs : %s",row$Data))
              cmsy$method$log <- row$Data
            },
            "estimates" = {
              print(sprintf("Downloading Estimates : %s",row$Data))
              contents <- httr::content(httr::GET(row$Data),"text")
              contents <- gsub("Results for Management", "Reference points and indicators", contents)
              cmsy$method$textRaw <<- contents
              contents <- gsub("\n\r", "<br/>", contents)
              contents <- gsub("\n", "<br/>", contents)
              contents <- gsub("B/Bmsy in last year", "<b>B/Bmsy in last year</b>", contents)
              contents <- gsub("----------------------------------------------------------", "", contents)
              cmsy$method$text <<- contents
            },
            "management_charts" = {
              print(sprintf("Downloading Management charts : %s",row$Data))
              fileManagementChart <-paste(cmsy_temp_dir,"/","cmsy_fileManagementChart",".jpeg",sep="")
              fileManagementChart <- if(Sys.info()[["sysname"]] == "Windows") {paste(gsub("\\\\", "/", fileManagementChart)) } else {fileManagementChart}
              print(fileManagementChart)
              downloadFile(row$Data, fileManagementChart)
              cmsy$method$managementChart <<- fileManagementChart
              cmsy$method$managementChartUrl <<- row$Data
            },
            "analysis_charts" = {
              print(sprintf("Downloading Analysis charts : %s",row$Data))
              fileAnalisysChart <- paste(cmsy_temp_dir,"/","cmsy_fileAnalisysChart",".jpeg",sep="")
              fileAnalisysChart <- if(Sys.info()[["sysname"]] == "Windows") {paste(gsub("\\\\", "/", fileAnalisysChart)) } else {fileAnalisysChart}
              print(fileAnalisysChart)
              downloadFile(row$Data, fileAnalisysChart)
              cmsy$method$analisysChart <<- fileAnalisysChart
              cmsy$method$analisysChartUrl <<- row$Data
            }
          )
        }))

        if (!is.null(session$userData$sessionMode())) if(session$userData$sessionMode()=="GCUBE") {
          flog.info("Uploading CMSY report to i-Marine workspace")
          reportFileName <- paste(tempdir(),"/","CMSY_report_",format(Sys.time(), "%Y%m%d_%H%M_%s"),".pdf",sep="")
          createCmsyPDFReport(reportFileName, cmsy, input)
          cmsyUploadVreResult$res <- FALSE
          
          basePath <- paste0("/Home/",session$userData$sessionUsername(),"/Workspace")
          flog.info(paste0("Base path for upload: ", basePath))
          flog.info(paste0("Username for upload: ", session$userData$sessionUsername()))
          flog.info(paste0("Token for upload: ", session$userData$sessionToken()))
          
          SH_MANAGER <- session$userData$storagehubManager()
          
          tryCatch({
            uploadToIMarineFolder(SH_MANAGER, reportFileName, basePath, uploadFolderName)
            cmsyUploadVreResult$res <- TRUE
          }, error = function(err) {
            flog.error("Error uploading CMSY report to the i-Marine Workspace: %s", err)
            cmsyUploadVreResult$res <- FALSE
          }, finally = {})
        }
      }, error = function(err) {
        flog.error("Error in CMSY: %s ",err)
        showModal(modalDialog(
          title = "Error",
          HTML(sprintf(getErrorMessage("CMSY"), err)),
          easyClose = TRUE,
          footer = NULL
        ))
        return(NULL)
      },
      finally = {
        js$hideComputing()
        js$enableAllButtons()
      })
    }
  })
  
  observeEvent(input$reset_cmsy, {
    resetCmsyInputValues()
    cmsy$method <- NULL
    cmsy$exploPlot <- FALSE
  })
  
  ####### END OBSERVERS #######
  
  ####### CMSY OUTPUT FUNCTION #######
  output$downloadCmsyReport <- downloadHandler(
    # For PDF output, change this to "report.pdf"
    filename = paste("CMSY_report_",format(Sys.time(), "%Y%m%d_%H%M_%s"),".pdf",sep=""),
    content = function(file) {
      createCmsyPDFReport(file, cmsy, input)
    }
  )
  output$renderCmsyLog <- renderText({
    if ("method" %in% names(cmsy)) {
      if (!is.null(cmsy$method)) {
        log <- paste0("<a href='", cmsy$method$log, "'>Download the log of the computation</a>")
        log
      } else { "" }
    } else { "" }
  })
  output$renderCmsyInfo <- renderText({
    if ("method" %in% names(cmsy)) {
      if (!is.null(cmsy$method)) {
        cmsy$method$text <- gsub("\n\r", "<br/>", cmsy$method$text)
        cmsy$method$text <- gsub("\n", "<br/>", cmsy$method$text)
        cmsy$method$text <- gsub("Results for Management", "Reference points and indicators", cmsy$method$text)
        #cmsy$method$text <- gsub("B/Bmsy in last year", "<b>B/Bmsy in last year</b>", cmsy$method$text)
        cmsy$method$text <- gsub("----------------------------------------------------------", "", cmsy$method$text)
        cmsy$method$text
      } else {  "" }
    } else {  "" }
  })
  output$renderCmsyManagementChart <- renderImage({
    if ("method" %in% names(cmsy)) {
      if (!is.null(cmsy$method)) if(!is.null(cmsy$method$managementChart)) {
        Sys.sleep(1)
        w1 <- session$clientData$output_renderCmsyManagementChart_width
        h1 <- (w1*3)/4
        list(src = cmsy$method$managementChart,
             contentType = 'image/jpg',
             width = "100%",
             height = "100%")
      } else {
        list(src = "NULL")
      }
    } else {
      list(src = "NULL")
    }
  })
  output$renderCmsyAnalysisChart <- renderImage({
    if ("method" %in% names(cmsy)) {
      if (!is.null(cmsy$method)) if(!is.null(cmsy$method$analisysChart)) {
        Sys.sleep(1)
        w2 <- session$clientData$output_renderCmsyAnalysisChart_width
        h2 <- (w2*3)/4
        list(src = cmsy$method$analisysChart,
             contentType = 'image/jpg',
             width = "100%",
             height = "100%")
      } else {
        list(src = "NULL")
      }
    } else {
      list(src = "NULL")
    }
  })
  ####### END CMSY OUTPUT FUNCTION #######
  
  ####### CMSY TEXT #######
  output$cmsyMethodTitle <- renderText({
    session$userData$page("cmsy")
    text <- "<span><h3><b>CMSY (Catch-Maximum Sustainable Yield) Method</b></h3></span>"
    text
  })
  output$downloadCmsyReportButton <- renderUI({
    if (!is.null(cmsy$method)) {
      downloadButton(session$ns("downloadCmsyReport"), label = "Download Report")
    }
  })
  output$CmsyVREUpload <- renderText(
    {
      text <- ""
      if (!is.null(cmsy$dlmTools) || !is.null(cmsy$legacy) || !is.null(cmsy$fast)) {
        if (!is.null(session$userData$sessionMode()) && session$userData$sessionMode() == "GCUBE") {
          if (isTRUE(cmsyUploadVreResult$res)) {
            text <- paste0(text, VREUploadText)
          }
        }
      }
      text
    }
  )
  output$titleCmsy <- renderText({
    if ("dlmTools" %in% names(cmsy)) {
      if (!is.null(cmsy$dlmTools)) {
        title <- "<h1> CMSY Method - Results</h1>"
        title
      } else {  "" }
    } else {  "" }
  })
  output$titleCmsyManagementChart <- renderText({
    if ("method" %in% names(cmsy)) {
      if (!is.null(cmsy$method)) {
        #title <- "<h2> Management Charts </h2>"
        title <- "<h2> Output Graphs </h2>"
        title
      } else {  "" }
    } else {  "" }
  })
  output$captionCmsyManagementChart <- renderText({
    if ("method" %in% names(cmsy)) {
      if (!is.null(cmsy$method)) {
  caption <- "The upper left panel shows catches relative to the estimate of MSY, with indication of 95% confidence limits in grey. The upper right panel shows the development of relative total biomass (B/Bmsy), with the grey area indicating uncertainty. The lower left graph shows relative exploitation (F/Fmsy), with Fmsy corrected for reduced recruitment below 0.5 Bmsy. The lower-right panel shows the trajectory of relative stock size (B/Bmsy) over relative exploitation (F/Fmsy)."
  caption
      } else {  "" }
    } else {  "" }
  })
  
  output$captionCmsyAnalisysChart <- renderText({
    if ("method" %in% names(cmsy)) {
      if (!is.null(cmsy$method)) {
        caption <- "Panel A shows in black the time series of catches and in blue the three-years moving average with indication of highest and lowest catch, as used in the estimation of prior biomass by the default rules. Panel B shows the explored r-k log space and in dark grey the r-k pairs which were found by the CMSY model to be compatible with the catches and the prior information. Panel C shows the most probable r-k pair and its approximate 95% confidence limits in blue. Panel D shows in blue the biomass trajectory estimated by CMSY. Dotted lines indicate the 2.5th and 97.5th percentiles. Vertical blue lines indicate the prior biomass ranges. Panel E shows in blue the harvest rate from CMSY. Panel F shows the Schaefer equilibrium curve of catch/MSY relative to B/k, here indented at B/k < 0.25 to account for reduced recruitment at low stock sizes. The blue dots are scaled by CMSY estimates."
        caption
      } else {  "" }
    } else {  "" }
  })
  
  output$titleCmsyAnalisysChart <- renderText({
    if ("method" %in% names(cmsy)) {
      if (!is.null(cmsy$method)) {
        title <- "<h2> Summary Analysis </h2>"
        title
      } else {  "" }
    } else {  "" }
  })
  output$tableCmsyUnits <- renderText({
    if ("method" %in% names(cmsy)) {
      if (!is.null(cmsy$method)) {
        caption <- "<br>Recalling the following acronyms/symbols, their description and the unit associated <br>
        <b>Acronym/Symbol &nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp; Description  &nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp; Unit <br></b>
        B  &nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp; Biomass, total weight of exploited fish in water   &nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;  tonnes <br>
        Bmsy &nbsp;&nbsp;&nbsp; Biomass capable of producing MSY &nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;  tonnes <br>
        CPUE &nbsp;&nbsp;&nbsp; Catch per unit effort &nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp; <i>n</i> h<sup>-1</sup> or kg h<sup>-1</sup><br>
        Fmsy &nbsp;&nbsp;&nbsp;&nbsp; Rate of fishing mortality compatible with MSY: Fmsy=0.5 r  &nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp; year<sup>-1</sup><br>
        k  &nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp; Parameter of the Schaefer model indicating unexploited stock size  &nbsp;&nbsp;&nbsp; tonnes<br>
        K  &nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp; Parameter of the von Bertalanffy somatic growth equation  &nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;year<sup>-1</sup><br>
        MSY &nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp; Maximum sustainable yield: MSY=rk/4 &nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;tonnes year<sup>-1</sup><br>
        P<sub>t</sub> &nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp; Relative biomass: P<sub>t</sub>=B<sub>t</sub>/k <br>
        q &nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp; Catchability coefficient: CPUE<sub>t</sub>=q B<sub>t</sub><br>
        r &nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp; Maximum intrinsic rate of population increase &nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp; year<sup>-1</sup> <br> 
        t &nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp; Instant of time; subscript indicating annual values: C<sub>t</sub>,B<sub>t</sub> or CPUE<sub>t</sub> &nbsp;&nbsp;&nbsp; years <br>
        <br><br><br>"
        caption
      } else {  "" }
    } else {  "" }
  })
  
  output$cmsyWorkflowConsiderationsText <- renderText(getWorkflowConsiderationTextForCMSY()) 
  
  output$cmsyDataConsiderationsText <- renderText(getDataConsiderationTextForCmsy())
  output$cmsyDataConsiderationsText2 <- renderText(getDataConsiderationTextForCmsy())
  
  output$cmsyMethodConsiderationsText <- renderText(getMethodConsiderationTextForCmsy()  )
  output$cmsyMethodConsiderationsText2 <- renderText(getMethodConsiderationTextForCmsy() )
  
  output$cmsyResultConsiderationsText <- renderText(getResultConsiderationTextForCmsy() )
  output$cmsyResultConsiderationsText2 <- renderText(getResultConsiderationTextForCmsy())
  
 
  
  ## Data exploration catch plots
  ## --------------------------
  
  
  output$plot_cmsy_explo1 <- renderPlot({
    if(cmsy$exploPlot){
      contents <- cmsyFileData()
    data_exp<-subset(contents,Stock==input$stock & yr %in% seq(input$CMSY_years_selected[1],input$CMSY_years_selected[2],by=1))
    par(mar = c(1,4,0,1), oma = c(3,1,1,0))
    plot(data_exp$yr,data_exp$ct,type='l',xlab='Years',ylab='Catch in tonnes') ## PLOT ONLY THE STOCK SELECTED, RESOLVE ERROR 
    }else{NULL}
  })
  
  output$title_cmsy_explo1 <- renderText({
    if(cmsy$exploPlot){
      txt <- "<p class=\"pheader_elefan\">Figure 1:  The catch time series of the selected stock.</p>"
      txt
    }else{NULL}
  })
  
}
