yprModule <- function(input, output, session) {
  
  yprExec <- reactiveValues()
  yprUploadVreResult <- reactiveValues()
  
  inputYprData <- reactiveValues()
  
  yprFileData <- reactive({
    contents <- validateFishMethodsFile(input$fileYpr$datapath)
    if (is.null(contents)) {
      shinyjs::disable("go_YPR")
      showModal(modalDialog(
        title = "Error",
        "Input file seems invalid",
        easyClose = TRUE,
        footer = NULL
      ))
      return (NULL)
    } else {
      shinyjs::enable("go_YPR")
      return (contents)  
    }
    
  })
  
  observeEvent(input$fileYpr, {
    inputYprData$data <- yprFileData()
  })
  
  observeEvent(input$go_YPR, {
    
    js$showComputing()
    js$removeBox("box_ypr_results")
    dat <- inputYprData$data
    
    flog.info("Starting YPR computation")
    res <- ypr_shinyApp(age=dat$age,wgt=dat$ssbwgt,partial=dat$partial,M=input$YPR_M,plus=input$YPR_Plus,oldest=input$YPR_oldest,maxF=input$YPR_maxF,incrF=input$YPR_incrF, graph = FALSE)
    js$hideComputing()
    if ('error' %in% names(res)) {
      showModal(modalDialog(
        title = "Error",
        res$error,
        easyClose = TRUE,
        footer = NULL
      ))
    } else {
      yprExec$results <- res
      js$showBox("box_ypr_results")
      
      if (!is.null(session$userData$sessionMode()) && session$userData$sessionMode()=="GCUBE") {
        print("uploading to VRE")
        reportFileName <- paste("/tmp/","Ypr_report_",format(Sys.time(), "%Y%m%d_%H%M_%s"),".pdf",sep="")
        createYprPDFReport(reportFileName, yprExec, input)
        yprUploadVreResult$res <- FALSE
        tryCatch({
          if (fileFolderExistsInPath(session$userData$sessionUsername(),session$userData$sessionToken(),paste0("/Home/",session$userData$sessionUsername(),"/Workspace/"), uploadFolderName) == FALSE) {
            print("Creating folder")
            createFolderWs(
              session$userData$sessionUsername(), session$userData$sessionToken(),
              paste0("/Home/",session$userData$sessionUsername(),"/Workspace/"),
              uploadFolderName, 
              uploadFolderDescription)
          }
          uploadToVREFolder(
            username = session$userData$sessionUsername(), 
            token = session$userData$sessionToken(), 
            relativePath = paste0("/Home/",session$userData$sessionUsername(),"/Workspace/", uploadFolderName, "/"), 
            file = reportFileName,
            overwrite = TRUE,
            archive = FALSE
          )
          yprUploadVreResult$res <- TRUE
        }, error = function(err) {
          print(paste0("Error uploading SBPR report to the Workspace: ", err))
          yprUploadVreResult$res <- FALSE
        }, finally = {})
      }
    }
  })
  output$yprFishingMortality <- renderText({
    if (is.na(session$userData$fishingMortality$FcurrGA) && is.na(session$userData$fishingMortality$FcurrSA) && is.na(session$userData$fishingMortality$Fcurr)) {
      text <- "<strong>You need to estimate Fcurrent before calculating YPR, using ELEFAN method if you have lengnth frequency data. The data used for ELEFAN and YPR analysis should come from the same fish stock.</strong>"
    } else {
      text <- ""
      if (!is.na(session$userData$fishingMortality$Fcurr)) {
        text <- paste0(text, "<strong>Fishing moratlity calculated with ELEFAN method: </strong>", session$userData$fishingMortality$Fcurr, "<br/>")
      }
      if (!is.na(session$userData$fishingMortality$FcurrGA)) {
        text <- paste0(text, "<strong>Fishing moratlity calculated with ELEFAN GA method: </strong>", session$userData$fishingMortality$FcurrGA, "<br/>")
      }
      if (!is.na(session$userData$fishingMortality$FcurrSA)) {
        text <- paste0(text, "<strong>Fishing moratlity calculated with ELEFAN SA method: </strong>", session$userData$fishingMortality$FcurrSA, "<br/>")
      }
    }
    text
  })
  output$yprOutPlot <- renderPlot({
    if ('results' %in% names(yprExec)) {
      YPR <- yprExec$results$F_vs_YPR
      plot(YPR[,2]~YPR[,1],ylab="Yield-Per-Recruit",xlab="Fishing Mortality (F)",type="l")
      abline(h = yprExec$results$Reference_Points[2,2], v = yprExec$results$Reference_Points[2,1], col = "black", lty = 2)
      abline(h = yprExec$results$Reference_Points[1,2], v = yprExec$results$Reference_Points[1,1], col = "red", lty = 2)
      legend(1.7, 0.09, legend=c("F-0.1", "F-Max"),col=c("red", "black"), lty=2:2, cex=0.9)
    }
  })
  output$yprOutTable <- renderTable({
    if ('results' %in% names(yprExec)) {
      #yprExec$results$Reference_Points
      df <- as.data.frame(yprExec$results$Reference_Points)
      if (!is.na(session$userData$fishingMortality$Fcurr)) {
        df$Fcurr <- session$userData$fishingMortality$Fcurr
      }
      else if (!is.na(session$userData$fishingMortality$FcurrGA)) {
        df$Fcurr <- session$userData$fishingMortality$FcurrGA
      }
      else if (!is.na(session$userData$fishingMortality$FcurrSA)) {
        df$Fcurr <- session$userData$fishingMortality$FcurrSA
      } else {
        df$Fcurr <- "You need to estimate Fcurrent before calculating F30%MSPR, using ELEFAN method if you have length frequency data."  
      }
      #colnames(yprExec$results$Reference_Points) <- c("F", "Yield Per Recruit")
      colnames(df) <- c("F", "Yield Per Recruit", "Fcurrent")
      df
    }
  }, 
  include.rownames=TRUE, align="c")
  output$yprDifference <- renderText({
    if ('results' %in% names(yprExec)) {
      differenceinYPR = round(yprExec$results$Reference_Points[2,2] - yprExec$results$Reference_Points[1,2], 6)
      text <- paste0("<b>Difference in YPR: </b>",round(differenceinYPR, 4))
      text
    }
  })
  output$downloadYprReport <- renderUI({
    if ("results" %in% names(yprExec)) {
      colnames(yprExec$results$Reference_Points) <- c("F", "Yield Per Recruit")
      downloadButton(session$ns('createYprReport'), 'Download Report')
    }
  })
  output$YPRVREUpload <- renderText(
    {
      text <- ""
      if ("results" %in% names(yprExec)) {
        if (!is.null(session$userData$sessionMode()) && session$userData$sessionMode() == "GCUBE") {
          if (isTRUE(yprUploadVreResult$res)) {
            text <- paste0(text, VREUploadText)
          }
        }
      }
      text
    }
  )
  output$createYprReport <- downloadHandler(
    filename = paste("Ypr_report_",format(Sys.time(), "%Y%m%d_%H%M_%s"),".pdf",sep=""),
    content = function(file) {
      createYprPDFReport(file, yprExec, input)
    }
  )
  
  output$YPRDataConsiderationsText <- renderText({
    text <- "<h5><b>Ensure that spawning stock weight-at-age data is representative of the full population, i.e., are all age groups sampled?</b></h5>"
    text <- paste0(text, "<h5>", "**If desired, the life history parameters pulled from FishBase.org in the Supporting Tools: 'Natural Mortality Estimators' tool could be used to provide estimates of M in the Optional Parameters section.", "</h5>")
    text
  })
}