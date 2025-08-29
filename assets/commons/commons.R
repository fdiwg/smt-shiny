library(httr)
library(jsonlite)
library(pracma)

########## CONSTANTS ##########

uploadFolderName <- "StockMonitoringTool"
uploadFolderDescription <- "SDG 14.4.1 VRE Stock Monitoring Tool results"
VREUploadText <- "The report has been uploaded to your VRE workspace"
gcubeTokenQueryParam <- "gcube-token"
apiUrl <- "https://api.d4science.org/rest/2/people/profile?gcube-token="
#vreToken <- "96d4f92a-ef32-42ab-a47a-425ee5618644-843339462"


######## END CONSTANTS ########

##### COMMON JAVASCRIPT CODE #####
jscode <- "
shinyjs.showBox = function(boxid) {
$('#' + boxid).parent('div').css('visibility','visible');
}
shinyjs.removeBox = function(boxid) {
$('#' + boxid).parent('div').css('visibility','hidden');
}
shinyjs.showBox2 = function(boxid) {
$('#' + boxid).parent('div').css('display','block');
}
shinyjs.removeBox2 = function(boxid) {
$('#' + boxid).parent('div').css('display','none');
}
shinyjs.disableAllButtons = function() {
$('.action-button').attr('disabled', true);
}
shinyjs.enableAllButtons = function() {
$('.action-button').attr('disabled', false);
}
shinyjs.showComputing = function() {
$('.loadingCustom').css('visibility','visible');
}
shinyjs.hideComputing = function() {
$('.loadingCustom').css('visibility','hidden');
}
shinyjs.expandBox = function(boxid) {
if (document.getElementById(boxid).parentElement.className.includes('collapsed-box')) {
$('#' + boxid).closest('.box').find('[data-widget=collapse]').click();
}};
shinyjs.collapseBox = function(boxid) {
if (!document.getElementById(boxid).parentElement.className.includes('collapsed-box')) {
$('#' + boxid).closest('.box').find('[data-widget=collapse]').click();
}}
"
##### END COMMON JAVASCRIPT CODE #####

app_load_spinner <- function(message = "") {
  html <- ""
  html <- paste0(html, "<div style=\"position: absolute; left: 50%;\">")
  html <- paste0(html, "<div style=\"color: white; width:150px; margin-left:-75px;\">", message, "</div>")
  html <- paste0(html, "</div>")
  html <- paste0(html, "<div class=\"sk-fading-circle\">")
  html <- paste0(html, "<div class=\"sk-circle1 sk-circle\"></div>")
  html <- paste0(html, "<div class=\"sk-circle2 sk-circle\"></div>")
  html <- paste0(html, "<div class=\"sk-circle3 sk-circle\"></div>")
  html <- paste0(html, "<div class=\"sk-circle4 sk-circle\"></div>")
  html <- paste0(html, "<div class=\"sk-circle5 sk-circle\"></div>")
  html <- paste0(html, "<div class=\"sk-circle6 sk-circle\"></div>")
  html <- paste0(html, "<div class=\"sk-circle7 sk-circle\"></div>")
  html <- paste0(html, "<div class=\"sk-circle8 sk-circle\"></div>")
  html <- paste0(html, "<div class=\"sk-circle9 sk-circle\"></div>")
  html <- paste0(html, "<div class=\"sk-circle10 sk-circle\"></div>")
  html <- paste0(html, "<div class=\"sk-circle11 sk-circle\"></div>")
  html <- paste0(html, "<div class=\"sk-circle12 sk-circle\"></div>")
  html <- paste0(html, "</div>")
  return (html)
}

getVREUsername <- function(url, token) {
  url_ <- paste0(url, token)
  response <-  GET(url_)
  response$status
  if (response$status == 200) {
    call <- fromJSON(content(response, as = "text"), flatten = TRUE)
    return (call$result$username)
  } else {
    return (NULL)
  }
}

buildUrl <- function(session, path) {
  port <- session$clientData$url_port
  host <- session$clientData$url_hostname
  protocol <- session$clientData$url_protocol
  url <- paste0(protocol, "//", host, ":", port, "/", path)
  return (url);
}


dev.on <- function(path, format, width, height){
    if(format == "pdf"){
        pdf(path, width = width, height = height)
    }else if(format == "png"){
        png(path, width = width, height = height, units = "in", res = 120)
    }else if(format == "jpeg"){
        jpeg(path, width = width, height = height, units = "in", res = 120)
    }else if(format == "tiff"){
        tiff(path, width = width, height = height, units = "in", res = 120)
    }else if(format == "bmp"){
        bmp(path, width = width, height = height, units = "in", res = 120)
    }else if(format == "ps"){
        postscript(path, width = width, height = height)
    }
}

save.table <- function(file, path, format){
    if(format == "csv"){
        write.csv(file, path, row.names = FALSE)
    }else if(format == "xls"){
        openxlsx::write.xlsx(file, path, row.names = FALSE)
    }else if(format == "xlsx"){
        openxlsx::write.xlsx(file, path, row.names = FALSE)
    }
}


########### Save reports to file #############
createCmsyPDFReport <- function(file, cmsy, input) {
  tempReport <- file.path(tempdir(), "cmsyReport.Rmd")
  file.copy("assets/cmsy/cmsyReport.Rmd", tempReport, overwrite = TRUE)


  if (!is.null(cmsy$method$analisysChartUrl)) {
    fileAnalisysChart <- paste(tempdir(),"/","cmsy_fileAnalisysChart",".jpeg",sep="")
    downloadFile(cmsy$method$analisysChartUrl, fileAnalisysChart)
    cmsy$method$analisysChart <- fileAnalisysChart
  }
  if (!is.null(cmsy$method$analisysChartUrl)) {
    fileManagementChart <-paste(tempdir(),"/","cmsy_fileManagementChart",".jpeg",sep="")
    #fileManagementChart <- tempfile(fileext=".jpg")
    downloadFile(cmsy$method$managementChartUrl, fileManagementChart)
    cmsy$method$managementChart <- fileManagementChart
  }

  # Set up parameters to pass to Rmd document
  params <- list(cmsy = cmsy, inputParams = input)

  # Knit the document, passing in the `params` list, and eval it in a
  # child of the global environment (this isolates the code in the document
  # from the code in this app).
  rmarkdown::render(tempReport, output_file = file, params = params)
}

# createElefanGaPDFReport <- function(file, elefan_ga, input, output) {
#   print(paste0("Input file", input$fileGa))
#   tempReport <- file.path(tempdir(), "elefan_ga.Rmd")
#   file.copy("assets/tropfishr/markdown/elefan_ga.Rmd", tempReport, overwrite = TRUE)
#   params <- list(elefan = elefan_ga, inputParams = input, outputParams = output)
#   return (rmarkdown::render(tempReport, output_file = file, params = params))
# }

createElefanGaPDFReport <- function(file, elefan_ga, input, output) {
  print(paste0("Input file", input$fileGa))
  tempReport <- file.path(tempdir(), "tropfishr.Rmd")
  file.copy("assets/LBM/tropfishr/markdown/tropfishr.Rmd", tempReport, overwrite = TRUE)
  return (rmarkdown::render(tempReport, output_file = file))
}

createLBIPDFReport <- function(file, lbi_dat, input, output) {
  print(paste0("Input file", input$fileLBI))
  tempReport <- file.path(tempdir(), "lbi.Rmd")
  file.copy("assets/LBM/lbi/markdown/lbi.Rmd", tempReport, overwrite = TRUE)
  return (rmarkdown::render(tempReport, output_file = file))
}

createLBSPRPDFReport <- function(file, lbspr_dat, input, output) {
  print(paste0("Input file", input$fileLBSPR))
  tempReport <- file.path(tempdir(), "lbspr.Rmd")
  file.copy("assets/LBM/lbspr/markdown/lbspr.Rmd", tempReport, overwrite = TRUE)
  return (rmarkdown::render(tempReport, output_file = file))
}

createSpictPDFReport <- function(file, spict_dat, input, output) {
    print(paste0("Input file", input$file))
    tempReport <- file.path(tempdir(), "spict.Rmd")
    file.copy("assets/SPM/spict/markdown/spict.Rmd", tempReport, overwrite = TRUE)
    return (rmarkdown::render(tempReport, output_file = file))
}

createSbprPDFReport <- function(file, sbprExec, input) {
  print(paste(sep=" ", file))
  tempReport <- file.path(tempdir(), "sbpr.Rmd")
  file.copy("assets/fishmethods/sbpr.Rmd", tempReport, overwrite = TRUE)
  sbprExec$perc <- input$SBPR_MSP
  params <- list(sbprExec = sbprExec, inputParams = input)
  rmarkdown::render(tempReport, output_file = file, params = params)
}

createYprPDFReport <- function(file, yprExec, input) {
  tempReport <- file.path(tempdir(), "ypr.Rmd")
  file.copy("assets/fishmethods/ypr.Rmd", tempReport, overwrite = TRUE)
  params <- list(yprExec = yprExec, inputParams = input)
  rmarkdown::render(tempReport, output_file = file, params = params)
}

clearResults <- function(id) {
  localJs <- paste0("document.getElementById('", id, "').parentElement.style.visibility = 'hidden'" )
  shinyjs::runjs(localJs)
}

uploadToIMarineFolder <- function(manager, reportFileName, basePath, folderName){
  folderID <- manager$searchWSFolderID(folderPath = folderName)
  if (is.null(folderID)) {
    flog.info("Creating folder [%s] in i-Marine workspace", folderName)
    manager$createFolder(name = folderName)
  }
  flog.info("Trying to upload %s to i-Marine workspace folder %s", reportFileName, file.path(basePath, folderName))
  manager$uploadFile(
    folderPath = file.path(basePath, folderName),
    file = reportFileName,
    description = "CMSY report"
  )
  flog.info("File %s successfully uploaded to the i-Marine folder %s", reportFileName, file.path(basePath, uploadFolderName))
}



########### Save zip to file #############

makeContentElefanGAzip <- function(file, elefan_ga, input, output) {
  print(paste0("Input file", input$fileGa))
  tempDir <- tempdir()
  allfiles <- c()

  ## all data
  path <- file.path(tempDir, paste0("TropFishR_data.RData"))
  allfiles <- c(allfiles, path)
  tmp <- shiny::reactiveValuesToList(elefan_ga)
  save(tmp, file = path, version = 2)

  ## Data plot
  path <- file.path(tempDir,paste0("TropFishR_data.", input$fig_format_ga))
  allfiles <- c(allfiles, path)
  dev.on(path, input$fig_format_ga, width = 11, height = 10)
  plotTropFishR.data(elefan_ga, input)
  dev.off()

  ## Growth plot
  path <- file.path(tempDir,paste0("TropFishR_growth.", input$fig_format_ga))
  allfiles <- c(allfiles, path)
  dev.on(path, input$fig_format_ga, width = 11, height = 10)
  plotTropFishR.growth(elefan_ga, input)
  dev.off()

  ## GA plot
  path <- file.path(tempDir,paste0("TropFishR_ga.", input$fig_format_ga))
  allfiles <- c(allfiles, path)
  dev.on(path, input$fig_format_ga, width = 11, height = 10)
  plotTropFishR.ga(elefan_ga, input)
  dev.off()

  ## Mort plot
  path <- file.path(tempDir,paste0("TropFishR_mort.", input$fig_format_ga))
  allfiles <- c(allfiles, path)
  dev.on(path, input$fig_format_ga, width = 11, height = 10)
  plotTropFishR.mort(elefan_ga, input)
  dev.off()

  ## Catch curve plot
  path <- file.path(tempDir,paste0("TropFishR_catchcurve.", input$fig_format_ga))
  allfiles <- c(allfiles, path)
  dev.on(path, input$fig_format_ga, width = 11, height = 10)
  plotTropFishR.catchcurve(elefan_ga, input)
  dev.off()

  ## Selectivity plot
  path <- file.path(tempDir,paste0("TropFishR_sel.", input$fig_format_ga))
  allfiles <- c(allfiles, path)
  dev.on(path, input$fig_format_ga, width = 11, height = 10)
  plotTropFishR.sel(elefan_ga, input)
  dev.off()

  ## YPR plot
  path <- file.path(tempDir,paste0("TropFishR_ypr.", input$fig_format_ga))
  allfiles <- c(allfiles, path)
  dev.on(path, input$fig_format_ga, width = 11, height = 10)
  plotTropFishR.ypr(elefan_ga, input)
  dev.off()

  ## Isopleth plot
  path <- file.path(tempDir,paste0("TropFishR_iso.", input$fig_format_ga))
  allfiles <- c(allfiles, path)
  dev.on(path, input$fig_format_ga, width = 11, height = 10)
  plotTropFishR.iso(elefan_ga, input)
  dev.off()

  ## Table with length data
  path <- file.path(tempDir, paste0("TropFishR_data.", input$tab_format_ga))
  allfiles <- c(allfiles, path)
  save.table(tableTropFishR.data(elefan_ga, input, format = "dataframe"),
             path, input$tab_format_ga)

  ## Table with input parameters
  path <- file.path(tempDir, paste0("TropFishR_input.", input$tab_format_ga))
  allfiles <- c(allfiles, path)
  save.table(tableTropFishR.input(elefan_ga, input, format = "dataframe"),
             path, input$tab_format_ga)

  ## Growth table
  path <- file.path(tempDir, paste0("TropFishR_growth.", input$tab_format_ga))
  allfiles <- c(allfiles, path)
  save.table(tableTropFishR.growth(elefan_ga, input, format = "dataframe"),
             path, input$tab_format_ga)

  ## Mort table
  path <- file.path(tempDir, paste0("TropFishR_mort.", input$tab_format_ga))
  allfiles <- c(allfiles, path)
  save.table(tableTropFishR.mort(elefan_ga, input, format = "dataframe"),
             path, input$tab_format_ga)

  ## Refs table
  path <- file.path(tempDir, paste0("TropFishR_refs.", input$tab_format_ga))
  allfiles <- c(allfiles, path)
  save.table(tableTropFishR.refs(elefan_ga, input, format = "dataframe"),
             path, input$tab_format_ga)

  ## Stock status table
  path <- file.path(tempDir, paste0("TropFishR_status.", input$tab_format_ga))
  allfiles <- c(allfiles, path)
  save.table(tableTropFishR.status(elefan_ga, input, format = "dataframe"),
             path, input$tab_format_ga)

  return(zip(zipfile = file, files = allfiles, flags = "-j"))
}



makeContentLBIzip <- function(file, lbi_dat, input, output) {
  print(paste0("Input file", input$fileLBI))
  tempDir <- tempdir()
  allfiles <- c()

  ## all data
  path <- file.path(tempDir, paste0("LBI_data.RData"))
  allfiles <- c(allfiles, path)
  tmp <- shiny::reactiveValuesToList(lbi_dat)
  save(tmp, file = path, version = 2)

  ## Table with length data
  path <- file.path(tempDir, paste0("LBI_data.", input$tab_format_lbi))
  allfiles <- c(allfiles, path)
  save.table(tableLBI.data(lbi_dat, input, format = "dataframe"),
             path, input$tab_format_lbi)

  ## Table with input parameters
  path <- file.path(tempDir, paste0("LBI_input.", input$tab_format_lbi))
  allfiles <- c(allfiles, path)
  save.table(tableLBI.inputPars(lbi_dat, input, format = "dataframe"),
             path, input$tab_format_lbi)

  ## Table with indicators
  path <- file.path(tempDir, paste0("LBI_indicators.", input$tab_format_lbi))
  allfiles <- c(allfiles, path)
  save.table(tableLBI.indicators(lbi_dat, input, format = "dataframe"),
             path, input$tab_format_lbi)

  ## Table with ratios
  path <- file.path(tempDir, paste0("LBI_ratios.", input$tab_format_lbi))
  allfiles <- c(allfiles, path)
  save.table(tableLBI.ratios(lbi_dat, input, format = "dataframe"),
             path, input$tab_format_lbi)

  ## Data plot
  path <- file.path(tempDir,paste0("LBI_data.", input$fig_format_lbi))
  allfiles <- c(allfiles, path)
  dev.on(path, input$fig_format_lbi, width = 11, height = 10)
  plotLBI.data(lbi_dat, input)
  dev.off()

  ## Fit plot
  path <- file.path(tempDir,paste0("LBI_fit.",input$fig_format_lbi))
  allfiles <- c(allfiles, path)
  dev.on(path, input$fig_format_lbi, width = 11, height = 11)
  plotLBI.fit(lbi_dat, input)
  dev.off()

  return(zip(zipfile = file, files = allfiles, flags = "-j"))
}


makeContentLBSPRzip <- function(file, lbspr_dat, input, output) {
  print(paste0("Input file", input$fileLBSPR))
  tempDir <- tempdir()
  allfiles <- c()

  ## all data
  path <- file.path(tempDir, paste0("LBSPR_data.RData"))
  allfiles <- c(allfiles, path)
  tmp <- shiny::reactiveValuesToList(lbspr_dat)
  save(tmp, file = path, version = 2)

  ## Table with length data
  path <- file.path(tempDir, paste0("LBSPR_data.", input$tab_format_lbspr))
  allfiles <- c(allfiles, path)
  save.table(tableLBSPR.data(lbspr_dat, input, format = "dataframe"),
             path, input$tab_format_lbspr)

  ## Table with input parameters
  path <- file.path(tempDir, paste0("LBSPR_input.", input$tab_format_lbspr))
  allfiles <- c(allfiles, path)
  save.table(tableLBSPR.inputPars(lbspr_dat, input, format = "dataframe"),
             path, input$tab_format_lbspr)

  ## Table with results
  path <- file.path(tempDir, paste0("LBSPR_results.", input$tab_format_lbspr))
  allfiles <- c(allfiles, path)
  save.table(tableLBSPR.results(lbspr_dat, input, format = "dataframe"),
             path, input$tab_format_lbspr)

  ## Data plot
  path <- file.path(tempDir,paste0("LBSPR_data.", input$fig_format_lbspr))
  allfiles <- c(allfiles, path)
  dev.on(path, input$fig_format_lbspr, width = 11, height = 10)
  plotLBSPR.data(lbspr_dat, input)
  dev.off()

  ## Sel plot
  path <- file.path(tempDir,paste0("LBSPR_sel.", input$fig_format_lbspr))
  allfiles <- c(allfiles, path)
  dev.on(path, input$fig_format_lbspr, width = 9, height = 8)
  plotLBSPR.sel(lbspr_dat, input)
  dev.off()

  ## Pie plot
  path <- file.path(tempDir,paste0("LBSPR_pie.", input$fig_format_lbspr))
  allfiles <- c(allfiles, path)
  dev.on(path, input$fig_format_lbspr, width = 9, height = 8)
  plotLBSPR.pie(lbspr_dat, input)
  dev.off()

  ## TS plot
  path <- file.path(tempDir,paste0("LBSPR_timeSeries.", input$fig_format_lbspr))
  allfiles <- c(allfiles, path)
  dev.on(path, input$fig_format_lbspr, width = 12, height = 9)
  plotLBSPR.ts(lbspr_dat, input)
  dev.off()

  return(zip(zipfile = file, files = allfiles, flags = "-j"))
}

makeContentSpictzip <- function(file, spict_dat, input, output) {
    print(paste0("Input file", input$file))
    tempDir <- tempdir()
    allfiles <- c()

    ## all data
    path <- file.path(tempDir, paste0("spict_data.RData"))
    allfiles <- c(allfiles, path)
    tmp <- shiny::reactiveValuesToList(spict_dat)
    save(tmp, file = path, version = 2)

    ## Data plot
    path <- file.path(tempDir,paste0("spict_data.", input$fig_format))
    allfiles <- c(allfiles, path)
    dev.on(path, input$fig_format, width = 11, height = 10)
    plotSpict.data(spict_dat$dataExplo$inp, input)
    dev.off()

    ## Data unc plot
    path <- file.path(tempDir,paste0("spict_data_unc.", input$fig_format))
    allfiles <- c(allfiles, path)
    dev.on(path, input$fig_format, width = 11, height = 10)
    plotSpict.data.unc(spict_dat$dataExplo$inp, input)
    dev.off()

    ## Diag 1 plot
    path <- file.path(tempDir,paste0("spict_diag1.", input$fig_format))
    allfiles <- c(allfiles, path)
    dev.on(path, input$fig_format, width = 11, height = 10)
    plotSpict.diag1(spict_dat, input)
    dev.off()

    ## Diag 2 plot
    path <- file.path(tempDir,paste0("spict_diag2.", input$fig_format))
    allfiles <- c(allfiles, path)
    dev.on(path, input$fig_format, width = 11, height = 10)
    plotSpict.diag2(spict_dat, input)
    dev.off()

    ## Priors plot
    path <- file.path(tempDir,paste0("spict_priors.", input$fig_format))
    allfiles <- c(allfiles, path)
    dev.on(path, input$fig_format, width = 11, height = 10)
    inp <- spict_dat$dataExplo$inp
    nopriors <- spict::get.no.active.priors(inp)
    par(mfrow = preferred_mfrow(nopriors))
    plotSpict.priors(inp, automfrow = FALSE, do.plot=nopriors)
    dev.off()

    ## Summary plot
    path <- file.path(tempDir,paste0("spict_res.", input$fig_format))
    allfiles <- c(allfiles, path)
    dev.on(path, input$fig_format, width = 11, height = 10)
    plotSpict.sum(spict_dat, input)
    dev.off()

    ## Absolute plot
    path <- file.path(tempDir,paste0("spict_res_abs.", input$fig_format))
    allfiles <- c(allfiles, path)
    dev.on(path, input$fig_format, width = 11, height = 10)
    plotSpict.abs(spict_dat, input)
    dev.off()

    ## Production curve plot
    path <- file.path(tempDir,paste0("spict_prod.", input$fig_format))
    allfiles <- c(allfiles, path)
    dev.on(path, input$fig_format, width = 11, height = 10)
    plotSpict.prod(spict_dat, input)
    dev.off()

    ## Priors2 plot
    path <- file.path(tempDir,paste0("spict_prior_posterior.", input$fig_format))
    allfiles <- c(allfiles, path)
    dev.on(path, input$fig_format, width = 11, height = 10)
    plotSpict.priors(spict_dat$results, automfrow = FALSE, do.plot=nopriors)
    dev.off()

    ## Residuals1 plot
    path <- file.path(tempDir,paste0("spict_resid_obs.", input$fig_format))
    allfiles <- c(allfiles, path)
    dev.on(path, input$fig_format, width = 11, height = 10)
    plotSpict.resid1(spict_dat, input)
    dev.off()

    ## Residuals2 plot
    path <- file.path(tempDir,paste0("spict_resid_process.", input$fig_format))
    allfiles <- c(allfiles, path)
    dev.on(path, input$fig_format, width = 11, height = 10)
    if(spict_dat$results$opt$convergence == 0) {
        plotSpict.resid2(spict_dat, input)
    } else {
        plot.new()
        legend("center",
               legend = "The process residuals could not be estimated, because the model did not converge.",
               pch = NA)
    }
    dev.off()

    ## Table with data
    path <- file.path(tempDir, paste0("spict_data.", input$tab_format))
    allfiles <- c(allfiles, path)
    save.table(tableSpict.data(spict_dat, input, format = "dataframe"),
               path, input$tab_format)

    ## Table with priors
    path <- file.path(tempDir, paste0("spict_priors.", input$tab_format))
    allfiles <- c(allfiles, path)
    save.table(tableSpict.priors(spict_dat, input, format = "dataframe"),
               path, input$tab_format)

    ## Table with Estimates
    path <- file.path(tempDir, paste0("spict_estimates.", input$tab_format))
    allfiles <- c(allfiles, path)
    save.table(tableSpict.estimates(spict_dat, input, format = "dataframe"),
               path, input$tab_format)

    ## Table with Stochastic refs
    path <- file.path(tempDir, paste0("spict_refs_s.", input$tab_format))
    allfiles <- c(allfiles, path)
    save.table(tableSpict.refs_s(spict_dat, input, format = "dataframe"),
               path, input$tab_format)

    ## Table with Deterministic refs
    path <- file.path(tempDir, paste0("spict_refs_d.", input$tab_format))
    allfiles <- c(allfiles, path)
    save.table(tableSpict.refs_d(spict_dat, input, format = "dataframe"),
               path, input$tab_format)

    ## Table with States
    path <- file.path(tempDir, paste0("spict_states.", input$tab_format))
    allfiles <- c(allfiles, path)
    save.table(tableSpict.states(spict_dat, input, format = "dataframe"),
               path, input$tab_format)

    ## Table with Forecast
    path <- file.path(tempDir, paste0("spict_forecast.", input$tab_format))
    allfiles <- c(allfiles, path)
    save.table(tableSpict.pred(spict_dat, input, format = "dataframe"),
               path, input$tab_format)

    return(zip(zipfile = file, files = allfiles, flags = "-j"))
}
