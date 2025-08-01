##
## This is the StockMonitoringTool Shiny web application. You can run the application by clicking
## the 'Run App' button above.
## The StockMonitoringTool shiny application will support the FAO - SDG 14.4.1 E-learning course
##
## Author: Enrico Anello <enrico.anello@fao.org> <enrico.anello@gmail.com>
##

## For local docker dev
## set to FALSE for VRE and local processing without WPS;
## set to TRUE for WPS not in VRE (requires token and docker setup)
withtoken <- FALSE
token <- '' ## specify for local docker + WPS

## packages
source("assets/commons/package_utils.R")
library(yaml)
library(jsonlite)
library(XML)
library(xml2)
library(rmarkdown)
library(shiny)
library(shinydashboard)
library(shinyjs)
library(shinythemes)
library(shinyWidgets)
library(RCurl)
library(ks)
library(ggplot2)
library(shinyBS)
library(lubridate)
library(waiter)
library(pracma)
library(googleVis)
library(stringr)
library(R.utils)
library(V8)
library(DT)
library(futile.logger)
library(nloptr)
library(R6)
library(openxlsx)
library(sodium)
library(keyring)
library(kableExtra)
library(ows4R)
library(d4storagehub4R)
library(shinysky)
library(rfishbase)
library(fishmethods)
library(TropFishR)
library(LBSPR)
library(rintrojs) ## install.packages("rintrojs")
library(spict) ## remotes::install_github("DTUAqua/spict/spict")
library(shinybusy) ## install.packages("shinybusy")

##version/date to show on app
SMT_VERSION = getAppVersion()
SMT_DATE = getAppDate()

##### Dependencies
## UI
source("ui/menu.R")
## CMSY
source("ui/cmsy/cmsyUI.R")
## Length-based methods
source("ui/LBM/commonUI.R")
source("ui/LBM/tropfishr/commonUI.R")
source("ui/LBM/tropfishr/tropfishrUI.R")
source("ui/LBM/lbi/commonUI.R")
source("ui/LBM/lbi/lbiUI.R")
source("ui/LBM/lbspr/commonUI.R")
source("ui/LBM/lbspr/lbsprUI.R")
source("ui/spict/commonUI.R")
source("ui/spict/spictUI.R")
## Others
## source("ui/fishMethods/commonUI.R")
## source("ui/fishMethods/sbprUI.R")
## source("ui/fishMethods/yprUI.R")
## Support
source("ui/support/BasicSchaeferUI.R")
source("ui/support/BasicVonBertalannfyUI.R")
source("ui/support/NaturalMortalityUI.R")
source("ui/support/SeasonalVonBertalannfyUI.R")
## SERVER
source("server/assumptions.R")
source("server/common.R")
## CMSY
source("server/cmsy/cmsyServer.R")
## Length-based methods
source("server/LBM/tropfishr/tropfishrServer.R")
source("server/LBM/lbi/lbiServer.R")
source("server/LBM/lbspr/lbsprServer.R")
source("server/spict/spictServer.R")
## Others
## source("server/fishMethods/sbprServer.R")
## source("server/fishMethods/yprServer.R")
## Support
source("server/support/BasicSchaeferServer.R")
source("server/support/BasicVonBertalannfyServer.R")
source("server/support/SeasonalVonBertalannfyServer.R")
source("server/support/NaturalMortalityServer.R")
## Functions
source("assets/commons/commons.R")
source("~/Documents/consulting/FAO/2025/glossary/glossary.R") ## TODO: add to assets/commons/ or somewhere else
source("assets/commons/labels.R")
## CMSY
source("assets/cmsy/CmsyFunction.R")
## LBM
source("assets/LBM/commons.R")
source("assets/LBM/tropfishr/algorithms/run_tropfishr.R")
source("assets/LBM/tropfishr/algorithms/temp_elefan_ga.R")  ## temporarily needed until TropFishR updated
source("assets/LBM/tropfishr/algorithms/temp_m_empirical.R")  ## temporarily needed until TropFishR updated
source("assets/LBM/tropfishr/algorithms/temp_predict_mod.R")  ## temporarily needed until TropFishR updated
source("assets/LBM/tropfishr/algorithms/tables.R")
source("assets/LBM/tropfishr/algorithms/plotting.R")
source("assets/LBM/tropfishr/algorithms/captions.R")
source("assets/LBM/tropfishr/algorithms/output.R")
source("assets/LBM/lbi/algorithms/run_lbi.R")
source("assets/LBM/lbi/algorithms/plotting.R")
source("assets/LBM/lbi/algorithms/tables.R")
source("assets/LBM/lbi/algorithms/captions.R")
source("assets/LBM/lbi/algorithms/output.R")
source("assets/LBM/lbspr/algorithms/run_lbspr.R")
source("assets/LBM/lbspr/algorithms/plotting.R")
source("assets/LBM/lbspr/algorithms/tables.R")
source("assets/LBM/lbspr/algorithms/captions.R")
source("assets/LBM/lbspr/algorithms/output.R")
## spict
source("assets/spict/algorithms/readin.R")
source("assets/spict/algorithms/run_spict.R")
source("assets/spict/algorithms/plotting.R")
source("assets/spict/algorithms/tables.R")
source("assets/spict/algorithms/captions.R")
source("assets/spict/algorithms/output.R")
## Others
## source("assets/fishmethods/methods.R")
## Support
source("assets/support/shaefer.R")
source("assets/support/vonBertalannfly.R")
source("assets/support/seasonalVonBertalannfly.R")
source("assets/support/naturalMortality.R")

fileLog <- Sys.getenv("SMT_LOG")
if (is.null(fileLog) || is.na(fileLog) || fileLog == "") {
    fileLog <- "session.log"
}
print(paste0("Logging to: ", fileLog))

pdf(NULL)
dev.off()
##dev.list()
t
set.seed(1)
d <- data(package = "TropFishR")
parallel <- FALSE
fishingMortality <- "NA"

sidebar <- dashboardSidebar(uiOutput("sidebar"))

ui <- tagList(
    use_waiter(),
    waiter_show_on_load(app_load_spinner("Initializing R session. This process may take a while...")),
    dashboardPage(
        dashboardHeader(title = 'Stock Monitoring Tool'),
        sidebar,
        dashboardBody(
            tags$div(
                     tags$div(
                              tags$span("Please wait while your request is being processed. This may take some time..."),
                              class="loadingCustomInner"
                          ),
                     tags$div(
                              tags$img(src = 'loading-circle.gif', height="20px"),
                              class="loadingCustomInner"
                          ),
                     class="loadingCustom"),
            useShinyjs(),
            extendShinyjs(text = jscode, functions =  c("showBox", "removeBox","showBox2", "removeBox2", "disableAllButtons", "enableAllButtons", "showComputing", "hideComputing", "expandBox","collapseBox")),
            introjsUI(),
            add_busy_spinner(spin = "fading-circle", color = "#112446"),
            tags$head(
                     tags$link(rel = "stylesheet", type = "text/css", href = "custom.css")
                 ),
            tags$head(tags$script(src="https://cdnjs.cloudflare.com/ajax/libs/eqcss/1.7.0/EQCSS.min.js")),
            tags$head(tags$script(type="text/eqcss", src="styles.eqcss")),
            tags$head(tags$script(type="text/javascript", src="custom.js")),
                                        #busyIndicator(wait = 7000),
            tabItems(
                tabItem("homeTab",htmlOutput("homeInfo"), selected=T),
                tabLBMIntro,
                tabElefanIntro,
                ## tabElefanSampleDataset,
                tabLBIIntro,
                ## tabLBISampleDataset,
                tabLBSPRIntro,
                ## tabLBSPRSampleDataset,
                ## tabFishMethodsIntro,
                ## tabFishMethodsSampleDataset,
                tabElefanGa("elefanGaModule"),
                ## tabElefanSa("elefanSaModule"),
                ## tabElefan("elefanModule"),
                ## tabSbpr("sbprModule"),
                ## tabYpr("yprModule"),
                tabLBI("lbiModule"),
                tabLBSPR("lbsprModule"),
                tabSPICTIntro,
                tabSPICT("spictModule"),
                tabCmsyIntro,
                tabCmsySampleDataset,
                tabCmsy("cmsyModule"),
                tabBasicSchaefer("basicShaeferModule"),
                tabBasicVonBertalannfy("vonBertalannfyModule"),
                tabSeasonalVonBertalannfy("seasonalVonBertalannfyModule"),
                tabNaturalMortality("naturalMortalityModule"),
                tabItem("glossaryTab",htmlOutput("glossary"))
            )
        )
    ), tags$footer(footer(SMT_VERSION, SMT_DATE), align = "center")
)


    server <- function(input, output, session) {

                                        #flog.threshold(DEBUG)
                                        #flog.appender(appender.file(fileLog))

        session$allowReconnect("force")
        waiter_hide()
        onStop(function() {
            flog.warn("Lost connection to R server")
        })

        output$sidebar <- renderUI({
            dashboardSidebar(
                sidebarMenu(id="smt-tabs",
                            menuItem("Home", tabName="homeTab"),
                            menuLengthMethods,
                            ## menuFishMethods,
                            menuSPICT,
                            menuCmsy,
                            menuSupportingTools,
                            menuItem("Glossary", tabName="glossaryTab")
                            )
            )
        })

        session$userData$page <- reactiveVal(NULL)

### Render the page set as last visited in session or by page= query param
        observe({
            currentPage <- NA
            if (!is.null(session$userData$page())) {
                currentPage <- session$userData$page()
            } else {
                query <- parseQueryString(session$clientData$url_search)
                if (!is.null(query$page)) {
                    currentPage <- query$page
                }
            }
            flog.info("Current Page: %s", currentPage)
            if (!is.na(currentPage)) {
                switch(currentPage,
                       'cmsy-intro'= {isolate({updateTabItems(session, "smt-tabs", "cmsyIntro")})},
                       'cmsy'= {isolate({updateTabItems(session, "smt-tabs", "cmsyWidget")})},
                       'cmsy-sample'= {isolate({updateTabItems(session, "smt-tabs", "cmsySampleDataset")})},
                       'lbm-intro' = {isolate({updateTabItems(session, "smt-tabs", "lbmIntro")})},
                       'elefan-intro' = {isolate({updateTabItems(session, "smt-tabs", "ElefanIntro")})},
                       'elefan-ga' = {isolate({updateTabItems(session, "smt-tabs", "ElefanGaWidget")})},
                       ## 'elefan-sa' = {isolate({updateTabItems(session, "smt-tabs", "ElefanSaWidget")})},
                       ## 'elefan' = {isolate({updateTabItems(session, "smt-tabs", "ElefanWidget")})},
                       ## 'elefan-sample' = {isolate({updateTabItems(session, "smt-tabs", "ElefanSampleDataset")})},
                       'lbi-intro' = {isolate({updateTabItems(session, "smt-tabs", "lbiIntro")})},
                       'lbi' = {isolate({updateTabItems(session, "smt-tabs", "lbiWidget")})},
                       ## 'lbi-sample' = {isolate({updateTabItems(session, "smt-tabs", "lbiSampleDataset")})},
                       'lbspr-intro' = {isolate({updateTabItems(session, "smt-tabs", "lbsprIntro")})},
                       'lbspr' = {isolate({updateTabItems(session, "smt-tabs", "lbsprWidget")})},
                       ## 'lbspr-sample' = {isolate({updateTabItems(session, "smt-tabs", "lbsprSampleDataset")})},
                       ## 'fishmethods-intro' = {isolate({updateTabItems(session, "smt-tabs", "FishMethodsIntro")})},
                       ## 'sbpr' = {isolate({updateTabItems(session, "smt-tabs", "SBPRWidget")})},
                       ## 'ypr' = {isolate({updateTabItems(session, "smt-tabs", "YPRWidget")})},
                       ## 'fishmethods-sample' = {isolate({updateTabItems(session, "smt-tabs", "FishMethodsSampleDataset")})},
                       'spict-intro' = {isolate({updateTabItems(session, "smt-tabs", "spictIntro")})},
                       'spict' = {isolate({updateTabItems(session, "smt-tabs", "spictWidget")})},
                       'basic-shaefer' = {isolate({updateTabItems(session, "smt-tabs", "BasicSchaefer")})},
                       'basic-von-bertalannfy' = {isolate({updateTabItems(session, "smt-tabs", "BasicVonBertalannfy")})},
                       'seasonal-von-bertalannfy' = {isolate({updateTabItems(session, "smt-tabs", "SeasonalVonBertalannfy")})},
                       'natural-mortality' = {isolate({updateTabItems(session, "smt-tabs", "NaturalMortality")})},
                       'home' = {isolate({updateTabItems(session, "smt-tabs", "homeTab")})},
                       'glossary' = {isolate({updateTabItems(session, "smt-tabs", "glossaryTab")})},
                       {isolate({updateTabItems(session, "smt-tabs", "homeTab")})}
                       )
            } else {
                isolate({updateTabItems(session, "smt-tabs", "homeTab")})
            }
        })


        ## session$userData$withtoken <- withtoken
        app_ctrl <- reactiveValues(
            withtoken = FALSE ## withtoken
        )
        session$userData$sessionToken <- reactiveVal(NULL)
        session$userData$sessionUsername <- reactiveVal(NULL)
        session$userData$sessionMode <- reactiveVal(NULL)
        session$userData$storagehubManager <- reactiveVal(NULL)
        session$userData$sessionWps <- reactiveVal(NULL)

        ## Hide any overlay when session starts
        observe({
            js$hideComputing()
        })

                                        #observer on token
        observe({
            if(!app_ctrl$withtoken){
                query <- parseQueryString(session$clientData$url_search)
                if(!is.null(query[[gcubeTokenQueryParam]])){
                    token <- query[[gcubeTokenQueryParam]]
                    session$userData$sessionToken(token)
                    app_ctrl$withtoken <- TRUE

                                        #instantiate storagehub manager (uses a keyring 'env' backend by default)
                    sh_manager = d4storagehub4R::StoragehubManager$new(token = session$userData$sessionToken(), logger = "INFO")
                    session$userData$sessionUsername(sh_manager$getUserProfile()$username)
                    session$userData$storagehubManager(sh_manager)

                                        #trace logs by user
                    fileLog <- sprintf("session_for_%s.log", sh_manager$getUserProfile()$username)

                    if (!is.null(session$userData$sessionToken())) {
                        flog.info("Session token is: %s", session$userData$sessionToken())
                    } else {
                        flog.info("Session token is: %s", "NULL")
                    }

                    if (!is.null(session$userData$sessionMode())) {
                        flog.info("Session mode is: %s", session$userData$sessionMode())
                    } else {
                        flog.info("Session mode is: %s", "NULL")
                    }

                    if (!is.null(session$userData$sessionUsername())) {
                        flog.info("Session username is: %s", session$userData$sessionUsername())
                        session$userData$sessionMode("GCUBE")
                    } else {
                        flog.info("Session username is: %s", "NULL")
                    }

                }
                flog.threshold(DEBUG)
                flog.appender(appender.file(fileLog))

            }
        ## else{

        ##     ## set manually
        ##     session$userData$sessionToken(token)

        ##     #instantiate storagehub manager (uses a keyring 'env' backend by default)
        ##     sh_manager = d4storagehub4R::StoragehubManager$new(token = session$userData$sessionToken(), logger = "INFO")
        ##     session$userData$sessionUsername(sh_manager$getUserProfile()$username)
        ##     session$userData$storagehubManager(sh_manager)

        ##     #trace logs by user
        ##     fileLog <- sprintf("session_for_%s.log", sh_manager$getUserProfile()$username)

        ##     if (!is.null(session$userData$sessionToken())) {
        ##         flog.info("Session token is: %s", session$userData$sessionToken())
        ##     } else {
        ##         flog.info("Session token is: %s", "NULL")
        ##     }

        ##     if (!is.null(session$userData$sessionMode())) {
        ##         flog.info("Session mode is: %s", session$userData$sessionMode())
        ##     } else {
        ##         flog.info("Session mode is: %s", "NULL")
        ##     }

        ##     if (!is.null(session$userData$sessionUsername())) {
        ##         flog.info("Session username is: %s", session$userData$sessionUsername())
        ##         session$userData$sessionMode("GCUBE")
        ##     } else {
        ##         flog.info("Session username is: %s", "NULL")
        ##     }

        ##   flog.threshold(DEBUG)
        ##   flog.appender(appender.file(fileLog))

        ## }
    })

    observeEvent(req(!is.null(session$userData$sessionToken())),{
        icproxy = XML::xmlParse(content(GET("https://registry.d4science.org/icproxy/gcube/service//ServiceEndpoint/DataAnalysis/DataMiner?gcube-scope=/d4science.research-infrastructures.eu/D4Research/SDG-Indicator14.4.1"), "text"))
        wps_uri = xpathSApply(icproxy, "//AccessPoint/Interface/Endpoint", xmlValue)[1]

        flog.info("WPS url select : %s",wps_uri)
        print(sprintf("WPS url select : %s",wps_uri))
        WPS <- WPSClient$new(
                             url = wps_uri,
                             serviceVersion = "1.0.0", logger ="DEBUG",
                             headers = c("gcube-token"= session$userData$sessionToken())
                         )

        session$userData$sessionWps(WPS)

    },once=T)

    session$userData$cmsy <- reactiveValues()

    ## session$userData$elefan_sa <- reactiveValues()
    ## session$userData$elefan <- reactiveValues()
    session$userData$sbprExec <- reactiveValues()
    session$userData$yprExec <- reactiveValues()
    session$userData$fishingMortality <- reactiveValues()

    session$userData$fishingMortality$FcurrGA <- NA
    session$userData$fishingMortality$FcurrSA <- NA
    session$userData$fishingMortality$Fcurr <- NA

    session$userData$cmsyUploadVreResult <- reactiveValues()
    session$userData$elefanGaUploadVreResult <- reactiveValues()
    ## session$userData$elefanSaUploadVreResult <- reactiveValues()
    ## session$userData$elefanUploadVreResult <- reactiveValues()
    session$userData$sbprUploadVreResult <- reactiveValues()
    session$userData$yprUploadVreResult <- reactiveValues()

    session$userData$lbiUploadVreResult <- reactiveValues()
    session$userData$lbsprUploadVreResult <- reactiveValues()

    callModule(cmsyModule, "cmsyModule")
    callModule(elefanGaModule, "elefanGaModule")
    ## callModule(elefanSaModule, "elefanSaModule")
    ## callModule(elefanModule, "elefanModule")
    ## callModule(sbprModule, "sbprModule")
    ## callModule(yprModule, "yprModule")
    callModule(lbiModule, "lbiModule")
    callModule(lbsprModule, "lbsprModule")
    callModule(spictModule, "spictModule")
    callModule(basicShaeferModule, "basicShaeferModule")
    callModule(vonBertalannfyModule, "vonBertalannfyModule")
    callModule(seasonalVonBertalannfyModule, "seasonalVonBertalannfyModule")
    callModule(naturalMortalityModule, "naturalMortalityModule")

    source("server/labels.R", local=TRUE)


}


## Run the application
shinyApp(ui = ui, server = server)
