tabSPICT <- function(id) {
    ns <- NS(id)
    tabItem("spictWidget",

            htmlOutput(ns("spictTitle")),

            htmlOutput("spictVersion", class="subTitle"),

            fluidRow(
                div(style = "display: inline-block; vertical-align:center; margin-left: 15px;",
                    "More information about "
                    ),
                div(style = "display: inline-block; vertical-align:center; margin-left: 5px;",
                    id = ns("info_wrapper"),
                    div(style = "display: inline-block; vertical-align:center; margin-left: 15px;",
                        actionButton(ns("workflowConsiderations"), "Workflow",
                                     class="topLevelInformationButton")
                        ),
                    div(style = "display: inline-block; vertical-align:center; margin-left: 5px;",
                        actionButton(ns("dataConsiderations"), "Data",
                                     class="topLevelInformationButton")
                        ),
                    div(style = "display: inline-block; vertical-align:center; margin-left: 5px;",
                        actionButton(ns("methodConsiderations"), "Methods",
                                     class="topLevelInformationButton")
                        ),
                    div(style = "display: inline-block; vertical-align:center; margin-left: 5px;",
                        actionButton(ns("resultConsiderations"), "Results",
                                     class="topLevelInformationButton")
                        )
                    ),
                div(style = "display: inline-block; vertical-align:center; margin-left: 10px;",
                    actionButton(ns("tour_general"), "Start SPiCT tour",
                                 class="btn btn-primary btn-sm")
                    )
            ),


            fluidRow(

                tags$head(tags$style(HTML(".shiny-notification { position:fixed; top: calc(50%); left: calc(50%); width: 250px; height: 80px;}"))),


                ## Input - Data upload
                ## -------------------------------

                box(id = "box_datupload",
                    title = tagList("Data Upload",
                                    actionButton(ns("dataConsiderations2"),
                                           tags$i(class = "fas fa-info",
                                                  style="font-size: 8px"),
                                           class="infoBubbleButton")),
                    width = NULL,
                    collapsible = FALSE,
                    solidHeader = TRUE,
                    class = "collapsed-box",

                    fluidRow(
                        column(5,
                               div(id = ns("file_wrapper"),
                                   fileInput(ns("file"), "Choose Input CSV File",
                                             accept = c(
                                                 "text/csv",
                                                 "text/comma-separated-values,text/plain",
                                                 ".csv")
                                             )
                                   )
                               ),
                        ## column(3,
                        ##        selectInput(ns("dateFormat"),
                        ##                    "Choose CSV date format",
                        ##                    choices = c("Automatic guess" = "auto",
                        ##                                "Year Month Day" = "ymd",
                        ##                                "Year Day Month" = "ydm",
                        ##                                "Day Month Year" = "dmy",
                        ##                                "Month Day Year" = "mdy" ))
                        ##        ),
                        column(2,
                               selectizeInput(
                                   ns("spictCSVsep"),
                                   tagList("CSV field separator",
                                           actionButton(ns("info_csv_sep"),
                                                        tags$i(class = "fas fa-info",
                                                               style="font-size: 8px"),
                                                        class="infoBubbleButton")),
                                   choices = c("Automatic guess" = "auto",
                                               "Comma (,)" = ",",
                                               "Semicolon (;)" = ";",
                                               "Space ( )" = ";",
                                               "Tab (\\t)" = "\t"),
                                   selected = "auto",
                                   options = list(create = TRUE)
                               )
                               ),
                        column(2,
                               selectizeInput(
                                   ns("spictCSVdec"),
                                   tagList("CSV decimal separator",
                                           actionButton(ns("info_csv_dec"),
                                                        tags$i(class = "fas fa-info",
                                                               style="font-size: 8px"),
                                                        class="infoBubbleButton")),
                                   choices = c("Automatic guess" = "auto",
                                               "Point (.)" = ".",
                                               "Comma (,)" = ","),
                                   selected = "auto",
                                   options = list(create = TRUE)
                               )
                               ),
                        column(2,
                               br(),
                               checkboxInput(
                                   ns("guess_cols"),
                                   tagList("Assign columns automatically?",
                                           actionButton(ns("info_col_auto"),
                                                  tags$i(class = "fas fa-info",
                                                           style="font-size: 8px"),
                                                  class="infoBubbleButton")),
                                   FALSE)
                               ),
                        column(1,
                               numericInput(ns("n_indices"),
                                            tagList("# Indices",
                                                    actionButton(ns("info_n_indices"),
                                                           tags$i(class = "fas fa-info",
                                                                  style="font-size: 8px"),
                                                           class="infoBubbleButton")),
                                            value = 1,
                                            min = 1, max = 20, step = 1)),
                        ),

                    fluidRow(
                        column(2,
                               uiOutput(ns("timeC_lab"))
                               ),
                        column(2,
                               uiOutput(ns("obsC_lab"))
                               ),
                        column(2,
                               uiOutput(ns("stdevC_lab"))
                               ),
                        column(2,
                               uiOutput(ns("timeI_selectors"))
                               ),
                        column(2,
                               uiOutput(ns("obsI_selectors"))
                               ),
                        column(2,
                               uiOutput(ns("stdevI_selectors"))
                               )
                    ),

                    ),

                ## Input - Settings
                ## -------------------------------
                br(),

                box(id = ns("box_settings"),
                    title = div("Data exploration & Settings",
                                id = ns("data_explo"),
                                actionButton(ns("methodConsiderations2"),
                                             tags$i(class = "fas fa-info",
                                                    style="font-size: 8px"),
                                             class="infoBubbleButton")),
                    width = NULL,
                    collapsible = FALSE, ## careful: if made collapsible the renderUi does not update! see: https://github.com/rstudio/shinydashboard/issues/234
                    solidHeader = TRUE,
                    class = "collapsed-box",

                    div(id = "settings_spict",
                        tabBox(
                            title = "",
                            width = NULL,
                            height = "600px",
                            side = "left",
                            selected = "data",
                            id = ns("settings"),

                            tabPanel(
                                title = div(id = ns("tab1"),
                                            "1. Data"),
                                value = "data",

                                box(title = "Data settings",
                                    width = 3,

                                    fluidRow(
                                        div(style = "display: inline-block; vertical-align:center; margin-left: 15px;",
                                            HTML("<b>Select years for analysis</b>")
                                            ),
                                        div(style = "display: inline-block; vertical-align:center; margin-left: 3px;",
                                            actionButton(ns("info_timerange"),
                                                         tags$i(class = "fas fa-info",
                                                                style="font-size: 8px"),
                                                         class="infoBubbleButton")
                                            )

                                    ),
                                    sliderInput(ns("timerange"),
                                                label = "",
                                                min = 1900,
                                                max = 2025,
                                                value = c(1900, 2025),
                                                step = 1,
                                                dragRange = TRUE,
                                                sep = ""),
                                    br(),
                                    fluidRow(
                                        column(6,
                                               uiOutput(ns("timeIshift_ui"))
                                               ),
                                        column(6,
                                               selectInput(inputId = ns("dteuler"),
                                                           tagList("dteuler",
                                                                   actionButton(ns("info_dteuler"),
                                                                                tags$i(class = "fas fa-info",
                                                                                       style="font-size: 8px"),
                                                                                class="infoBubbleButton")),
                                                           choices = c("1/2"=1/2,
                                                                       "1/4"=1/4,
                                                                       "1/8"=1/8,
                                                                       "1/16"=1/16,
                                                                       "1/32"=1/32,
                                                                       "1/64"=1/64),
                                                           selected = 1/8,
                                                           width = '100%'
                                                           )
                                               )
                                    ),
                                    br(),

                                    fluidRow(
                                        column(6,
                                               textInput(ns("catchUnit"),
                                                         tagList("Catch unit",
                                                                 actionButton(ns("info_catchunit"),
                                                                      tags$i(class = "fas fa-info",
                                                                             style="font-size: 8px"),
                                                                      class="infoBubbleButton")),
                                               "t", width = "100%")
                                               ),
                                        column(6)
                                    ),

                                    br(),


                                    fluidRow(
                                        column(6,
                                               checkboxInput(
                                                   ns("robflagc"),
                                                   tagList("Robust flag (catch)",
                                                           actionButton(ns("info_robflagc"),
                                                                tags$i(class = "fas fa-info",
                                                                       style="font-size: 8px"),
                                                                class="infoBubbleButton")),
                                               FALSE)
                                               ),
                                        column(6,
                                               uiOutput(ns("robflagi_ui")),
                                               )
                                    ),
                                    br(),
                                    br(),
                                    fluidRow(
                                        column(6,
                                               selectInput(ns("fig_format"),
                                                           tagList("Figure format",
                                                                   actionButton(
                                                                       ns("info_fig_format"),
                                                                       tags$i(class = "fas fa-info",
                                                                              style="font-size: 8px"),
                                                                       class="infoBubbleButton")),
                                                           choices = c("pdf","png","jpeg","tiff","bmp","ps"),
                                                           selected = "pdf",
                                                           multiple = FALSE, width = "100%")
                                               ),
                                        column(6,
                                               selectInput(ns("tab_format"),
                                                           tagList("Table format",
                                                                   actionButton(ns("info_tab_format"),
                                                                                tags$i(class = "fas fa-info",
                                                                                       style="font-size: 8px"),
                                                                                class="infoBubbleButton")),
                                                           choices = c("csv","xls","xlsx"),
                                                           selected = "csv",
                                                           multiple = FALSE,
                                                           width = "100%")
                                               )
                                    )
                                    ),
                                    box(title = "Data exploration",
                                        id = "box_exploPlots",
                                        width = 9,
                                        tags$div(
                                                 style = "margin-left: 10%; margin-right: 10%; text-align: center;",
                                                 uiOutput(ns("plot_explo1_ui")),
                                                 div(
                                                     style = "margin-top: 15px;",
                                                     htmlOutput(ns("title_explo1"))
                                                 )
                                             ),
                                        br(),
                                        tags$div(
                                                 style = "margin-left: 10%; margin-right: 10%; text-align: center;",
                                                 uiOutput(ns("plot_explo2_ui")),
                                                 div(
                                                     style = "margin-top: 15px;",
                                                     htmlOutput(ns("title_explo2"))
                                                 )
                                             )
                                        )
                            ),

                            tabPanel(
                                title = div(id = ns("tab2"),
                                            "2. Priors"),
                                value = "priors",

                                box(title = tagList("Configure priors",
                                                    actionButton(ns("info_config_priors"),
                                                                 tags$i(class = "fas fa-info",
                                                                        style="font-size: 8px"),
                                                                 class="infoBubbleButton")),
                                    width = 4,
                                    wellPanel(
                                        # Header row
                                        fluidRow(
                                            column(2, tags$b("Prior")),
                                            column(2, tags$b("Activate")),
                                            column(2, tags$b("Mean")),
                                            column(2, tags$b("SD")),
                                            column(4, tags$b("Mean on log scale"))
                                        ),
                                            br(),
                                        # log(n)
                                            fluidRow(
                                                column(2, "log(n)"),
                                                column(2, checkboxInput(ns("lognPrior"), NULL, TRUE)),
                                                column(2, numericInput(ns("lognMu"), NULL, 2)),
                                                column(2, numericInput(ns("lognSd"), NULL, 2)),
                                                column(4, checkboxInput(ns("lognLog"), NULL, TRUE))
                                            ),
                                        # log(alpha)
                                            fluidRow(
                                                column(2, "log(alpha)"),
                                                column(2, checkboxInput(ns("logAlphaPrior"), NULL, TRUE)),
                                                column(2, numericInput(ns("logAlphaMu"), NULL, 1)),
                                                column(2, numericInput(ns("logAlphaSd"), NULL, 2)),
                                                column(4, checkboxInput(ns("logAlphaLog"), NULL, TRUE))
                                            ),
                                        # log(beta)
                                            fluidRow(
                                                column(2, "log(beta)"),
                                                column(2, checkboxInput(ns("logBetaPrior"), NULL, TRUE)),
                                                column(2, numericInput(ns("logBetaMu"), NULL, 1)),
                                                column(2, numericInput(ns("logBetaSd"), NULL, 2)),
                                                column(4, checkboxInput(ns("logBetaLog"), NULL, TRUE))
                                            ),
                                            fluidRow(
                                                column(2,"log(B0/K)"),
                                                column(
                                                    2,
                                                    checkboxInput(
                                                        ns("logbkfracPrior"),
                                                        NULL,
                                                        FALSE),
                                                    ),
                                                column(
                                                    2,
                                                    numericInput(
                                                        ns("logbkfracMu"),
                                                        NULL,
                                                        NA)
                                                ),
                                                column(
                                                    2,
                                                    numericInput(
                                                        ns("logbkfracSd"),
                                                        NULL,
                                                        NA)
                                                ),
                                                column(
                                                    4,
                                                    checkboxInput(
                                                        ns("logbkfracLog"),
                                                        NULL,
                                                        FALSE)
                                                )
                                            ),
                                            fluidRow(
                                                column(2,"log(K)"),
                                                column(
                                                    2,
                                                    checkboxInput(
                                                        ns("logKPrior"),
                                                               NULL,
                                                               FALSE),
                                                           ),
                                                       column(
                                                           2,
                                                           numericInput(
                                                               ns("logKMu"),
                                                               NULL,
                                                               NA)
                                                       ),
                                                       column(
                                                           2,
                                                           numericInput(
                                                               ns("logKSd"),
                                                               NULL,
                                                               NA)
                                                       ),
                                                       column(
                                                           4,
                                                           checkboxInput(
                                                               ns("logKLog"),
                                                               NULL,
                                                               FALSE)
                                                       )
                                                   ),
                                                   fluidRow(
                                                       column(2,"log(r)"),
                                                       column(
                                                           2,
                                                           checkboxInput(
                                                               ns("logrPrior"),
                                                               NULL,
                                                               FALSE),
                                                           ),
                                                       column(
                                                           2,
                                                           numericInput(
                                                               ns("logrMu"),
                                                               NULL,
                                                               NA)
                                                       ),
                                                       column(
                                                           2,
                                                           numericInput(
                                                               ns("logrSd"),
                                                               NULL,
                                                               NA)
                                                       ),
                                                       column(
                                                           4,
                                                           checkboxInput(
                                                               ns("logrLog"),
                                                               NULL,
                                                               FALSE)
                                                       )
                                                   ),
                                                   fluidRow(
                                                       column(2, "log(m)"),
                                                       column(
                                                           2,
                                                           checkboxInput(
                                                               ns("logmPrior"),
                                                               NULL,
                                                               FALSE),
                                                           ),
                                                       column(
                                                           2,
                                                           numericInput(
                                                               ns("logmMu"),
                                                               NULL,
                                                               NA)
                                                       ),
                                                       column(
                                                           2,
                                                           numericInput(
                                                               ns("logmSd"),
                                                               NULL,
                                                               NA)
                                                       ),
                                                       column(
                                                           4,
                                                           checkboxInput(
                                                               ns("logmLog"),
                                                               NULL,
                                                               FALSE)
                                                       )
                                                   ),

                                                   uiOutput(ns("logq_priors_ui")),

                                                   fluidRow(
                                                       column(2, "log(sdb)"),
                                                       column(
                                                           2,
                                                           checkboxInput(
                                                               ns("logsdbPrior"),
                                                               NULL,
                                                               FALSE),
                                                    ),
                                                column(
                                                    2,
                                                    numericInput(
                                                        ns("logsdbMu"),
                                                        NULL,
                                                        NA)
                                                ),
                                                column(
                                                    2,
                                                    numericInput(
                                                        ns("logsdbSd"),
                                                        NULL,
                                                        NA)
                                                ),
                                                column(
                                                    4,
                                                    checkboxInput(
                                                        ns("logsdbLog"),
                                                        NULL,
                                                        FALSE)
                                                )
                                            ),

                                            uiOutput(ns("logsdi_priors_ui")),

                                            fluidRow(
                                                column(2, "log(sdf)"),
                                                column(
                                                    2,
                                                    checkboxInput(
                                                        ns("logsdfPrior"),
                                                        NULL,
                                                        FALSE),
                                                    ),
                                                column(
                                                    2,
                                                    numericInput(
                                                        ns("logsdfMu"),
                                                        NULL,
                                                        NA)
                                                ),
                                                column(
                                                    2,
                                                    numericInput(
                                                        ns("logsdfSd"),
                                                        NULL,
                                                        NA)
                                                ),
                                                column(
                                                    4,
                                                    checkboxInput(
                                                        ns("logsdfLog"),
                                                        NULL,
                                                        FALSE)
                                                )
                                            ),
                                            fluidRow(
                                                column(2, "log(sdc)"),
                                                column(
                                                    2,
                                                    checkboxInput(
                                                        ns("logsdcPrior"),
                                                        NULL,
                                                        FALSE),
                                                    ),
                                                column(
                                                    2,
                                                    numericInput(
                                                        ns("logsdcMu"),
                                                        NULL,
                                                        NA)
                                                ),
                                                column(
                                                    2,
                                                    numericInput(
                                                        ns("logsdcSd"),
                                                        NULL,
                                                        NA)
                                                ),
                                                column(
                                                    4,
                                                    checkboxInput(
                                                        ns("logsdcLog"),
                                                        NULL,
                                                        FALSE)
                                                )
                                            )
                                    )
                                    ),
                                box(title = "Prior distributions",
                                    id = "box_priorsPlots",
                                    width = 8,
                                    tags$div(
                                             style = "margin-left: 10%; margin-right: 10%; text-align: center;",
                                             uiOutput(ns("plot_priors_ui")),
                                             div(
                                                 style = "margin-top: 15px;",
                                                 htmlOutput(ns("title_priors"))
                                             )
                                         )
                                    )
                            ),


                            tabPanel(
                                title = div(id = ns("tab3"),"3. Summary & Diagnostics"),
                                value = "diag",

                                fluidRow(
                                    column(3,

                                           uiOutput(ns("text_diag1")),

                                           br()
                                           ),

                                    column(9,

                                           tags$div(
                                                    style = "margin-left: 10%; margin-right: 10%; text-align: center;",
                                                    plotOutput(ns("plot_diag1"),
                                                               width = "90%",
                                                               height = "800px"),
                                                    div(
                                                        style = "margin-top: 15px;",
                                                        htmlOutput(ns("title_diag1"))
                                                    ),
                                                    plotOutput(ns("plot_diag2"),
                                                               width = "90%",
                                                               height = "350px"),
                                                    div(
                                                        style = "margin-top: 15px;",
                                                        htmlOutput(ns("title_diag2"))
                                                    )
                                                )
                                           )
                                )
                            )
                        ))
                    ),



                ## Action buttons
                ## -------------------------------
                br(),

                box(title = tagList("Run Assessment & Download Report and Results",
                                    actionButton(ns("infoAssessment"),
                                                 tags$i(class = "fas fa-info",
                                                        style="font-size: 8px"),
                                                 class="infoBubbleButton")),
                    width = NULL,
                    collapsible = FALSE,
                    solidHeader = TRUE,
                    class = "collapsed-box",
                    collapsed = FALSe,

                    br(),

                    fluidRow(
                        div(style = "display: inline-block; vertical-align:center; margin-left: 20px;",
                            disabled(actionButton(ns("check_spict"),
                                                  "Run Check",
                                                  class="topLevelInformationButton"))
                            ),
                        div(style = "display: inline-block; vertical-align:center; margin-left: 20px;",
                            disabled(actionButton(ns("go_spict"),
                                                  "Run Assessment",
                                                  class="topLevelInformationButton"))
                            ),
                        div(style = "display: inline-block; vertical-align:center; margin-left: 20px;",
                            actionButton(ns("reset_spict"),
                                         "Reset",
                                         class="topLevelInformationButton")
                            ),
                        div(style = "display: inline-block; vertical-align:center; margin-left: 20px;",
                            disabled(downloadButton(ns("createSpictReport"),
                                                    "Download Report",
                                                    class="topLevelInformationButton"))
                            ),
                        div(style = "display: inline-block; vertical-align:center; margin-left: 20px;",
                            disabled(downloadButton(ns("createSpictzip"),
                                                    "Download Results (zip)",
                                                    class="topLevelInformationButton"))
                            ),

                        div(style = "display: inline-block; vertical-align:center; margin-left: 20px;",
                            actionButton(ns("tour_res"),
                                         "Start results tour",
                                         class="btn btn-primary btn-sm")
                            )
                    ),
                    br(),br()
                    ),

                br(),


                ## Results
                ## -------------------------------
                br(),
                box(id = "box_results",
                    title = tagList("Assessment Results",
                                    actionButton(ns("resultConsiderations2"),
                                                 tags$i(class = "fas fa-info",
                                                        style="font-size: 8px"),
                                                 class="infoBubbleButton")),
                    width = NULL,
                    height = "3300px",
                    collapsible = FALSE,
                    solidHeader = TRUE,
                    class = "collapsed-box",
                    collapsed = FALSE,

                    div(id = "results_spict",
                        tabBox(
                            title = "",
                            width = NULL,
                            height = "600px",
                            side = "left",
                            selected = "res1",
                            id = ns("results"),

                            tabPanel(
                                title = div(id = ns("tab_res1"),
                                            "1. Main results"),
                                value = "res1",

                                fluidRow(
                                    column(
                                        7,
                                        style = "padding-right: 15px;padding-left: 5px;",
                                        tags$div(
                                                 style = "width: 100%; margin: 0 auto; text-align: center;",
                                                 plotOutput(ns("plot_sum"), width = "100%", height = "700px"),
                                                 div(style = "margin-top: 10px;", htmlOutput(ns("title_sum")))
                                             )
                                    ),
                                    column(
                                        5,
                                        tags$div(
                                                 div(style = "margin-bottom:0px; margin-left: 0%; margin-right: 0%;",
                                                     htmlOutput(ns("title_table_est"))
                                                     ),
                                                 dataTableOutput(ns("table_est")),
                                                 style = "margin-left: 5%; margin-right: 30%;"
                                             ),
                                        br(),
                                        tags$div(
                                                 div(style = "margin-bottom:0px; margin-left: 0%; margin-right: 0%;",
                                                     htmlOutput(ns("title_table_refs_s"))
                                                     ),
                                                 dataTableOutput(ns("table_refs_s")),
                                                 style = "margin-left: 5%; margin-right: 30%;"
                                             ),
                                        br(),
                                        tags$div(
                                                 div(style = "margin-bottom:0px; margin-left: 0%; margin-right: 0%;",
                                                     htmlOutput(ns("title_table_states"))
                                                     ),
                                                 dataTableOutput(ns("table_states")),
                                                 style = "margin-left: 5%; margin-right: 30%;"
                                             )
                                    )
                                )

                            ),
                            tabPanel(
                                title = div(id = ns("tab_res2"),
                                            "2. Additional results"),
                                value = "res2",

                                fluidRow(
                                    column(
                                        4,
                                        plotOutput(ns("plot_prod"),
                                                   width = "100%",
                                                   height = "500px"),
                                        div(
                                            style = "margin-top: 10px;",
                                            htmlOutput(ns("title_prod"))
                                        )
                                    ),
                                    column(
                                        8,
                                        tags$div(
                                                 style = "margin-left: 10%; margin-right: 10%; text-align: center;",
                                                 uiOutput(ns("plot_priors2_ui")),
                                                 div(
                                                     style = "margin-top: 15px;",
                                                     htmlOutput(ns("title_priors2"))
                                                 )
                                             )
                                    )
                                ),

                                br(),

                                plotOutput(ns("plot_abs"), width = "100%", height = "400px"),
                                div(style = "margin-top: 10px;", htmlOutput(ns("title_abs"))),
                                br(),

                                tags$div(
                                         div(style = "margin-bottom:0px; margin-left: 0%; margin-right: 0%;",
                                             htmlOutput(ns("title_table_refs_d"))
                                             ),
                                         dataTableOutput(ns("table_refs_d")),
                                         style = "margin-left: 5%; margin-right: 30%;"
                                     ),
                                br(),
                                tags$div(
                                         div(style = "margin-bottom:0px; margin-left: 0%; margin-right: 0%;",
                                             htmlOutput(ns("title_table_pred"))
                                             ),
                                         dataTableOutput(ns("table_pred")),
                                         style = "margin-left: 5%; margin-right: 30%;"
                                     )
                            ),
                            tabPanel(
                                title = div(id = ns("tab_res3"),
                                            "3. Diagnostics"),
                                value = "res3",

                                fluidRow(
                                    column(
                                        6,
                                        tags$div(
                                                 style = "width: 90%; margin: 0 auto; text-align: center;",
                                                 plotOutput(ns("plot_resid1"),
                                                            height = "800px"),
                                                 div(
                                                     style = "margin-top: 10px;",
                                                     htmlOutput(ns("title_resid1"))
                                                 )
                                             )
                                    ),
                                    column(
                                        6,
                                        tags$div(
                                                 style = "width: 90%; margin: 0 auto; text-align: center;",
                                                 plotOutput(ns("plot_resid2"),
                                                            height = "800px"),
                                                 div(
                                                     style = "margin-top: 10px;",
                                                     htmlOutput(ns("title_resid2"))
                                                 )
                                             )
                                    )
                                )
                            )
                        )
                        )
                    )
            )
            )
}
