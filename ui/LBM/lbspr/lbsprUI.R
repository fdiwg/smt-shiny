tabLBSPR <- function(id) {
    ns <- NS(id)
    tabItem("lbsprWidget",

            htmlOutput(ns("lbsprTitle")),

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
                    actionButton(ns("tour_general"), "Start LBSPR tour",
                                 class="btn btn-primary btn-sm")
                    )
            ),



            fluidRow(

                ## Notifications in middle of screen (if decided to keep -> move to custom.css?)
                tags$head(tags$style(HTML(".shiny-notification { position:fixed; top: calc(50%); left: calc(50%); width: 250px; height: 80px;}"))),


                ## Input - Data upload
                ## -------------------------------
                box(id = "box_datupload",
                    title = p("Data Upload",
                              actionButton(ns("dataConsiderations2"),
                                           tags$i(class = "fas fa-info",
                                                  style="font-size: 8px"),
                                           class="infoBubbleButton")),
                    width = NULL,
                    collapsible = FALSE,
                    solidHeader = TRUE,
                    class = "collapsed-box",

                    box(width = 6,
                        div(id = ns("file_wrapper"),
                            fileInput(ns("fileLBSPR"), "Choose Input CSV File",
                                      accept = c(
                                          "text/csv",
                                          "text/comma-separated-values,text/plain",
                                          ".csv")
                                      )
                            )
                        ),
                    box(width = 2,
                        selectizeInput(
                            ns("lbsprCSVsep"),
                            tagList("CSV field separator",
                                    actionButton(ns("info_csv_sep"),
                                                 tags$i(class = "fas fa-info",
                                                        style="font-size: 8px"),
                                                 class="infoBubbleButton")),
                            choices = c("Automatic guess" = "auto",
                                        "Comma (,)" = ",",
                                        "Semicolon (;)" = ";",
                                        "Space ( )" = " ",
                                        "Tab (\\t)" = "\t"),
                            selected = "auto",
                            options = list(create = TRUE)
                        )
                        ),
                    box(width = 2,
                        selectizeInput(
                            ns("lbsprCSVdec"),
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
                    box(width = 2,
                        selectizeInput(ns("lbsprDateFormat"),
                                       tagList("CSV date format",
                                               actionButton(ns("info_csv_date"),
                                                            tags$i(class = "fas fa-info",
                                                                   style="font-size: 8px"),
                                                            class="infoBubbleButton")),
                                       choices = c("Automatic guess" = "auto",
                                                   "Year Month Day" = "ymd",
                                                   "Year Day Month" = "ydm",
                                                   "Day Month Year" = "dmy",
                                                   "Month Day Year" = "mdy" ),
                                       selected = "auto" ## , ## TODO
                                       ## options = list(create = TRUE)
                                       )
                        )
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

                    div(id = "settings_lbspr",
                        tabBox(
                            title = "",
                            width = NULL,
                            height = "810px",
                            side="left",
                            selected = "data",
                            id = "settings",

                            tabPanel(
                                title = div(id = ns("tab1"), "1. Data and settings"),
                                value = "data",

                                box(title = "",
                                    width = 4,

                                    fluidRow(
                                        div(style = "display: inline-block; vertical-align:center; margin-left: 15px;",
                                            HTML("<b>Select years for analysis</b>")
                                            ),
                                        div(style = "display: inline-block; vertical-align:center; margin-left: 3px;",
                                            actionButton(ns("infoYearSel"),
                                                         tags$i(class = "fas fa-info",
                                                                style="font-size: 8px"),
                                                         class="infoBubbleButton")
                                            )

                                    ),
                                    div(style = "margin-top:-3px",
                                        uiOutput(ns("LBSPR_years_selected_out"))
                                        ),

                                    br(),

                                    selectInput(ns("LBSPR_lengthUnit"),
                                                "Choose length unit",
                                                choices = c("cm", "mm", "in")),

                                    br(),

                                    ## Bin size
                                    ## ---------------------
                                    fluidRow(
                                        column(6,
                                               fluidRow(
                                                   div(style = "display: inline-block; vertical-align:center; margin-left: 15px;",
                                                       HTML("<b>Bin Size</b>")
                                                       ),
                                                   div(style = "display: inline-block; vertical-align:center; margin-left: 3px;",
                                                       actionButton(ns("infoBS"),
                                                                    tags$i(class = "fas fa-info",
                                                                           style="font-size: 8px"),
                                                                    ## size = "extra-small",
                                                                    ##                                                      style='padding:1px; font-size:70%',
                                                                    class="infoBubbleButton")
                                                       )
                                               ),
                                               div(style = "margin-top:-5px",
                                                   uiOutput(ns("LBSPR_binSize_out")),
                                                   )
                                               ),
                                        ## Linf
                                        ## ---------------------
                                        column(6,
                                               fluidRow(
                                                   div(style = "display: inline-block; vertical-align:center; margin-left: 15px;",
                                                       HTML('<b>L<sub style="font-size:14px">&#8734;</sub></b>')
                                                       ),
                                                   div(style = "display: inline-block; vertical-align:center; margin-left: 3px;",
                                                       actionButton(ns("infoLinf"),
                                                                    tags$i(class = "fas fa-info",
                                                                           style="font-size: 8px"),
                                                                    ## size = "extra-small",
                                                                    ##                                                      style='padding:1px; font-size:70%',
                                                                    class="infoBubbleButton")
                                                       )
                                               ),
                                               div(style = "margin-top:-3px",
                                                   uiOutput(ns("LBSPR_Linf_out")),
                                                   )
                                               )
                                    ),

                                    br(),

                                    ## M/K
                                    ## ---------------------
                                    fluidRow(
                                        column(6,
                                               fluidRow(
                                                   div(style = "display: inline-block; vertical-align:center; margin-left: 15px;",
                                                       HTML("<b>M/K</b>")
                                                       ),
                                                   div(style = "display: inline-block; vertical-align:center; margin-left: 3px;",
                                                       actionButton(ns("infoMK"),
                                                                    tags$i(class = "fas fa-info",
                                                                           style="font-size: 8px"),
                                                                    ## size = "extra-small",
                                                                    ##                                                      style='padding:1px; font-size:70%',
                                                                    class="infoBubbleButton")
                                                       )
                                               ),
                                               div(style = "margin-top:-10px",
                                                   uiOutput(ns("LBSPR_MK_out")),
                                                   )
                                               ),
                                        column(6,
                                               br(),
                                               fluidRow(
                                                   div(style = "display: inline-block; vertical-align: center; margin-left: 5px; margin-top: 10px",
                                                       HTML('<p style="font-size: 14px">Enter M and K directly?</p>')
                                                       ),
                                                   div(style = "display: inline-block; vertical-align: center; margin-left: 8px; margin-top: 15px",
                                                       checkboxInput(ns("LBSPR_split_mk"),
                                                                     "",
                                                                     FALSE)
                                                       )
                                               )
                                               )

                                    ),

                                    box(id="box_lbspr_split_mk",
                                        width = 12,

                                        ## K
                                        ## ---------------------
                                        column(6,
                                               fluidRow(
                                                   div(style = "display: inline-block; vertical-align:center; margin-left: 15px;",
                                                       HTML("<b>M</b>")
                                                       ),
                                                   div(style = "display: inline-block; vertical-align:center; margin-left: 3px;",
                                                       actionButton(ns("infoM"),
                                                                    tags$i(class = "fas fa-info",
                                                                           style="font-size: 8px"),
                                                                    ## size = "extra-small",
                                                                    ##                                                      style='padding:1px; font-size:70%',
                                                                    class="infoBubbleButton")
                                                       )
                                               ),
                                               div(style = "margin-top:-3px",
                                                   numericInput(ns("LBSPR_M"),
                                                                label = "",
                                                                min = 0.0001,
                                                                max = 10,
                                                                value = NULL,
                                                                step = 0.01)
                                                   )
                                               ),

                                        ## M
                                        ## ---------------------
                                        column(6,
                                               fluidRow(
                                                   div(style = "display: inline-block; vertical-align:center; margin-left: 15px;",
                                                       HTML("<b>K</b>")
                                                       ),
                                                   div(style = "display: inline-block; vertical-align:center; margin-left: 3px;",
                                                       actionButton(ns("infoK"),
                                                                    tags$i(class = "fas fa-info",
                                                                           style="font-size: 8px"),
                                                                    ## size = "extra-small",
                                                                    ##                                                      style='padding:1px; font-size:70%',
                                                                    class="infoBubbleButton")
                                                       )
                                               ),
                                               div(style = "margin-top:-3px",
                                                   numericInput(ns("LBSPR_K"),
                                                                label = "",
                                                                min = 0.0001,
                                                                max = 10,
                                                                value = NULL,
                                                                step = 0.01)
                                                   )
                                               )
                                        ),
                                    br(),

                                    ## Lm50 + Lm95
                                    ## ---------------------
                                    box(
                                        title = p(HTML(paste0("Maturity information")),
                                                  actionButton(ns("infoMaturity"),
                                                               tags$i(class = "fas fa-info",
                                                                      style="font-size: 8px"),
                                                               class="infoBubbleButton")),
                                        width = 12,
                                        height = "180px",

                                        fluidRow(
                                            column(6,
                                                   fluidRow(
                                                       div(style = "display: inline-block; vertical-align:center; margin-left: 15px; margin-top: 5px",
                                                           HTML('<b>L<sub style="font-size:12px">m50<sub></b>')
                                                           ),
                                                       ),
                                                   div(style = "margin-top:-3px",
                                                       uiOutput(ns("LBSPR_Lm50_out")),
                                                       )
                                                   ),
                                            column(6,
                                                   fluidRow(
                                                       div(style = "display: inline-block; vertical-align:center; margin-left: 15px; margin-top: 5px",
                                                           HTML('<b>L<sub style="font-size:12px">m95<sub></b>')
                                                           ),
                                                       ),
                                                   div(style = "margin-top:-3px",
                                                       uiOutput(ns("LBSPR_Lm95_out")),
                                                       )
                                                   )
                                        )
                                    ),

                                    br(),

                                    ## LWa and LWb
                                    ## ---------------------
                                    box(
                                        title = p(HTML(paste0("Length-weight relationship (",
                                                              withMathJax("\\(W = a \ L^{b}\\)"),")")),
                                                  actionButton(ns("infoLengthWeight"),
                                                               tags$i(class = "fas fa-info",
                                                                      style="font-size: 8px"),
                                                               class="infoBubbleButton")),
                                        width = 12,
                                        height = "180px",
                                        fluidRow(
                                            column(6,
                                                   fluidRow(
                                                       div(style = "display: inline-block; vertical-align:center; margin-left: 15px; margin-top: 5px",
                                                           HTML('<b>Constant (a)</b>')
                                                           ),
                                                       ),
                                                   div(style = "margin-top:-3px",
                                                       numericInput(ns("LBSPR_LWa"),
                                                                    label = "",
                                                                    min = 0.0001,
                                                                    max = 10,
                                                                    value = 1e-4,
                                                                    step = 0.01,
                                                                    width = "80%"))
                                                   ),
                                            column(6,
                                                   fluidRow(
                                                       div(style = "display: inline-block; vertical-align:center; margin-left: 15px; margin-top: 5px",
                                                           HTML('<b>Exponent (b)</b>')
                                                           ),
                                                       ),
                                                   div(style = "margin-top:-3px",
                                                       numericInput(ns("LBSPR_LWb"),
                                                                    label = "",
                                                                    min = 0.0001,
                                                                    max = 10,
                                                                    value = 3,
                                                                    step = 0.1,
                                                                    width = "80%"))
                                                   )
                                        )
                                    ),

                                    br(),

                                    ## Reference points
                                    ## ---------------------
                                    box(
                                        title = p(HTML(paste0("SPR reference points")),
                                                  actionButton(ns("infoRefs"),
                                                               tags$i(class = "fas fa-info",
                                                                      style="font-size: 8px"),
                                                               class="infoBubbleButton")),
                                        width = 12,
                                        height = "180px",
                                        fluidRow(
                                            column(6,
                                                   fluidRow(
                                                       div(style = "display: inline-block; vertical-align:center; margin-left: 15px; margin-top: 5px",
                                                           HTML('<b>SPR limit</b>')
                                                           ),
                                                       ),
                                                   div(style = "margin-top:-3px",
                                                       numericInput(ns("LBSPR_sprLim"),
                                                                    label = "",
                                                                    min = 0.0001,
                                                                    max = 1,
                                                                    value = 0.2,
                                                                    step = 0.01,
                                                        width = "80%"))
                                       ),
                                column(6,
                                       fluidRow(
                                           div(style = "display: inline-block; vertical-align:center; margin-left: 15px; margin-top: 5px",
                                               HTML('<b>SPR target</b>')
                                               ),
                                           ),
                                       div(style = "margin-top:-3px",
                                           numericInput(ns("LBSPR_sprTarg"),
                                                        label = "",
                                                        min = 0.0001,
                                                        max = 1,
                                                        value = 0.4,
                                                        step = 0.1,
                                                        width = "80%"))
                                       )
                            )
                        ),

                        br(),
                        br(),

                        fluidRow(
                            column(6,
                                   selectInput(ns("fig_format_lbspr"),
                                               "Format of archived figures",
                                               choices = c("pdf","png","jpeg","tiff","bmp","ps"),
                                               selected = "pdf",
                                               multiple = FALSE,
                                               width = "100%")
                                   ),
                            column(6,
                                   selectInput(ns("tab_format_lbspr"),
                                               "Format of archived tables",
                                               choices = c("csv","xls","xlsx"),
                                               selected = "csv",
                                                                multiple = FALSE,
                                                                width = "100%")
                                                    )
                                         ),

                                         br()

                                         ), ## end box settings

                                     box(title = "",
                                         id = "box_exploPlots",
                                         width = 8,
                                         tags$div(
                                                  plotOutput(ns("plot_explo1"), width = "95%",
                                                             height = "700px"),
                                                  div(style = "margin-top:-5px; margin-left: 10px",
                                                      htmlOutput(ns("title_explo1"))
                                                      ),
                                                  ## plotOutput(ns("plot_explo2"), width = "90%",
                                                  ##            height = "280px"),
                                                  ## div(style = "margin-top:-10px; margin-left: 10px",
                                                  ##     htmlOutput(ns("title_explo2"))
                                                  ##     ),
                                                  style = "margin-left: 10%;"
                                              )
                                         )

                                     ),

                            tabPanel(
                                title = div(id = ns("tab2"),"2. Summary & Diagnostics"),
                                value = "diag",

                                fluidRow(
                                    column(3,

                                           uiOutput(ns("text_diag1")),

                                           br()
                                           ),

                                    column(9,

                                           tags$div(
                                                    plotOutput(ns("plot_diag1"),
                                                               width = "90%",
                                                               height = "400px"),
                                                    div(style = "margin-top:-10px; margin-left: 10px",
                                                        htmlOutput(ns("title_diag1"))
                                                        ),
                                                    plotOutput(ns("plot_diag2"),
                                                               width = "90%",
                                                               height = "400px"),
                                                    div(style = "margin-top:-10px; margin-left: 10px",
                                                        htmlOutput(ns("title_diag2"))
                                                        ),
                                                    plotOutput(ns("plot_diag3"),
                                                               width = "90%",
                                                               height = "600px"),
                                                    div(style = "margin-top:-10px; margin-left: 10px",
                                                        htmlOutput(ns("title_diag3"))
                                                        ),
                                                    style = "margin-left: 10%;"
                                                )
                                           )
                                )
                            )
                        )
                        )

                    ),## end of settings box


                ## Action buttons
                ## -------------------------------
                br(),

                box(title = p("Run Assessment & Download Report and Results",
                              actionButton(ns("infoAssessment"),
                                           tags$i(class = "fas fa-info",
                                                  style="font-size: 8px"),
                                           class="infoBubbleButton")),
                    width = NULL,
                    collapsible = FALSE,
                    solidHeader = TRUE,
                    class = "collapsed-box",
                    collapsed = FALSE,

                    br(),

                    fluidRow(
                        div(style = "display: inline-block; vertical-align:center; margin-left: 20px;",
                            disabled(actionButton(ns("go_lbspr"),
                                                  "Run Assessment",
                                                  class="topLevelInformationButton"))
                            ),
                        div(style = "display: inline-block; vertical-align:center; margin-left: 20px;",
                            actionButton(ns("reset_lbspr"),
                                         "Reset",
                                         class="topLevelInformationButton")
                            ),
                        div(style = "display: inline-block; vertical-align:center; margin-left: 20px;",
                            disabled(downloadButton(ns("createLBSPRReport"),
                                                    "Download Report",
                                                    class="topLevelInformationButton"))
                            ),
                        div(style = "display: inline-block; vertical-align:center; margin-left: 20px;",
                            disabled(downloadButton(ns("createLBSPRzip"),
                                                    "Download Results (zip)",
                                                    class="topLevelInformationButton"))
                            ),
                        div(style = "display: inline-block; vertical-align:center; margin-left: 20px;",
                            actionButton(ns("tour_res"), "Start results tour",
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
                    title = p("Results",
                              actionButton(ns("resultConsiderations2"),
                                           tags$i(class = "fas fa-info",
                                                  style="font-size: 8px"),
                                           class="infoBubbleButton")),
                    width = NULL,
                    height = "2100px",
                    collapsible = FALSE,
                    solidHeader = TRUE,
                    class = "collapsed-box",
                    collapsed = FALSE,

                    fluidRow(
                        br(),
                        tags$div(
                                 div(style = "margin-bottom:0px; margin-left: 3px",
                                     htmlOutput(ns("title_table_lbspr"))
                                     ),
                                 dataTableOutput(ns("table_lbspr")),
                                 style = "margin-left: 15%; margin-right: 15%;"
                             ),
                        br(),
                        br(),
                        br(),
                        column(6, align="center",
                               tags$div(
                                        plotOutput(ns("plot_lbsprPie"),
                                                   width = "600px",
                                                   height = "500px"),
                                        div(style = "margin-top:0px; margin-left: 3px",
                                            htmlOutput(ns("title_lbsprPie"))
                                            ),
                                        style = "margin-left: 5%; margin-right: 5%;"
                                    )
                               ),
                        column(6, align="center",
                               tags$div(
                                        plotOutput(ns("plot_lbsprSel"),
                                                   width = "700px",
                                                   height = "600px"),
                                        div(style = "margin-top:0px; margin-left: 5px",
                                            htmlOutput(ns("title_lbsprSel"))
                                            ),
                                        style = "margin-left: 5%; margin-right: 5%;"
                                    )
                               ),
                        br(),
                        br(),
                        br(),
                        br(),
                        br(),
                        column(12, align="center",
                               tags$div(
                                        plotOutput(ns("plot_lbsprTimeSeries"),
                                                   width = "1400px",
                                                   height = "600px"),
                                        div(style = "margin-top: 5px; margin-left: 3px",
                                            htmlOutput(ns("title_lbsprTimeSeries"))
                                            ),
                                        style = "margin-left: 5%; margin-right: 5%; margin-top: 10px;"
                                    )
                               )

                    )## end fluidRow

                    ) ## end of results box


            ) ## end fluidRow


            )## end tabItem
} ## end tab function
