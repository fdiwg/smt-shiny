tabLBI <- function(id) {
    ns <- NS(id)
    tabItem("lbiWidget",

            htmlOutput(ns("lbiTitle")),

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
                    actionButton(ns("tour_general"), "Start LBI tour",
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
                            fileInput(ns("fileLBI"), "Choose Input CSV File",
                                      accept = c(
                                          "text/csv",
                                          "text/comma-separated-values,text/plain",
                                          ".csv")
                                      )
                            )
                        ),
                    box(width = 2,
                        selectizeInput(
                            ns("lbiCSVsep"),
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
                            ns("lbiCSVdec"),
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
                        selectizeInput(ns("lbiDateFormat"),
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
                                       selected = "auto" ## ,
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

                    div(id = "settings_lbi",
                        tabBox(
                            title = "",
                            width = NULL,
                            height = "810px",
                            side="left",
                            selected = "data",
                            id = "settings",


                            tabPanel(
                                title = div(id = ns("tab1"),"1. Data and settings"),
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
                                        uiOutput(ns("LBI_years_selected_out"))
                                        ),

                                    br(),

                                    selectInput(ns("lbi_lengthUnit"),
                                                "Choose length unit",
                                                choices = c("cm", "mm", "in")),

                                    br(),

                                    ## bin size + M/K
                                    column(6,
                                           fluidRow(
                                               div(style = "display: inline-block; vertical-align:center; margin-left: 15px; margin-top: 10px;",
                                                   HTML("<b>Bin Size</b>")
                                                   ),
                                               div(style = "display: inline-block; vertical-align:center; margin-left: 3px; margin-top: 10px;",
                                                   actionButton(ns("infoBS"),
                                                                tags$i(class = "fas fa-info",
                                                                       style="font-size: 8px"),
                                                                ## size = "extra-small",
                                                                ##                                                      style='padding:1px; font-size:70%',
                                                                class="infoBubbleButton")
                                                   )
                                           ),
                                           div(style = "margin-top:-5px",
                                               uiOutput(ns("LBI_binSize_out")),
                                               )
                                           ),
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
                                                   ),
                                               div(style = "display: inline-block; vertical-align:center; margin-left: 5px;",
                                                   HTML("<p>or enter M and K?</p>")
                                                   ),
                                               div(style = "display: inline-block; vertical-align:center; margin-left: 8px;",
                                                   checkboxInput(ns("LBI_split_mk"),
                                                                 "",
                                                                 FALSE)
                                                   ),
                                               ),
                                           div(style = "margin-top:-10px",
                                               uiOutput(ns("LBI_MK_out")),
                                               )
                                           ),

                                    br(),
                                    br(),

                                    box(id="box_lbi_split_mk",
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
                                                   numericInput(ns("LBI_M"),
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
                                                   numericInput(ns("LBI_K"),
                                                                label = "",
                                                                min = 0.0001,
                                                                max = 10,
                                                                value = NULL,
                                                                step = 0.01)
                                                   )
                                               )
                                        ),
                                    br(),
                                    br(),

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
                                               uiOutput(ns("LBI_Linf_out")),
                                               )
                                           ),


                                    ## Lm50
                                    ## ---------------------
                                    column(6,
                                           fluidRow(
                                               div(style = "display: inline-block; vertical-align:center; margin-left: 15px;",
                                                   HTML('<b>L<sub style="font-size:12px">m50<sub></b>')
                                                   ),
                                               div(style = "display: inline-block; vertical-align:center; margin-left: 3px;",
                                                   actionButton(ns("infoLm50"),
                                                                tags$i(class = "fas fa-info",
                                                                       style="font-size: 8px"),
                                                                ## size = "extra-small",
                                                                ##                                                      style='padding:1px; font-size:70%',
                                                                class="infoBubbleButton")
                                                   )
                                           ),
                                           div(style = "margin-top:-3px",
                                               uiOutput(ns("LBI_Lm50_out")),
                                               )
                                           ),


                                    br(),
                                    br(),
                                    br(),

                        ## LWa and LWb
                        ## ---------------------

                        box(
                            title = p(HTML(paste0("Length-weight relationship (",
                                                  withMathJax("\\(W = a \ L^{b}\\)"),"; optional)")),
                                      actionButton(ns("infoLengthWeight"),
                                                   tags$i(class = "fas fa-info",
                                                          style="font-size: 8px"),
                                                   class="infoBubbleButton")),
                            width = 12,
                            height = "180px",
                            fluidRow(
                                column(6,
                                       numericInput(ns("LBI_LWa"),
                                                    label=" Constant  (a) ",
                                                    min = 0.0001,
                                                    max = 10,
                                                    value = NULL,
                                                    step = 0.01,
                                                    width = "80%")),
                                column(6,
                                       numericInput(ns("LBI_LWb"),
                                                    label="Exponent (b) ",
                                                    min = 0.0001,
                                                    max = 10,
                                                    value = NULL,
                                                    step = 0.1,
                                                    width = "80%"))
                            )
                        ),

                        br(),
                        br(),

                        fluidRow(
                            column(6,
                                   selectInput(ns("fig_format_lbi"),
                                               "Format of archived figures",
                                               choices = c("pdf","png","jpeg","tiff","bmp","ps"),
                                               selected = "pdf",
                                               multiple = FALSE,
                                               width = "100%")
                                   ),
                            column(6,
                                   selectInput(ns("tab_format_lbi"),
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
                            disabled(actionButton(ns("go_lbi"),
                                                  "Run Assessment",
                                                  class="topLevelInformationButton"))
                            ),
                        div(style = "display: inline-block; vertical-align:center; margin-left: 20px;",
                            actionButton(ns("reset_lbi"),
                                         "Reset",
                                         class="topLevelInformationButton")
                            ),
                        div(style = "display: inline-block; vertical-align:center; margin-left: 20px;",
                            disabled(downloadButton(ns("createLBIReport"),
                                                    "Download Report",
                                                    class="topLevelInformationButton"))
                            ),
                        div(style = "display: inline-block; vertical-align:center; margin-left: 20px;",
                            disabled(downloadButton(ns("createLBIzip"),
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
                    height = "2500px",
                    collapsible = FALSE,
                    solidHeader = TRUE,
                    class = "collapsed-box",
                    collapsed = FALSE,

                    fluidRow(
                        br(),
                        tags$div(
                                 div(style = "margin-bottom:0px; margin-left: 3px",
                                     htmlOutput(ns("title_table_intro_lbi"))
                                     ),
                                 dataTableOutput(ns("table_intro_lbi")),
                                 style = "margin-left: 15%; margin-right: 15%; margin-bottom: 30px;"
                             ),
                        br(),
                        br(),
                        br(),
                        tags$div(
                                 div(style = "margin-bottom:0px; margin-left: 3px",
                                     htmlOutput(ns("title_table_lbi"))
                                     ),
                                 dataTableOutput(ns("table_lbi")),
                                 style = "margin-left: 15%; margin-right: 15%; margin-bottom: 30px;"
                             ),
                        br(),
                        br(),
                        br(),
                        tags$div(
                                 div(style = "margin-bottom:0px; margin-left: 3px",
                                     htmlOutput(ns("title_table2_lbi"))
                                     ),
                                 dataTableOutput(ns("table2_lbi")),
                                 style = "margin-left: 20%; margin-right: 20%; margin-bottom: 30px;"
                             ),
                        br(),
                        br(),
                        br(),
                        column(12, align="center",
                               tags$div(
                                        plotOutput(ns("plot_lbiFit"),
                                                   width = "900px",
                                                   height = "900px"),
                                        div(style = "margin-top:0px; margin-left: 3px",
                                            htmlOutput(ns("title_lbiFit"))
                                            ),
                                        style = "margin-left: 5%; margin-right: 5%;"
                                    )
                               )

                    )## end fluidRow

                    ) ## end of results box


            ) ## end fluidRow


            )## end tabItem
} ## end tab function
