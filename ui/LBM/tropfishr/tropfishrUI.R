tabElefanGa <- function(id) {
    ns <- NS(id)
    tabItem("ElefanGaWidget",

            htmlOutput(ns("elefanGaTitle")),

            htmlOutput("tropFishRLibVersion1", class="subTitle"),

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
                    actionButton(ns("tour_general"), "Start TropFishR tour",
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
                            fileInput(ns("fileGa"), "Choose Input CSV File",
                                      accept = c(
                                          "text/csv",
                                          "text/comma-separated-values,text/plain",
                                          ".csv")
                                      )
                            )
                        ),
                    box(width = 2,
                        selectizeInput(
                            ns("elefanGaCSVsep"),
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
                            ns("elefanGaCSVdec"),
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
                        selectizeInput(ns("elefanGaDateFormat"),
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

                    div(id = "settings_elefan",
                        tabBox(
                            title = "",
                            width = NULL,
                            height = "900px",
                            side="left",
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
                                            actionButton(ns("infoYearSel"),
                                                         tags$i(class = "fas fa-info",
                                                                style="font-size: 8px"),
                                                         class="infoBubbleButton")
                                            )

                                    ),
                                    div(style = "margin-top:-3px",
                                        uiOutput(ns("ELEFAN_years_selected_out"))
                                        ),

                                    br(),



                                    selectInput(ns("ELEFAN_agg"),
                                                p("Aggregate data by",
                                                  actionButton(ns("infoAGG"),
                                                               tags$i(class = "fas fa-info",
                                                                      style="font-size: 8px"),
                                                               class="infoBubbleButton")),
                                                choices = c("Choose one"="",
                                                            c("none","month","quarter","year")),
                                                selected = "none",
                                                width ='100%'),
                                    br(),

                                    selectInput(ns("elefan_lengthUnit"),
                                                "Length unit",
                                                choices = c("cm", "mm", "in")),

                                    br(),

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
                                    div(style = "margin-top:-3px",
                                        uiOutput(ns("ELEFAN_binSize_out")),
                                        ),
                                    br(),

                                    ## numericInput(ns("ELEFAN_GA_PLUS_GROUP"),
                                    ##              p("Plus group",
                                    ##                actionButton(ns("infoPG"),
                                    ##                             tags$i(class = "fas fa-info",
                                    ##                                    style="font-size: 12px"),
                                    ##                             class="topLevelInformationButton")),
                                    ##              0, min = 0, max = 100000, step=1,
                                    ##              width ='50%'),

                                    numericInput(ns("ELEFAN_GA_MA"),
                                                 p("Moving Average (MA)",
                                                   actionButton(ns("infoMA"),
                                                                tags$i(class = "fas fa-info",
                                                                       style="font-size: 8px"),
                                                                class="infoBubbleButton")),
                                                 5, min = 1, max = 101, step=2,
                                                 width ='100%'),

                                    br(),

                                    checkboxInput(ns("ELEFAN_GA_addlsqrt"),
                                                  p("Additional squareroot transformation?",
                                                    actionButton(ns("infoAT"),
                                                                 tags$i(class = "fas fa-info",
                                                                        style="font-size: 8px"),
                                                                 class="infoBubbleButton")),
                                                  FALSE),

                                    br(),

                                    fluidRow(
                                        column(6,
                                               selectInput(ns("fig_format_ga"),
                                                           "Format of archived figures",
                                                           choices = c("pdf","png","jpeg","tiff","bmp","ps"),
                                                           selected = "pdf",
                                                           multiple = FALSE,
                                                           width = "100%")
                                               ),
                                        column(6,
                                               selectInput(ns("tab_format_ga"),
                                                           "Format of archived tables",
                                                           choices = c("csv","xls","xlsx"),
                                                           selected = "csv",
                                                           multiple = FALSE,
                                                           width = "100%")
                                               )
                                    ),

                                    br()

                                    ),
                                box(title = "Data exploration",
                                    id = "box_exploPlots",
                                    width = 9,
                                    tags$div(
                                             plotOutput(ns("plot_explo1"), width = "90%",
                                                        height = "600px"),
                                             div(style = "margin-top:-10px; margin-left: 10px",
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
                                title = div(id = ns("tab2"),"2. ELEFAN"),
                                value = "elefan",
                                box(title = p("Search space for growth parameters",
                                              actionButton(ns("info_searchSpace"),
                                                           tags$i(class = "fas fa-info",
                                                                  style="font-size: 8px"),
                                                           class="infoBubbleButton")),
                                    width = 9,
                                    box(width=6,
                                        fluidRow(
                                            div(style = "display: inline-block; vertical-align:center; margin-left: 15px;",
                                                HTML(paste0("<b> Asymptotic length (",withMathJax("\\(L_\\infty\\)"),") </b>"))
                                                ),
                                            div(style = "display: inline-block; vertical-align:center; margin-left: 3px;",
                                                actionButton(ns("infolinf"),
                                                             tags$i(class = "fas fa-info",
                                                                    style="font-size: 8px"),
                                                             class="infoBubbleButton")
                                                )

                                        ),
                                        div(style = "margin-top:-3px",
                                            uiOutput(ns("ELEFAN_GA_Linf_out")),
                                            ),
                                        br(),
                                        sliderInput(ns("ELEFAN_GA_K"),
                                                    p(HTML(paste0("Growth rate (",withMathJax("\\(K\\)"),")")),
                                                      actionButton(ns("infok"),
                                                                   tags$i(class = "fas fa-info",
                                                                          style="font-size: 8px"),
                                                                   class="infoBubbleButton")),
                                                    value=c(0.05,1), min = 0, max = 10, step=0.01),
                                        br(),
                                        sliderInput(ns("ELEFAN_GA_t_anchor"),
                                                    p(HTML(paste0("Time anchor (",withMathJax("\\(t_{a}\\)"),")")),
                                                      actionButton(ns("infotanchor"),
                                                                   tags$i(class = "fas fa-info",
                                                                          style="font-size: 8px"),
                                                                   class="infoBubbleButton")),
                                                    value=c(0,1), min = 0, max = 1, step=0.01),
                                        ),
                                    box(width=6,
                                        checkboxInput(ns("ELEFAN_GA_seasonalised"),
                                                      p("Seasonal model?",
                                                        actionButton(ns("infoSeason"),
                                                                     tags$i(class = "fas fa-info",
                                                                            style="font-size: 8px"),
                                                                     class="infoBubbleButton")),
                                                      FALSE)
                                        ),
                                    br(),
                                    box(id="box_elefan_ga_seasonPar",
                                        width = 6,
                                        sliderInput(ns("ELEFAN_GA_C"),
                                                    p(HTML(paste0("Amplitude (",withMathJax("\\(C\\)"),")")),
                                                      actionButton(ns("infoC"),
                                                                   tags$i(class = "fas fa-info",
                                                                          style="font-size: 8px"),
                                                                   class="infoBubbleButton")),
                                                    value=c(0,1), min = 0, max = 1, step=0.01),
                                        br(),
                                        sliderInput(ns("ELEFAN_GA_ts"),
                                                    p(HTML(paste0("Summer point (",withMathJax("\\(t_{s}\\)"),")")),
                                                      actionButton(ns("infots"),
                                                                   tags$i(class = "fas fa-info",
                                                                          style="font-size: 8px"),
                                                                   class="infoBubbleButton")),
                                                    value=c(0,1), min = 0, max = 1, step=0.01)
                                        ),

                                    box(width = 12,
                                        fluidRow(
                                            column(3,
                                                   checkboxInput(ns("provideGP"),
                                                                 p("Provide growth parameters?",
                                                                   actionButton(ns("infoProvideGP"),
                                                                                tags$i(class = "fas fa-info",
                                                                                       style="font-size: 8px"),
                                                                                class="infoBubbleButton")),
                                                                 FALSE)
                                                   ),
                                            column(9,
                                                   box(id = "box_provide_gp",
                                                       width=12,
                                                       fluidRow(
                                                           column(4,
                                                                  numericInput(ns("provide_Linf"),
                                                                               p(HTML(paste0("<b> Asymptotic length (",withMathJax("\\(L_\\infty\\)"),") </b>")),
                                                                                 actionButton(ns("infok"),
                                                                                              tags$i(class = "fas fa-info",
                                                                                                     style="font-size: 8px"),
                                                                                              class="infoBubbleButton")),
                                                                               value = NA, min = 0, max = 1e5, step=0.001, width = "80%")
                                                                  ),
                                                           column(4,
                                                                  numericInput(ns("provide_K"),
                                                                               p(HTML(paste0("Growth rate (",withMathJax("\\(K\\)"),")")),
                                                                                 actionButton(ns("infok"),
                                                                                              tags$i(class = "fas fa-info",
                                                                                                     style="font-size: 8px"),
                                                                                              class="infoBubbleButton")),
                                                                               value = NA, min = 0, max = 1e5, step=0.001, width = "80%")
                                                                  ),
                                                           column(4,
                                                                  numericInput(ns("provide_t_anchor"),
                                                                               p(HTML(paste0("Time anchor (",withMathJax("\\(t_{a}\\)"),")")),
                                                                                 actionButton(ns("infotanchor"),
                                                                                              tags$i(class = "fas fa-info",
                                                                                                     style="font-size: 8px"),
                                                                                              class="infoBubbleButton")),
                                                                               value = NA, min = 0, max = 1, step=0.001, width = "80%")
                                                                  )
                                                       ),

                                                       br(),

                                                       box(id = "box_provide_gp_sea",
                                                           width=12,
                                                           fluidRow(
                                                               column(2),
                                                               column(4,
                                                                      numericInput(ns("provide_C"),
                                                                                   p(HTML(paste0("Amplitude (",withMathJax("\\(C\\)"),")")),
                                                                                     actionButton(ns("infoC"),
                                                                                                  tags$i(class = "fas fa-info",
                                                                                                         style="font-size: 8px"),
                                                                                                  class="infoBubbleButton")),
                                                                                   value = NA, min = 0, max = 10, step=0.001, width = "80%")
                                                                      ),
                                                               column(4,
                                                                      numericInput(ns("provide_ts"),
                                                                                   p(HTML(paste0("Summer point (",withMathJax("\\(t_{s}\\)"),")")),
                                                                                     actionButton(ns("infots"),
                                                                                                  tags$i(class = "fas fa-info",
                                                                                                         style="font-size: 8px"),
                                                                                                  class="infoBubbleButton")),
                                                                                   value = NA, min = 0, max = 1, step=0.001, width = "85%")
                                                                      ),
                                                               column(2)
                                                           )
                                                           )

                                                       )
                                                   )
                                        )
                                        )
                                    ),

                                box(title = p("ELEFAN's genetic algorithm",
                                              actionButton(ns("info_GA"),
                                                           tags$i(class = "fas fa-info",
                                                                  style="font-size: 8px"),
                                                           class="infoBubbleButton")),
                                    width = 3,
                                    numericInput(ns("ELEFAN_GA_popSize"),
                                                 p("Population size:",
                                                   actionButton(ns("infoPopSize"),
                                                                tags$i(class = "fas fa-info",
                                                                       style="font-size: 8px"),
                                                                class="infoBubbleButton")),
                                                 100, min = 50, max = 1e3, step=1,
                                                 width = "90%"),
                                    numericInput(ns("ELEFAN_GA_maxiter"),
                                                 p("Maximum number of generations",
                                                   actionButton(ns("infoMaxIter"),
                                                                tags$i(class = "fas fa-info",
                                                                       style="font-size: 8px"),
                                                                class="infoBubbleButton")),
                                                 50, min = 20, max = 1e3, step=1,
                                                 width = "90%"),
                                    numericInput(ns("ELEFAN_GA_run"),
                                                 p("Number of generations without improvement",
                                                   actionButton(ns("infoMaxRuns"),
                                                                tags$i(class = "fas fa-info",
                                                                       style="font-size: 8px"),
                                                                class="infoBubbleButton")),
                                                 20, min = 10, max = 1e3, step=1,
                                                 width = "90%"),

                                    numericInput(ns("ELEFAN_GA_pmutation"),
                                                 p("Probability of mutation",
                                                   actionButton(ns("infoPmut"),
                                                                tags$i(class = "fas fa-info",
                                                                       style="font-size: 8px"),
                                                                class="infoBubbleButton")),
                                                 0.2, min = 0.1, max = 1, step=0.1,
                                                 width = "90%"),

                                    numericInput(ns("ELEFAN_GA_pcrossover"),
                                                 p("Probability of crossover",
                                                   actionButton(ns("infoPcross"),
                                                                tags$i(class = "fas fa-info",
                                                                       style="font-size: 8px"),
                                                                class="infoBubbleButton")),
                                                 0.8, min = 0.1, max = 1, step=0.1,
                                                 width = "90%"),

                                    numericInput(ns("ELEFAN_GA_elitism"),
                                                 p("Degree of elitism",
                                                   actionButton(ns("infoElite"),
                                                                tags$i(class = "fas fa-info",
                                                                       style="font-size: 8px"),
                                                                class="infoBubbleButton")),
                                                 5, min = 1, max = 1e2, step=1,
                                                 width = "90%")
                                    )

                            ),

                            tabPanel(
                                title = div(id = ns("tab3"),"3. Biological parameters"),
                                value = "bio_pars",

                                fluidRow(

                                    box(
                                        title = p(HTML(paste0("Length-weight relationship (",
                                                              withMathJax("\\(W = a \ L^{b}\\)"),")")),
                                                  actionButton(ns("infoLengthWeight"),
                                                               tags$i(class = "fas fa-info",
                                                                      style="font-size: 8px"),
                                                               class="infoBubbleButton")),

                                        width = 4,
                                        height = "200px",
                                        fluidRow(
                                            column(6,
                                                   numericInput(ns("LWa"),
                                                                label=" Constant  (a) ",
                                                                min = 0.0001,
                                                                max = 10,
                                                                value = 0.01,
                                                                step = 0.01,
                                                                width = "60%")),
                                            column(6,
                                                   numericInput(ns("LWb"),
                                                                label="Exponent (b) ",
                                                                min = 0.0001,
                                                                max = 10,
                                                                value = 3,
                                                                step = 0.1,
                                                                width = "60%"))
                                        )
                                    ),

                                    box(title = p("Natural mortality",
                                                  actionButton(ns("infoNatM"),
                                                               tags$i(class = "fas fa-info",
                                                                      style="font-size: 8px"),
                                                               class="infoBubbleButton")),
                                        width = 8,
                                        height = "200px",
                                        fluidRow(
                                            column(5,
                                                   selectInput(ns("natM"),
                                                               "Method:",
                                                               choices = c("Then's growth formula",
                                                                           "Pauly's growth & temp. formula",
                                                                           "Then's max. age formula",
                                                                           "Gislason's length-based formula",
                                                                           "Lorenzen's length-based formula"),
                                                               selected = "Then's growth formula",
                                                               width ='80%')
                                                   ),
                                            column(6,
                                                   div(id ="ui_natM_pauly",
                                                       fluidRow(
                                                           column(7,
                                                                  numericInput(ns("temp"),
                                                                               label = "Average ambient sea surface temperature (SST)",
                                                                               min = 0,
                                                                               step = 0.5,
                                                                               value = 20,
                                                                               width = "60%")
                                                                  ),
                                                           column(5,
                                                                  div(style="margin-top:15px;",
                                                                      checkboxInput(ns("schooling"),
                                                                                    label = "Correction for schooling?",
                                                                                    value = FALSE)
                                                                      )
                                                                  )
                                                       )
                                                       ),
                                                   div(id ="ui_natM_then_tmax",
                                                       ## tmax for Then_tmax
                                                       numericInput(ns("tmax"),
                                                                    label = "Maximum age",
                                                                    min = 0,
                                                                    max = 200,
                                                                    value = 20,
                                                                    step = 1,
                                                                    width = '40%'),
                                                       )
                                                   )
                                        )
                                        )

                                ),

                                fluidRow(

                                    box(title = p("Maturity (optional)",
                                                  actionButton(ns("infoMat"),
                                                               tags$i(class = "fas fa-info",
                                                                      style="font-size: 8px"),
                                                               class="infoBubbleButton")),
                                        width=12,
                                        height = "200px",
                                        fluidRow(
                                            column(3,
                                                   selectInput(ns("selectMat"),
                                                               p("Maturity"),
                                                               choices = c("No maturity",
                                                                           "Define Lm50 & Lm75",
                                                                           "Define Lm50 & (Lm75-Lm25)",
                                                                           "Other"),
                                                               selected = "No maturity",
                                                               width = "90%")
                                                   ),
                                            column(1),
                                            column(4,
                                                   div(
                                                       id ="ui_lm50",
                                                       numericInput(ns("lm50_user"),
                                                                    label=p(withMathJax("\\(L_{m50}\\)")),
                                                                    value = NULL,
                                                                    min = 0,
                                                                    step = 1,
                                                                    width = "60%")
                                                   ),
                                                   column(6,
                                                          div(
                                                              id ="ui_per_lm1",
                                                              numericInput(ns("per_lm1_user"),
                                                                           p("Prob. of maturation (", withMathJax("\\(mX_{1}\\)"),")"),
                                                                           value = NULL,
                                                                           min = 0, step = 1, max = 100,
                                                                           width = "90%")
                                                          )),
                                                   column(6,
                                                          div(
                                                              id ="ui_lm1",
                                                              numericInput(ns("lm1_user"),
                                                                           p("Length at ",
                                                                             withMathJax("\\(mX_{1}\\)"),
                                                                             " (",withMathJax("\\(L_{mX_1}\\)"),")"),
                                                                           value = NULL,
                                                                           min = 0, step = 1,
                                                                           width = "90%")
                                                          ))
                                                   ),
                                            column(4,
                                                   div(
                                                       id ="ui_lm75",
                                                       numericInput(ns("lm75_user"),
                                                                    label=p(withMathJax("\\(L_{m75}\\)")),
                                                                    value = NULL,
                                                                    min = 0,
                                                                    step = 1,
                                                                    width = "60%")
                                                   ),
                                                   div(
                                                       id ="ui_wqsm",
                                                       numericInput(ns("wqsm_user"),
                                                                    p("Width (",withMathJax("\\(L_{m75}-L_{m25}\\)")),

                                                                    value = NULL, min = 0, step = 1,
                                                                    width = "60%")
                                                   ),
                                                   column(6,
                                                          div(
                                                              id ="ui_per_lm2",
                                                              numericInput(ns("per_lm2_user"),
                                                                           p("Prob. of maturation (",withMathJax("\\(mX_{2}\\)"),")"),
                                                                           value = NULL,
                                                                           min = 0, step = 1, max = 100,
                                                                           width = "90%")
                                                          )),
                                                   column(6,
                                                          div(
                                                              id ="ui_lm2",
                                                              numericInput(ns("lm2_user"),
                                                                           p("Length at ",
                                                                             withMathJax("\\(mX_{2}\\)"),
                                                                             " (",withMathJax("\\(L_{mX_2}\\)"),")"),
                                                                           value = NULL,
                                                                           min = 0, step = 1,
                                                                           width = "90%")
                                                          ))
                                                   )
                                        ),
                                        br(), br()
                                        )
                                )

                            ),


                            tabPanel(
                                title = div(id = ns("tab4"),"4. Other settings"),
                                value = "other",

                                fluidRow(

                                    box(title = p("Adjust length data",
                                                  actionButton(ns("infoAdjData"),
                                                               tags$i(class = "fas fa-info",
                                                                      style="font-size: 8px"),
                                                               class="infoBubbleButton")),
                                        width = 2,
                                        height = "200px",
                                        fluidRow(
                                            column(12,
                                                   fluidRow(
                                                       div(style = "display: inline-block; vertical-align:center; margin-left: 15px;",
                                                           HTML("<b>Select years (stock status)</b>")
                                                           ),
                                                       ),
                                                   div(style = "margin-top:-3px",
                                                       uiOutput(ns("ELEFAN_years_selected_cc_out"))
                                                       )
                                                   )
                                        ),
                                        br(), br()
                                        ),

                                    box(title = p("Selectivity",
                                                  actionButton(ns("infoSelect"),
                                                               tags$i(class = "fas fa-info",
                                                                      style="font-size: 8px"),
                                                               class="infoBubbleButton")),
                                        width=10,
                                        height = "200px",
                                        fluidRow(
                                            column(3,
                                                   selectInput(
                                                       ns("select"),
                                                       "Selectivity",
                                                       choices = c(
                                                           "Estimate",
                                                           "Define Ls50 & Ls75",
                                                           "Define Ls50 & (Ls75 - Ls25)",
                                                           "Other"
                                                       ),
                                                       selected = "Estimate",
                                                       width = "100%"
                                                   ),
                                                   ),
                                            column(1),
                                            column(4,
                                                   div(
                                                       id ="ui_l50",
                                                       numericInput(ns("l50_user"),
                                                                    p(withMathJax("\\(L_{s50}\\)")," (user defined)"),
                                                                    value = NULL, min = 0, step = 1,
                                                                    width = "80%")
                                                   ),

                                                   column(6,
                                                          div(
                                                              id ="ui_per_l1",
                                                              numericInput(ns("per_l1_user"),
                                                                           p("Prob. of selection (",
                                                                             withMathJax("\\(X_{1}\\)"),")"),
                                                                           value = NULL,
                                                                           min = 0, step = 1, max = 100,
                                                                           width = "100%")
                                                          )
                                                          ),
                                                   column(6,
                                                          div(
                                                              id ="ui_l1",
                                                              numericInput(ns("l1_user"),
                                                                           p("Length at ",
                                                                             withMathJax("\\(X_{1}\\)"),
                                                                             " (",withMathJax("\\(L_{sX1}\\)"),")"),
                                                                           value = NULL, min = 0, step = 1,
                                                                           width = "100%")
                                                          )
                                                          )
                                                   ),
                                            column(4,
                                                   div(
                                                       id ="ui_l75",
                                                       numericInput(ns("l75_user"),
                                                                    p(withMathJax("\\(L_{s75}\\)")," (user defined)"),
                                                                    value = NULL, min = 0, step = 1,
                                                                    width = "80%")
                                                   ),
                                                   div(
                                                       id ="ui_wqs",
                                                       numericInput(ns("wqs_user"),
                                                                    p("Width (",withMathJax("\\(L_{s75}-L_{s25}\\)"),"; user defined)"),
                                                                    value = NULL, min = 0, step = 1,
                                                                    width = "80%")
                                                   ),
                                                   column(6,
                                                          div(
                                                              id ="ui_per_l2",
                                                              numericInput(ns("per_l2_user"),
                                                                           p("Prob. of selection (",
                                                                             withMathJax("\\(X_{2}\\)"),")"),
                                                                           value = NULL,
                                                                           min = 0, step = 1, max = 100,
                                                                           width = "100%")
                                                          )
                                                          ),
                                                   column(6,
                                                          div(
                                                              id ="ui_l2",
                                                              numericInput(ns("l2_user"),
                                                                           p("Length at ",
                                                                             withMathJax("\\(X_{2}\\)"),
                                                                             " (",withMathJax("\\(L_{sX2}\\)"),")"),
                                                                           value = NULL, min = 0, step = 1,
                                                                           width = "100%")
                                                          )
                                                          )
                                                   )
                                        ),
                                        br(), br()
                                        )
                                ),

                                box(title = p("Prediction range",
                                              actionButton(ns("infoPred"),
                                                           tags$i(class = "fas fa-info",
                                                                  style="font-size: 8px"),
                                                           class="infoBubbleButton")),
                                    width=12,
                                    height = "200px",
                                    fluidRow(
                                        ##                                         column(1),
                                        column(2,
                                               div(style = "margin-top:32px; margin-left:100px;",
                                                   "Fishing mortality")
                                               ),
                                        column(1,
                                               numericInput(ns("fRangeSteps"),
                                                            label = "Steps",
                                                            value = 100,
                                                            min = 0,
                                                            step = 1,
                                                            width = "100%"
                                                            )
                                               ),
                                        column(1,
                                               numericInput(ns("fRangeMin"),
                                                            label = "Min",
                                                            value = 0,
                                                            min = 0,
                                                            step = 0.1,
                                                            width = "100%"
                                                            )
                                               ),
                                        column(1,
                                               numericInput(ns("fRangeMax"),
                                                            label = "Max",
                                                            value = 3,
                                                            min = 0,
                                                            step = 0.1,
                                                            width = "100%"
                                                            )
                                               ),
                                        column(1),
                                        ##                                         column(1),
                                        column(2,
                                               div(style = "margin-top:32px; margin-left:20px;",
                                                   p("Length at 50% selectivity (",withMathJax("\\(L_{50}\\)"),")"))
                                               ),
                                        column(1,
                                               numericInput(ns("lcRangeSteps"),
                                                            label = "Steps",
                                                            value = 100,
                                                            min = 0,
                                                            step = 1,
                                                            width = "100%"
                                                            )
                                               ),
                                        column(1,
                                               div(
                                                   id ="ui_lcMin",
                                                   numericInput(ns("lcRangeMin"),
                                                                label = "Min",
                                                                value = 0,
                                                                min = 0,
                                                                step = 1,
                                                                width = "100%"
                                                                )
                                               )
                                               ),
                                        column(1,
                                               div(
                                                   id ="ui_lcMax",
                                                   numericInput(ns("lcRangeMax"),
                                                                label = "Max",
                                                                value = 0,
                                                                min = 0,
                                                                step = 1,
                                                                width = "100%"
                                                                )
                                               )
                                               ),
                                        column(1)
                                    )
                                    )


                                     ),

                            tabPanel(
                                title = div(id = ns("tab5"),"5. Summary & Diagnostics"),
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

                        ))
                    ),



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
                    collapsed = FALSe,

                    br(),

                    fluidRow(
                        div(style = "display: inline-block; vertical-align:center; margin-left: 50px;",
                            disabled(actionButton(ns("check_ga"),
                                                  "Run Check",
                                                  class="topLevelInformationButton"))
                            ),
                        div(style = "display: inline-block; vertical-align:center; margin-left: 20px;",
                            disabled(actionButton(ns("go_ga"),
                                                  "Run Assessment",
                                                  class="topLevelInformationButton"))
                            ),
                        div(style = "display: inline-block; vertical-align:center; margin-left: 20px;",
                            actionButton(ns("reset_ga"),
                                         "Reset",
                                         class="topLevelInformationButton")
                            ),
                        div(style = "display: inline-block; vertical-align:center; margin-left: 20px;",
                            disabled(downloadButton(ns("createElefanGAReport"),
                                                    "Download Report",
                                                    class="topLevelInformationButton"))
                            ),
                        div(style = "display: inline-block; vertical-align:center; margin-left: 20px;",
                            disabled(downloadButton(ns("createElefanGAzip"),
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
                    title = p("Assessment Results",
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

                    fluidRow (
                        column(
                            7,
                            tags$div(
                                     plotOutput(ns("plot_growthCurves"), width = "90%",
                                                height = "600px"),
                                     div(style = "margin-top:0px; margin-left: 10%; margin-right: 10%;",
                                         htmlOutput(ns("title_growthCurves"))
                                         ),
                                     style = "margin-left: 0%; margin-right: 0%;"
                                 )
                        ),
                        column(
                            5,
                            br(),br(),br(),
                            tags$div(
                                     div(style = "margin-bottom:0px; margin-left: 0%; margin-right: 0%;",
                                         htmlOutput(ns("title_table_growth"))
                                         ),
                                     dataTableOutput(ns("table_growth")),
                                     style = "margin-left: 5%; margin-right: 15%;"
                                 )
                        )

                    ),

                    br(),br(),

                    fluidRow (
                        column(
                            6,
                            tags$div(
                                     plotOutput(ns("plot_elefanFit"), width = "80%",
                                                height = "450px"),
                                     div(style = "margin-top:0px; margin-left: 10%; margin-right: 10%;",
                                         htmlOutput(ns("title_elefanFit"))
                                         ),
                                     style = "margin-left: 0%; margin-right: 0%;"
                                 )
                        ),
                        column(
                            6,
                            tags$div(
                                     plotOutput(ns("plot_mort"), width = "80%",
                                                height = "450px"),
                                     div(style = "margin-top:0px; margin-left: 10%; margin-right: 10%;",
                                         htmlOutput(ns("title_mort"))
                                         ),
                                     style = "margin-left: 0%; margin-right: 0%;"
                                 )
                        )

                    ),

                    br(), br(),



                    fluidRow (
                        column(
                            6,
                            tags$div(
                                     plotOutput(ns("plot_catchCurve"), width = "80%",
                                                height = "450px"),
                                     div(style = "margin-top:0px; margin-left: 10%; margin-right: 10%;",
                                         htmlOutput(ns("title_catchCurve"))
                                         ),
                                     style = "margin-left: 0%; margin-right: 0%;"
                                 )
                        ),
                        column(
                            6,
                            tags$div(
                                     plotOutput(ns("plot_select"), width = "80%",
                                                height = "450px"),
                                     div(style = "margin-top:0px; margin-left: 10%; margin-right: 10%;",
                                         htmlOutput(ns("title_select"))
                                         ),
                                     style = "margin-left: 0%; margin-right: 0%;"
                                 )
                        )

                    ),

                    br(), br(),

                    fluidRow(
                        column(
                            5,
                            tags$div(
                                     div(style = "margin-bottom:0px; margin-left: 0%; margin-right: 0%;",
                                         htmlOutput(ns("title_table_mort"))
                                         ),
                                     dataTableOutput(ns("table_mort")),
                                     style = "margin-left: 25%; margin-right: 5%;"
                                 )
                        ),
                        column(2),
                        column(
                            5,
                            tags$div(
                                     div(style = "margin-bottom:0px; margin-left: 0%; margin-right: 0%;",
                                         htmlOutput(ns("title_table_refs"))
                                         ),
                                     dataTableOutput(ns("table_refs")),
                                     style = "margin-left: 5%; margin-right: 30%;"
                                 )
                        )
                    ),

                    br(),br(),

                    fluidRow(
                        column(
                            6,
                            tags$div(
                                     plotOutput(ns("plot_ypr"), width = "90%",
                                                height = "800px"),
                                     div(style = "margin-top:0px; margin-left: 10%; margin-right: 10%;",
                                         htmlOutput(ns("title_ypr"))
                                         ),
                                     style = "margin-left: 0%; margin-right: 0%;"
                                 )
                        ),
                        column(
                            6,
                            tags$div(
                                     plotOutput(ns("plot_ypr_iso"), width = "90%",
                                                height = "800px"),
                                     div(style = "margin-top:0px; margin-left: 10%; margin-right: 10%;",
                                         htmlOutput(ns("title_ypr_iso"))
                                         ),
                                     style = "margin-left: 0%; margin-right: 0%;"
                                 )
                        )
                    ),

                    br(), br(),

                    fluidRow(
                        column(
                            5,
                            tags$div(
                                     div(style = "margin-bottom:0px; margin-left: 0px; margin-right: 0px;",
                                         htmlOutput(ns("title_table_stockstatus"))
                                         ),
                                     dataTableOutput(ns("table_stockstatus")),
                                     style = "margin-left: 20%; margin-right: 5%;"
                                 )
                        ),
                        column(2),
                        column(
                            5,
                            tags$div(
                                     div(style = "margin-bottom:0px; margin-left: 0px; margin-right: 0px;",
                                         htmlOutput(ns("title_table_forOtherMethods"))
                                         ),
                                     dataTableOutput(ns("table_forOtherMethods")),
                                     style = "margin-left: 5%; margin-right: 25%;"
                                 )
                        )
                    ),

                    br()


                    ##         "Results of the Thompson and Bell model: Curves of yield and biomass per recruit. The black dot represents yield and biomass under current fishing pressure. The yellow and red dashed lines represent fishing mortality for maximum sustainable yield (Fmsy) and fishing mortality to fish the stock at 50% of the virgin biomass (F0.5).",

                    ##         "Exploration of impact of different exploitation rates and Lc values on the relative yield per recruit.",


                    )
            )
            )


}
