tabLBI <- function(id) {
    ns <- NS(id)
    tabItem("lbiWidget",

            htmlOutput(ns("lbiTitle")),

            fluidRow(
                div(style = "display: inline-block; vertical-align:center; margin-left: 15px;",
                    "More information about "
                    ),
                div(style = "display: inline-block; vertical-align:center; margin-left: 15px;",
                    actionButton(ns("lbiWorkflowConsiderations"), "Workflow",
                                 class="topLevelInformationButton")
                    ),
                div(style = "display: inline-block; vertical-align:center; margin-left: 5px;",
                    actionButton(ns("lbiDataConsiderations"), "Data",
                                 class="topLevelInformationButton")
                    ),
                div(style = "display: inline-block; vertical-align:center; margin-left: 5px;",
                    actionButton(ns("lbiMethodConsiderations"), "Methods",
                                 class="topLevelInformationButton")
                    ),
                div(style = "display: inline-block; vertical-align:center; margin-left: 5px;",
                    actionButton(ns("lbiResultConsiderations"), "Results",
                                 class="topLevelInformationButton")
                    )
            ),



            fluidRow(

                ## Notifications in middle of screen (if decided to keep -> move to custom.css?)
                tags$head(tags$style(HTML(".shiny-notification { position:fixed; top: calc(50%); left: calc(50%); width: 250px; height: 80px;}"))),

                ## Information tabs
                ## -------------------------------
                bsModal("modalWorkflowLBI", "Workflow Considerations - LBI",
                        ns("lbiWorkflowConsiderations"),
                        size = "large",
                        htmlOutput(ns("lbiWorkflowConsiderationsText"))),

                bsModal("modalDataLBI", "Data Loading and Formatting Considerations - LBI",
                        ns("lbiDataConsiderations"),
                        size = "large",
                        htmlOutput(ns("lbiDataConsiderationsText"))),

                bsModal("modalData2LBI", "Data Considerations - LBI",
                        ns("lbiDataConsiderations2"),
                        size = "large",
                        htmlOutput(ns("lbiDataConsiderationsText2"))),

                bsModal("modalMethodLBI", "Methodological Considerations - LBI",
                        ns("lbiMethodConsiderations"),
                        size = "large",
                        htmlOutput(ns("lbiMethodConsiderationsText"))),

                bsModal("modalMethod2LBI", "Methodological Considerations - LBI",
                        ns("lbiMethodConsiderations2"),
                        size = "large",
                        htmlOutput(ns("lbiMethodConsiderationsText2"))),

                bsModal("modalResultsLBI", "Results Considerations - LBI",
                        ns("lbiResultConsiderations"),
                        size = "large",
                        htmlOutput(ns("lbiResultConsiderationsText"))),

                bsModal("modalResults2LBI", "Results Considerations - LBI",
                        ns("lbiResultConsiderations2"),
                        size = "large",
                        htmlOutput(ns("lbiResultConsiderationsText2"))),

                bsModal("info_assessment_lbi", "Assessment, Reset & Report", ns("infoAssessment"),
                        size = "large",
                        HTML("<p><b>'Calculate Indicators'</b> estimates the length-based indicators and produces ",
                             "two tables and a figure in the results section upon successful completion. ",
                             "<br> <br> <b>'Reset'</b> removes all results, the uploaded dataset, ",
                             "and resets all settings to default values. <br> <br> After successful calculation ",
                             "of the indicators, two additional buttons <b>'Download Report'</b> and ",
                             "<b>'Download Results (zip)'</b> allow you to download an automated assessment ",
                             "report as a pdf document ",
                             "and a zip archive with all results, respectively. ",
                             "The report is also automatically ",
                             "uploaded to your private workspace.</p>"
                             )),

                bsModal("info_yearsel_lbi", "Selected years", ns("infoYearSel"),
                        size = "large",
                        HTML("<p>Select all or a range of years in the uploaded data set to be ",
                             "included in the analysis. ",
                             "<br><br> The indicators are estimated for every year separately.</p>")),

                bsModal("info_binsize_lbi", "Bin size", ns("infoBS"),
                        size = "large",
                        HTML(paste0("<p>The bin size corresponds to the length interval over ",
                                    "which the length frequency ",
                                    "data are aggregated, for example 2 cm. <br><br> Ideally, ",
                                    "the bin size is as small as ",
                                    "possible, but large enough to reduce noise. By default the ",
                                    "bin size is set dependent ",
                                    "on the maximum length: ", withMathJax("\\(0.23 L_{max}^{0.6}\\)"),
                                    " (Wang et al. 2020).</p>"))),

                bsModal("info_linf_lbi", withMathJax("\\L_{\\infty}\\)"), ns("infoLinf"),
                        size = "large",
                        HTML(paste0("<p>The asymptotic length (",
                                    withMathJax("\\L_{\\infty}\\)"),
                                    ") refers to the asymptote of the von Bertlanffy growth function, ",
                                    "thus describing the theoretical maximum length at which growth is ",
                                    "zero. (Refer to the 'Von Bertalanffy growth function' ",
                                    "tab for more information). This parameter is required for the ",
                                    "estimation of the indicators and ",
                                    " can be estimated from monthly length-frequency data with TropFishR ",
                                    "(see side menu on the left) or extracted from literature (e.g. ",
                                    "<a href='http://www.fishbase.org/search.php' ",
                                    "target='blank_'> FishBase</a> or <a href='https://www.sealifebase.ca' ",
                                    "target='blank_'> SeaLifeBase</a> for invertebrates",
                                    ").</p>"))),


                bsModal("info_lmat_lbi", withMathJax("\\L_{m50}\\)"), ns("infoLm50"),
                        size = "large",
                        HTML(paste0("<p>The length at 50% maturity (",withMathJax("\\L_{m50}\\)"),") ",
                                    "is an important life history parameter and defines the length ",
                                    "at which 50% of the population reached maturity. This parameter ",
                                    "has to be estimated externally (outside SMT) or extracted from ",
                                    "literature (e.g. ",
                                    "<a href='http://www.fishbase.org/search.php' ",
                                    "target='blank_'> FishBase</a> or <a href='https://www.sealifebase.ca' ",
                                    "target='blank_'> SeaLifeBase</a> for invertebrates",
                                    ").</p>"))),


                bsModal("info_mk_lbi", "M/K", ns("infoMK"),
                        size = "large",
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
                                    "</p>"))),

                bsModal("info_m_lbi", "M", ns("infoM"),
                        size = "large",
                        HTML(paste0("<p>M is the ",
                                    "instantaneous natural ",
                                    "mortality rate and can be estimated from monthly length-frequency data with ",
                                    "TropFishR (see side menu on the left) or extracted from literature (",
                                    "e.g. ",
                                    "<a href='http://www.fishbase.org/search.php' ",
                                    "target='blank_'> FishBase</a> or <a href='https://www.sealifebase.ca' ",
                                    "target='blank_'> SeaLifeBase</a> for invertebrates",
                                    ").",
                                    "</p>"))),

                bsModal("info_k_lbi", "K", ns("infoK"),
                        size = "large",
                        HTML(paste0("<p>K is the von Bertalanffy growth constant and can be estimated",
                                    "from monthly length-frequency data with ",
                                    "TropFishR (see side menu on the left) or extracted from literature (",
                                    "e.g. ",
                                    "<a href='http://www.fishbase.org/search.php' ",
                                    "target='blank_'> FishBase</a> or <a href='https://www.sealifebase.ca' ",
                                    "target='blank_'> SeaLifeBase</a> for invertebrates",
                                    ").",
                                    "</p>"))),


                bsModal("info_lengthweight_lbi", "Length-weight relationship", ns("infoLengthWeight"),
                        size = "large",
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
                                    "target='blank_'> SeaLifeBase</a> for invertebrates. This information is optional ",
                                    "and if missing ",
                                    "the ",withMathJax("\\(L_{maxy}\\)")," indicator is not calculated.",
                                    "</p>"))
                        ),


                ## Input - Data upload
                ## -------------------------------
                box(id = "box_datupload",
                    title = p("Data Upload",
                              actionButton(ns("lbiDataConsiderations2"),
                                           tags$i(class = "fas fa-info",
                                                  style="font-size: 8px"),
                                           class="infoBubbleButton")),
                    width = NULL,
                    collapsible = FALSE,
                    solidHeader = TRUE,
                    class = "collapsed-box",

                    box(width = 6,
                        fileInput(ns("fileLBI"), "Choose Input CSV File",
                                  accept = c(
                                      "text/csv",
                                      "text/comma-separated-values,text/plain",
                                      ".csv")
                                  )
                    ),
                    box(width = 3,
                        selectInput(ns("lbiDateFormat"),
                                    "Choose CSV date format",
                                    choices = c("Automatic guess" = "auto",
                                                "Year Month Day" = "ymd",
                                                "Year Day Month" = "ydm",
                                                "Day Month Year" = "dmy",
                                                "Month Day Year" = "mdy" ))
                    ),
                    box(width = 3,
                        selectInput(ns("LBI_lengthUnit"),
                                    "Choose length unit",
                                    choices = c("cm", "mm", "in"))
                    )
                    ),

                ## Input - Settings
                ## -------------------------------
                br(),

                box(id = "box_settings",
                    title = p("Settings",
                              actionButton(ns("lbiMethodConsiderations2"),
                                           tags$i(class = "fas fa-info",
                                                  style="font-size: 8px"),
                                           class="infoBubbleButton")),
                    width = NULL,
                    collapsible = FALSE, ## careful: if made collapsible the renderUi does not update! see: https://github.com/rstudio/shinydashboard/issues/234
                    solidHeader = TRUE,
                    class = "collapsed-box",


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
                                       HTML("<b>Linf</b>")
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
                                       HTML("<b>Lm50</b>")
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
                            uiOutput(ns("downloadReport_lbi"))
                            ),
                        div(style = "display: inline-block; vertical-align:center; margin-left: 20px;",
                            uiOutput(ns("downloadzip_lbi"))
                            ),
                        div(style = "display: inline-block; vertical-align:center; margin-left: 20px;",
                            uiOutput(ns("lbiVREUpload"))
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
                              actionButton(ns("lbiResultConsiderations2"),
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
