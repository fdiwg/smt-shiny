tabLBSPR <- function(id) {
    ns <- NS(id)
    tabItem("lbsprWidget",

            htmlOutput(ns("lbsprTitle")),

            fluidRow(
                div(style = "display: inline-block; vertical-align:center; margin-left: 15px;",
                    "More information about "
                    ),
                div(style = "display: inline-block; vertical-align:center; margin-left: 15px;",
                    actionButton(ns("lbsprWorkflowConsiderations"), "Workflow",
                                 class="topLevelInformationButton")
                    ),
                div(style = "display: inline-block; vertical-align:center; margin-left: 5px;",
                    actionButton(ns("lbsprDataConsiderations"), "Data",
                                 class="topLevelInformationButton")
                    ),
                div(style = "display: inline-block; vertical-align:center; margin-left: 5px;",
                    actionButton(ns("lbsprMethodConsiderations"), "Methods",
                                 class="topLevelInformationButton")
                    ),
                div(style = "display: inline-block; vertical-align:center; margin-left: 5px;",
                    actionButton(ns("lbsprResultConsiderations"), "Results",
                                 class="topLevelInformationButton")
                    )
            ),



            fluidRow(

                ## Notifications in middle of screen (if decided to keep -> move to custom.css?)
                tags$head(tags$style(HTML(".shiny-notification { position:fixed; top: calc(50%); left: calc(50%); width: 250px; height: 80px;}"))),

                ## Information tabs
                ## -------------------------------
                bsModal("modalWorkflowLBSPR", "Workflow Considerations - LBSPR",
                        ns("lbsprWorkflowConsiderations"),
                        size = "large",
                        htmlOutput(ns("lbsprWorkflowConsiderationsText"))),

                bsModal("modalDataLBSPR", "Data Loading and Formatting Considerations - LBSPR",
                        ns("lbsprDataConsiderations"),
                        size = "large",
                        htmlOutput(ns("lbsprDataConsiderationsText"))),

                bsModal("modalData2LBSPR", "Data Considerations - LBSPR",
                        ns("lbsprDataConsiderations2"),
                        size = "large",
                        htmlOutput(ns("lbsprDataConsiderationsText2"))),

                bsModal("modalMethodLBSPR", "Methodological Considerations - LBSPR",
                        ns("lbsprMethodConsiderations"),
                        size = "large",
                        htmlOutput(ns("lbsprMethodConsiderationsText"))),

                bsModal("modalMethod2LBSPR", "Methodological Considerations - LBSPR",
                        ns("lbsprMethodConsiderations2"),
                        size = "large",
                        htmlOutput(ns("lbsprMethodConsiderationsText2"))),

                bsModal("modalResultsLBSPR", "Results Considerations - LBSPR",
                        ns("lbsprResultConsiderations"),
                        size = "large",
                        htmlOutput(ns("lbsprResultConsiderationsText"))),

                bsModal("modalResults2LBSPR", "Results Considerations - LBSPR",
                        ns("lbsprResultConsiderations2"),
                        size = "large",
                        htmlOutput(ns("lbsprResultConsiderationsText2"))),

                bsModal("info_assessment_lbspr", "Assessment, Reset & Report", ns("infoAssessment"),
                        size = "large",
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
                             )),

                bsModal("info_yearsel_lbspr", "Selected years", ns("infoYearSel"),
                        size = "large",
                        HTML("<p>Select all or a range of years in the uploaded data set to be ",
                             "included in the analysis. ",
                             "<br><br> The method estimates SPR and fishing mortality for every year.</p>")),

                bsModal("info_binsize_lbspr", "Bin size", ns("infoBS"),
                        size = "large",
                        HTML(paste0("<p>The bin size corresponds to the length interval over ",
                                    "which the length frequency ",
                                    "data are aggregated, for example 2 cm. <br><br> Ideally, ",
                                    "the bin size is as small as ",
                                    "possible, but large enough to reduce noise. By default the ",
                                    "bin size is set dependent ",
                                    "on the maximum length: ", withMathJax("\\(0.23 L_{max}^{0.6}\\)"),
                                    " (Wang et al. 2020).</p>"))),

                bsModal("info_linf_lbspr", withMathJax("\\L_{\\infty}\\)"), ns("infoLinf"),
                        size = "large",
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
                                    ").</p>"))),


                bsModal("info_mk_lbspr", "M/K", ns("infoMK"),
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

                bsModal("info_m_lbspr", "M", ns("infoM"),
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

                bsModal("info_k_lbspr", "K", ns("infoK"),
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


                bsModal("info_lengthweight_lbspr", "Length-weight relationship", ns("infoLengthWeight"),
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
                                    "target='blank_'> SeaLifeBase</a>  for invertebrates. This information is optional ",
                                    "and if missing ",
                                    "the ",withMathJax("\\(L_{maxy}\\)")," indicator is not calculated.",
                                    "</p>"))
                        ),

                bsModal("info_maturity_lbspr", "Maturity information", ns("infoMaturity"),
                        size = "large",
                        HTML(paste0("<p>The length at 50% maturity (",withMathJax("\\L_{m50}\\)"),") ",
                                    "is an important life history parameter and defines the length ",
                                    "at which 50% of the population reached maturity. This parameter ",
                                    "has to be estimated externally (outside SMT) or extracted from ",
                                    "literature (e.g. ",
                                    "<a href='http://www.fishbase.org/search.php' ",
                                    "target='blank_'> FishBase</a> or <a href='https://www.sealifebase.ca' ",
                                    "target='blank_'> SeaLifeBase</a> for invertebrates",
                                    ").</p>"))
                        ),

                bsModal("info_refs_lbspr", "SPR reference points", ns("infoRefs"),
                        size = "large",
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
                                    " </p>"))
                        ),


                ## Input - Data upload
                ## -------------------------------
                box(id = "box_datupload",
                    title = p("Data Upload",
                              actionButton(ns("lbsprDataConsiderations2"),
                                           tags$i(class = "fas fa-info",
                                                  style="font-size: 8px"),
                                           class="infoBubbleButton")),
                    width = NULL,
                    collapsible = FALSE,
                    solidHeader = TRUE,
                    class = "collapsed-box",

                    box(width = 6,
                        fileInput(ns("fileLBSPR"), "Choose Input CSV File",
                                  accept = c(
                                      "text/csv",
                                      "text/comma-separated-values,text/plain",
                                      ".csv")
                                  )
                    ),
                    box(width = 3,
                        selectInput(ns("lbsprDateFormat"),
                                    "Choose CSV date format",
                                    choices = c("Automatic guess" = "auto",
                                                "Year Month Day" = "ymd",
                                                "Year Day Month" = "ydm",
                                                "Day Month Year" = "dmy",
                                                "Month Day Year" = "mdy" ))
                    ),
                    box(width = 3,
                        selectInput(ns("lbspr_lengthUnit"),
                                    "Choose length unit",
                                    choices = c("cm", "mm", "in"))
                    )
                    ),

                ## Input - Settings
                ## -------------------------------
                br(),

                box(id = "box_settings",
                    title = p("Data exploration & Settings",
                              actionButton(ns("lbsprMethodConsiderations2"),
                                           tags$i(class = "fas fa-info",
                                                  style="font-size: 8px"),
                                           class="infoBubbleButton")),
                    width = NULL,
                    collapsible = FALSE, ## careful: if made collapsible the renderUi does not update! see: https://github.com/rstudio/shinydashboard/issues/234
                    solidHeader = TRUE,
                    class = "collapsed-box",

                    tabBox(
                        title = "",
                        width = NULL,
                        height = "810px",
                        side="left",
                        selected = "1. Data and settings",
                        id = "settings",

                        tabPanel("1. Data and settings",

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

                        tabPanel("2. Summary & Diagnostics",

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
                            uiOutput(ns("downloadReport_lbspr"))
                            ),
                        div(style = "display: inline-block; vertical-align:center; margin-left: 20px;",
                            uiOutput(ns("downloadzip_lbspr"))
                            ),
                        div(style = "display: inline-block; vertical-align:center; margin-left: 20px;",
                            uiOutput(ns("lbsprVREUpload"))
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
                              actionButton(ns("lbsprResultConsiderations2"),
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
