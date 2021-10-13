tabElefanGa <- function(id) {
    ns <- NS(id)
    tabItem("ElefanGaWidget",

            htmlOutput(ns("elefanGaTitle")),

            htmlOutput("tropFishRLibVersion1", class="subTitle"),

            fluidRow(
                div(style = "display: inline-block; vertical-align:center; margin-left: 15px;",
                    "More information about "
                    ),
                div(style = "display: inline-block; vertical-align:center; margin-left: 15px;",
                    actionButton(ns("elefanGAWorkflowConsiderations"), "Workflow",
                                 class="topLevelInformationButton")
                ),
                div(style = "display: inline-block; vertical-align:center; margin-left: 5px;",
                    actionButton(ns("elefanGADataConsiderations"), "Data",
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


            fluidRow(

                ## Notifications in middle of screen (if decided to keep -> move to custom.css?)
                tags$head(tags$style(HTML(".shiny-notification { position:fixed; top: calc(50%); left: calc(50%); width: 250px; height: 80px;}"))),

                ## Information tabs
                ## -------------------------------
                bsModal("modalWorkflow", "Workflow Considerations - TropFishR",
                        ns("elefanGAWorkflowConsiderations"),
                        size = "large",
                        htmlOutput(ns("elefanGAWorkflowConsiderationsText"))),

                bsModal("modalExampleGA", "Data Loading and Formatting Considerations - TropFishR",
                        ns("elefanGADataConsiderations"),
                        size = "large",
                        htmlOutput(ns("elefanGADataConsiderationsText"))),

                bsModal("modalExampleGA2", "Data Considerations - TropFishR",
                        ns("dataConsiderations2"),
                        size = "large",
                        htmlOutput(ns("elefanGADataConsiderationsText2"))),

                bsModal("modalMethodGA", "Methodological Considerations - TropFishR",
                        ns("methodConsiderations"),
                        size = "large",
                        htmlOutput(ns("elefanGAmethodConsiderationsText"))),

                bsModal("modalMethodGA2", "Methodological Considerations - TropFishR",
                        ns("methodConsiderations2"),
                        size = "large",
                        htmlOutput(ns("elefanGAmethodConsiderationsText2"))),

                bsModal("modalResultsGA", "Results Considerations - TropFishR",
                        ns("resultConsiderations"),
                        size = "large",
                        htmlOutput(ns("elefanGAresultConsiderationsText"))),

                bsModal("modalResultsGA2", "Results Considerations - TropFishR",
                        ns("resultConsiderations2"),
                        size = "large",
                        htmlOutput(ns("elefanGAresultConsiderationsText2"))),

                bsModal("info_yearsel_GA", "Selected years", ns("infoYearSel"),
                        size = "large",
                        HTML("<p>Select all or a range of years in the uploaded data set to be included in the analysis. <br><br> In theory, the longer the period covered by the data set, the better. However, data sets covering a long period with monthly aggregated data, might lead to a long run time and make the assumption that the growth parameters did not change over this period. In this case, you could consider to choose only the most recent years or choosing a quarterly aggregation of the data (see info button to 'Aggregate data by').</p>")),

                bsModal("info_agg", "Data aggregation", ns("infoAGG"),
                        size = "large",
                        HTML("<p>Define whether the aggregation of the dataset should be kept ('none' is the default), or if the dataset should be aggregated by 'month', 'quarter', and 'year'. <br><br> Note that if 'month' is chosen, the data is assigned to the middle of respective sampling times (i.e. day 15 of each month). <br><br> In theory, the longer the period covered by the data set, the better. However, data sets covering a long period with monthly aggregated data, might lead to a long run time and might make the assumption that the growth parameters did not change over this period. Choosing only the most recent years or changing the aggregation 'quarter' and 'year' can be helpful to decrease computation time. <br><br> Note that a coarser (i.e. quarterly or yearly) aggregation reduces the information content of the data set. </p>")),

                bsModal("info_binsize", "Bin size", ns("infoBS"),
                        size = "large",
                        HTML(paste0("<p>The bin size corresponds to the length interval over which the length frequency data are aggregated, for example 2 cm. <br><br> The combination of bin size and moving average (MA) critically affects the separation of peaks (i.e. potential cohorts) in the dataset and thus the estimation of growth parameters by ELEFAN. The bin size should be defined before defining the MA value. Ideally, the bin size is as small as possible, but large enough so that adjacent bins with high and low counts correspond to potential cohorts rather than noise. Wang et al. (2020) recommended defining the bin size dependent on the maximum length by ",withMathJax("\\(0.23 L_{max}^{0.6}\\)")," (default).</p>"))),

                bsModal("info_ma", "Moving average (MA)", ns("infoMA"),
                        size = "large",
                        HTML(paste0("<p>The moving average (", withMathJax("\\(MA\\)"), ") is a statistical operation that calculates a series of averages of different subsets of the full data set and is used in the restructuring of the length frequency data. The value indicates the number of length classes to be used in the calculation of the averages and must be a positive odd number (e.g. 5 or 7). <br><br>The combination of bin size and MA critically affects the separation of peaks (i.e. potential cohorts) in the dataset and, thus, the estimation of growth parameters by ELEFAN. Ideally, the MA value should be defined after defining the bin size and should lead to visually distinct peaks, particularly among small length classes. One option for the MA value is to set it equal to the number of length classes (bins) that potentially correspond to the youngest cohort.</p>"))),

                ## bsModal("info_pg", "Plus group", ns("infoPG"),
                ##         size = "large",
                ##         "Allows to lump together all catches larger than given size (so called 'plus group'). Note: This can greatly affect the estimation of ",withMathJax("\\(L_\\infty\\)")," with ELEFAN. Default '0' implies that no plus group is used."),

                bsModal("info_at", "Additional squareroot transformation", ns("infoAT"),
                        size = "large",
                        HTML(paste0("The additional squareroot transformation reduces the influence of low frequency values (Brey et al. 1988) and is defined by ",
                                    withMathJax("\\(F_i = F_i / sqrt(1+2/F_i)\\)"),", where ",withMathJax("\\(F_i\\)")," is the frequency after the application of the moving average (MA) for length class i. This additional transformation might be useful if length frequency data includes many low values (Brey et al. 1988)." ))),

                bsModal("info_searchspace", "Search space for growth parameters", ns("info_searchSpace"),
                        size = "large",
                        HTML(paste0("<p>ELEFAN uses the Genetic Algorithm (GA) to find the set of von Bertalanffy growth (VBG) growth parameters (",withMathJax("\\(L_\\infty\\)"),", ",withMathJax("\\(K\\)"),",",withMathJax("\\(t_a\\)"),") which describe the growth curve that best fits the uploaded data set. In this tab, you can define the search space for each growth parameter. Note that the algorithm only searches within the defined parameter range. Thus, it is recommended to define a wider, rather than narrower, range. <br><br> By default, a reasonable range is defined for all parameters based on the input data uploaded. </p>"))),

                bsModal("info_linf", withMathJax("\\(L_\\infty\\)"), ns("infolinf"),
                        size = "large",
                        p(withMathJax("\\(L_\\infty\\)")," defines the asymptotic length of the von Bertalanffy growth (VBG) function. The default range is dependent upon the uploaded data set and defined as +/- 20% around the rough estimate of ",withMathJax("\\(L_\\infty = L_\\max/0.95\\)"),". Note that the maximum possible range is limited to +/- 75% of this estimate.")),

                bsModal("info_k", withMathJax("\\(K\\)"), ns("infok"),
                        size = "large",
                        HTML(paste0("<p>The growth coefficient (", withMathJax("\\(K\\)"), ") of the von Bertalanffy growth (VBG) function determines the slope of the growth curve: a low ",withMathJax("\\(K\\)")," defines a slow-growing species and a high ",withMathJax("\\(K\\)")," defines a fast growing species. <br><br>If no prior knowledge about this life-history parameter is available, it is recommended to define a wide search space from 0.01 to 2-4. If prior information is available, a narrower range can be considered and would reduce the run time of the assessment.</p>"))),

                bsModal("info_tanchor", withMathJax("\\(t_{a}\\)"), ns("infotanchor"),
                        size = "large",
                        HTML(paste0(withMathJax("\\(t_{a}\\)"), " is the time point anchoring the growth curves in the year-length coordinate system, corresponding to the peak spawning month. In other words, it corresponds to the fraction of the year where yearly repeating growth curves cross length equal to zero; for example a value of 0.25 refers to April 1st of any year. Values for this field are between 0 and 1."))),

                bsModal("info_season", "Seasonal model", ns("infoSeason"),
                        size = "large",
                        p("This checkbox allows to use the seasonal model (or seasonalised von Bertalanffy growth (VBG) curve) which enables the calculation of the seasonal growth parameters (",
                          withMathJax("\\(C\\)"), " and ", withMathJax("\\(t_{s}\\)"), "). The use of the seasonalised VBG is recommend if strong seasonality in the growth of the fish is expected, for instance, due to strong seasonal temperature differences in temperate regions. Note, that the estimation of the two additional parameters of the seasonalised VBG might increase the data requirements and, thus, uncertainty if the cohort signals in the data are poor." )),

                bsModal("info_C", withMathJax("\\(C\\)"), ns("infoC"),
                        size = "large",
                        p("The amplitude of the oscillation (", withMathJax("\\(C\\)"), ") of the seasonalised von Bertalanffy growth (VBG) curve. The higher the value of C, the more pronounced the seasonal oscillations are. C = 0 implies that there is no seasonality in the growth rate. If C = 1, the growth rate becomes zero at the winter point (",withMathJax("\\(WP = 0.5 - t_s\\)"),"). Values of C>1 would imply that the individuals shrink in length. Possible values for C are between 0 and 1, which is also equal to the default search space for this parameter. Generally, it is not recommended to decrease the search space for this parameter.")),

                bsModal("info_ts", withMathJax("\\(t_\\s\\)"), ns("infots"),
                        size = "large",
                        p("The summer point (", withMathJax("\\(t_{s}\\)"), ") of the seasonalised von Bertalanffy growth (VBG) curve defines the point in time where the seasonally varying growth rate is the largest represented by the fraction of the calendar year, e.g. 0.25 corresponds to April 1st. Values for this field are between 0 and 1, which is also equal to the default search space for this parameter. Generally, it is not recommended to decrease the search space for this parameter.")),

                bsModal("info_ga", "ELEFAN's genetic algorithm", ns("info_GA"),
                        size = "large",
                        HTML(paste0("<p>Genetic algorithms (GAs) are stochastic search algorithms inspired by the basic principles of biological evolution and natural selection.  GAs simulate the evolution of living organisms, where the fittest individuals dominate over the weaker ones, by mimicking the biological mechanisms of evolution, such as selection, crossover and mutation.<br><br> Changing default parameters can have a substantial effect on the optimization process and, thus, on estimated growth parameters. Therefore, please apply caution when changing these parameters. In fact, values should only be increased from the default, though please note that this will increase the run time of the assessment.</p>"))),

                bsModal("info_popsize", "Population size", ns("infoPopSize"),
                        size = "large",
                        "Size of the inital population for the genetic algorithm. In theory, the higher the population size the better, however, large population size increase virtual memory demands substantially. Minimum is 50 and maximum 1000."),

                bsModal("info_maxiter", "Maximum number of iterations", ns("infoMaxIter"),
                        size = "large",
                        "Maximum number of iterations to run before the GA search is halted. Note that this parameter might affect the run time significantly. Minimum is 20 and maximum 1000."),

                bsModal("info_MaxRuns", "Maximum number of runs", ns("infoMaxRuns"),
                        size = "large",
                        p("Number of consecutive generations without any improvement in the best fitness value before the GA is stopped. Note that this parameter might affect the run time significantly. Minimum is 10 and maximum 1000.")),

                bsModal("info_pmut", "Probability of mutation", ns("infoPmut"),
                        size = "large",
                        "The probability of mutation in a parent chromosome is used to maintain genetic diversity from one generation to the next. Usually mutation occurs with a small probability and is set to 0.2 by default. If it is set too high, the search will turn into a primitive random search. The probability has to be between 0 and 1."),

                bsModal("info_pcross", "Probability of crossover", ns("infoPcross"),
                        size = "large",
                        "The probability of crossover between pairs of chromosomes is used to combine the genetic information of two parents to generate offspring. Typically this is a large value and is set to 0.8 by default. The probability has to be between 0 and 1."),

                bsModal("info_elite", "Degree of elitism", ns("infoElite"),
                        size = "large",
                        "Number of individuals of the parent generation with the best fitness values that survive to the next generation without changes. By default, the top 5% of individuals will survive at each iteration. Minimum is 1 and maximum 100."),


                ## bsModal("info_yearselcc", "Selected years", ns("infoYearSelCC"),
                ##         size = "large",
                ##         "Select all or a range of years covered by uploaded data set for the estimation of total and fishing mortality using the catch curve. If multiple years are selected the total and fishing mortality rates correspond to the average rates of selected years."),

                bsModal("info_pred", "Prediction range", ns("infoPred"),
                        size = "large",
                        HTML("<p>The prediction range determines the fishing mortality rates and length at 50% selectivity (L50) values which are used in the yield per recruit model. The model estimates yield per recruit (YPR), biomass per recruit (BPR), and spawning potential ratio (SPR; if maturity parameters are provided) for each combination of fishing mortality and L50 value. Thus, the prediction ranges (F and L50) affect the axes of Figures 6 and 7. <br> <br> The range for fishing mortality can be defined by the number of 'Steps' between the minimum ('Min') and maximum ('Max') mortality rate. <br> <br> If the selectivity is estimated (default), only the number of 'Steps' can be changed for the L50 range. If the selectivity parameters are provided (e.g. L50 and L75), the minimum ('Min') and maximum ('Max') of the L50 range can be changed.</p>")),

                bsModal("info_lengthweight", "Length-weight relationship", ns("infoLengthWeight"),
                        size = "large",
                        HTML(paste0("<p>The estimation of the yield and biomass per recruit requires information about the average weight per length class, which can be estimated with the length-weight relationship. A common assumption is the allometric relationship ",withMathJax("\\(W = a L^{b}\\)"),", with the constant <i>a</i> in ",
                                    withMathJax("\\( g/cm^{3}\\)"), " and the exponent ",
                                    withMathJax("\\( b\\)")," being unitless. <br><br>Ideally, the parameters are estimated based on length and weight measurements of the stock under study. Alternatively, information about the length-weight relationship of the species under study can be found on <a href='http://www.fishbase.org/search.php' target='blank_'> FishBase</a> or <a href='https://www.sealifebase.ca' target='blank_'> SeaLifeBase</a>  for invertebrates. By default the parameters are set to ",
                                    withMathJax("\\(a = 0.01 g/cm^{3}\\)")," and ",
                                    withMathJax("\\(b = 3\\)"),".",
                                    "</p>"))
                        ),

                bsModal("info_adjdata", "Adjust data (other settings)", ns("infoAdjData"),
                        size = "large",
                        HTML(paste0("<p> Select the year(s) for the estimation of the stock status. The mortality rates estimated by the catch curve correspond to all years selected. If several years are selected, the samples for selected years are combined and the estimated rates correspond to the average values over all years selected. </p>"))),

                bsModal("info_mat", "Maturity (optional)", ns("infoMat"),
                        size = "large",
                        HTML("<p>If available, maturity information about your species can be provided and allows estimation of the current Spawning Potential Ratio (SPR) and SPR-related reference points. The model assumes a logistic maturity ogive. <br><br>Maturity information can be provided (i) in terms of the length at 50% and 75% maturity ('Define Lm50 & Lm75'), (ii) in terms of the length at 50% maturity and the maturation width, which is the difference between the length at 75% maturity and 25% maturity ('Define Lm50 & (Lm75-Lm25)'), or (iii) in terms of two specified lengths (LmX1 and LmX2) at specified probabilities of maturity (mX1 and mX2) ('Other'). <br><br>Ideally, maturity information is collected directly from the stock under study e.g. by determining the maturation states of the gonads. Alternatively, you may find maturity information about your species on <a href='http://www.fishbase.org/search.php' target='blank_'> FishBase</a> or <a href='https://www.sealifebase.ca' target='blank_'> SeaLifeBase</a>  for invertebrates.</p>")
                        ),

                bsModal("info_select", "Gear selectivity", ns("infoSelect"),
                        size = "large",
                        HTML("<p>The specifics of how fish are caught by fisheries and thus the probability of capture for fish of various length classes are dependent on the fishing gear, which is referred to as gear selectivity. Find more information about some examples of fishing gear selectivity <a href='http://www.fao.org/3/X7788E/X7788E00.htm' target='blank_'> here </a>. <br><br>TropFishR assumes a logistic gear selection ogive and allows the estimation of gear selectivity by means of the catch curve ('Estimate' is the default).  Alternatively, the gear selectivity can be defined (i) in terms of the length at 50% and 75% selection ('Define L50 & L75'), (ii) in terms of the length at 50% selection and the selection width, which is the difference between the length at 75% selection and 25% selection ('Define L50 & (L75-L25)'), or (iii) in terms of two specified lengths (LX1 and LX2) at specified probabilities of selection (X1 and X2) ('Other').  <br> <br> Note that estimated and specified selectivity corresponds to a logistic curve (trawl-like selectivity).</p>")
                        ),

                bsModal("info_natm", "Natural mortality", ns("infoNatM"),
                        size = "large",
                        HTML("<p>The natural mortality rate (M) is required to estimate the fishing mortality (F) from the total mortality (Z) estimated by the catch curve (F = Z - M). The natural mortality is estimated by an empirical formula based on estimated growth parameters. The options are: <br> - Then's growth formula (<a href='https://doi.org/10.1093/icesjms/fsu136' target='_blank'>Then et al. 2015</a>), <br> - Pauly's growth and temperature formula (<a href='https://doi.org/10.1093/icesjms/39.2.175' target='_blank'>Pauly 1980</a>), and <br> - Then's maximum age formula (<a href='https://doi.org/10.1093/icesjms/fsu136' target='_blank'>Then et al. 2015</a>); <br><br> While the first option does not require any additional information, the second requires the average annual sea surface temperature (SST) in degrees Celsius and allows corrections for schooling fish (multiplication by 0.8). The third option requires an estimate of the maximum age of the fish.<br><br>Please see the Natural Mortality estimator page in the Supporting Tools menu for more information. </p>")
                        ),


                bsModal("info_assessmentGA", "Check, Assessment, Reset & Report", ns("infoAssessment"),

                        size = "large",
                        HTML("<p>It is recommended to run a quick check by pressing <b>'Run Check'</b> before running the main assessment.
                             While the main assessment can take a few minutes to run, depending on the settings of the ELEFAN optimation routine and the sample size of the dataset,
                             the check is performed in a matter of seconds and can identify issues in the data or settings. The check does not produce results (figures or tables),
                             but a notification in the middle of the screen will inform you whether the check was successful. <br> <br>

                             <b>'Run Assessment'</b> performs the main assessment and should yield plenty of figures and tables in the result section upon successful completion.
                             The run may take several minutes and depends on the size of the dataset, aggregation, bin size, and parameter search space. Run time with the sample
                             dataset and default settings is 2-4 minutes. <br> <br> <b>'Reset'</b> removes all results, the uploaded dataset, and resets all settings to default values. <br> <br>

                             After successful completion of the main assessment, an additional button <b>'Download Report'</b> allows you to download a pdf document with all results.
                             This report is also automatically uploaded to your private workspace.</p>"
                             )),



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

                    box(
                        fileInput(ns("fileGa"), "Choose Input CSV File",
                                  accept = c(
                                      "text/csv",
                                      "text/comma-separated-values,text/plain",
                                      ".csv")
                                  )
                    ),
                    box(
                        selectInput(ns("elefanGaDateFormat"),
                                    "Choose CSV date format",
                                    choices = c("Automatic guess" = "auto",
                                                "Year Month Day" = "ymd",
                                                "Year Day Month" = "ydm",
                                                "Day Month Year" = "dmy",
                                                "Month Day Year" = "mdy" ))
                    )
                    ),



                ## Input - Settings
                ## -------------------------------
                br(),

                box(id = "box_settings",
                    title = p("Assessment Settings",
                              actionButton(ns("methodConsiderations2"),
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
                        height = "730px",
                        side="left",
                        selected = "1. Data",
                        id = "settings",


                        tabPanel("1. Data",

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
                                                  5, min = 3, max = 101, step=2,
                                                  width ='100%'),

                                     br(),

                                     checkboxInput(ns("ELEFAN_GA_addlsqrt"),
                                                   p("Additional squareroot transformation?",
                                                     actionButton(ns("infoAT"),
                                                                  tags$i(class = "fas fa-info",
                                                                         style="font-size: 8px"),
                                                                  class="infoBubbleButton")),
                                                   FALSE)
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



                        tabPanel("2. ELEFAN",
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

                        tabPanel("3. Biological parameters",

                                 fluidRow(

                                     box(
                                         title = p(HTML(paste0("Length-weight relationship (",
                                                               withMathJax("\\(W = a \ L^{b}\\)"),")")),
                                                   actionButton(ns("infoLengthWeight"),
                                                                tags$i(class = "fas fa-info",
                                                                       style="font-size: 8px"),
                                                                class="infoBubbleButton")),

                                         br(),
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
                                         br(),
                                         fluidRow(
                                             column(5,
                                                    selectInput(ns("natM"),
                                                                "Method:",
                                                                choices = c("Then's growth formula",
                                                                            "Pauly's growth & temp. formula",
                                                                            "Then's max. age formula"),
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
                                         br(),
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


                        tabPanel("4. Other settings",

                                 fluidRow(

                                     box(title = p("Adjust length data",
                                                   actionButton(ns("infoAdjData"),
                                                                tags$i(class = "fas fa-info",
                                                                       style="font-size: 8px"),
                                                                class="infoBubbleButton")),
                                         width = 2,
                                         height = "200px",
                                         br(),
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
                                         br(),
                                         fluidRow(
                                             column(3,
                                                    selectInput(ns("select"),
                                                                p("Selectivity"),
                                                                choices = c("Estimate",
                                                                            "Define L50 & L75",
                                                                            "Define L50 & (L75-L25)",
                                                                            "Other"),
                                                                selected = "Estimate",
                                                                width = "100%")
                                                    ),
                                             column(1),
                                             column(4,
                                                    div(
                                                        id ="ui_l50",
                                                        numericInput(ns("l50_user"),
                                                                     p(withMathJax("\\(L_{50}\\)")," (user defined)"),
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
                                                                       " (",withMathJax("\\(L_{X1}\\)"),")"),
                                                                     value = NULL, min = 0, step = 1,
                                                                     width = "100%")
                                                    )
                                                    )
                                                    ),
                                             column(4,
                                                    div(
                                                        id ="ui_l75",
                                                        numericInput(ns("l75_user"),
                                                                     p(withMathJax("\\(L_{75}\\)")," (user defined)"),
                                                                     value = NULL, min = 0, step = 1,
                                                                     width = "80%")
                                                    ),
                                                    div(
                                                        id ="ui_wqs",
                                                        numericInput(ns("wqs_user"),
                                                                     p("Width (",withMathJax("\\(L_{75}-L_{25}\\)"),"; user defined)"),
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
                                                                       " (",withMathJax("\\(L_{X2}\\)"),")"),
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
                                     br(),
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
                                 )


                    )
                    ),



                ## Action buttons
                ## -------------------------------
                br(),

                box(title = p("Run Assessment & Download Report",
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
                            uiOutput(ns("downloadReport_ga"))
                            ),
                        div(style = "display: inline-block; vertical-align:center; margin-left: 20px;",
                            uiOutput(ns("ElefanGaVREUpload"))
                            ),
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
                    height = "2600px",
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
                                     div(style = "margin-top:0px; margin-left: 10px",
                                         htmlOutput(ns("title_growthCurves"))
                                         ),
                                     style = "margin-left: 10%;"
                                 )
                        ),
                        column(
                            5,
                            br(),
                            tags$div(
                                     div(style = "margin-bottom:0px; margin-left: 3px",
                                         htmlOutput(ns("title_table_growth"))
                                         ),
                                     tableOutput(ns("table_growth")),
                                     style = "margin-left: 20%;"
                                 ),
                            br(),
                            tags$div(
                                     plotOutput(ns("plot_elefanFit"), width = "80%",
                                                height = "400px"),
                                     div(style = "margin-top:0px; margin-left: 3px",
                                         htmlOutput(ns("title_elefanFit"))
                                         ),
                                     style = "margin-left: 0%;"
                                 )
                        )

                    ),

                    br(),br(),

                    fluidRow (
                        column(
                            6,
                            tags$div(
                                     plotOutput(ns("plot_catchCurve"), width = "80%",
                                                height = "400px"),
                                     div(style = "margin-top:0px; margin-left: 10px",
                                         htmlOutput(ns("title_catchCurve"))
                                         ),
                                     style = "margin-left: 10%;"
                                 )
                        ),
                        column(
                            6,
                            tags$div(
                                     plotOutput(ns("plot_select"), width = "80%",
                                                height = "400px"),
                                     div(style = "margin-top:0px; margin-left: 10px",
                                         htmlOutput(ns("title_select"))
                                         ),
                                     style = "margin-left: 10%;"
                                 )
                        )

                    ),

                    br(), br(),

                    fluidRow(
                        column(
                            4,
                            tags$div(
                                     div(style = "margin-bottom:0px; margin-left: 7px",
                                         htmlOutput(ns("title_table_mort"))
                                         ),
                                     tableOutput(ns("table_mort")),
                                     style = "margin-left: 5%;"
                                 )
                        ),
                        column(
                            4,
                            tags$div(
                                     div(style = "margin-bottom:0px; margin-left: 2px",
                                         htmlOutput(ns("title_table_refs"))
                                         ),
                                     tableOutput(ns("table_refs")),
                                     style = "margin-left: 0%;"
                                 )
                        ),
                        column(
                            4,
                            tags$div(
                                     div(style = "margin-bottom:0px; margin-left: 2px",
                                         htmlOutput(ns("title_table_stockstatus"))
                                         ),
                                     tableOutput(ns("table_stockstatus")),
                                     style = "margin-left: 0%;"
                                 )
                        )
                    ),

                    br(),br(),

                    fluidRow(
                        column(
                            6,
                            tags$div(
                                     plotOutput(ns("plot_ypr"), width = "90%",
                                                height = "700px"),
                                     div(style = "margin-top:0px; margin-left: 10px",
                                         htmlOutput(ns("title_ypr"))
                                         ),
                                     style = "margin-left: 10%;"
                                 )
                        ),
                        column(
                            6,
                            tags$div(
                                     plotOutput(ns("plot_ypr_iso"), width = "90%",
                                                height = "700px"),
                                     div(style = "margin-top:0px; margin-left: 10px",
                                         htmlOutput(ns("title_ypr_iso"))
                                         ),
                                     style = "margin-left: 10%;"
                                 )
                        )
                    )


                    ##         "Results of the Thompson and Bell model: Curves of yield and biomass per recruit. The black dot represents yield and biomass under current fishing pressure. The yellow and red dashed lines represent fishing mortality for maximum sustainable yield (Fmsy) and fishing mortality to fish the stock at 50% of the virgin biomass (F0.5).",

                    ##         "Exploration of impact of different exploitation rates and Lc values on the relative yield per recruit.",


                    )
            )
            )


}
