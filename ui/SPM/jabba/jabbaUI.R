tabJABBA <- function(id) {
  ns <- NS(id)

  tabItem(
    "jabbaWidget",
    htmlOutput(ns("jabbaTitle")),
    htmlOutput(ns("jabbaVersion"), class = "subTitle"),

    fluidRow(
      div(style = "display: inline-block; vertical-align:center; margin-left: 15px;", "More information about "),
      div(style = "display: inline-block; vertical-align:center; margin-left: 5px;",
          actionButton(ns("workflowConsiderations"), "Workflow", class = "topLevelInformationButton")),
      div(style = "display: inline-block; vertical-align:center; margin-left: 5px;",
          actionButton(ns("dataConsiderations"), "Data", class = "topLevelInformationButton")),
      div(style = "display: inline-block; vertical-align:center; margin-left: 5px;",
          actionButton(ns("methodConsiderations"), "Methods", class = "topLevelInformationButton")),
      div(style = "display: inline-block; vertical-align:center; margin-left: 5px;",
          actionButton(ns("resultConsiderations"), "Results", class = "topLevelInformationButton")),
      div(style = "display: inline-block; vertical-align:center; margin-left: 10px;",
          actionButton(ns("tour_general"), "Start JABBA tour", class = "btn btn-primary btn-sm"))
    ),

    fluidRow(
      box(id = ns("box_datupload"), width = 12, solidHeader = TRUE, title = tagList("Data Upload",
          actionButton(ns("dataConsiderations2"), tags$i(class = "fas fa-info", style = "font-size: 8px"), class = "infoBubbleButton")),
          fluidRow(
            column(4, fileInput(ns("file"), "Choose Input CSV File",
                                accept = c("text/csv", "text/comma-separated-values,text/plain", ".csv"))),
            column(2, selectizeInput(ns("csv_sep"), tagList("CSV field separator",
                                actionButton(ns("info_csv_sep"), tags$i(class = "fas fa-info", style = "font-size: 8px"), class = "infoBubbleButton")),
                                choices = c("Automatic guess" = "auto", "Comma (,)" = ",", "Semicolon (;)" = ";", "Tab (\\t)" = "\t"), selected = "auto")),
            column(2, selectizeInput(ns("csv_dec"), tagList("CSV decimal separator",
                                actionButton(ns("info_csv_dec"), tags$i(class = "fas fa-info", style = "font-size: 8px"), class = "infoBubbleButton")),
                                choices = c("Automatic guess" = "auto", "Point (.)" = ".", "Comma (,)" = ","), selected = "auto")),
            column(2, checkboxInput(ns("guess_cols"), tagList("Assign columns automatically?",
                                actionButton(ns("info_col_auto"), tags$i(class = "fas fa-info", style = "font-size: 8px"), class = "infoBubbleButton")), TRUE)),
            column(2, br(), actionButton(ns("reset_jabba"), "Reset", class = "btn-warning"))
          ),
          uiOutput(ns("column_mapping")),
          fluidRow(
            column(6, verbatimTextOutput(ns("data_checks"))),
            column(6, plotOutput(ns("input_plot"), height = "260px"))
          ),
          DT::dataTableOutput(ns("preview"))
      )
    ),

    fluidRow(
      box(id = ns("box_settings"), width = 12, solidHeader = TRUE, title = tagList("Assessment Settings",
          actionButton(ns("methodConsiderations2"), tags$i(class = "fas fa-info", style = "font-size: 8px"), class = "infoBubbleButton")),
          fluidRow(
            column(3, textInput(ns("assessment_name"), "Assessment name", value = "SMT_JABBA")),
            column(3, textInput(ns("scenario_name"), "Scenario", value = "base")),
            column(3, selectInput(ns("model_type"), "Production model", choices = c("Schaefer", "Fox", "Pella", "Pella_m"), selected = "Schaefer")),
            column(3, textInput(ns("catch_metric"), "Catch metric", value = "(t)"))
          ),
          fluidRow(
            column(3, numericInput(ns("start_year"), "Start year", value = NA, step = 1)),
            column(3, numericInput(ns("end_year"), "End year", value = NA, step = 1)),
            column(3, selectInput(ns("harvest_label"), tagList("Exploitation terminology",
                                    actionButton(ns("info_harvest_label"), tags$i(class = "fas fa-info", style = "font-size: 8px"), class = "infoBubbleButton")),
                                  choices = get0("JABBA_HARVEST_LABELS", ifnotfound = c("F/Fmsy (JABBA convention)" = "Fmsy", "H/Hmsy (harvest rate)" = "Hmsy")),
                                  selected = get0("JABBA_HARVEST_LABEL_DEFAULT", ifnotfound = "Fmsy"))),
            column(3, numericInput(ns("Plim"), "Recruitment impairment Plim", value = 0, min = 0, max = 0.8, step = 0.01))
          ),
          fluidRow(
            column(3, selectInput(ns("r_dist"), "r prior distribution", choices = c("lnorm", "range"), selected = "lnorm")),
            column(3, numericInput(ns("r_mu"), "r prior mean / lower", value = 0.2, min = 0, step = 0.01)),
            column(3, numericInput(ns("r_cv"), "r prior CV / upper", value = 0.5, min = 0, step = 0.01)),
            column(3, selectInput(ns("psi_dist"), "Initial depletion prior", choices = c("lnorm", "beta"), selected = "lnorm"))
          ),
          fluidRow(
            column(3, numericInput(ns("psi_mu"), "Initial depletion mean", value = 0.9, min = 0.01, max = 1.3, step = 0.01)),
            column(3, numericInput(ns("psi_cv"), "Initial depletion CV", value = 0.25, min = 0, step = 0.01)),
            column(3, checkboxInput(ns("addCatchCV"), "Add catch error", value = TRUE)),
            column(3, numericInput(ns("catchCV"), "Catch CV", value = 0.1, min = 0, step = 0.01))
          ),
          fluidRow(
            column(3, selectInput(ns("catch_error"), "Catch error type", choices = c("random", "under"), selected = "random")),
            column(3, checkboxInput(ns("sigma_est"), "Estimate observation variance", value = TRUE)),
            column(3, checkboxInput(ns("sigma_proc"), "Estimate process variance", value = TRUE)),
            column(3, checkboxInput(ns("proc_dyn"), "Dynamic process error", value = FALSE))
          ),
          fluidRow(
            column(3, checkboxInput(ns("useKprior"), "Use K prior", value = FALSE)),
            column(3, selectInput(ns("K_dist"), "K prior distribution", choices = c("lnorm", "range"), selected = "lnorm")),
            column(3, numericInput(ns("Kmu"), "K prior mean / lower", value = 10000, min = 0, step = 100)),
            column(3, numericInput(ns("Kcv"), "K prior CV / upper", value = 0.5, min = 0, step = 0.01))
          ),
          fluidRow(
            column(3, checkboxInput(ns("useBprior"), "Use biomass status prior", value = FALSE)),
            column(2, numericInput(ns("BpriorMu"), "B prior mean", value = 0.5, min = 0, step = 0.01)),
            column(2, numericInput(ns("BpriorCv"), "B prior CV", value = 0.3, min = 0, step = 0.01)),
            column(2, numericInput(ns("BpriorYear"), "B prior year", value = NA, step = 1)),
            column(3, selectInput(ns("BpriorType"), "B prior type", choices = c("bk", "bbmsy", "ffmsy"), selected = "bk"))
          )
      )
    ),

    ## Advanced Settings box (fixed.obsE, proc.dev.all, P_bound): hidden for now.
    ## The server-side implementation (run_jabba.R, jabbaServer.R) is kept and
    ## switched off by JABBA_ADVANCED_SETTINGS_ENABLED (run_jabba.R), so JABBA's
    ## package defaults apply regardless of any input value. To re-enable: uncomment
    ## this block and set the flag to TRUE.
    ##     fluidRow(
    ##       box(id = ns("box_advanced"), width = 12, solidHeader = TRUE, collapsible = TRUE, collapsed = TRUE,
    ##           title = tagList("Advanced Settings",
    ##                           actionButton(ns("advancedConsiderations"), tags$i(class = "fas fa-info", style = "font-size: 8px"), class = "infoBubbleButton")),
    ##           p("JABBA options kept at their package defaults unless changed here. They are needed to reproduce some documented JABBA configurations."),
    ##           fluidRow(
    ##             column(3, checkboxInput(ns("obsE_default"), "Use JABBA default minimum observation error", value = TRUE)),
    ##             column(3, numericInput(ns("fixed_obsE"), tagList("Minimum observation error (log-scale SD)",
    ##                                     actionButton(ns("info_fixed_obsE"), tags$i(class = "fas fa-info", style = "font-size: 8px"), class = "infoBubbleButton")),
    ##                                     value = 0.1, min = 0, step = 0.01)),
    ##             column(3, checkboxInput(ns("proc_dev_all"), tagList("Process deviations from the first catch year",
    ##                                     actionButton(ns("info_proc_dev_all"), tags$i(class = "fas fa-info", style = "font-size: 8px"), class = "infoBubbleButton")), value = TRUE)),
    ##             column(3, helpText("JABBA default for the minimum observation error: 0.1 without SE columns, 0.001 with SE columns."))
    ##           ),
    ##           fluidRow(
    ##             column(3, numericInput(ns("P_bound_lo"), tagList("Lower soft bound on B/K",
    ##                                     actionButton(ns("info_P_bound"), tags$i(class = "fas fa-info", style = "font-size: 8px"), class = "infoBubbleButton")),
    ##                                     value = 0.02, min = 0, step = 0.005)),
    ##             column(3, numericInput(ns("P_bound_hi"), "Upper soft bound on B/K", value = 1.3, min = 0, step = 0.05)),
    ##             column(6, helpText("JABBA defaults 0.02 and 1.3. The bounds apply a soft penalty on log(B/K) outside the interval, not a hard constraint."))
    ##           )
    ##       )
    ##     ),

    fluidRow(
      box(id = ns("box_run"), width = 12, solidHeader = TRUE, title = "Run and outputs",
          fluidRow(
            column(2, numericInput(ns("ni"), "Iterations", value = 30000, min = 1000, step = 1000)),
            column(2, numericInput(ns("nb"), "Burn-in", value = 5000, min = 0, step = 1000)),
            column(2, numericInput(ns("nt"), "Thinning", value = 5, min = 1, step = 1)),
            column(2, numericInput(ns("nc"), "Chains", value = 2, min = 1, step = 1)),
            column(2, numericInput(ns("seed"), "Seed", value = 123, min = 1, step = 1)),
            column(2, checkboxInput(ns("quickmcmc"), "Quick MCMC test run", value = TRUE))
          ),
          fluidRow(
            column(3, checkboxInput(ns("do_ppc"), "Posterior predictive checks", value = TRUE)),
            column(3, checkboxInput(ns("save_png"), "Save JABBA plots to output zip", value = TRUE)),
            column(2, br(), actionButton(ns("check_jabba"), "Check Input", class = "btn-info")),
            column(2, br(), actionButton(ns("run_jabba"), "Run Assessment", class = "btn-primary")),
            column(2, br(), shinyjs::disabled(downloadButton(ns("download_report"), "Download Report")),
                   shinyjs::hidden(actionButton(ns("retry_report"), "Retry PDF report", class = "btn-warning", style = "margin-top: 5px;")))
          ),
          verbatimTextOutput(ns("run_status"))
      )
    ),

    fluidRow(
      box(width = 12, solidHeader = TRUE, title = tagList("Results",
          actionButton(ns("resultConsiderations2"), tags$i(class = "fas fa-info", style = "font-size: 8px"), class = "infoBubbleButton")),
          tabsetPanel(
            tabPanel("Summary", DT::dataTableOutput(ns("summary_table"))),
            tabPanel("Trajectories", plotOutput(ns("summary_plot"), height = "650px")),
            tabPanel("CPUE fit", plotOutput(ns("cpue_plot"), height = "520px")),
            tabPanel("Kobe", plotOutput(ns("kobe_plot"), height = "520px")),
            tabPanel("Residuals", plotOutput(ns("residual_plot"), height = "520px")),
            tabPanel("Convergence",
                     div(style = "margin-top: 8px;", actionButton(ns("info_convergence"), "About these diagnostics", icon = icon("info"), class = "btn btn-default btn-sm")),
                     verbatimTextOutput(ns("convergence_status")),
                     DT::dataTableOutput(ns("convergence_table")),
                     plotOutput(ns("trace_plot"), height = "auto")),
            tabPanel("Residual table", DT::dataTableOutput(ns("diagnostics_table")))
          ),
          br(),
          shinyjs::disabled(downloadButton(ns("download_rds"), "Download JABBA object (.rds)")),
          shinyjs::disabled(downloadButton(ns("download_summary_csv"), "Download summary table (.csv)")),
          shinyjs::disabled(downloadButton(ns("download_zip"), "Download full output (.zip)"))
      )
    )
  )
}
