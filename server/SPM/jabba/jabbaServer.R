jabbaModule <- function(input, output, session) {
  ns <- session$ns

  jabba_dat <- reactiveValues(raw = NULL, input_data = NULL, result = NULL, error = NULL, checked = FALSE,
                              source = NULL, report = NULL)

  ## Report/upload state for the current run. `upload$status` is one of
  ## "not_attempted" (no PDF), "standalone" (no VRE session), "unavailable"
  ## (VRE session without a usable workspace connection), "failed", "ok".
  empty_report_state <- function() {
    list(pdf_ok = FALSE, pdf_error = NULL,
         upload = list(attempted = FALSE, ok = FALSE, status = "not_attempted", path = NULL, message = NULL))
  }

  ## session$userData reactive values are created by app.R; they are absent
  ## when the module is exercised on its own.
  user_data <- function(name) {
    f <- session$userData[[name]]
    if (is.function(f)) f() else NULL
  }

  ## Discard the current result and delete its run-specific temporary
  ## directory (outputs stay available until a reset, a new run or the end
  ## of the session).
  discard_jabba_result <- function() {
    jabba_cleanup_result(jabba_dat$result)
    jabba_dat$result <- NULL
    jabba_dat$report <- NULL
  }
  session$onSessionEnded(function() {
    isolate(jabba_cleanup_result(jabba_dat$result))
  })

  ## Download controls are enabled only while the corresponding output
  ## exists; the download handlers enforce the same condition server-side.
  observe({
    res <- jabba_dat$result
    rep <- jabba_dat$report
    has_result <- !is.null(res)
    has_pdf <- has_result && isTRUE(rep$pdf_ok) && jabba_pdf_available(res)
    for (id in c("download_rds", "download_summary_csv", "download_zip")) {
      if (has_result) shinyjs::enable(id) else shinyjs::disable(id)
    }
    if (has_pdf) shinyjs::enable("download_report") else shinyjs::disable("download_report")
    if (has_result && !has_pdf) shinyjs::show("retry_report") else shinyjs::hide("retry_report")
  })

  require_result <- function() {
    res <- jabba_dat$result
    if (is.null(res)) stop("No JABBA result is available. Run the assessment first.")
    res
  }

  ## Render the PDF report of a completed run into the run directory.
  render_report <- function(res) {
    tryCatch({
      create_jabba_pdf_report(res$report_file, res)
      list(ok = TRUE, error = NULL)
    }, error = function(e) {
      msg <- jabba_redact_secrets(conditionMessage(e))
      flog.error("JABBA PDF report could not be rendered: %s", msg)
      list(ok = FALSE, error = msg)
    })
  }

  ## Automatic upload of the PDF report to the user's VRE workspace: called at
  ## most once per run, only with an authenticated session and a usable
  ## StorageHub manager. Success is reported only when the client returned an
  ## item identifier (StoragehubManager$uploadFile() stops on any non-200
  ## response). Credentials are never logged or displayed.
  upload_report <- function(res) {
    mode <- user_data("sessionMode")
    if (!identical(mode, "GCUBE")) {
      return(list(attempted = FALSE, ok = FALSE, status = "standalone", path = NULL, message = NULL))
    }
    manager <- user_data("storagehubManager")
    username <- user_data("sessionUsername")
    if (is.null(manager) || is.null(username) || !nzchar(username)) {
      flog.warn("JABBA report upload not attempted: no usable workspace connection in this session")
      return(list(attempted = FALSE, ok = FALSE, status = "unavailable", path = NULL,
                  message = "no usable workspace connection is available in this session."))
    }
    basePath <- paste0("/Home/", username, "/Workspace/")
    report_name <- basename(res$report_file)
    flog.info("Uploading JABBA report %s to the i-Marine workspace of user %s", report_name, username)
    tryCatch({
      item_id <- uploadToIMarineFolder(manager, res$report_file, basePath, uploadFolderName,
                                       description = "JABBA assessment report")
      if (is.null(item_id) || length(item_id) != 1 || is.na(item_id) || !nzchar(as.character(item_id))) {
        stop("the workspace client returned no item identifier")
      }
      list(attempted = TRUE, ok = TRUE, status = "ok", path = paste0(uploadFolderName, "/", report_name),
           message = NULL, item_id = as.character(item_id))
    }, error = function(e) {
      msg <- jabba_redact_secrets(conditionMessage(e), extra = user_data("sessionToken"))
      flog.error("JABBA report upload failed for user %s: %s", username, msg)
      list(attempted = TRUE, ok = FALSE, status = "failed", path = NULL, message = msg)
    })
  }

  output$jabbaTitle <- renderText({
    session$userData$page("jabba")
    "<span><h3><b>Stock assessment with JABBA</b></h3></span>"
  })

  output$jabbaVersion <- renderText({
    if (!requireNamespace("JABBA", quietly = TRUE)) {
      "<b>JABBA is not installed in this R session.</b> Install JAGS, rjags and jabbamodel/JABBA before running an assessment. The rest of SMT can still be used."
    } else {
      paste0("JABBA package detected: ", as.character(utils::packageVersion("JABBA")))
    }
  })

  observeEvent(input$file, {
    req(input$file)
    jabba_dat$error <- NULL
    discard_jabba_result()
    jabba_dat$checked <- FALSE
    md5 <- tryCatch(unname(tools::md5sum(input$file$datapath)), error = function(e) NA_character_)
    sample_md5 <- tryCatch(unname(tools::md5sum("www/data/jabba_sample.csv")), error = function(e) NA_character_)
    jabba_dat$source <- list(file_name = input$file$name, md5 = md5,
                             is_bundled_sample = !is.na(md5) && identical(md5, sample_md5))
    jabba_dat$raw <- tryCatch(
      read_jabba_csv(input$file$datapath, input$csv_sep, input$csv_dec),
      error = function(e) {
        jabba_dat$error <- conditionMessage(e)
        NULL
      }
    )
  })

  observeEvent(input$reset_jabba, {
    jabba_dat$raw <- NULL
    jabba_dat$input_data <- NULL
    discard_jabba_result()
    jabba_dat$error <- NULL
    jabba_dat$checked <- FALSE
    jabba_dat$source <- NULL
    updateTextInput(session, "assessment_name", value = "SMT_JABBA")
    updateTextInput(session, "scenario_name", value = "base")
    updateCheckboxInput(session, "quickmcmc", value = TRUE)
    updateSelectInput(session, "harvest_label", selected = JABBA_HARVEST_LABEL_DEFAULT)
    if (jabba_advanced_enabled()) {
      updateCheckboxInput(session, "obsE_default", value = TRUE)
      updateNumericInput(session, "fixed_obsE", value = 0.1)
      updateCheckboxInput(session, "proc_dev_all", value = TRUE)
      updateNumericInput(session, "P_bound_lo", value = 0.02)
      updateNumericInput(session, "P_bound_hi", value = 1.3)
    }
  })

  ## Advanced-setting validation (JABBA-required conditions only; see run_jabba.R).
  ## While JABBA_ADVANCED_SETTINGS_ENABLED is FALSE (UI box hidden) the settings
  ## are always JABBA's defaults and this never reports a problem.
  advanced_problems <- function() {
    chk <- jabba_check_advanced(jabba_advanced_settings(input))
    if (isTRUE(chk$ok)) NULL else paste(chk$messages, collapse = "\n")
  }

  output$column_mapping <- renderUI({
    req(jabba_dat$raw)
    cols <- names(jabba_dat$raw)
    guess <- jabba_guess_columns(jabba_dat$raw)
    selected_year <- if (isTRUE(input$guess_cols)) guess$year else character(0)
    selected_catch <- if (isTRUE(input$guess_cols)) guess$catch else character(0)
    selected_cpue <- if (isTRUE(input$guess_cols)) guess$cpue else character(0)
    selected_se <- if (isTRUE(input$guess_cols)) guess$se else character(0)

    tagList(
      fluidRow(
        column(3, selectInput(ns("year_col"), "Year column", choices = cols, selected = selected_year)),
        column(3, selectInput(ns("catch_col"), "Catch column", choices = cols, selected = selected_catch)),
        column(3, selectizeInput(ns("cpue_cols"), "CPUE/index column(s)", choices = cols, selected = selected_cpue, multiple = TRUE)),
        column(3, selectizeInput(ns("se_cols"), "Optional SE column(s)", choices = cols, selected = selected_se, multiple = TRUE))
      ),
      helpText("Tip: leave SE empty if the index series does not have standard errors. If SE is supplied, choose one SE column per selected CPUE/index column, in the same order.")
    )
  })

  selected_jabba_data <- reactive({
    req(jabba_dat$raw, input$year_col, input$catch_col, input$cpue_cols)
    make_jabba_input_data(
      jabba_dat$raw,
      input$year_col,
      input$catch_col,
      input$cpue_cols,
      input$se_cols,
      input$start_year,
      input$end_year
    )
  })

  output$preview <- DT::renderDataTable({
    req(jabba_dat$raw)
    DT::datatable(utils::head(jabba_dat$raw, 20), options = list(scrollX = TRUE, pageLength = 10))
  })

  output$data_checks <- renderText({
    if (!is.null(jabba_dat$error)) return(paste("Error:", jabba_dat$error))
    if (is.null(jabba_dat$raw)) return("Upload a CSV file to start.")
    dat <- tryCatch(selected_jabba_data(), error = function(e) e)
    if (inherits(dat, "error")) return(paste("Input check failed:", conditionMessage(dat)))
    format_jabba_checks(dat$checks)
  })

  output$input_plot <- renderPlot({
    req(jabba_dat$raw)
    dat <- tryCatch(selected_jabba_data(), error = function(e) NULL)
    if (is.null(dat)) {
      graphics::plot.new(); graphics::text(0.5, 0.5, "Map columns to preview data."); return()
    }
    oldpar <- graphics::par(no.readonly = TRUE); on.exit(graphics::par(oldpar), add = TRUE)
    graphics::par(mar = c(4, 4, 2, 4))
    graphics::plot(dat$catch$Year, dat$catch$Catch, type = "h", lwd = 2, xlab = "Year", ylab = "Catch", main = "Catch and scaled CPUE")
    cp <- dat$cpue[, -1, drop = FALSE]
    if (ncol(cp) > 0 && any(!is.na(as.matrix(cp)))) {
      scale_to <- max(dat$catch$Catch, na.rm = TRUE)
      for (i in seq_len(ncol(cp))) {
        z <- cp[[i]]
        z <- z / max(z, na.rm = TRUE) * scale_to
        graphics::lines(dat$cpue$Year, z, type = "b", pch = 16 + i)
      }
      graphics::legend("topright", legend = c("Catch", names(cp)), lty = c(1, rep(1, ncol(cp))), pch = c(NA, 17:(16+ncol(cp))), cex = 0.8, bty = "n")
    }
  })

  observeEvent(input$check_jabba, {
    adv_msg <- advanced_problems()
    if (!is.null(adv_msg)) {
      jabba_dat$error <- adv_msg
      jabba_dat$checked <- FALSE
      showModal(modalDialog(title = "JABBA advanced settings", HTML(gsub("\n", "<br>", adv_msg)), easyClose = TRUE, footer = NULL))
      return(NULL)
    }
    dat <- tryCatch(selected_jabba_data(), error = function(e) e)
    if (inherits(dat, "error")) {
      jabba_dat$error <- conditionMessage(dat)
      jabba_dat$checked <- FALSE
      showModal(modalDialog(title = "JABBA input check failed", jabba_dat$error, easyClose = TRUE, footer = NULL))
    } else {
      jabba_dat$input_data <- dat
      jabba_dat$checked <- isTRUE(dat$checks$ok)
      jabba_dat$error <- NULL
      showModal(modalDialog(title = "JABBA input check", HTML(gsub("\n", "<br>", format_jabba_checks(dat$checks))), easyClose = TRUE, footer = NULL))
    }
  })

  ## One click = one assessment run, one PDF render attempt and at most one
  ## automatic upload attempt. Failures are kept apart: a fit error leaves no
  ## result; a report error keeps the result and the other downloads; an
  ## upload error keeps the result and the local report.
  observeEvent(input$run_jabba, {
    ## Validate the advanced settings first: an invalid request is refused
    ## before the previous result is discarded.
    adv_msg <- advanced_problems()
    if (!is.null(adv_msg)) {
      jabba_dat$error <- adv_msg
      return(NULL)
    }
    jabba_dat$error <- NULL
    discard_jabba_result()
    dat <- tryCatch(selected_jabba_data(), error = function(e) e)
    if (inherits(dat, "error")) {
      jabba_dat$error <- conditionMessage(dat)
      return(NULL)
    }
    if (!isTRUE(dat$checks$ok)) {
      jabba_dat$error <- paste(dat$checks$messages, collapse = "\n")
      return(NULL)
    }
    jabba_dat$input_data <- dat
    withProgress(message = "Running JABBA", value = 0, {
      incProgress(0.1, detail = "Preparing input")
      res <- tryCatch({
        incProgress(0.2, detail = "Building and fitting the JABBA model (JAGS)")
        run_jabba_model(dat, input, source = jabba_dat$source)
      }, error = function(e) {
        jabba_dat$error <- conditionMessage(e)
        NULL
      })
      if (!is.null(res)) {
        jabba_dat$result <- res
        state <- empty_report_state()
        incProgress(0.75, detail = "Rendering the PDF report")
        rep <- render_report(res)
        state$pdf_ok <- rep$ok
        state$pdf_error <- rep$error
        if (rep$ok) {
          incProgress(0.9, detail = "Uploading the report to the VRE workspace")
          state$upload <- upload_report(res)
        }
        jabba_dat$report <- state
      }
      incProgress(1, detail = "Done")
    })
  })

  ## Explicit retry of the PDF report for the current run, without re-running
  ## the assessment. The automatic upload is attempted only if no attempt was
  ## made for this run yet, so a retry never duplicates an upload.
  observeEvent(input$retry_report, {
    res <- jabba_dat$result
    if (is.null(res)) return(NULL)
    state <- jabba_dat$report
    if (is.null(state)) state <- empty_report_state()
    if (isTRUE(state$pdf_ok) && jabba_pdf_available(res)) return(NULL)
    withProgress(message = "Rendering the PDF report", value = 0.3, {
      rep <- render_report(res)
      state$pdf_ok <- rep$ok
      state$pdf_error <- rep$error
      if (rep$ok && !isTRUE(state$upload$attempted)) {
        incProgress(0.5, detail = "Uploading the report to the VRE workspace")
        state$upload <- upload_report(res)
      }
      jabba_dat$report <- state
      incProgress(1, detail = "Done")
    })
  })

  output$run_status <- renderText({
    if (!is.null(jabba_dat$error)) return(paste("Error:", jabba_dat$error))
    res <- jabba_dat$result
    if (is.null(res)) {
      return(paste(
        "Upload data, map columns, run 'Check Input', then run the assessment.",
        "Use 'Quick MCMC test run' for a first smoke test; switch it off for a real run.",
        sep = "\n"
      ))
    }
    rep <- jabba_dat$report
    lines <- c(
      paste0("Assessment completed (", format(res$run_time, "%Y-%m-%d %H:%M:%S"), ").")
    )
    if (isTRUE(rep$pdf_ok)) {
      lines <- c(lines, paste0("PDF report generated: ", basename(res$report_file), "."))
    } else {
      lines <- c(lines, paste0("PDF generation failed: ", if (!is.null(rep$pdf_error)) rep$pdf_error else "unknown error", ".",
                               "\nThe fitted object, summary table and output archive remain available. Use 'Retry PDF report' to generate the report again without re-running the assessment."))
    }
    up <- rep$upload
    if (!is.null(up)) {
      lines <- c(lines, switch(up$status,
        ok = paste0("Report uploaded to your VRE workspace: ", up$path, "."),
        failed = paste0("Report upload failed: ", up$message, " The report can still be downloaded from this page."),
        unavailable = paste0("Workspace upload not attempted: ", up$message),
        standalone = "Workspace upload not applicable: the SMT was not opened from the VRE with a user token.",
        not_attempted = "Workspace upload not attempted: no PDF report is available for this run.",
        ""
      ))
    }
    lines <- c(lines, jabba_convergence_status_text(res))
    paste(lines, collapse = "\n")
  })

  output$summary_table <- DT::renderDataTable({
    req(jabba_dat$result)
    DT::datatable(jabba_summary_table(jabba_dat$result), options = list(scrollX = TRUE, pageLength = 15)) %>%
      DT::formatSignif(c("median", "lower_95", "upper_95"), 4)
  })

  output$diagnostics_table <- DT::renderDataTable({
    req(jabba_dat$result)
    DT::datatable(jabba_diagnostics_table(jabba_dat$result), options = list(scrollX = TRUE, pageLength = 15))
  })

  ## Convergence tab: status text, diagnostics table and per-chain trace plots
  ## from the chain-ordered draws of this run (see run_jabba.R); when the draws
  ## could not be captured the tab says so and shows no diagnostics.
  output$convergence_status <- renderText({
    req(jabba_dat$result)
    jabba_convergence_status_text(jabba_dat$result)
  })

  output$convergence_table <- DT::renderDataTable({
    req(jabba_dat$result)
    tab <- jabba_convergence_table(jabba_dat$result)
    if (is.null(tab)) tab <- data.frame(diagnostics = "Not available for this run (chain-ordered draws could not be captured).", stringsAsFactors = FALSE)
    DT::datatable(tab, rownames = FALSE, options = list(scrollX = TRUE, pageLength = 30, dom = "t"))
  })

  trace_plot_height <- function() {
    cv <- jabba_dat$result$convergence
    n <- if (!is.null(cv) && isTRUE(cv$available)) sum(cv$varying) else 1
    max(260, 230 * ceiling(n / 3))
  }
  output$trace_plot <- renderPlot({
    req(jabba_dat$result)
    jabba_plot_traces(jabba_dat$result)
  }, height = trace_plot_height)

  output$summary_plot <- renderPlot({
    req(jabba_dat$result)
    plot_jabba_if_available("jbplot_summary", jabba_dat$result)
  })

  output$cpue_plot <- renderPlot({
    req(jabba_dat$result)
    plot_jabba_if_available("jbplot_cpuefits", jabba_dat$result)
  })

  output$kobe_plot <- renderPlot({
    req(jabba_dat$result)
    plot_jabba_if_available("jbplot_kobe", jabba_dat$result)
  })

  output$residual_plot <- renderPlot({
    req(jabba_dat$result)
    plot_jabba_if_available("jbplot_residuals", jabba_dat$result)
  })

  ## Downloads never trigger uploads. Each handler re-checks server-side
  ## that the requested output exists.
  output$download_rds <- downloadHandler(
    filename = function() paste0("jabba_result_", Sys.Date(), ".rds"),
    content = function(file) saveRDS(require_result(), file)
  )

  output$download_summary_csv <- downloadHandler(
    filename = function() paste0("jabba_summary_", Sys.Date(), ".csv"),
    content = function(file) utils::write.csv(jabba_summary_table(require_result()), file, row.names = FALSE)
  )

  output$download_zip <- downloadHandler(
    filename = function() paste0("jabba_output_", Sys.Date(), ".zip"),
    content = function(file) jabba_zip_outputs(require_result(), file)
  )

  output$download_report <- downloadHandler(
    filename = function() {
      res <- jabba_dat$result
      if (jabba_pdf_available(res)) basename(res$report_file) else "JABBA_report_unavailable.pdf"
    },
    content = function(file) {
      res <- require_result()
      if (!jabba_pdf_available(res)) stop("The PDF report for this run is not available. Use 'Retry PDF report'.")
      if (!file.copy(res$report_file, file, overwrite = TRUE)) stop("The PDF report could not be copied.")
    },
    contentType = "application/pdf"
  )

  observeEvent(input$workflowConsiderations, showModal(modalDialog(title = "JABBA workflow", HTML(getWorkflowConsiderationTextForJabba()), easyClose = TRUE, footer = NULL)))
  observeEvent(input$dataConsiderations, showModal(modalDialog(title = "JABBA data", HTML(getDataConsiderationTextForJabba()), easyClose = TRUE, footer = NULL)))
  observeEvent(input$dataConsiderations2, showModal(modalDialog(title = "JABBA data", HTML(getDataConsiderationTextForJabba()), easyClose = TRUE, footer = NULL)))
  observeEvent(input$methodConsiderations, showModal(modalDialog(title = "JABBA method", HTML(getMethodConsiderationTextForJabba()), easyClose = TRUE, footer = NULL)))
  observeEvent(input$methodConsiderations2, showModal(modalDialog(title = "JABBA method", HTML(getMethodConsiderationTextForJabba()), easyClose = TRUE, footer = NULL)))
  observeEvent(input$resultConsiderations, showModal(modalDialog(title = "JABBA results", HTML(getResultConsiderationTextForJabba()), easyClose = TRUE, footer = NULL)))
  observeEvent(input$resultConsiderations2, showModal(modalDialog(title = "JABBA results", HTML(getResultConsiderationTextForJabba()), easyClose = TRUE, footer = NULL)))
  observeEvent(input$advancedConsiderations, showModal(modalDialog(title = "JABBA advanced settings", HTML(getAdvancedConsiderationTextForJabba()), easyClose = TRUE, footer = NULL)))
  ## Per-field help for the (currently hidden) Advanced Settings box: each popup
  ## describes its own option only (texts in ui/SPM/jabba/commonUI.R).
  observeEvent(input$info_fixed_obsE, showModal(modalDialog(title = "Minimum observation error (fixed.obsE)", HTML(getFixedObsEHelpTextForJabba()), easyClose = TRUE, footer = NULL)))
  observeEvent(input$info_proc_dev_all, showModal(modalDialog(title = "Process deviations (proc.dev.all)", HTML(getProcDevAllHelpTextForJabba()), easyClose = TRUE, footer = NULL)))
  observeEvent(input$info_P_bound, showModal(modalDialog(title = "Soft bounds on B/K (P_bound)", HTML(getPBoundHelpTextForJabba()), easyClose = TRUE, footer = NULL)))
  observeEvent(input$info_harvest_label, showModal(modalDialog(title = "Exploitation terminology (F/Fmsy or H/Hmsy)", HTML(getHarvestLabelHelpTextForJabba()), easyClose = TRUE, footer = NULL)))
  observeEvent(input$info_convergence, showModal(modalDialog(title = "MCMC convergence diagnostics", HTML(getConvergenceHelpTextForJabba()), easyClose = TRUE, footer = NULL, size = "l")))
  observeEvent(input$info_csv_sep, showModal(modalDialog(title = "CSV separator", "Use automatic detection unless your file is read as a single column. European CSV files often use semicolons.", easyClose = TRUE, footer = NULL)))
  observeEvent(input$info_csv_dec, showModal(modalDialog(title = "CSV decimal", "Use point or comma depending on how numeric values are stored in your CSV file.", easyClose = TRUE, footer = NULL)))
  observeEvent(input$info_col_auto, showModal(modalDialog(title = "Automatic column mapping", "SMT guesses year, catch, CPUE and SE columns from their names. Always verify the mapping before running JABBA.", easyClose = TRUE, footer = NULL)))

  observeEvent(input$tour_general, {
    rintrojs::introjs(session, options = list(steps = list(
      list(element = paste0("#", ns("box_datupload")), intro = "Upload catch and CPUE/index data, then map columns."),
      list(element = paste0("#", ns("box_settings")), intro = "Set production model, priors, process/observation error assumptions and the exploitation terminology (F/Fmsy or H/Hmsy)."),
      list(element = paste0("#", ns("box_run")), intro = "Check inputs, run a quick MCMC smoke test, then run the full model. The PDF report is generated after each run and, in the VRE, uploaded to your workspace."))
    ))
  })
}
