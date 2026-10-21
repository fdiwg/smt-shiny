# Native JABBA integration helpers for SMT Shiny
# These functions deliberately keep JABBA optional at application start-up.
# The app can load without JAGS/JABBA, but the JABBA assessment button will fail
# with a user-facing installation message if the runtime dependency is missing.
#
# Terminology: the user selects the exploitation terminology through JABBA's own
# `harvest.label` option ("Fmsy", JABBA's convention and the default, or "Hmsy");
# SMT applies it to the labels of plots, tables, CSV files and the PDF. The
# fitted object is left exactly as JABBA returns it (slots "F"/"FFmsy",
# `refpts$fmsy`, estimates rows "Hmsy"/"H_Hmsy.cur").

jabba_required_columns <- c("year", "catch")

## ---- exploitation terminology (JABBA's harvest.label option) ----
JABBA_HARVEST_LABELS <- c("F/Fmsy (JABBA convention)" = "Fmsy", "H/Hmsy (harvest rate)" = "Hmsy")
JABBA_HARVEST_LABEL_DEFAULT <- "Fmsy"

jabba_harvest_label <- function(input) {
  x <- tryCatch(input$harvest_label, error = function(e) NULL)
  if (is.null(x) || length(x) != 1 || is.na(x) || !(x %in% JABBA_HARVEST_LABELS)) JABBA_HARVEST_LABEL_DEFAULT else as.character(x)
}

## Terms used in tables, CSV files, the PDF and the UI for the chosen label.
jabba_harvest_terms <- function(label = JABBA_HARVEST_LABEL_DEFAULT) {
  if (identical(label, "Hmsy")) {
    list(label = "Hmsy", sym = "H", ref = "Hmsy", ratio = "H/Hmsy", cur = "H_Hmsy.cur",
         name = "harvest rate H", ref_name = "Hmsy",
         definition = "the harvest rate H, with Hmsy = MSY/Bmsy",
         sym_html = "H", ref_html = "H<sub>MSY</sub>", ratio_html = "H/H<sub>MSY</sub>",
         sym_tex = "$H$", ref_tex = "$H_{MSY}$", ratio_tex = "$H/H_{MSY}$",
         other = "F/Fmsy (JABBA convention)")
  } else {
    list(label = "Fmsy", sym = "F", ref = "Fmsy", ratio = "F/Fmsy", cur = "F_Fmsy.cur",
         name = "fishing mortality F", ref_name = "Fmsy",
         definition = "the fishing mortality F (JABBA's convention), with Fmsy = MSY/Bmsy",
         sym_html = "F", ref_html = "F<sub>MSY</sub>", ratio_html = "F/F<sub>MSY</sub>",
         sym_tex = "$F$", ref_tex = "$F_{MSY}$", ratio_tex = "$F/F_{MSY}$",
         other = "H/Hmsy (harvest rate)")
  }
}

jabba_terms_of <- function(jabba_res) {
  lab <- tryCatch(jabba_res$settings$harvest.label, error = function(e) NULL)
  jabba_harvest_terms(if (is.null(lab)) JABBA_HARVEST_LABEL_DEFAULT else lab)
}

read_jabba_csv <- function(path, sep = "auto", dec = "auto") {
  if (is.null(path) || !file.exists(path)) stop("No CSV file was found.")
  seps <- if (identical(sep, "auto")) c(",", ";", "\t") else sep
  decs <- if (identical(dec, "auto")) c(".", ",") else dec

  best <- NULL
  best_score <- -Inf
  best_msg <- NULL
  for (s in seps) {
    for (d in decs) {
      candidate <- try(utils::read.csv(path, sep = s, dec = d, stringsAsFactors = FALSE, check.names = FALSE), silent = TRUE)
      if (inherits(candidate, "try-error")) {
        best_msg <- as.character(candidate)
        next
      }
      score <- ncol(candidate) * 10 + nrow(candidate)
      if (ncol(candidate) >= 2 && nrow(candidate) >= 1 && score > best_score) {
        best <- candidate
        best_score <- score
      }
    }
  }
  if (is.null(best)) stop("Could not read the CSV file. Last error: ", best_msg)
  names(best) <- trimws(names(best))
  best
}

jabba_guess_columns <- function(dat) {
  cols <- names(dat)
  lower <- tolower(cols)
  first_match <- function(pattern, default = NULL) {
    hit <- cols[grep(pattern, lower, perl = TRUE)][1]
    if (length(hit) == 0 || is.na(hit)) default else hit
  }
  se_hits <- cols[grep("(^se$|^se_|_se$|stderr|std|stdev|sd$|^cv$|_cv$)", lower, perl = TRUE)]
  cpue_hits <- cols[grep("cpue|index|indice|abundance|survey|lpue", lower, perl = TRUE)]
  cpue_hits <- setdiff(cpue_hits, se_hits)
  list(
    year = first_match("^year$|annee|année|yr$", cols[1]),
    catch = first_match("catch|capture|landings|landing|debarq|débarq|catch_t", cols[min(2, length(cols))]),
    cpue = cpue_hits,
    se = se_hits
  )
}

jabba_to_numeric <- function(x) {
  if (is.numeric(x)) return(x)
  suppressWarnings(as.numeric(gsub(",", ".", trimws(as.character(x)), fixed = TRUE)))
}

make_jabba_input_data <- function(dat, year_col, catch_col, cpue_cols = character(0), se_cols = character(0),
                                  start_year = NA, end_year = NA) {
  if (is.null(dat) || !is.data.frame(dat)) stop("No input data are available.")
  cpue_cols <- cpue_cols[!is.na(cpue_cols) & nzchar(cpue_cols)]
  se_cols <- se_cols[!is.na(se_cols) & nzchar(se_cols)]
  required <- unique(c(year_col, catch_col, cpue_cols, se_cols))
  missing_cols <- setdiff(required, names(dat))
  if (length(missing_cols) > 0) stop("Missing selected column(s): ", paste(missing_cols, collapse = ", "))
  if (length(cpue_cols) < 1) stop("Select at least one CPUE/index column.")

  yrs <- jabba_to_numeric(dat[[year_col]])
  catch <- data.frame(Year = yrs, Catch = jabba_to_numeric(dat[[catch_col]]))
  cpue <- data.frame(Year = yrs)
  for (i in seq_along(cpue_cols)) cpue[[make.names(cpue_cols[i])]] <- jabba_to_numeric(dat[[cpue_cols[i]]])

  se <- NULL
  if (length(se_cols) > 0) {
    se <- data.frame(Year = yrs)
    for (i in seq_along(se_cols)) se[[make.names(se_cols[i])]] <- jabba_to_numeric(dat[[se_cols[i]]])
    if (ncol(se) != ncol(cpue)) {
      stop("The number of SE columns must match the number of CPUE columns, or SE columns should be left empty.")
    }
  }

  keep <- !is.na(catch$Year)
  catch <- catch[keep, , drop = FALSE]
  cpue <- cpue[keep, , drop = FALSE]
  if (!is.null(se)) se <- se[keep, , drop = FALSE]
  if (!is.na(start_year)) {
    keep <- catch$Year >= start_year
    catch <- catch[keep, , drop = FALSE]
    cpue <- cpue[keep, , drop = FALSE]
    if (!is.null(se)) se <- se[keep, , drop = FALSE]
  }
  if (!is.na(end_year)) {
    keep <- catch$Year <= end_year
    catch <- catch[keep, , drop = FALSE]
    cpue <- cpue[keep, , drop = FALSE]
    if (!is.null(se)) se <- se[keep, , drop = FALSE]
  }

  ord <- order(catch$Year)
  catch <- catch[ord, , drop = FALSE]
  cpue <- cpue[ord, , drop = FALSE]
  if (!is.null(se)) se <- se[ord, , drop = FALSE]

  catch <- stats::aggregate(Catch ~ Year, data = catch, FUN = sum, na.rm = TRUE)
  cpue <- cpue[match(catch$Year, cpue$Year), , drop = FALSE]
  if (!is.null(se)) se <- se[match(catch$Year, se$Year), , drop = FALSE]

  checks <- check_jabba_input(catch, cpue, se)
  list(catch = catch, cpue = cpue, se = se, checks = checks,
       selected_columns = list(year = year_col, catch = catch_col, cpue = cpue_cols, se = se_cols))
}

check_jabba_input <- function(catch, cpue, se = NULL) {
  msg <- character(0)
  ok <- TRUE
  if (nrow(catch) < 3) { ok <- FALSE; msg <- c(msg, "At least 3 years of catch data are required.") }
  if (any(is.na(catch$Year)) || any(is.na(catch$Catch))) { ok <- FALSE; msg <- c(msg, "Year and catch cannot contain missing values.") }
  if (any(catch$Catch < 0, na.rm = TRUE)) { ok <- FALSE; msg <- c(msg, "Catch cannot contain negative values.") }
  if (any(diff(catch$Year) != 1)) { ok <- FALSE; msg <- c(msg, "Catch years must be sequential without gaps for JABBA.") }
  if (ncol(cpue) < 2) { ok <- FALSE; msg <- c(msg, "At least one CPUE/index series is required.") }
  ## Index observations: count every non-missing value across all index series
  ## (JABBA accepts missing years per index) and the years with at least one
  ## observation; a year does not need all series to be observed.
  idx <- !is.na(as.matrix(cpue[, -1, drop = FALSE]))
  nobs <- sum(idx)
  n_obs_years <- sum(rowSums(idx) > 0)
  if (n_obs_years < 2) { ok <- FALSE; msg <- c(msg, "At least two years with a CPUE/index observation are required.") }
  if (!is.null(se)) {
    if (!identical(cpue$Year, se$Year)) { ok <- FALSE; msg <- c(msg, "SE years must match CPUE years.") }
    if (any(se[, -1, drop = FALSE] < 0, na.rm = TRUE)) { ok <- FALSE; msg <- c(msg, "SE values cannot be negative.") }
  }
  if (length(msg) == 0) msg <- c("Input checks passed.")
  list(ok = ok, messages = msg, n_years = nrow(catch), first_year = min(catch$Year), last_year = max(catch$Year), n_indices = ncol(cpue) - 1, n_cpue_obs = nobs, n_cpue_years = n_obs_years)
}

format_jabba_checks <- function(checks) {
  paste(c(
    paste0("Status: ", ifelse(isTRUE(checks$ok), "OK", "Review required")),
    paste0("Years: ", checks$first_year, "-", checks$last_year, " (", checks$n_years, " years)"),
    paste0("Indices: ", checks$n_indices),
    paste0("Index observations (non-missing): ", checks$n_cpue_obs, " in ", checks$n_cpue_years, " years"),
    "Messages:",
    paste0("- ", checks$messages)
  ), collapse = "\n")
}

## Advanced settings (fixed.obsE, proc.dev.all, P_bound).
## The UI box is currently hidden (ui/SPM/jabba/jabbaUI.R) and the feature is
## switched off here: while the flag is FALSE every input value is ignored and
## JABBA's package defaults apply, so values retained by a browser, bookmarked
## or injected can never reach build_jabba(). Validation scripts set the flag
## to TRUE to exercise the implementation directly.
JABBA_ADVANCED_SETTINGS_ENABLED <- FALSE

jabba_advanced_enabled <- function() {
  isTRUE(get0("JABBA_ADVANCED_SETTINGS_ENABLED", ifnotfound = FALSE))
}

jabba_default_advanced <- function() list(fixed_obsE = NULL, proc_dev_all = TRUE, P_bound = NULL)

jabba_advanced_settings <- function(input, enabled = jabba_advanced_enabled()) {
  if (!isTRUE(enabled)) return(jabba_default_advanced())
  ## Inputs may also be absent (older callers / tests): then JABBA's defaults apply.
  use_default_obsE <- is.null(input$obsE_default) || isTRUE(input$obsE_default)
  list(
    fixed_obsE = if (use_default_obsE) NULL else suppressWarnings(as.numeric(input$fixed_obsE)),
    proc_dev_all = if (is.null(input$proc_dev_all)) TRUE else isTRUE(input$proc_dev_all),
    P_bound = if (is.null(input$P_bound_lo) || is.null(input$P_bound_hi)) NULL else suppressWarnings(as.numeric(c(input$P_bound_lo, input$P_bound_hi)))
  )
}

## Validation of the advanced settings. "required" conditions are those without
## which JABBA's model cannot be built or evaluated (fixed.obsE enters as
## fixed.obsE^2; P_bound enters as log(K * P_bound), so both bounds must be
## positive and ordered). Practical ranges are only suggested in the help text,
## never enforced, so documented configurations stay possible.
jabba_check_advanced <- function(adv) {
  msg <- character(0)
  if (!is.null(adv$fixed_obsE)) {
    if (length(adv$fixed_obsE) != 1 || is.na(adv$fixed_obsE) || !is.finite(adv$fixed_obsE)) msg <- c(msg, "Minimum observation error must be a number (or use the JABBA default).")
    else if (adv$fixed_obsE < 0) msg <- c(msg, "Minimum observation error cannot be negative (0 removes the floor).")
  }
  if (!is.null(adv$P_bound)) {
    pb <- adv$P_bound
    if (length(pb) != 2 || any(is.na(pb)) || any(!is.finite(pb))) msg <- c(msg, "Both B/K penalty bounds must be numbers.")
    else {
      if (pb[1] <= 0) msg <- c(msg, "The lower B/K penalty bound must be positive (JABBA evaluates log(K * bound)).")
      if (pb[2] <= pb[1]) msg <- c(msg, "The upper B/K penalty bound must be larger than the lower bound.")
    }
  }
  list(ok = length(msg) == 0, messages = msg)
}

build_jabba_settings <- function(input, years) {
  b_prior <- c(FALSE, 0.5, NA, "bk")
  if (isTRUE(input$useBprior)) {
    byr <- suppressWarnings(as.numeric(input$BpriorYear))
    if (is.na(byr)) byr <- max(years)
    b_prior <- c(input$BpriorMu, input$BpriorCv, byr, input$BpriorType)
  }
  settings <- list(
    assessment = input$assessment_name,
    scenario = input$scenario_name,
    model.type = input$model_type,
    add.catch.CV = isTRUE(input$addCatchCV),
    catch.cv = input$catchCV,
    catch.error = input$catch_error,
    Plim = input$Plim,
    r.dist = input$r_dist,
    r.prior = c(input$r_mu, input$r_cv),
    K.dist = input$K_dist,
    K.prior = if (isTRUE(input$useKprior)) c(input$Kmu, input$Kcv) else NULL,
    psi.dist = input$psi_dist,
    psi.prior = c(input$psi_mu, input$psi_cv),
    b.prior = b_prior,
    sigma.est = isTRUE(input$sigma_est),
    sigma.proc = isTRUE(input$sigma_proc),
    proc.dyn = isTRUE(input$proc_dyn),
    catch.metric = input$catch_metric,
    ## JABBA's harvest.label option: the user's terminology choice, default "Fmsy".
    harvest.label = jabba_harvest_label(input)
  )
  ## Advanced settings: only included when they differ from JABBA's defaults,
  ## so that the default call to build_jabba() is unchanged (JABBA's own
  ## defaults apply and are reported as such).
  adv <- jabba_advanced_settings(input)
  if (!is.null(adv$fixed_obsE)) settings$fixed.obsE <- adv$fixed_obsE
  if (!isTRUE(adv$proc_dev_all)) settings$proc.dev.all <- FALSE
  if (!is.null(adv$P_bound) && !isTRUE(all.equal(adv$P_bound, jabba_default_P_bound()))) settings$P_bound <- adv$P_bound
  settings
}

## JABBA's default soft bounds on B/K, read from the pinned package itself.
jabba_default_P_bound <- function() {
  d <- tryCatch(eval(formals(JABBA::build_jabba)$P_bound), error = function(e) NULL)
  if (is.numeric(d) && length(d) == 2) d else c(0.02, 1.3)
}

## Requested versus effective advanced settings, the latter taken from the
## object build_jabba() returned (jagsdata$P_bound, jagsdata$stI, jagsdata$SE2).
jabba_effective_advanced <- function(jbinput, jdat, settings) {
  jd <- jbinput$jagsdata
  se_supplied <- !is.null(jdat$se)
  ## effective minimum observation error, backed out of JABBA's SE2 matrix:
  ## without SE columns SE2 = fixed.obsE^2; with SE columns SE2 = SE^2 + fixed.obsE^2.
  eff_obsE <- NA_real_
  if (!is.null(jd$SE2)) {
    if (!se_supplied) eff_obsE <- sqrt(jd$SE2[1, 1])
    else {
      sem <- as.matrix(jdat$se[, -1, drop = FALSE]); cell <- which(!is.na(sem))[1]
      if (!is.na(cell)) eff_obsE <- sqrt(max(jd$SE2[cell] - sem[cell]^2, 0))
    }
  }
  list(
    enabled = jabba_advanced_enabled(),
    fixed_obsE = list(requested = if (is.null(settings$fixed.obsE)) "JABBA default" else settings$fixed.obsE,
                      effective = eff_obsE,
                      default_rule = if (se_supplied) "0.001 (SE columns supplied)" else "0.1 (no SE columns)"),
    proc_dev_all = list(requested = if (is.null(settings$proc.dev.all)) TRUE else settings$proc.dev.all,
                        effective = isTRUE(jbinput$settings$proc.dev.all),
                        start_index = if (!is.null(jd$stI)) as.integer(jd$stI) else NA_integer_,
                        start_year = if (!is.null(jd$stI) && !is.null(jbinput$data$yr)) jbinput$data$yr[as.integer(jd$stI)] else NA),
    P_bound = list(requested = if (is.null(settings$P_bound)) "JABBA default" else settings$P_bound,
                   effective = if (!is.null(jd$P_bound)) as.numeric(jd$P_bound) else NA)
  )
}

## ---- MCMC convergence diagnostics: chain-ordered draws captured from the fit ----
## JABBA::fit_jabba() keeps only R2jags' randomly permuted pooled draws
## (`sims.list`) and drops the R2jags object, which holds the chain-ordered draws
## (`sims.array`), so no convergence diagnostic can be computed from the fitted
## object (external report, 13.3 and 15.4). To obtain valid diagnostics for the
## SAME fit, run_jabba_model() fits the model through a per-call adapted copy of
## the pinned fit_jabba() function: the installed function object with ONE
## statement inserted after its single `mod <- R2jags::jags(...)` call, which
## stores `mod` in an environment private to that call. Nothing installed is
## modified (no package source, no namespace, no tracing); the copy keeps JABBA's
## namespace as its enclosing environment, so JABBA's internal helpers resolve
## unchanged, and the fit is bit-identical to a plain fit_jabba() call at the
## same seed (validated). Guards: the installed JABBA must be the pinned version
## and commit and the function body must have exactly the expected structure;
## otherwise, and on any error while extracting the diagnostics, the assessment
## and its outputs are kept, the diagnostics are reported as unavailable with the
## reason, and the fit is never repeated. Only the chain-ordered draws of the
## parameters shown in the diagnostics are retained (< 2 MB); the R2jags object
## is released immediately. This is an explicitly approved, temporary adaptation,
## to be removed once JABBA returns the chain-ordered draws itself.
JABBA_MCMC_DIAGNOSTICS_ENABLED <- TRUE
JABBA_PINNED_VERSION <- "2.3.7"
JABBA_PINNED_SHA <- "c62db86c99dc5b5a0e115fd53fdd40f7c882b7c9"

jabba_mcmc_diagnostics_enabled <- function() isTRUE(get0("JABBA_MCMC_DIAGNOSTICS_ENABLED", ifnotfound = TRUE))

## Guard: pinned version and commit, and the expected body structure of fit_jabba().
## `fun` is only given by tests (structure checks on synthetic functions).
jabba_capture_guard <- function(fun = NULL,
                                expected_version = get0("JABBA_PINNED_VERSION", ifnotfound = NA_character_),
                                expected_sha = get0("JABBA_PINNED_SHA", ifnotfound = NA_character_)) {
  fail <- function(reason) list(ok = FALSE, reason = reason, index = NA_integer_)
  if (!requireNamespace("JABBA", quietly = TRUE)) return(fail("the JABBA package is not installed"))
  desc <- tryCatch(utils::packageDescription("JABBA"), error = function(e) NULL)
  if (is.null(desc)) return(fail("the JABBA package description could not be read"))
  if (!identical(as.character(desc$Version), as.character(expected_version))) {
    return(fail(sprintf("installed JABBA version %s is not the pinned version %s", desc$Version, expected_version)))
  }
  sha <- if (is.null(desc$RemoteSha)) NA_character_ else as.character(desc$RemoteSha)
  if (is.na(sha) || !identical(sha, as.character(expected_sha))) {
    return(fail(sprintf("installed JABBA commit %s is not the pinned commit %s", if (is.na(sha)) "unknown" else substr(sha, 1, 8), substr(as.character(expected_sha), 1, 8))))
  }
  if (is.null(fun)) {
    fun <- get0("fit_jabba", envir = asNamespace("JABBA"), mode = "function", inherits = FALSE)
    if (!is.function(fun)) return(fail("JABBA::fit_jabba was not found"))
    if (!identical(environment(fun), asNamespace("JABBA"))) return(fail("fit_jabba is not a JABBA namespace function"))
  }
  b <- body(fun)
  if (!is.call(b) || !identical(b[[1]], as.name("{"))) return(fail("fit_jabba has an unexpected body form"))
  stmts <- as.list(b)[-1]
  is_target <- vapply(stmts, function(e) {
    is.call(e) && identical(e[[1]], as.name("<-")) && identical(e[[2]], as.name("mod")) &&
      is.call(e[[3]]) && identical(e[[3]][[1]], quote(R2jags::jags))
  }, logical(1))
  if (sum(is_target) != 1) {
    return(fail(sprintf("expected exactly one `mod <- R2jags::jags(...)` statement in fit_jabba, found %d", sum(is_target))))
  }
  reads <- vapply(stmts, function(e) any(grepl("mod$BUGSoutput$sims.list", deparse(e), fixed = TRUE)), logical(1))
  if (!any(reads) || which(reads)[1] <= which(is_target)) return(fail("fit_jabba does not read mod$BUGSoutput$sims.list after the JAGS call as expected"))
  list(ok = TRUE, reason = NULL, index = which(is_target))
}

## The adapted copy: JABBA::fit_jabba with `assign("mod", mod, envir = capture_env)`
## inserted after the JAGS call. Self-checked to differ by that statement only.
jabba_capturing_fit <- function(capture_env, guard = jabba_capture_guard()) {
  if (!isTRUE(guard$ok)) stop(guard$reason)
  orig <- get("fit_jabba", envir = asNamespace("JABBA"), mode = "function")
  f <- orig
  stmts <- as.list(body(f))                       # element 1 is `{`
  ins <- call("assign", "mod", as.name("mod"), envir = capture_env)
  at <- guard$index + 1L
  body(f) <- as.call(append(stmts, list(ins), after = at))
  ## self-check, ignoring the source-reference attributes of the installed body
  strip <- function(l) { attributes(l) <- NULL; l }
  if (!identical(strip(as.list(body(f))[-(at + 1L)]), strip(stmts)) || !identical(formals(f), formals(orig)) ||
      !identical(environment(f), asNamespace("JABBA"))) stop("the adapted copy of fit_jabba failed its self-check")
  f
}

jabba_convergence_unavailable <- function(reason) {
  list(available = FALSE, reason = as.character(reason))
}

## Monitored JAGS nodes shown in the diagnostics: model parameters, catchability
## and observation variance per index, reference points and the final-year status.
jabba_mcmc_keys <- function(node_names, n_years) {
  idx <- node_names[grepl("^(tau2|q)\\[[0-9]+\\]$", node_names)]
  idx <- idx[order(sub("\\[.*", "", idx), as.integer(sub(".*\\[([0-9]+)\\]", "\\1", idx)))]
  ## with a single index JAGS names the catchability and observation-variance nodes "q" and "tau2" (no index)
  k <- c("K", "r", "psi", "sigma2", "m", "q", "tau2", idx, "Hmsy", "SBmsy", "MSY", sprintf("BtoBmsy[%d]", n_years), sprintf("HtoHmsy[%d]", n_years))
  k[k %in% node_names]
}

## Diagnostics from the captured R2jags object. Estimators (named explicitly in
## every output; they are not interchangeable):
##  - "R-hat": the conventional Gelman-Rubin potential scale reduction factor with
##    the Brooks-Gelman degrees-of-freedom correction, coda::gelman.diag() on the
##    whole post-burn-in chains, raw scale (no split chains, no rank normalisation;
##    it is NOT the rank-normalised split-R-hat of Vehtari et al. 2021). Needs 2+ chains.
##  - "ESS": autocorrelation-based effective sample size, coda::effectiveSize():
##    per chain n x var / spectral density at frequency zero (AR fit), summed over chains.
##  - For reference only (kept in the object, not shown): R2jags' own summary,
##    where R-hat is computed on the log scale for all-positive nodes and "n.eff"
##    is the between-chain formula m*n*min(sigma2hat/B, 1) of R2WinBUGS, rounded.
jabba_convergence_from_mod <- function(mod, jbinput, fit, thresholds = list(rhat = 1.05, ess = 400)) {
  if (!inherits(mod, "rjags") || is.null(mod$BUGSoutput)) stop("the captured object is not an R2jags fit")
  bo <- mod$BUGSoutput
  sa <- bo$sims.array
  if (!is.array(sa) || length(dim(sa)) != 3 || is.null(dimnames(sa)[[3]])) stop("the captured draws are not a chain-ordered array")
  n_years <- length(fit$yr)
  keys <- jabba_mcmc_keys(dimnames(sa)[[3]], n_years)
  if (length(keys) == 0) stop("none of the expected parameter nodes was found in the captured draws")
  draws <- sa[, , keys, drop = FALSE]
  ## back to the reported units: fit_jabba() scales the catch by its mean before JAGS
  mucatch <- mean(jbinput$jagsdata$TC, na.rm = TRUE)
  if (!is.finite(mucatch) || mucatch <= 0) stop("catch scaling factor not available")
  for (k in intersect(keys, c("K", "SBmsy", "MSY"))) draws[, , k] <- draws[, , k] * mucatch
  for (k in keys[grepl("^q(\\[|$)", keys)]) draws[, , k] <- draws[, , k] / mucatch
  n_keep <- dim(draws)[1]; n_chains <- dim(draws)[2]
  dimnames(draws) <- list(NULL, paste0("chain", seq_len(n_chains)), keys)
  thin <- if (!is.null(bo$n.thin)) as.integer(bo$n.thin) else 1L
  burn <- if (!is.null(bo$n.burnin)) as.integer(bo$n.burnin) else 0L
  varying <- vapply(keys, function(k) isTRUE(stats::var(as.vector(draws[, , k])) > 0), logical(1))
  kv <- keys[varying]
  chain_mcmc <- function(ch) coda::mcmc(matrix(draws[, ch, kv], nrow = n_keep, ncol = length(kv), dimnames = list(NULL, kv)), start = burn + thin, thin = thin)
  ml <- coda::mcmc.list(lapply(seq_len(n_chains), chain_mcmc))
  ess <- coda::effectiveSize(ml)
  rhat <- rhat_upper <- stats::setNames(rep(NA_real_, length(kv)), kv)
  if (n_chains >= 2) {
    gd <- coda::gelman.diag(ml, confidence = 0.95, transform = FALSE, autoburnin = FALSE, multivariate = FALSE)$psrf
    rhat[rownames(gd)] <- gd[, "Point est."]; rhat_upper[rownames(gd)] <- gd[, "Upper C.I."]
  }
  geweke <- tryCatch(sapply(ml, function(ch) coda::geweke.diag(ch)$z), error = function(e) NULL)
  if (is.matrix(geweke)) dimnames(geweke) <- list(kv, paste0("chain", seq_len(n_chains)))
  sm <- bo$summary
  r2jags <- if (is.matrix(sm) && all(c("Rhat", "n.eff") %in% colnames(sm))) sm[intersect(kv, rownames(sm)), c("Rhat", "n.eff"), drop = FALSE] else NULL
  tab <- data.frame(node = keys, varying = unname(varying),
                    rhat = unname(rhat[keys]), rhat_upper = unname(rhat_upper[keys]), ess = unname(ess[keys]),
                    stringsAsFactors = FALSE, row.names = NULL)
  tab$flag <- vapply(seq_len(nrow(tab)), function(i) {
    if (!tab$varying[i]) return("fixed (not estimated)")
    f <- character(0)
    if (!is.na(tab$rhat[i]) && tab$rhat[i] > thresholds$rhat) f <- c(f, sprintf("R-hat > %s", format(thresholds$rhat)))
    if (!is.na(tab$ess[i]) && tab$ess[i] < thresholds$ess) f <- c(f, sprintf("ESS < %d", as.integer(thresholds$ess)))
    if (n_chains < 2) f <- c(f, "R-hat needs 2+ chains")
    if (length(f)) paste(f, collapse = "; ") else ""
  }, character(1))
  list(available = TRUE,
       method = "chain-ordered draws captured from this run's own JAGS fit through a per-call adapted copy of JABBA::fit_jabba (pinned version)",
       jabba_version = as.character(utils::packageVersion("JABBA")), jabba_sha = JABBA_PINNED_SHA,
       draws = draws, keys = keys, varying = varying,
       n_chains = n_chains, n_keep = n_keep, n_iter = if (!is.null(bo$n.iter)) as.integer(bo$n.iter) else NA_integer_, n_burnin = burn, n_thin = thin,
       table = tab, geweke_z = geweke, r2jags_summary = r2jags, thresholds = thresholds,
       estimators = list(rhat = "Gelman-Rubin potential scale reduction factor (conventional R-hat; coda::gelman.diag, Brooks-Gelman df correction, whole chains, raw scale; not the rank-normalised split-R-hat)",
                         ess = "autocorrelation-based effective sample size (coda::effectiveSize: AR spectral density at frequency zero, summed over chains)"),
       n_flagged = sum(nzchar(tab$flag) & tab$varying))
}

jabba_mcmc_node_label <- function(node, tm, yrs) {
  yN <- max(yrs)
  if (node == "Hmsy") return(sprintf("%s (%s at MSY)", tm$ref, tm$name))
  if (node == "SBmsy") return("Bmsy")
  if (node == "sigma2") return("sigma2 (process variance)")
  if (node == "psi") return(sprintf("psi (B%s/K)", min(yrs)))
  if (node == "m") return("m (shape)")
  if (grepl("^BtoBmsy\\[", node)) return(sprintf("B%s/Bmsy", yN))
  if (grepl("^HtoHmsy\\[", node)) return(sprintf("%s%s/%s", tm$sym, yN, tm$ref))
  if (node == "tau2") return("tau2 (observation variance)")
  if (node == "q") return("q (catchability)")
  if (grepl("^tau2\\[", node)) return(sprintf("tau2 (obs. variance, index %s)", sub(".*\\[([0-9]+)\\]", "\\1", node)))
  if (grepl("^q\\[", node)) return(sprintf("q (catchability, index %s)", sub(".*\\[([0-9]+)\\]", "\\1", node)))
  node
}

## User-facing diagnostics table (UI, PDF, zip CSV): parameter, R-hat (point and
## 97.5 % upper limit), ESS and flags; NULL when diagnostics are unavailable.
jabba_convergence_table <- function(jabba_res) {
  cv <- jabba_res$convergence
  if (is.null(cv) || !isTRUE(cv$available)) return(NULL)
  tm <- jabba_terms_of(jabba_res)
  tab <- cv$table
  data.frame(parameter = vapply(tab$node, jabba_mcmc_node_label, character(1), tm = tm, yrs = jabba_res$fit$yr),
             node = tab$node,
             Rhat = round(tab$rhat, 3), Rhat_upper = round(tab$rhat_upper, 3), ESS = round(tab$ess),
             flag = tab$flag, stringsAsFactors = FALSE, row.names = NULL)
}

jabba_convergence_summary <- function(jabba_res) {
  cv <- jabba_res$convergence
  if (is.null(cv) || !isTRUE(cv$available)) return(NULL)
  tab <- cv$table[cv$table$varying, , drop = FALSE]
  list(n_chains = cv$n_chains, n_keep = cv$n_keep, n_iter = cv$n_iter, n_burnin = cv$n_burnin, n_thin = cv$n_thin,
       n_params = nrow(tab), n_flagged = sum(nzchar(tab$flag)),
       rhat_max = if (all(is.na(tab$rhat))) NA_real_ else max(tab$rhat, na.rm = TRUE),
       ess_min = if (all(is.na(tab$ess))) NA_real_ else min(tab$ess, na.rm = TRUE),
       thresholds = cv$thresholds)
}

## Statement used in the PDF and the UI. Diagnostics are described as what they
## are: they can reveal some failures to converge; they never prove convergence.
jabba_convergence_statement <- function(jabba_res = NULL) {
  cv <- if (is.null(jabba_res)) NULL else jabba_res$convergence
  if (is.null(cv) || !isTRUE(cv$available)) {
    reason <- if (!is.null(cv$reason)) paste0(" Reason for this run: ", cv$reason, ".") else ""
    return(paste0("For this run the chain-ordered MCMC draws are not available: the fitted object of this JABBA version holds the pooled posterior draws in random order (R2jags sims.list), ",
                  "so trace plots, Gelman-Rubin R-hat, effective sample sizes and order-based tests cannot be computed and are not reported.", reason,
                  " Confirm important results with longer runs and different seeds, or run JABBA directly with chain-level diagnostics."))
  }
  s <- jabba_convergence_summary(jabba_res)
  paste0("Convergence diagnostics were computed on the chain-ordered, post-burn-in, thinned draws of this run's own JAGS fit (", s$n_chains, " chain", if (s$n_chains != 1) "s" else "", " x ", format(s$n_keep, big.mark = ","), " kept draws). ",
         "R-hat is the conventional Gelman-Rubin potential scale reduction factor (coda::gelman.diag, Brooks-Gelman correction, whole chains, no rank normalisation); ",
         "ESS is the autocorrelation-based effective sample size (coda::effectiveSize) summed over chains. ",
         "Values with R-hat above ", format(s$thresholds$rhat), " or ESS below ", format(s$thresholds$ess), " are flagged (", s$n_flagged, " of ", s$n_params, " parameters flagged in this run). ",
         "These diagnostics can reveal some failures of the sampler to converge; favourable diagnostics do not prove convergence, and a quick MCMC run is never a basis for advice.")
}

## One-paragraph status for the run-status box.
jabba_convergence_status_text <- function(jabba_res) {
  cv <- jabba_res$convergence
  if (is.null(cv) || !isTRUE(cv$available)) {
    return(paste0("MCMC convergence cannot be assessed for this run (", if (!is.null(cv$reason)) cv$reason else "chain-ordered draws not available", "): review index fits, residuals and sensitivity to seeds and priors before using outputs for scientific advice."))
  }
  s <- jabba_convergence_summary(jabba_res)
  paste0("MCMC diagnostics (R-hat, ESS, trace plots) for ", s$n_params, " parameters are in the Convergence tab and in the PDF: ",
         if (s$n_flagged > 0) sprintf("%d flagged (max R-hat %s, min ESS %s). ", s$n_flagged, formatC(s$rhat_max, digits = 3, format = "f"), formatC(s$ess_min, digits = 0, format = "f")) else sprintf("none flagged (max R-hat %s, min ESS %s). ", formatC(s$rhat_max, digits = 3, format = "f"), formatC(s$ess_min, digits = 0, format = "f")),
         "Favourable diagnostics do not prove convergence: review index fits, residuals and sensitivity to seeds and priors before using outputs for scientific advice.")
}

## Trace plots of the retained chain-ordered draws, one panel per parameter,
## chains distinguished by colour; x = MCMC iteration (after burn-in, thinned).
jabba_plot_traces <- function(jabba_res, nodes = NULL, ncol = 3, cex = 0.9) {
  cv <- jabba_res$convergence
  if (is.null(cv) || !isTRUE(cv$available)) {
    graphics::plot.new(); graphics::text(0.5, 0.5, "Trace plots are not available for this run:\nchain-ordered draws could not be captured.", cex = cex); return(invisible(NULL))
  }
  tm <- jabba_terms_of(jabba_res)
  if (is.null(nodes)) nodes <- cv$keys[cv$varying]
  nodes <- nodes[nodes %in% cv$keys]
  if (length(nodes) == 0) { graphics::plot.new(); graphics::text(0.5, 0.5, "No varying parameter to display."); return(invisible(NULL)) }
  it <- cv$n_burnin + cv$n_thin * seq_len(cv$n_keep)
  cols <- grDevices::hcl.colors(max(cv$n_chains, 2), "Dark 3")
  oldpar <- graphics::par(no.readonly = TRUE); on.exit(graphics::par(oldpar), add = TRUE)
  graphics::par(mfrow = c(ceiling(length(nodes) / ncol), ncol), mar = c(3.2, 3.2, 2.8, 0.6), mgp = c(2, 0.6, 0), cex = cex)
  for (nd in nodes) {
    y <- cv$draws[, , nd, drop = FALSE]
    row <- cv$table[cv$table$node == nd, ]
    graphics::matplot(it, matrix(y, nrow = cv$n_keep), type = "l", lty = 1, lwd = 0.6, col = cols[seq_len(cv$n_chains)],
                      xlab = "MCMC iteration", ylab = "", main = "")
    graphics::title(main = jabba_mcmc_node_label(nd, tm, jabba_res$fit$yr), line = 1.3, cex.main = 0.95)
    sub <- if (!is.na(row$rhat)) sprintf("R-hat %.3f, ESS %.0f", row$rhat, row$ess) else sprintf("ESS %.0f", row$ess)
    graphics::mtext(sub, side = 3, line = 0.25, cex = 0.7 * cex, col = if (nzchar(row$flag)) "red3" else "grey30")
  }
  invisible(NULL)
}

run_jabba_model <- function(jdat, input, source = NULL) {
  ## `source` (optional): list(file_name, md5, is_bundled_sample) describing the
  ## uploaded file. Everything needed to describe the completed run (data,
  ## priors, settings, seed, versions, warnings) is captured here so that the
  ## report describes this run even if the user changes the controls later.
  if (!requireNamespace("JABBA", quietly = TRUE)) {
    stop("The JABBA R package is not installed. Install JAGS/rjags and JABBA, for example: remotes::install_github('jabbamodel/JABBA').")
  }
  output_dir <- tempfile("smt_jabba_")
  dir.create(output_dir, recursive = TRUE, showWarnings = FALSE)
  settings <- build_jabba_settings(input, jdat$catch$Year)
  requested <- list(ni = input$ni, nb = input$nb, nt = input$nt, nc = input$nc)
  seed <- input$seed
  warnings_seen <- character(0)
  capture_warning <- function(w) {
    warnings_seen <<- c(warnings_seen, conditionMessage(w))
    invokeRestart("muffleWarning")
  }

  jbinput <- withCallingHandlers(
    do.call(JABBA::build_jabba, c(list(
      catch = jdat$catch,
      cpue = jdat$cpue,
      se = jdat$se,
      verbose = FALSE
    ), settings)),
    warning = capture_warning
  )

  ## Fit through the per-call adapted copy of fit_jabba() when the guard passes;
  ## otherwise through JABBA::fit_jabba() itself. One fit in either case.
  capture_env <- new.env(parent = emptyenv())
  fitter <- JABBA::fit_jabba
  capture_problem <- NULL
  if (!jabba_mcmc_diagnostics_enabled()) {
    capture_problem <- "diagnostics capture is switched off in this installation"
  } else {
    guard <- jabba_capture_guard()
    if (!isTRUE(guard$ok)) {
      capture_problem <- guard$reason
    } else {
      fitter <- tryCatch(jabba_capturing_fit(capture_env, guard), error = function(e) {
        capture_problem <<- paste("the adapted copy of fit_jabba could not be built:", conditionMessage(e)); JABBA::fit_jabba
      })
    }
  }
  fit <- withCallingHandlers(
    fitter(
      jbinput,
      ni = requested$ni,
      nt = requested$nt,
      nb = requested$nb,
      nc = requested$nc,
      quickmcmc = isTRUE(input$quickmcmc),
      do.ppc = isTRUE(input$do_ppc),
      save.trj = TRUE,
      save.all = FALSE,
      save.jabba = FALSE,
      save.csvs = TRUE,
      output.dir = output_dir,
      seed = seed,
      verbose = FALSE
    ),
    warning = capture_warning
  )

  convergence <- if (!is.null(capture_problem)) {
    jabba_convergence_unavailable(capture_problem)
  } else {
    tryCatch(jabba_convergence_from_mod(capture_env$mod, jbinput, fit),
             error = function(e) jabba_convergence_unavailable(paste("the diagnostics could not be extracted from the captured fit:", conditionMessage(e))))
  }
  rm(list = ls(capture_env, all.names = TRUE), envir = capture_env)   # release the R2jags object
  if (!isTRUE(convergence$available) && exists("flog.warn", mode = "function")) flog.warn("JABBA convergence diagnostics unavailable for this run: %s", convergence$reason)

  if (isTRUE(input$save_png)) safe_jabba_plots(fit, output_dir)
  advanced <- jabba_effective_advanced(jbinput, jdat, settings)
  run_time <- Sys.time()
  run_id <- sub("^smt_jabba_", "", basename(output_dir))
  list(
    input = jbinput, fit = fit, output_dir = output_dir, settings = settings, data = jdat,
    advanced = advanced,
    convergence = convergence,
    run_time = run_time, run_id = run_id,
    source = source,
    mcmc = list(
      requested = requested,
      used = fit$settings$mcmc[c("ni", "nb", "nt", "nc")],
      n_saved = if (is.data.frame(fit$pars_posterior)) nrow(fit$pars_posterior) else NA_integer_,
      quickmcmc = isTRUE(input$quickmcmc),
      do_ppc = isTRUE(input$do_ppc),
      seed = seed
    ),
    warnings = unique(warnings_seen),
    versions = jabba_versions(),
    report_file = file.path(output_dir, jabba_report_filename(run_time, run_id))
  )
}

jabba_versions <- function() {
  pkg_version <- function(p) tryCatch(as.character(utils::packageVersion(p)), error = function(e) NA_character_)
  desc <- tryCatch(utils::packageDescription("JABBA"), error = function(e) NULL)
  smt <- tryCatch(jsonlite::read_json("package.json"), error = function(e) NULL)
  list(
    r = R.version.string,
    platform = R.version$platform,
    smt_version = if (!is.null(smt$version)) as.character(smt$version) else NA_character_,
    smt_date = if (!is.null(smt$date)) as.character(smt$date) else NA_character_,
    jabba = pkg_version("JABBA"),
    jabba_sha = if (!is.null(desc) && !is.null(desc$RemoteSha)) as.character(desc$RemoteSha) else NA_character_,
    rjags = pkg_version("rjags"),
    R2jags = pkg_version("R2jags"),
    coda = pkg_version("coda"),
    jags = tryCatch(as.character(rjags::jags.version()), error = function(e) NA_character_)
  )
}

jabba_report_filename <- function(run_time = Sys.time(), run_id = "") {
  ## One name per run: timestamp (as the other SMT reports) plus the run
  ## identifier, so two runs in the same second cannot collide.
  paste0("JABBA_report_", format(run_time, "%Y%m%d_%H%M_%s"), if (nzchar(run_id)) paste0("_", run_id), ".pdf")
}

jabba_redact_secrets <- function(msg, extra = NULL) {
  ## Remove credential material from messages before they are logged or shown.
  ## StoragehubManager error messages can embed the upload URL, which carries
  ## the gcube-token as a query parameter.
  msg <- paste(as.character(msg), collapse = " ")
  msg <- gsub("(gcube-token=)[^&'\"[:space:]]+", "\\1<redacted>", msg)
  msg <- gsub("(Bearer )[^'\"[:space:]]+", "\\1<redacted>", msg)
  for (s in extra) if (!is.null(s) && length(s) == 1 && !is.na(s) && nzchar(s)) msg <- gsub(s, "<redacted>", msg, fixed = TRUE)
  msg
}

safe_jabba_call <- function(fun, ..., output.dir = getwd()) {
  if (!requireNamespace("JABBA", quietly = TRUE)) return(invisible(FALSE))
  if (!exists(fun, where = asNamespace("JABBA"), mode = "function")) return(invisible(FALSE))
  f <- get(fun, envir = asNamespace("JABBA"), mode = "function")
  tryCatch({ f(..., output.dir = output.dir, as.png = TRUE, verbose = FALSE); TRUE }, error = function(e) FALSE)
}

safe_jabba_plots <- function(fit, output_dir) {
  dir.create(output_dir, recursive = TRUE, showWarnings = FALSE)
  invisible(c(
    safe_jabba_call("jbplot_catch", fit, output.dir = output_dir),
    safe_jabba_call("jbplot_cpuefits", fit, output.dir = output_dir),
    safe_jabba_call("jbplot_summary", fit, output.dir = output_dir),
    safe_jabba_call("jbplot_kobe", fit, output.dir = output_dir),
    safe_jabba_call("jbplot_residuals", fit, output.dir = output_dir),
    safe_jabba_call("jbplot_stdresiduals", fit, output.dir = output_dir),
    safe_jabba_call("jbplot_ppdist", fit, output.dir = output_dir),
    safe_jabba_call("jbplot_mcmc", fit, output.dir = output_dir)
  ))
}

jabba_quantity_labels <- function(jabba_res) {
  ## Labels for JABBA's `estimates` rows in the chosen terminology (F, JABBA's
  ## convention, or H).
  tm <- jabba_terms_of(jabba_res)
  yrs <- jabba_res$fit$yr
  y1 <- min(yrs); yN <- max(yrs)
  labs <- c(
    K = "K: carrying capacity (catch units)",
    r = "r: intrinsic rate of population increase",
    psi = sprintf("psi: initial depletion B%s/K", y1),
    sigma.proc = "sigma.proc: process error standard deviation (log scale)",
    m = "m: production-function shape parameter",
    Hmsy = sprintf("%s: %s at MSY", tm$ref, tm$name),
    SBmsy = "Bmsy: biomass at MSY",
    MSY = "MSY: maximum sustainable yield",
    bmsyk = "Bmsy/K"
  )
  labs[paste0("P", y1)] <- sprintf("B%s/K", y1)
  labs[paste0("P", yN)] <- sprintf("B%s/K", yN)
  labs["B_Bmsy.cur"] <- sprintf("B%s/Bmsy", yN)
  labs["H_Hmsy.cur"] <- sprintf("%s%s/%s", tm$sym, yN, tm$ref)
  labs
}

## JABBA's estimates row names ("Hmsy", "H_Hmsy.cur") shown in the chosen terminology.
jabba_quantity_names <- function(q, jabba_res) {
  tm <- jabba_terms_of(jabba_res)
  q[q == "Hmsy"] <- tm$ref
  q[q == "H_Hmsy.cur"] <- tm$cur
  q
}

jabba_summary_table <- function(jabba_res) {
  ## Posterior medians and 95 % credible intervals (2.5 % and 97.5 %
  ## posterior quantiles) of parameters and reference points, from the
  ## `estimates` element of the fitted JABBA object.
  fit <- jabba_res$fit
  est <- fit$estimates
  if (is.data.frame(est) && nrow(est) > 0) {
    labs <- jabba_quantity_labels(jabba_res)
    q <- rownames(est)
    desc <- unname(labs[q]); desc[is.na(desc)] <- ""
    return(data.frame(quantity = jabba_quantity_names(q, jabba_res), description = desc,
                      median = est$mu, lower_95 = est$lci, upper_95 = est$uci,
                      stringsAsFactors = FALSE, row.names = NULL))
  }
  data.frame(quantity = character(0), description = character(0), median = numeric(0),
             lower_95 = numeric(0), upper_95 = numeric(0))
}

jabba_parameter_table <- function(jabba_res) {
  ## JABBA's per-parameter table: posterior median and 95 % CI. The Geweke.p and
  ## Heidel.p columns of fit$pars are deliberately NOT reported: JABBA computes
  ## them on R2jags' randomly permuted pooled draws, so they cannot indicate
  ## convergence. They remain in the downloadable fitted object unchanged.
  p <- jabba_res$fit$pars
  if (!is.data.frame(p) || nrow(p) == 0) return(NULL)
  data.frame(parameter = rownames(p), median = p$Median, lower_95 = p$LCI, upper_95 = p$UCI,
             stringsAsFactors = FALSE, row.names = NULL)
}

jabba_timeseries_table <- function(jabba_res) {
  ## Annual posterior medians and 95 % CIs. The JABBA array slot named "F"
  ## holds the annual catch/biomass ratio; its columns are named in the chosen
  ## terminology (F_ or H_ prefix).
  ts <- jabba_res$fit$timeseries
  yrs <- jabba_res$fit$yr
  if (is.null(ts) || length(dim(ts)) != 3) return(NULL)
  tm <- jabba_terms_of(jabba_res)
  get <- function(slot, stat) if (slot %in% dimnames(ts)[[3]]) as.numeric(ts[, stat, slot]) else rep(NA_real_, length(yrs))
  out <- data.frame(
    year = yrs,
    B = get("B", "mu"), B_lower = get("B", "lci"), B_upper = get("B", "uci"),
    H = get("F", "mu"), H_lower = get("F", "lci"), H_upper = get("F", "uci"),
    B_Bmsy = get("BBmsy", "mu"), B_Bmsy_lower = get("BBmsy", "lci"), B_Bmsy_upper = get("BBmsy", "uci"),
    H_Hmsy = get("FFmsy", "mu"), H_Hmsy_lower = get("FFmsy", "lci"), H_Hmsy_upper = get("FFmsy", "uci"),
    B_K = get("BB0", "mu"), B_K_lower = get("BB0", "lci"), B_K_upper = get("BB0", "uci")
  )
  names(out) <- sub("^H_Hmsy", paste0(tm$sym, "_", tm$ref), names(out))
  names(out) <- sub("^H(_lower|_upper)?$", paste0(tm$sym, "\\1"), names(out))
  out
}

jabba_kobe_probabilities <- function(jabba_res) {
  ## Posterior probabilities of the final-year Kobe quadrants (stock = B/Bmsy,
  ## harvest = H/Hmsy posterior draws).
  k <- jabba_res$fit$kobe
  if (!is.data.frame(k) || nrow(k) == 0 || !all(c("stock", "harvest") %in% names(k))) return(NULL)
  n <- nrow(k)
  r <- jabba_terms_of(jabba_res)$ratio
  data.frame(
    quadrant = c(sprintf("Green: B/Bmsy > 1 and %s < 1", r), sprintf("Yellow (recovering): B/Bmsy < 1 and %s < 1", r),
                 sprintf("Orange (overfishing): B/Bmsy > 1 and %s > 1", r), sprintf("Red: B/Bmsy < 1 and %s > 1", r)),
    probability = round(100 * c(mean(k$stock > 1 & k$harvest < 1), mean(k$stock < 1 & k$harvest < 1),
                                mean(k$stock > 1 & k$harvest > 1), mean(k$stock < 1 & k$harvest > 1)), 1),
    year = if ("yr" %in% names(k)) k$yr[1] else NA, draws = n, stringsAsFactors = FALSE
  )
}

jabba_runs_table <- function(jabba_res) {
  ## Runs test on the log residuals per index (JABBA::jbrunstest); NULL if the
  ## function is unavailable or fails.
  if (!requireNamespace("JABBA", quietly = TRUE) || !exists("jbrunstest", where = asNamespace("JABBA"))) return(NULL)
  out <- tryCatch(JABBA::jbrunstest(jabba_res$fit), error = function(e) NULL)
  if (is.data.frame(out)) return(out)
  NULL
}

jabba_diagnostics_table <- function(jabba_res) {
  fit <- jabba_res$fit
  out <- data.frame(component = names(fit), class = vapply(fit, function(x) paste(class(x), collapse = "/"), character(1)), stringsAsFactors = FALSE)
  if (!is.null(fit$diags) && is.data.frame(fit$diags)) return(fit$diags)
  out
}

jabba_zip_outputs <- function(jabba_res, file) {
  ## Stage the files in a run-specific temporary directory and zip them with
  ## absolute paths (flags = "-j" drops the directory part, as the other SMT
  ## zip helpers do). The process working directory is never changed.
  tmp <- tempfile("jabba_zip_")
  dir.create(tmp, recursive = TRUE, showWarnings = FALSE)
  on.exit(unlink(tmp, recursive = TRUE), add = TRUE)
  saveRDS(jabba_res, file.path(tmp, "jabba_result.rds"))
  utils::write.csv(jabba_res$data$catch, file.path(tmp, "catch.csv"), row.names = FALSE)
  utils::write.csv(jabba_res$data$cpue, file.path(tmp, "cpue.csv"), row.names = FALSE)
  if (!is.null(jabba_res$data$se)) utils::write.csv(jabba_res$data$se, file.path(tmp, "se.csv"), row.names = FALSE)
  utils::write.csv(jabba_summary_table(jabba_res), file.path(tmp, "summary_table.csv"), row.names = FALSE)
  ts <- jabba_timeseries_table(jabba_res)
  if (is.data.frame(ts)) utils::write.csv(ts, file.path(tmp, "timeseries_table.csv"), row.names = FALSE)
  cvt <- jabba_convergence_table(jabba_res)
  if (is.data.frame(cvt)) utils::write.csv(cvt, file.path(tmp, "mcmc_diagnostics.csv"), row.names = FALSE)
  allfiles <- list.files(tmp, full.names = TRUE)
  if (!is.null(jabba_res$output_dir) && dir.exists(jabba_res$output_dir)) {
    allfiles <- c(allfiles, list.files(jabba_res$output_dir, full.names = TRUE, recursive = TRUE))
  }
  utils::zip(zipfile = file, files = allfiles, flags = "-j")
}

jabba_is_owned_tempdir <- function(path) {
  ## TRUE only for run directories created by run_jabba_model() in this R
  ## session's tempdir(); never matches shared or other sessions' files.
  if (is.null(path) || length(path) != 1 || !nzchar(path)) return(FALSE)
  if (!startsWith(basename(path), "smt_jabba_")) return(FALSE)
  identical(normalizePath(dirname(path), mustWork = FALSE), normalizePath(tempdir(), mustWork = FALSE))
}

jabba_cleanup_result <- function(jabba_res) {
  ## Remove the run-specific output directory of a JABBA result (if any).
  if (is.null(jabba_res)) return(invisible(FALSE))
  dir <- jabba_res$output_dir
  if (jabba_is_owned_tempdir(dir) && dir.exists(dir)) {
    unlink(dir, recursive = TRUE)
    return(invisible(TRUE))
  }
  invisible(FALSE)
}

plot_jabba_if_available <- function(fun, jabba_res, fallback = "Plot is not available for this JABBA object/version.") {
  if (!requireNamespace("JABBA", quietly = TRUE)) {
    graphics::plot.new(); graphics::text(0.5, 0.5, "JABBA is not installed."); return(invisible(NULL))
  }
  if (!exists(fun, where = asNamespace("JABBA"), mode = "function")) {
    graphics::plot.new(); graphics::text(0.5, 0.5, fallback); return(invisible(NULL))
  }
  f <- get(fun, envir = asNamespace("JABBA"), mode = "function")
  tryCatch(f(jabba_res$fit, add = FALSE, as.png = FALSE, verbose = FALSE), error = function(e) {
    graphics::plot.new(); graphics::text(0.5, 0.5, paste(fallback, conditionMessage(e), sep = "\n"))
  })
}

jabba_report_context <- function(jabba_res) {
  fit <- jabba_res$fit
  list(
    settings = jabba_res$settings,
    jabba_settings = fit$settings,
    checks = jabba_res$data$checks,
    selected_columns = jabba_res$data$selected_columns,
    source = jabba_res$source,
    mcmc = jabba_res$mcmc,
    versions = jabba_res$versions,
    warnings = jabba_res$warnings,
    run_time = jabba_res$run_time,
    run_id = jabba_res$run_id,
    report_name = if (!is.null(jabba_res$report_file)) basename(jabba_res$report_file) else NA_character_,
    summary = jabba_summary_table(jabba_res),
    parameters = jabba_parameter_table(jabba_res),
    advanced = jabba_res$advanced,
    terms = jabba_terms_of(jabba_res),
    convergence_available = isTRUE(jabba_res$convergence$available),
    convergence_reason = if (!isTRUE(jabba_res$convergence$available)) jabba_res$convergence$reason else NULL,
    convergence_summary = jabba_convergence_summary(jabba_res),
    convergence_table = jabba_convergence_table(jabba_res),
    convergence_statement = jabba_convergence_statement(jabba_res),
    timeseries = jabba_timeseries_table(jabba_res),
    kobe = jabba_kobe_probabilities(jabba_res),
    runs = jabba_runs_table(jabba_res),
    stats = fit$stats,
    diags = if (is.data.frame(fit$diags)) fit$diags else NULL
  )
}

jabba_try_plot <- function(fun, jabba_res, ..., object = c("fit", "input")) {
  ## Calls a JABBA plotting function on the current device. Returns
  ## list(ok, message); on failure a placeholder is drawn so the report can
  ## state explicitly that the figure could not be generated.
  object <- match.arg(object)
  obj <- if (object == "fit") jabba_res$fit else jabba_res$input
  if (!requireNamespace("JABBA", quietly = TRUE) || !exists(fun, where = asNamespace("JABBA"), mode = "function")) {
    graphics::plot.new(); graphics::text(0.5, 0.5, paste0(fun, " is not available in this JABBA version."))
    return(invisible(list(ok = FALSE, message = "function not available")))
  }
  f <- get(fun, envir = asNamespace("JABBA"), mode = "function")
  res <- tryCatch({ f(obj, ...); list(ok = TRUE, message = "") }, error = function(e) {
    graphics::plot.new(); graphics::text(0.5, 0.5, paste0("Figure could not be generated (", fun, "):\n", conditionMessage(e)), cex = 0.8)
    list(ok = FALSE, message = conditionMessage(e))
  })
  invisible(res)
}

create_jabba_pdf_report <- function(file, jabba_res, template = "assets/SPM/jabba/markdown/jabba_report.Rmd") {
  ## Renders the PDF report for a completed run into `file`. Rendering happens
  ## in its own temporary directory (removed afterwards); the process working
  ## directory is not changed by this function. Errors propagate to the caller.
  if (!file.exists(template)) stop("JABBA report template not found: ", template)
  work <- tempfile("jabba_render_")
  dir.create(work, recursive = TRUE, showWarnings = FALSE)
  on.exit(unlink(work, recursive = TRUE), add = TRUE)
  tmp_rmd <- file.path(work, "jabba_report.Rmd")
  file.copy(template, tmp_rmd, overwrite = TRUE)
  out <- rmarkdown::render(
    input = tmp_rmd,
    output_file = "jabba_report.pdf",
    output_dir = work,
    intermediates_dir = work,
    params = list(jabba_result = jabba_res, context = jabba_report_context(jabba_res)),
    envir = new.env(parent = globalenv()),
    quiet = TRUE
  )
  if (!file.exists(out) || file.size(out) == 0) stop("The PDF report was not produced.")
  dir.create(dirname(file), recursive = TRUE, showWarnings = FALSE)
  if (!file.copy(out, file, overwrite = TRUE)) stop("Could not write the PDF report to ", file)
  invisible(file)
}

jabba_pdf_available <- function(jabba_res) {
  !is.null(jabba_res) && !is.null(jabba_res$report_file) && file.exists(jabba_res$report_file) && file.size(jabba_res$report_file) > 0
}
