tabJABBAIntro <- tabItem("jabbaIntro", htmlOutput("jabbaIntroOut"))

tabJABBASampleDataset <- tabItem(
  "jabbaSampleDataset",
  h3("JABBA sample dataset"),
  fluidRow(
    box(width = 12, solidHeader = TRUE, title = "Example format",
        div(class = "alert alert-warning", role = "alert", style = "margin: 10px 0;",
            tags$b("Synthetic demonstration data. "),
            "This file illustrates the input format and lets you try the interface. ",
            "It is not a validated scientific assessment example and must not be used for management advice."),
        p("JABBA uses annual total catch and one or more relative abundance indices, usually CPUE."),
        tags$ul(
          tags$li("The catch series must be complete and sequential."),
          tags$li("CPUE indices may contain missing values."),
          tags$li("SE columns are optional and should match the selected CPUE indices."),
          tags$li("Use one row per year, with columns such as year, catch, cpue_ll, se_cpue_ll."),
          tags$li("For a full assessment, use a substantially longer and documented time series than this small test file.")
        ),
        downloadButton("downloadJabbaSampleData", "Download sample CSV")
    )
  ),
  fluidRow(
    box(width = 12, solidHeader = TRUE, title = "Preview",
        DT::dataTableOutput("jabbaSamplePreview")
    )
  )
)

getWorkflowConsiderationTextForJabba <- function() {
  paste0(
    "<h4>To run the JABBA workflow in SMT:</h4>",
    "<ol>",
    "<li>Upload a CSV file with annual catch and at least one CPUE or abundance-index time series.</li>",
    "<li>Map the year, catch, CPUE and optional standard-error columns.</li>",
    "<li>Check the input. JABBA requires a complete sequential catch series.</li>",
    "<li>Set model structure and priors. For a first test, keep Quick MCMC enabled.</li>",
    "<li>Run the assessment, inspect convergence diagnostics, residuals, CPUE fits and stock-status plots.</li>",
    "<li>After each successful run the PDF report is generated automatically; download it together with the summary table, the fitted R object or the full output zip. When the SMT is opened from the VRE with your account, the PDF report is also uploaded once to the <i>StockMonitoringTool</i> folder of your private workspace and the run status reports whether the upload succeeded.</li>",
    "<li>If the PDF could not be generated, the other downloads stay available and 'Retry PDF report' generates it again without re-running the assessment.</li>",
    "</ol>"
  )
}

getDataConsiderationTextForJabba <- function() {
  paste0(
    "<h4>Required data</h4>",
    "<ul>",
    "<li><b>Catch:</b> annual total catch by year; the catch time series must be sequential without missing years.</li>",
    "<li><b>Index:</b> one or more relative abundance indices, typically standardized CPUE. Missing index years are allowed.</li>",
    "<li><b>SE:</b> optional standard errors for the selected index series. Select exactly one SE column per CPUE column, in the same order.</li>",
    "</ul>",
    "<p>The sample dataset is synthetic demonstration data illustrating the file format only; it is not a validated scientific assessment example. A real JABBA assessment needs documented catch reconstruction, index standardization and sensitivity tests.</p>"
  )
}

getMethodConsiderationTextForJabba <- function() {
  paste0(
    "<h4>Model considerations</h4>",
    "<p>JABBA fits Bayesian surplus-production models in JAGS. SMT exposes the main choices needed for routine use: production curve, r prior, optional K prior, initial depletion prior, biomass-status prior, catch error, and observation/process variance settings.</p>",
    "<p>The 'Exploitation terminology' control selects the terminology used for the exploitation rate in plots, tables, CSV files and the PDF report: F/F<sub>MSY</sub> (JABBA's convention, default) or H/H<sub>MSY</sub> (harvest rate). The downloadable fitted JABBA object keeps JABBA's own element names.</p>",
    "<p>Use Quick MCMC only for smoke tests. For analytical outputs, increase iterations, inspect chain behavior and run sensitivity scenarios.</p>"
  )
}

## Help texts of the Advanced Settings box (currently hidden; see jabbaUI.R).
## One function per option so that each info button shows only its own content.
getFixedObsEHelpTextForJabba <- function() {
  paste0(
    "<h4>Minimum observation error (JABBA argument <code>fixed.obsE</code>)</h4>",
    "<p><b>What it is:</b> a fixed standard deviation of the observation error of the abundance indices, on the log scale (dimensionless; it applies to log(index)). ",
    "JABBA adds its square to the observation variance of every index observation.</p>",
    "<p><b>Default rule (JABBA):</b> 0.1 when no uncertainty (SE) columns are mapped; 0.001 when SE columns are mapped.</p>",
    "<p><b>Interaction with supplied uncertainties:</b> without SE columns it is the whole fixed part of the observation error. ",
    "With SE columns the observation variance of each observation is SE<sup>2</sup> + fixed.obsE<sup>2</sup>, so it acts as a floor on top of the supplied SE ",
    "(JABBA replaces a missing SE value by 0.3 before adding the floor). When 'Estimate observation variance' is on, JABBA adds a further estimated variance component for each index. ",
    "Setting it to 0 removes the fixed part entirely (JABBA then uses a negligible 10<sup>-10</sup> variance when no SE columns are supplied).</p>",
    "<p><b>Required:</b> a number of 0 or more. Documented JABBA examples use values between 0.01 and 0.2; the ICCAT swordfish example uses 0.2.</p>"
  )
}

getProcDevAllHelpTextForJabba <- function() {
  paste0(
    "<h4>Process deviations from the first catch year (JABBA argument <code>proc.dev.all</code>)</h4>",
    "<p><b>Timing:</b> checked (JABBA default), a process-error deviation is estimated for every year of the catch series, including the first. ",
    "Unchecked, deviations are estimated only from the first year that has at least one abundance-index observation.</p>",
    "<p><b>Before the first index year</b> (unchecked only): JABBA fixes the process precision at 10,000 (standard deviation 0.01 on the log scale), ",
    "so until the first index year the biomass follows the production equation given K, r and the initial depletion, with the catches removed, approximately deterministically (the early-year process deviations are very small, not zero). ",
    "The year from which deviations are estimated is recorded in the results and the PDF report.</p>",
    "<p><b>When to consider it:</b> long catch series that start well before the first index, where free process deviations in the unobserved years would be driven by the priors only. ",
    "The ICCAT swordfish example uses this option.</p>"
  )
}

getPBoundHelpTextForJabba <- function() {
  paste0(
    "<h4>Soft bounds on B/K (JABBA argument <code>P_bound</code>)</h4>",
    "<p><b>What they are:</b> a lower and an upper bound on the depletion B/K. For every year JABBA computes a penalty equal to log(B/K) minus log(bound) when B/K is below the lower bound or above the upper bound, and 0 otherwise.</p>",
    "<p><b>Penalty interpretation:</b> the penalty enters the model as a normal likelihood term with precision 1,000 (standard deviation about 0.03 on the log scale), ",
    "so B/K can still cross a bound at a likelihood cost that grows quickly with the distance: this is a soft penalty, not a hard constraint. ",
    "JABBA records no warning when a bound is crossed; inspect the biomass trajectory.</p>",
    "<p><b>Defaults (JABBA):</b> 0.02 and 1.3. The ICCAT swordfish example uses 0.02 and 1.1.</p>",
    "<p><b>Required:</b> lower bound above 0 and below the upper bound (JABBA evaluates log(K x bound)).</p>"
  )
}

getHarvestLabelHelpTextForJabba <- function() {
  paste0(
    "<h4>Exploitation terminology</h4>",
    "<p>Selects the terminology used for the exploitation rate and its MSY reference point in JABBA's plots, the summary and time-series tables, the CSV files and the PDF report of the run:</p>",
    "<ul>",
    "<li><b>F/Fmsy (JABBA convention, default):</b> F, F<sub>MSY</sub> and F/F<sub>MSY</sub>, as in JABBA and the assessments published with it.</li>",
    "<li><b>H/Hmsy (harvest rate):</b> H, H<sub>MSY</sub> and H/H<sub>MSY</sub>.</li>",
    "</ul>",
    "<p>The choice sets JABBA's <code>harvest.label</code> option. The downloadable fitted object keeps JABBA's own element names. ",
    "The terminology used by a run is recorded in its PDF report.</p>"
  )
}

getConvergenceHelpTextForJabba <- function() {
  paste0(
    "<h4>What is shown</h4>",
    "<p>For each run SMT retains the <b>chain-ordered</b>, post-burn-in, thinned MCMC draws of the model parameters (K, r, psi, process variance, catchability and observation variance per index), ",
    "the reference points (", "F<sub>MSY</sub> or H<sub>MSY</sub>, B<sub>MSY</sub>, MSY) and the final-year status ratios, and computes:</p>",
    "<ul>",
    "<li><b>R-hat</b>: the conventional Gelman&ndash;Rubin potential scale reduction factor with the Brooks&ndash;Gelman degrees-of-freedom correction (<code>coda::gelman.diag</code>), ",
    "computed on the whole post-burn-in chains on the raw scale, with its 97.5 % upper limit. It is <i>not</i> the rank-normalised split-R-hat of Vehtari et al. (2021), which is stricter; it needs at least two chains.</li>",
    "<li><b>ESS</b>: the autocorrelation-based effective sample size (<code>coda::effectiveSize</code>: for each chain the number of draws divided by the integrated autocorrelation time, estimated from the spectral density at frequency zero, then summed over chains).</li>",
    "<li><b>Trace plots</b>: one panel per parameter, chains in different colours, against the MCMC iteration.</li>",
    "</ul>",
    "<p>Parameters with R-hat above 1.05 or ESS below 400 are flagged. A quick MCMC test run will normally show flags; it is for workflow checks only.</p>",
    "<h4>What is not shown, and why</h4>",
    "<p>R2jags also prints an 'n.eff'. It is a different estimator (R2WinBUGS: m&middot;n&middot;min(&sigma;&#770;<sup>2</sup>/B, 1), based on the between-chain variance only, computed on the log scale for positive parameters and rounded to one or two significant digits). ",
    "With two chains it is very imprecise, is capped at the total number of draws and can differ from the autocorrelation-based ESS by more than an order of magnitude in either direction. ",
    "It is kept in the downloadable fitted object for reference and is not reported as an effective sample size. ",
    "JABBA's own Geweke.p and Heidel.p columns were computed by the package on randomly permuted draws and are not convergence diagnostics; they stay in the fitted object and are not shown.</p>",
    "<h4>How the draws are obtained</h4>",
    "<p>JABBA's fitting function returns only randomly permuted pooled draws. SMT therefore fits the model through a guarded, per-run adapted copy of the pinned <code>fit_jabba()</code> function that keeps the R2jags object of the same fit; ",
    "nothing in the installed packages is modified and the fit is identical to a plain JABBA call at the same seed. If the installed JABBA is not the pinned version, or the capture fails, the assessment and all downloads are kept and the run status and the PDF state that convergence cannot be assessed for that run.</p>",
    "<h4>Interpretation</h4>",
    "<p><b>These diagnostics can reveal some failures of the sampler to converge; favourable diagnostics do not prove convergence.</b> ",
    "Use them together with the index fits, the residual diagnostics and sensitivity runs with other seeds, longer chains and alternative priors before using results for advice.</p>"
  )
}

getAdvancedConsiderationTextForJabba <- function() {
  paste0(
    "<h4>Advanced settings (JABBA options)</h4>",
    "<p>Three JABBA options that are needed to reproduce some documented JABBA configurations. Each field has its own help button. ",
    "Requested and effective values are recorded in the fitted object and in the PDF report. Leave the defaults unless you have a documented reason to change them.</p>",
    "<ul>",
    "<li><b>Minimum observation error (fixed.obsE):</b> fixed log-scale observation-error SD added to every index observation (JABBA default 0.1 without SE columns, 0.001 with SE columns).</li>",
    "<li><b>Process deviations (proc.dev.all):</b> whether process deviations are estimated from the first catch year (JABBA default) or only from the first index year.</li>",
    "<li><b>Soft bounds on B/K (P_bound):</b> soft penalty bounds on the depletion B/K (JABBA defaults 0.02 and 1.3).</li>",
    "</ul>"
  )
}

getResultConsiderationTextForJabba <- function() {
  paste0(
    "<h4>Result interpretation</h4>",
    "<ul>",
    "<li>Use B/B<sub>MSY</sub> and F/F<sub>MSY</sub> (H/H<sub>MSY</sub> when that terminology is selected) together, not in isolation.</li>",
    "<li>Posterior estimates are reported as medians with 95% credible intervals (2.5% and 97.5% posterior quantiles).</li>",
    "<li>Inspect CPUE fits and residuals before trusting stock-status estimates.</li>",
    "<li>Check diagnostics and sensitivity to priors, catch assumptions and index selection.</li>",
    "<li>Do not use a quick-MCMC run for scientific advice.</li>",
    "<li><b>MCMC convergence diagnostics</b> (Convergence tab and PDF): R-hat (the conventional Gelman-Rubin potential scale reduction factor, Brooks-Gelman correction, whole chains; not the rank-normalised split-R-hat), the autocorrelation-based effective sample size (coda::effectiveSize) and per-chain trace plots, all computed from the chain-ordered draws of this run's own JAGS fit; parameters with R-hat above 1.05 or ESS below 400 are flagged. ",
    "<b>These diagnostics can reveal some failures to converge; favourable diagnostics do not prove convergence.</b> ",
    "If the chain-ordered draws cannot be captured for a run (for example with a JABBA version other than the pinned one), the run status and the PDF state that convergence cannot be assessed, and the assessment and downloads remain available. ",
    "JABBA's own Geweke.p and Heidel.p columns in the downloadable fitted object were computed on randomly permuted draws and are not convergence diagnostics.</li>",
    "</ul>"
  )
}
