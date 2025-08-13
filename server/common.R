#test this

getDataConsiderationTextForCmsy <- function() {
  text <- "<strong>Dataset must include:</strong><br/>"
  text <- paste0(text, "<ul>")
  text <- paste0(text, "<li><strong>Stock:</strong> a unique fish stock name or identifier (e.g. “cod-2532”), repeated for each year.</li>")
  text <- paste0(text, "<li><strong>yr:</strong> the reporting year of the catch (e.g. 2004). One row for each year. Years have to be consecutive from the first to the last year without any missing years.</li>")
  text <- paste0(text, "<li><strong>ct:</strong> catch value, in tonnes (e.g. 12345). One row for each year. Gaps with no entries are not accepted and must be filled by interpolating missing or incorrect values, e.g., do not accept zero as entry if data are missing, instead use mean of adjacent values to replace zero or fill any gaps.</li>")
  text <- paste0(text, "<li><strong>bt:</strong> the value of the biomass (in tonnes, e.g. 34567), or the value of the CPUE or stock size index (e.g. 0.123), or NA if there is no information. Gaps filled with NA are acceptable for bt, i.e., abundance data can be fewer than catch data.</li>")
  text <- paste0(text, "</ul>")
  text <- paste0(text, "<br/>")
  text <- paste0(text, "<p>Other columns are identifiers that you may choose to include, but they are not necessary to run the model.</p>")
  text <- paste0(text, "<br/>")
  text <- paste0(text, "<p>Use the ",
                 "<a href='https://data.d4science.org/shub/E_WWI3clpMdGVONG84eVNvblBxOWVPQ2FPUVArYkptQ1JsM2c1K2hkYWpjSlhmeUw1eXI2RGFLZUdMZW5aSis4aQ=='> sample dataset </a>",
                 " as a template to prepare your data.</p>")
  text <- paste0(text, "<br/>")
  text <- paste0(text, "<strong>Specific considerations regarding your own dataset:</strong><br/>")
  text <- paste0(text, "<li>Save your dataset in 'csv' format.</li>")
  text <- paste0(text, "<li>The separator for the .csv file should be a comma ‘,’. The default might differ depending on the language settings of your spreadsheet manipulation program (e.g. Excel).</li>")
  text <- paste0(text, "<li>Note that years with missing data should be filled with an 'NA' value.</li>")
  text <- paste0(text, "<li>Note that the column names of your dataset should exactly match those of the sample dataset.</li>")
  text <- paste0(text, "<li><strong>Please ensure your time-series is at least 15 years in length from starting year to ending year.</strong></li>")

}

getWorkflowConsiderationTextForCMSY <- function() {
  text <- "<h4> To run the CMSY method in the Stock Monitoring Tool :</h4>"
  text <- paste0(text, "<ol>")
  text <- paste0(text, "<li>Upload a csv file data set of catch time series for one or multiple stocks (see Data Considerations or the <a href='https://data.d4science.org/shub/E_WWI3clpMdGVONG84eVNvblBxOWVPQ2FPUVArYkptQ1JsM2c1K2hkYWpjSlhmeUw1eXI2RGFLZUdMZW5aSis4aQ=='>CMSY Sample dataset</a>).</li>")
  text <- paste0(text, "<ol type='a'>")
  text <- paste0(text, "<li> Select the stock upon which to perform the analysis </li>")
  text <- paste0(text, "</ol>")
  text <- paste0(text, "<li> Adjust the Assessment Settings")
  text <- paste0(text, "<ol type='a'>")
  text <- paste0(text, "<li> Data Selection - select the years of data to include in the analysis, and over which to calculate the catchability. </li>")
  text <- paste0(text, "<li> Assessment settings - set the search space of the CMSY algorithm to estimate probable r-K pairs (set resilience and depletion). </li>")
  text <- paste0(text, "<li> Optional information - if you have reference point values from previous assessments, you can choose to enter and compare them to the results of the present analysis.</li>")
  text <- paste0(text, "</ol>")
  text <- paste0(text, "</li>")
  text <- paste0(text, "<li> Run the Assessment:")
  text <- paste0(text, "<ol type='a'>")
  text <- paste0(text, "<li> Run the assessment (Run Assessment button). This may take some time. For example, the sample dataset takes about 1 min.</li>")
  text <- paste0(text, "</ol>")
  text <- paste0(text, "</li>")
  text <- paste0(text, "<li> Download the report as a pdf (Download Report button)</li>")
  text <- paste0(text, "<li> The Reset button removes the uploaded data and resets the settings to default</li>")
  text <- paste0(text, "</ol>")
  text <- paste0(text, "Further information can be found in the popup information buttons at each field, and in the Data, Methods, and Results Considerations tabs. Note that error messages may display in the center of the page and in place of any figures where an error has occurred.")
  text <- paste0(text, "</p>")
  return (text)
}

getMethodConsiderationTextForCmsy <- function() {
  text <- "<b> </b>"
  # text <- paste0(text, "<br>")
  # text <- paste0(text, "<ol>")
  text <- paste0(text, "The Schaefer production model parameters are r and k. Different combinations of these parameters will produce different time series of
                 biomass. CMSY is a Monte Carlo method where the Schaefer model is run many times to calculate annual biomasses for r-k pairs randomly drawn from prior distributions.
                 The model determines which r-k pairs are valid: e.g., those pairs that result in a biomass time series that do not <br><br>
                <ol>(1) result in a stock collapse, or </ol>
                <ol>(2) allow the stock to exceed carrying capacity.</ol> <br>
                 Those r-k pairs that result in a final relative biomass estimate between the values specified in the inputs (the final depletion range) are accepted
                 and used to calculate MSY (rk/4) and biomass over time. The geometric means of the resulting density distributions of r, k and MSY are taken as the most
                 probable values. CMSY estimates fisheries reference points (MSY, F_msy, B_msy), relative stock size (B/B_msy), and fishing mortality or exploitation (F/F_msy).
                 These are estimated from catch data, a prior value for the resilience, productivity, or intrinsic growth rate (r), and broad
                 prior ranges for the ratio of biomass to unfished biomass (B/k) at the beginning and the end of the time series and an intermediate year.")
  # text <- paste0(text, "</ol>")
  text <- paste0(text, "<br><br>")
  text <- paste0(text,
                 "<p>The CMSY method for data-limited stock assessment is described in ",
                 "<a href='https://www.researchgate.net/publication/309283306_Estimating_fisheries_reference_points_from_catch_and_resilience'  target='blank_'> Froese et al. (2017)</a>",
                 ", and compiled into an R algorithm available on Github : ",
                 "<a href='https://github.com/SISTA16/cmsy'  target='blank_'> CMSY 2019 v9f </a>",
                 ". <br>A complete user guide with best practice advice by the authors is available : ", "<a href='https://github.com/SISTA16/cmsy/blob/master/CMSY_2019_9f_UserGuide.pdf'  target='blank_'> CMSY_2019_9f_UserGuide</a>",
                 ". <br><br>The CMSY+ version available in the Stock Monitoring Tool ( CMSY_2019_9f .R) is a further development of the one used in Froese et al.
(2017). The main differences are faster execution because of parallel processing and the estimation of default B/k priors has been improved. A major improvement for CMSY+ is the introduction of multivariate normal priors for r and k in log space, replacing the previous uniform prior distributions.
This has allowed also for a simplified determination of the ‘best’ r-k pair in CMSY+ and faster run times. <br><br>

<strong>The CMSY+ version hosted on the Stock Monitoring Tool does not currently allow for abundance data to be included in the analyses, thus the Bayesian state-space implementation of
the Schaefer surplus production model (BSM) is not activated.</strong> </p>")
  return (text)
}




getResultConsiderationTextForCmsy <- function() {
  text <- "<b>CMSY method outputs </b><br>CMSY+ will first do a Monte-Carlo analysis of catch and priors for r and B/k. If CMSY+ does not find any viable points,
 review all your priors to verify that they are indeed plausible. Increase the final prior biomass range if it is very narrow (e.g. change 0.01-0.1 to 0.01 – 0.3)."
  text <- paste0(text, "<br>")
  text <- paste0(text, "<br>")
  text <- paste0(text, "<ul>")
  text <- paste0(text, "<li>")
  text <- paste0(text,"<b>Catch time series : Figure 1</b>")
  text <- paste0(text, "<br>")
  text <- paste0(text, "Directly after the successful upload of your data set, Figure 1 shows the catch time series for the selected stock (x-axis = year, y-axis = catch in tonnes). The time series should be at least 15 years long. The longer the time series the more confidence in the results. Use the catch time series to:")
  text <- paste0(text, "<ul>")
  text <- paste0(text, "<li>identify the range of years over which to perform the CMSY analysis. The start year should correspond to the first year when data are considered reliable;</li>")
  text <- paste0(text, "<li>determine over what range to calculate the catchability (q), ideally corresponding to at least 5 recent years where catches are were stable or had similar trends; and </li>")
  text <- paste0(text, "<li>identify if there is a  year with particularly high or low biomass in the time series, e.g. exploitation changed from light to full, or where an extraordinarily large year class entered the fishery. This can be
                 used as an intermediate year to inform the intermediate depletion range prior.</li>")
  text <- paste0(text, "</ul>")
  text <- paste0(text, "</li>")
  text <- paste0(text, "</ul>")
  text <- paste0(text, "<br>")
  text <- paste0(text, "After successfully running the CMSY method (click 'Run Method'), two figures with sub-figures summarise the results of the CMSY method. ")
  text <- paste0(text, "<br>")
  text <- paste0(text, "<br>")
  text <- paste0(text, "<ul>")
  text <- paste0(text, "<li> <b>CMSY : Figure 2 </b> ")
  text <- paste0(text, "<ul>")
  text <- paste0(text, "<li>The upper left panel shows catches relative to the estimate of MSY, with indication of 95% confidence limits in grey. </li>")
  text <- paste0(text, "<li>The upper right panel shows the development of relative total biomass (B/Bmsy), with the grey area indicating uncertainty. </li>")
  text <- paste0(text, "<li>The lower left graph shows relative exploitation (F/Fmsy), with Fmsy corrected for reduced recruitment below 0.5 Bmsy.  </li>")
  text <- paste0(text, "<li>The lower-right panel shows the trajectory of relative stock size (B/Bmsy) over relative exploitation (F/Fmsy). </li>")
  text <- paste0(text, "</ul>")
  text <- paste0(text, "</li>")
  text <- paste0(text, "</ul>")
  text <- paste0(text, "<br>")
  text <- paste0(text, "<ul>")
  text <- paste0(text, "<li><b>CMSY : Figure 3 </b>")
  text <- paste0(text, "<ul>")
  text <- paste0(text, "<li>Panel A shows in black the time series of catches and in blue the three-years moving average with indication of highest and lowest catch, as used in the estimation of prior biomass by the default rules. </li>")
  text <- paste0(text, "<li>Panel B shows the explored r-k log space and in dark grey the r-k pairs which were found by the CMSY model to be compatible with the catches and the prior information.  </li>")
  text <- paste0(text, "<li>Panel C shows the most probable r-k pair and its approximate 95% confidence limits in blue.  </li>")
  text <- paste0(text, "<li>Panel D shows in blue the biomass trajectory estimated by CMSY. Dotted lines indicate the 2.5th and 97.5th percentiles. Vertical blue lines indicate the prior biomass ranges.  </li>")
  text <- paste0(text, "<li>Panel E shows in blue the harvest rate from CMSY.  </li>")
  text <- paste0(text, "<li>Panel F shows the Schaefer equilibrium curve of catch/MSY relative to B/k, here indented at B/k < 0.25 to account for reduced recruitment at low stock sizes. The blue dots are scaled by CMSY estimates. </li>")
  text <- paste0(text, "</ul>")
  text <- paste0(text, "</li>")
  text <- paste0(text, "</ul>")
  text <- paste0(text, "</li>")
  text <- paste0(text, "</ul>")
  text <- paste0(text, "<br>")
  text <- paste0(text, "<br>")
  return (text)
}


linkEA <- "<a href='https://data.d4science.org/shub/E_Snp1NzhlUHlFOWg0M3lUL3lZWU0yMVVTVFY5NDhaYkI1ODY4blFtSi9NYlFiMVAyamxMNGc2QXh0TlpRNmdNdQ==' target='_blank'>click to download</a>"
linkSE <- "<a href='https://data.d4science.org/shub/E_ZEhvM2ZpSVFZd0ZOSXl5MjlGZC90eC9vOUI1NG1hQ0NEMERtRGVHR2hqbThBZGxBYS9QWG5kc1BaWTluVHNvQw==' target='_blank'>click to download</a>"
linkOSG <- "<a href='https://data.d4science.org/shub/E_SHJJRUJ5S1lxOGhLOW1zVWpKbUJFQzE1c2wzbnltRlVBUmYrUEphbDRkN1BxQzlYOUtwQWRzZ0JMdnJrSmRhZA==' target='_blank'>click to download</a>"
## TODO upload albacore data
linkAL <- "<a href='https://data.d4science.org/shub/E_ZEhvM2ZpSVFZd0ZOSXl5MjlGZC90eC9vOUI1NG1hQ0NEMERtRGVHR2hqbThBZGxBYS9QWG5kc1BaWTluVHNvQw==' target='_blank'>click to download</a>"
## TODO upload lobster data
linkLO <- "<a href='https://data.d4science.org/shub/E_ZEhvM2ZpSVFZd0ZOSXl5MjlGZC90eC9vOUI1NG1hQ0NEMERtRGVHR2hqbThBZGxBYS9QWG5kc1BaWTluVHNvQw==' target='_blank'>click to download</a>"



## ELEFAN
## ------------------------------------------------------
getDataConsiderationTextForElefan <- function() {
    text <- "<b>Two data formats are accepted:</b><br>"
  text <- paste0(text, "<b>1. Format: Length frequency data must include:</b>")
  text <- paste0(text, "<ul>")
  text <- paste0(text,
                 "<li>A column indicating the length classes of measured individuals (first column of dataset).</li>")
  text <- paste0(text, "<li>A row indicating the dates when individuals were measured (first row of dataset excluding the first column).</li>")
  text <- paste0(text,
                 "<li>The number of individuals caught per length class (rows) and per sampling date (columns).</li>")
  text <- paste0(text, "</ul>")
  text <- paste0(text, "<br>")
  text <- paste0(text, "<p> An example dataset in this format can be downloaded here (",linkSE,"). </p>")
  text <- paste0(text, "<br>")
  text <- paste0(text, "<b>2. Format: Raw length measurements must include:</b>")
  text <- paste0(text, "<ul>")
  text <- paste0(text, "<li>A column indicating the dates when individuals were measured (first column of dataset).</li>")
  text <- paste0(text,
                 "<li>A column indicating the length of measured individuals (second column of dataset).</li>")

  text <- paste0(text,
                 "<li>Optionally: A column indicating the number of individuals of the given length (third column of the dataset).</li>")
  text <- paste0(text, "</ul>")
  text <- paste0(text, "<br>")
  text <- paste0(text, "<p> An example dataset in this format can be downloaded here (",linkEA,"). </p>")
  text <- paste0(text, "<br>")
  text <- paste0(text, "<br>")
  text <- paste0(text, "<b>Specific considerations regarding your own dataset:</b>")
  text <- paste0(text, "<ul>")
  text <- paste0(text, "<li>", "Save your dataset in 'csv' format.", "</li>")
  text <- paste0(text, "<li>", "The separator for the .csv file should be a comma ‘,’ semicolon ';' or tab. The default might differ depending on the language settings of your spreadsheet manipulation program (e.g. Excel).", "</li>")
  text <- paste0(text, "<li>", "Use a '.' to separate decimals in the data. The default might differ depending on the language settings of your spreadsheet manipulation program (e.g. Excel).", "</li>")
  text <- paste0(text, "<li>The date format of your data file should be automatically detected, or select the date format used in your data file under <b>'Choose CSV date format'</b>, e.g. DD/MM/YYYY format to select 'Day Month Year'.</li>")
  text <- paste0(text, "<li>For data format 1, the columns should represent catches representative for a whole year. If no date column is provided for data format 2, it is assumed that all samples are representative for a single year.</li>")
  text <- paste0(text, "<li>Your data set should at least be representative of a whole year.</li>")
  text <- paste0(text, "</ul>")
  text <- paste0(text, "<br>")

  text <- paste0(text, "<br>")
  text <- paste0(text, "<p> Further information about differnet formats of length-frequency datasets ",
                 "and how to convert one format to the other are described in a ",
                 "<a href='https://cran.r-project.org/web/packages/TropFishR/vignettes/lfqData.html'  target='blank_'>tutorial by Mildenberger (2020)</a>. </p>")

    return (text)
}

getWorkflowConsiderationTextForElefan <- function() {
  text <- "<h4> To run this length-based workflow in the Stock Monitoring Tool :</h4>"
  text <- paste0(text, "<ol>")
  text <- paste0(text, "<li> Upload a size frequency data set (see Data Considerations or one of the sample datasets (e.g., ",linkSE,"))</li>")
  text <- paste0(text, "<li> Adjust the Assessment Settings")
  text <- paste0(text, "<ol type='a'>")
  text <- paste0(text, "<li> Data Settings - select the years of data to include in the analysis, adjust the aggregation, bin size and moving average (MA) of the dataset to optimise cohort recognition by the Elefan algorithm </li>")
  text <- paste0(text, "<li> Elefan - set the search space of the Elefan algorithm to estimate growth parameters </li>")
  text <- paste0(text, "<li> Stock status - set the parameters required to estimate mortality and perform YPR (and SPR; optional) </li>")
  text <- paste0(text, "</ol>")
  text <- paste0(text, "</li>")
  text <- paste0(text, "<li> Check and Run the Assessment:")
  text <- paste0(text, "<ol type='a'>")
  text <- paste0(text, "<li> Run a check on the parameterisations prior to running the full assessment (Run Check button)</li>")
  text <- paste0(text, "<li> Run the assessment ('Run Assessment' button). Watch for the progress bar in the center of the screen</li>")
  text <- paste0(text, "</ol>")
  text <- paste0(text, "</li>")
  text <- paste0(text, "<li> Download the report as a pdf (Download Report button). The report should also automatically upload to your private workspace. </li>")
  text <- paste0(text, "<li> The Reset button removes the uploaded data and resets the settings to default</li>")
  text <- paste0(text, "</ol>")
  text <- paste0(text, "Further information can be found in the popup information buttons at each field, and in the Data, Methods, and Results Considerations tabs. Note that error messages may display in the center of the page and in place of any figures where an error has occurred.")
  text <- paste0(text, "</p>")
    return (text)
}


getMethodConsiderationTextForElefan <- function() {
  text <- "<b>Consecutive steps of the length-based stock assessment routine workflow:</b>"
  text <- paste0(text, "<br>")
  text <- paste0(text, "<ol>")
  text <- paste0(text, "<li> ELEFAN: Estimation of growth parameters of the von Bertlanffy growth (VBG) function.</li>")
  text <- paste0(text, "<li> Empirical formulae: Estimation of the natural mortality rate (M).</li>")
  text <- paste0(text, "<li> Catch curve: Estimation of the total (Z) and fishing (F) mortality rate.</li>")
  text <- paste0(text, "<li> Yield per recruit analysis (YPR): Estimation of reference points based on fishing ",
                 "mortality and Spawning Potential Ratio (SPR), such as ",withMathJax("\\(F_{max}\\)"),", ",withMathJax("\\(F_{0.1}\\)"),", ",
                 withMathJax("\\(F_{35\\%SPR}\\)"),", as well as the stock status ",
                 "(e.g. ",withMathJax("\\(F/F_{0.1}\\)")," or SPR).</li>")
  text <- paste0(text, "</ol>")
  text <- paste0(text, "<br>")
  text <- paste0(text,
                 "<p>This length-based stock assessment routine was outlined in the technical report ",
                 "<a href='http://www.fao.org/documents/card/en/c/9bb12a06-2f05-5dcb-a6ca-2d6dd3080f65/'  target='blank_'> Introduction to Tropical Fish Stock Assessment (FAO, 1998)</a>",
                 ", and compiled into the R package ",
                 "<a href='https://cran.r-project.org/web/packages/TropFishR'  target='blank_'> TropFishR </a>",
                 " by ", "<a href='https://doi.org/10.1111/2041-210X.12791'  target='blank_'> Mildenberger et al. (2017)</a>",
                 ". The above mentioned methods make specific assumptions about the data, the stock, or the ",
                 "fisheries targeting the stock. It is important to be aware of these assumptions and, thus, the ",
                 "limitations of the results.</p>")
  text <- paste0(text, "<br>")
  text <- paste0(text, "<br>")
  text <- paste0(text, tropfishrAssumptionsHTML())
  return (text)
}

getResultConsiderationTextForElefan <- function() {
  text <- "<b>Data exploration:</b>"
  text <- paste0(text, "<br>")
  text <- paste0(text, "<br>")
  text <- paste0(text,
                 "Directly after the successful upload of your data set, <b>Figure 1</b> shows the length-frequency distributions per sampling time (x axis). Panel A shows the raw data, while panel B shows the restructured data, i.e. the data after subtracting the moving average (MA) from each length class. The combination of bin size and MA critically affects the separation of peaks (i.e. potential cohorts) in the dataset and thus the estimation of growth parameters by ELEFAN. Blue shading indicates a high count per length bin (panel A) and a high positive value (panel B). Red shading indicates a negative value (only panel B). A good bin size value reduces noise in the data by aggregation and should be defined before the MA value. A good MA value leads to visually distinct peaks, particularly among the smaller length classes.")
  text <- paste0(text, "<br><br>")
  text <- paste0(text, "After setting the parameter search space and running the assessment successfully (click 'Run Assessment'),
  two figures and a table summarise the results of the estimation of growth parameters by ELEFAN using the genetic algorithm (GA)
                 and four figures and three tables summarise the stock status results.")
  text <- paste0(text, "<br>")
  text <- paste0(text, "<br>")
  text <- paste0(text,"<b>ELEFAN:</b>")
  text <- paste0(text, "<br>")
  text <- paste0(text, "<br>")
  text <- paste0(text, "<ul>")
  text <- paste0(text, "<li><b>Figure 2</b> shows the same length frequency distributions as Figure 1 overlaid with the growth curves estimated by ELEFAN. This plot allows the visual inspection of how well the estimated curves connect the peaks in the length-frequency data (i.e. potential cohorts) in the raw (A) and restructured (B) data set.",
                 "</li>")
  text <- paste0(text, "<br>")
  text <- paste0(text, "<li><b>Table 1</b> includes the estimated von Bertlanffy growth parameters (",
                 withMathJax("\\(L_\\infty, K, t_a\\)"),"), the growth performance coefficient phi', and the best score value (Rn). The growth performance index is calculated based on the formula ",withMathJax("\\(phi' = \\log_{10} K + 2 \\log_{10} L_\\infty\\)"), "and provides a metric that accounts for the correlation of ",withMathJax("\\(L_\\infty\\)")," and ",withMathJax("\\(K\\)")," to compare growth parameters among analyses or species. The Rn value is the sum of all values associated with the bins after restructuring that intersect the growth curves. The value allows comparison of the fit of estimated growth parameters to the data set for different MA values, growth paramater search spaces, or ELEFAN optimisation parameters (e.g. population size), but not for different data sets or bin sizes.",
                 "</li>")
  text <- paste0(text, "<br>")
  text <- paste0(text, "<li><b>Figure 3 </b> shows the improvement of the ELEFAN fit in terms of the average and best score value (fitness value) of the genetic algorithm used in ELEFAN_GA over the number of iterations (generations). Ideally, the number of iterations (generations) is large enough so that there are no large jumps visible during the last iterations of the best and average score value.",
                 "</li>")
  text <- paste0(text, "<br>")
  text <- paste0(text, "<li><b>Figure 4 </b> shows the estimated total, fishing, and natural mortality rates by length. The dashed lines correspond to the average mortality rates for the main exploited length classes, i.e. the length classes that are considered in the regression analysis for the length-converted catch curve. Note, that the natural mortality rates estimated by the empirical formulae after Then et al. (2015) and Pauly et al. (1980) are independent of length. By contrast, the formulae after Gislason et al. (2010) and Lorenzen et al. (2022) estimate length-dependent natural mortality rates based on the asymptotic length, growth coefficient, and body length of the fish.",
                 "</li>")
  text <- paste0(text, "</ul>")
  text <- paste0(text, "<br>")
  text <- paste0(text, "<br>")
  text <- paste0(text, "<br>")
  text <- paste0(text,"<b>Stock status:</b>")
  text <- paste0(text, "<br>")
  text <- paste0(text, "<br>")
  text <- paste0(text, "<ul>")
  text <- paste0(text,
                 "<li><b>Figure 5</b> shows the logarithm of the catch per length interval against the relative age (x axis). ",
                 "Highlighted points were used in the regression analysis of the catch curve for the estimation of the total ",
                 "mortality rate (Z), which ",
                 "corresponds to the slope of the displayed regression line. The selection of points is automatic and based on a ",
                 "list of expert recommendations.",
                 "</li>")
  text <- paste0(text, "<br>")
  text <- paste0(text, "<li><b>Figure 6</b> shows the probability of capture as a measure of the selectivity used in the assessment. ",
                 "The curve is either based on selectivity parameters provided by the user (L50 and L75 or selection width) or was estimated ",
                 "by the catch curve. The selection ogive that is displayed is used for the Yield Per Recruit analysis (YPR).",
                 "</li>")
  text <- paste0(text, "<br>")
  text <- paste0(text, "<li><b>Table 2</b> lists the estimated mortality rates (Z, F, M), the exploitation ",
                 "rate (E), and the estimated/provided selectivity parameters (",withMathJax('\\(L_{50}\\)'),", ",
                 withMathJax('\\(L_{75}\\)'),").",
                 "</li>")
  text <- paste0(text, "<br>")
  text <- paste0(text, "<li><b>Table 3</b> includes the estimated reference points (",withMathJax('\\(F_{max}\\)'),", ",
                 withMathJax('\\(F_{0.1}\\)'),", ",withMathJax('\\(F_{0.5}\\)'),"), ",
                 "and the SPR-based reference points (",withMathJax('\\(F_{30\\%SPR}\\)'),",",withMathJax('\\(F_{35\\%SPR}\\)'),",",
                 withMathJax('\\(F_{40\\%SPR}\\)'),") if the maturity parameters (",withMathJax('\\(L_{m50}\\)')," and ",
                 withMathJax('\\(L_{m75}\\)'),") are provided.",
                 "</li>")
  text <- paste0(text, "<br>")
  text <- paste0(text, "<li><b>Table 4</b> presents the estimated stock status in terms of the current ",
                 "fishing mortality (F) relative to reference points (",withMathJax('\\(F_{max}\\)'),", ",
                 withMathJax('\\(F_{0.1}\\)'),", ",withMathJax('\\(F_{0.5}\\)'),"). If the maturity ",
                 "parameters (",withMathJax('\\(L_{m50}\\)')," and ",
                 withMathJax('\\(L_{m75}\\)'),") are provided, additional reference points (",withMathJax('\\(F_{30\\%SPR}\\)'),
                 ", ",withMathJax('\\(F_{35\\%SPR}\\)'),", ",
                 withMathJax('\\(F_{40\\%SPR}\\)'),") and the current Spawning Potential Ratio (SPR) are included as well.",
                 "</li>")
  text <- paste0(text, "<br>")
  text <- paste0(text,
                 "<li><b>Figure 7</b> shows the results of the Yield Per Recruit (YPR) model as yield and biomass per recruit ",
                 "in panel A and B, respectively, against a range of fishing mortality rates (x axis). ",
                 "Grey segments display the estimated reference points. Fmax is defined as the fishing ",
                 " mortality (F) leading to the maximum yield per recruit. ",withMathJax('\\(F_{0.1}\\)'),
                 " corresponds to F where the slope of the yield per ",
                 "recruit curve is equal to 10% of the slope in the origin and represents a more conservative reference point than Fmax. ",
                 "F0.5 corresponds to F where the biomass per recruit is equal to 50% of the biomass per recruit without fishing. ",
                 "If the maturity parameters ",withMathJax('\\(L_{m50}\\)')," and ",
                 withMathJax('\\(L_{m75}\\)')," are provided, an optional third panel (C) shows the Spawning Potential Ratio (SPR) ",
                 "for a range of fishing mortality rates. Furthermore, the three reference points ",
                 withMathJax('\\(F_{30\\%SPR}\\)'),", ",
                 withMathJax('\\(F_{35\\%SPR}\\)'),", and ",
                 withMathJax('\\(F_{40\\%SPR}\\)')," are shown, which ",
                 "correspond to F that leads to an SPR of 30%, 35%, and 40% respectively.",
                 "</li>")
  text <- paste0(text, "<br>")
  text <- paste0(text, "<li><b>Figure 8</b> shows the yield per recruit (A) and biomass per recruit (B) for a range of fishing mortality ",
                 "rates (x axis) and gear selectivity (y axis) combinations. Colors indicate high (red) to low (blue) yield and biomass.",
                 "The black dot indicates the current yield and biomass per recruit.",
                 "</li>")
  text <- paste0(text, "<br>")
  text <- paste0(text, "<li><b>Table 5</b> Parameters estimated with TropFishR that can be used as input for the other length-based stock assessment methods (e.g. LBI and LBSPR).",
                 "</li>")
  text <- paste0(text, "<br>")
  text <- paste0(text, "</ul>")
    return (text)
}



## LBI
## ------------------------------------------------------
getDataConsiderationTextForLBI <- function() {
    text <- "<b>Two data formats are accepted:</b><br>"
  text <- paste0(text, "<b>1. Format: Length frequency data must include:</b>")
  text <- paste0(text, "<ul>")
  text <- paste0(text,
                 "<li>A column indicating the length classes of measured individuals (first column of dataset).</li>")
  text <- paste0(text, "<li>A row indicating the dates when individuals were measured (first row of dataset excluding the first column).</li>")
  text <- paste0(text,
                 "<li>The number of individuals caught per length class (rows) and per sampling date (columns).</li>")
  text <- paste0(text, "</ul>")
  text <- paste0(text, "<br>")
  text <- paste0(text, "<p> An example dataset in this format can be downloaded here (",linkSE,"). </p>")
  text <- paste0(text, "<br>")
  text <- paste0(text, "<b>2. Format: Raw length measurements must include:</b>")
  text <- paste0(text, "<ul>")
  text <- paste0(text, "<li>A column indicating the dates when individuals were measured (first column of dataset).</li>")
  text <- paste0(text,
                 "<li>A column indicating the length of measured individuals (second column of dataset).</li>")

  text <- paste0(text,
                 "<li>Optionally: A column indicating the number of individuals of the given length (third column of the dataset).</li>")
  text <- paste0(text, "</ul>")
  text <- paste0(text, "<br>")
  text <- paste0(text, "<p> An example dataset in this format can be downloaded here (",linkEA,"). </p>")
  text <- paste0(text, "<br>")
  text <- paste0(text, "<br>")
  text <- paste0(text, "<b>Specific considerations regarding your own dataset:</b>")
  text <- paste0(text, "<ul>")
  text <- paste0(text, "<li>", "Save your dataset in 'csv' format.", "</li>")
  text <- paste0(text, "<li>", "The separator for the .csv file should be a comma ‘,’ semicolon ';' or tab. The default might differ depending on the language settings of your spreadsheet manipulation program (e.g. Excel).", "</li>")
  text <- paste0(text, "<li>", "Use a '.' to separate decimals in the data. The default might differ depending on the language settings of your spreadsheet manipulation program (e.g. Excel).", "</li>")
  text <- paste0(text, "<li>The date format of your data file should be automatically detected, or select the date format used in your data file under <b>'Choose CSV date format'</b>, e.g. DD/MM/YYYY format to select 'Day Month Year'.</li>")
  text <- paste0(text, "<li>For data format 1, the columns should represent catches representative for a whole year. If no date column is provided for data format 2, it is assumed that all samples are representative for a single year.</li>")
  text <- paste0(text, "<li>Your data set should at least be representative of a whole year.</li>")
  text <- paste0(text, "</ul>")
  text <- paste0(text, "<br>")

  text <- paste0(text, "<br>")
  text <- paste0(text, "<p> Further information about differnet formats of length-frequency datasets ",
                 "and how to convert one format to the other are described in a ",
                 "<a href='https://cran.r-project.org/web/packages/TropFishR/vignettes/lfqData.html'  target='blank_'>tutorial by Mildenberger (2020)</a>. </p>")

    return (text)
}

getWorkflowConsiderationTextForLBI <- function() {
  text <- "<h4> To run this length-based workflow in the Stock Monitoring Tool:</h4>"
  text <- paste0(text, "<ol>")
  text <- paste0(text, "<li> Upload a size frequency data set (see Data Considerations or one of the sample datasets (e.g., ",linkSE,"))</li>")
  text <- paste0(text, "<li> Specify life history parameters such as the M/K ratio, Linf and Lm50. </li>")
  text <- paste0(text, "<li> If available specify the parameters of the length-weight relationship. </li>")
  text <- paste0(text, "<li> Run the assessment ('Run Assessment' button). </li>")
  text <- paste0(text, "<li> Download an automated assessment report as a pdf document ('Download Report' button). The report will also be automatically uploaded to your private workspace. </li>")
text <- paste0(text, "<li> Download the results as a zip archive ('Download Results (zip)' button).</li>")
  text <- paste0(text, "<li> The 'Reset' button removes the uploaded data and resets the settings to default vealues.</li>")
  text <- paste0(text, "</ol>")
  text <- paste0(text, "Further information can be found in the popup information buttons at each field, and in the Data, Methods, and Results Considerations tabs. Note, that error messages may display in the center of the page and in place of any figures where an error has occurred.")
  text <- paste0(text, "</p>")
    return (text)
}

getMethodConsiderationTextForLBI <- function() {
  text <- "<b>Length-based indicators:</b>"
  text <- paste0(text, "<br><br>")
  text <- paste0(text, '<table style="width:100%;text-align:center;" border=1px>')
  text <- paste0(text, '<tr bgcolor="#eee" align="center">')
  text <- paste0(text,
                 '<th style="width:15%">Indicator</th>',
                 '<th style="width:30%">Description</th>',
                 '<th style="width:10%">Reference point</th>',
                 '<th style="width:15%">Indicator ratio</th>',
                 '<th style="width:10%">Expected value</th>',
                 '<th style="width:20%">Property</th>'
                 )
  text <- paste0(text, '</tr>')
  text <- paste0(text, '<tr>')
  text <- paste0(text,
                 '<td>',
                 withMathJax('\\(L_{max5\\%}\\)'),
                 '</td>',
                 '<td>',
                 'Mean length of the largest 5% of individuals in the catch',
                 '</td>',
                 '<td rowspan="2">',
                 withMathJax('\\(L_{\\infty}\\)'),
                 '</td>',
                 '<td>',
                 withMathJax('\\(L_{max5\\%}/L_{\\infty}\\)'),
                 '</td>',
                 '<td rowspan="2">',
                 "> 0.8",
                 '</td>',
                 '<td rowspan="3">',
                 "Conservation (large individuals)",
                 '</td>'
                 )
  text <- paste0(text, '</tr>')
  text <- paste0(text, '<tr>')
  text <- paste0(text,
                 '<td>',
                 withMathJax('\\(L_{95\\%}\\)'),
                 '</td>',
                 '<td>',
                 '95% percentile of length distribution',
                 '</td>',
                 '<td>',
                 withMathJax('\\(L_{95\\%}/L_{\\infty}\\)'),
                 '</td>'
                 )
  text <- paste0(text, '</tr>')
  text <- paste0(text, '<tr>')
  text <- paste0(text,
                 '<td>',
                 withMathJax('\\(P_{mega}\\)'),
                 '</td>',
                 '<td>',
                 'Proportion of fish larger than optimal harvest length (',
                 withMathJax('\\(L_{opt}\\)'),') + 10%',
                 '</td>',
                 '<td>',
                 '0.3',
                 '</td>',
                 '<td>',
                 withMathJax('\\(P_{mega}\\)'),
                 '</td>',
                 '<td>',
                 "> 0.3",
                 '</td>'
                 )
  text <- paste0(text, '</tr>')
  text <- paste0(text, '<tr>')
  text <- paste0(text,
                 '<td>',
                 withMathJax('\\(L_{25\\%}\\)'),
                 '</td>',
                 '<td>',
                 '25% percentile of length distribution',
                 '</td>',
                 '<td>',
                 withMathJax('\\(L_{mat}\\)'),
                 '</td>',
                 '<td>',
                 withMathJax('\\(L_{25\\%}/L_{mat}\\)'),
                 '</td>',
                 '<td rowspan="2">',
                 "> 1",
                 '</td>',
                 '<td rowspan="2">',
                 "Conservation (immatures)",
                 '</td>'
                 )
  text <- paste0(text, '</tr>')
  text <- paste0(text, '<tr>')
  text <- paste0(text,
                 '<td>',
                 withMathJax('\\(L_{c}\\)'),
                 '</td>',
                 '<td>',
                 'Length at 50% modal abundance',
                 '</td>',
                 '<td>',
                 withMathJax('\\(L_{mat}\\)'),
                 '</td>',
                 '<td>',
                 withMathJax('\\(L_{c}/L_{mat}\\)'),
                 '</td>'
                 )
  text <- paste0(text, '</tr>')
  text <- paste0(text, '<tr>')
  text <- paste0(text,
                 '<td>',
                 withMathJax('\\(L_{mean}\\)'),
                 '</td>',
                 '<td>',
                 'Mean length of individuals ', withMathJax('\\(> L_{c}\\)'),
                 '</td>',
                 '<td>',
                 withMathJax('\\(L_{opt}\\)'),
                 '</td>',
                 '<td>',
                 withMathJax('\\(L_{mean}/L_{opt}\\)'),
                 '</td>',
                 '<td rowspan="2">',
                 withMathJax('\\(\\approx 1\\)'),
                 '</td>',
                 '<td rowspan="2">',
                 "Optimal yield",
                 '</td>'
                 )
  text <- paste0(text, '</tr>')
  text <- paste0(text, '<tr>')
  text <- paste0(text,
                 '<td>',
                 withMathJax('\\(L_{maxy}\\)'),
                 '</td>',
                 '<td>',
                 'Length class with maximum biomass in catch',
                 '</td>',
                 '<td>',
                 withMathJax('\\(L_{opt}\\)'),
                 '</td>',
                 '<td>',
                 withMathJax('\\(L_{maxy}/L_{opt}\\)'),
                 '</td>'
                 )
  text <- paste0(text, '</tr>')
  text <- paste0(text, '<tr>')
  text <- paste0(text,
                 '<td>',
                 withMathJax('\\(L_{mean}\\)'),
                 '</td>',
                 '<td>',
                 'Mean length of individuals ', withMathJax('\\(> L_{c}\\)'),
                 '</td>',
                 '<td>',
                 withMathJax('\\(L_{F=M}\\)'),
                 '</td>',
                 '<td>',
                 withMathJax('\\(L_{mean}/L_{F=M}\\)'),
                 '</td>',
                 '<td>',
                 withMathJax('\\(\\leq 1\\)'),
                 '</td>',
                 '<td>',
                 "MSY",
                 '</td>'
                 )
  text <- paste0(text, '</tr>')
  text <- paste0(text, '</table>')
  text <- paste0(text, "<br>")
  text <- paste0(text,
                 "<p>These indicators make specific assumptions about the data, the stock, or the ",
                 "fisheries targeting the stock. It is important to be aware of these assumptions and, thus, the ",
                 "limitations of the results.</p>")
  text <- paste0(text, "<br>")
  text <- paste0(text, "<br>")
  text <- paste0(text, lbiAssumptionsHTML())
  return (text)
}

getResultConsiderationTextForLBI <- function() {
  text <- "<b>Data exploration:</b>"
  text <- paste0(text, "<br>")
  text <- paste0(text, "<br>")
  text <- paste0(text, "Directly after the successful upload of your data set, <b>Figure 1</b> shows the length-frequency distributions for each year. The bin size affects the binning of the data and thus the
distributions in this graph. A good bin size is as small as possible while at the same time large enough
to reduce the noise in the data.")
  text <- paste0(text, "<br><br><br>")
  text <- paste0(text, "<b>Stock status indicators:</b>")
  text <- paste0(text, "<br>")
  text <- paste0(text, "<br>")
  text <- paste0(text, "After inserting the required input parameters and calculating the indicators (click 'Run assessment'), two tables and one figure summarise the estimated length-based indicators and ratios relative to reference points.")
  text <- paste0(text, "<br>")
  text <- paste0(text, "<br>")
  text <- paste0(text, "<ul>")
  text <- paste0(text, "<li><b>Table 1</b> includes the estimated length-based indicators. Note, ",
                 "that the indicator ", withMathJax('\\(L_{maxy}\\)'),
                 " is only estimated and included in this table if the parameters of the length-weight ",
                 "relationship (a and b) are specified. The indicators in this table relative to the ",
                 "respective reference points are used to derive the stock status in Table 2 and Figure 2.",
                 "</li>")
  text <- paste0(text, "<br>")
  text <- paste0(text, "<li><b>Table 2 </b> includes the length-based indicators relative to ",
                 "specific reference levels for each LBI (columns) for each (selected) year of the uploaded ",
                 "data set (rows). Based on expected values listed in the LBI ",
                 "description table in the Methods tab, the cell with each ratio is either highlighted ",
                 "green or red ",
                 "indicating that the ratio is above or below the expected value, respectively. Ideally, ",
                 "all cells are highlighted green for a given year, thus, showing no indication of ",
                 "overfishing. Note, that green and red cells in the same year do not mean that the ",
                 "indicators contradict each other, but could indicate that the stock performs ",
                 "differently regarding different properties (see LBI description table in the methods tab ",
                 "for more details on the ",
                 "properties of each indicator). For example, the indicators could indicate that the ",
                 "proportion of older larger individuals is smaller than expected (red ",
                 withMathJax('\\(L_{95\\%}/L_{\\infty}\\)'),") ",
                 "but at the same time the proportion of immature individuals is as expected ",
                 "(green ", withMathJax('\\(L_{25\\%}/L_{mat}\\)'),
                 "). Generally, the more indicators are red, the larger the likeliness of ",
                 "overexploitation.",
                 "</li>")
  text <- paste0(text, "<br>")
  text <- paste0(text, "<li><b>Figure 2</b> shows the estimated length-based indicators relative to reference ",
                 "levels as a point for each year. The expected values for each LBI ratio ",
                 "are represented as dashed horizontal lines.",
                 "Ideally, the indicator ratios are equal or close to the expected values, e.g. ",
                 withMathJax('\\(L_{mean}/L_{opt}\\)'),
                 " is equal or close to 1. The further the points/lines are from the ",
                 "expected values of the various indicator ratios, the more evidence of overfishing.",
                 "Whenever the graph includes a greenish area the horizontal line represents a ",
                 "limit reference point, rather than a target reference point and any value above ",
                 "the limit reference point (i.e. in the greenish area) is considered desirable.",
                 "</li>")
  text <- paste0(text, "</ul>")
    return (text)
}




## LBSPR
## ------------------------------------------------------
getDataConsiderationTextForLBSPR <- function() {
  text <- "<b>Two data formats are accepted:</b><br>"
  text <- paste0(text, "<b>1. Format: Length frequency data must include:</b>")
  text <- paste0(text, "<ul>")
  text <- paste0(text,
                 "<li>A column indicating the length classes of measured individuals (first column of dataset).</li>")
  text <- paste0(text, "<li>A row indicating the dates when individuals were measured (first row of dataset excluding the first column).</li>")
  text <- paste0(text,
                 "<li>The number of individuals caught per length class (rows) and per sampling date (columns).</li>")
  text <- paste0(text, "</ul>")
  text <- paste0(text, "<br>")
  text <- paste0(text, "<p> An example dataset in this format can be downloaded here (",linkSE,"). </p>")
  text <- paste0(text, "<br>")
  text <- paste0(text, "<b>2. Format: Raw length measurements must include:</b>")
  text <- paste0(text, "<ul>")
  text <- paste0(text, "<li>A column indicating the dates when individuals were measured (first column of dataset).</li>")
  text <- paste0(text,
                 "<li>A column indicating the length of measured individuals (second column of dataset).</li>")

  text <- paste0(text,
                 "<li>Optionally: A column indicating the number of individuals of the given length (third column of the dataset).</li>")
  text <- paste0(text, "</ul>")
  text <- paste0(text, "<br>")
  text <- paste0(text, "<p> An example dataset in this format can be downloaded here (",linkEA,"). </p>")
  text <- paste0(text, "<br>")
  text <- paste0(text, "<br>")
  text <- paste0(text, "<b>Specific considerations regarding your own dataset:</b>")
  text <- paste0(text, "<ul>")
  text <- paste0(text, "<li>", "Save your dataset in 'csv' format.", "</li>")
  text <- paste0(text, "<li>", "The separator for the .csv file should be a comma ‘,’ semicolon ';' or tab. The default might differ depending on the language settings of your spreadsheet manipulation program (e.g. Excel).", "</li>")
  text <- paste0(text, "<li>", "Use a '.' to separate decimals in the data. The default might differ depending on the language settings of your spreadsheet manipulation program (e.g. Excel).", "</li>")
  text <- paste0(text, "<li>The date format of your data file should be automatically detected, or select the date format used in your data file under <b>'Choose CSV date format'</b>, e.g. DD/MM/YYYY format to select 'Day Month Year'.</li>")
  text <- paste0(text, "<li>For data format 1, the columns should represent catches representative for a whole year. If no date column is provided for data format 2, it is assumed that all samples are representative for a single year.</li>")
  text <- paste0(text, "<li>Your data set should at least be representative of a whole year.</li>")
  text <- paste0(text, "</ul>")
  text <- paste0(text, "<br>")

  text <- paste0(text, "<br>")
  text <- paste0(text, "<p> Further information about differnet formats of length-frequency datasets ",
                 "and how to convert one format to the other are described in a ",
                 "<a href='https://cran.r-project.org/web/packages/TropFishR/vignettes/lfqData.html'  target='blank_'>tutorial by Mildenberger (2020)</a>. </p>")

    return (text)
}

getWorkflowConsiderationTextForLBSPR <- function() {
  text <- "<h4> To run this length-based workflow in the Stock Monitoring Tool:</h4>"
  text <- paste0(text, "<ol>")
  text <- paste0(text, "<li> Upload a size frequency data set (see Data Considerations or one of the sample datasets (e.g., ",linkSE,"))</li>")
  text <- paste0(text, "<li> Specify life history parameters such as the ratio of natural mortality and the von Bertlanaffy growth rate (",withMathJax('\\(M/K\\)'),"), the asymptotic length of the von Bertalanffy growth function (",withMathJax('\\(L_{\\infty}\\)'),"), and the maturity parameters (",withMathJax('\\(L_{m50}\\)')," and ",withMathJax('\\(L_{m95}\\)'),"). </li>")
  text <- paste0(text, "<li> If available specify the parameters of the length-weight relationship (a and b). Note, that the default values of the LBSPR method of 1e-4 and 3 are assumed for these two parameters, respectively, if not specified. </li>")
  text <- paste0(text, "<li> Run the assessment ('Run Assessment' button). </li>")
  text <- paste0(text, "<li> Download an automated assessment report as a pdf document ('Download Report' button). The report will also be automatically uploaded to your private workspace. </li>")
text <- paste0(text, "<li> Download the results as a zip archive ('Download Results (zip)' button).</li>")
  text <- paste0(text, "<li> The 'Reset' button removes the uploaded data and resets the settings to default vealues.</li>")
  text <- paste0(text, "</ol>")
  text <- paste0(text, "Further information can be found in the popup information buttons at each field, and in the Data, Methods, and Results Considerations tabs. Note, that error messages may display in the center of the page and in place of any figures where an error has occurred.")
  text <- paste0(text, "</p>")
    return (text)
}

getMethodConsiderationTextForLBSPR <- function() {
  text <- "<b>Length-based spawning potential ratio (LBSPR):</b>"
  text <- paste0(text, "<br>")
  text <- paste0(text,
                 "<p>This method makes specific assumptions about the data, the stock, or the ",
                 "fisheries targeting the stock. It is important to be aware of these assumptions and, thus, the ",
                 "limitations of the results.</p>")
  text <- paste0(text, "<br>")
  text <- paste0(text, "<br>")
  text <- paste0(text, lbsprAssumptionsHTML())
  return (text)
}

getResultConsiderationTextForLBSPR <- function() {
  text <- "<b>Data exploration:</b>"
  text <- paste0(text, "<br>")
  text <- paste0(text, "<br>")
  text <- paste0(text, "Directly after the successful upload of your data set, ",
                 "<b>Figure 1</b> shows the length-frequency distributions for each year. ",
                 "The bin size affects the binning of the data and thus the distributions in ",
                 "this graph. A good bin size is as small as possible while at the same time ",
                 "large enough to reduce the noise in the data.")
  text <- paste0(text, "<br><br><br>")
  text <- paste0(text, "<b>Stock status and selectivity</b>")
  text <- paste0(text, "<br>")
  text <- paste0(text, "<br>")
  text <- paste0(text, "After inserting the required input parameters and running the assessment ",
                 "(click 'Run Assessment'), one table and three figures summarise the estimated ",
                 "spawning potential ratio, selectivity patterns, and the ratio of fishing and ",
                 "natural mortality rate.")
  text <- paste0(text, "<br>")
  text <- paste0(text, "<br>")
  text <- paste0(text, "<ul>")
  text <- paste0(text, "<li><b>Table 1</b> presents the estimated spawning potential ratio (SPR) in ",
                 "percent, the estimated selectivity parameters (",
                 withMathJax('\\(L_{s50\\%}\\)')," and ", withMathJax('\\(L_{s95\\%}\\)'),") in the",
                 "same length unit as uploaded length measurements, ",
                 "and the fishing mortality relative to the natural mortality (F/M) for each year ",
                 "included in the assessment. The uncertainty of estimated parameters is provided as ",
                 "the 95% confidence limits in brackets.",
                 "</li>")
  text <- paste0(text, "<br>")
  text <- paste0(text, "<li><b>Figure 2 </b> shows the estimated spawning potential ratio (SPR) value ",
                 "in relation to the SPR reference points. The estimated SPR is indicated by the dashed line ",
                 "with the SPR value next to the dashed line. The colored areas indicate the SPR reference points: ",
                 "The red area/line indicates the proportion of SPR below the limit reference point; the green area/line ",
                 "indicates the proportion above the limit and below the target reference point; and the the ",
                 "yellow area indicates the proportion above the target reference point. ",
                 "",
                 "Note, that when the ",
                 "assessment is done for multiple years, only the last year of the assessment is shown in this ",
                 "graph.",
                 "</li>")
  text <- paste0(text, "<br>")
  text <- paste0(text, "<li><b>Figure 3 </b> presents the provided maturity information and",
                 "estimated selectivity information. The curves indicate the proportion of the ",
                 "stock that is mature or vulnerable to the gear (y axis) at a given length (x axis).",
                 "</li>")
  text <- paste0(text, "<br>")
  text <- paste0(text, "<li><b>Figure 4 </b> shows a time series plot of the estimated selectivity parameters, ",
                 "fishing mortality relative to natural mortality, and spawning potential ratio (SPR) with 95% confidence intervals. The lines ",
                 "correspond to the smoothed estimates over time.",
                 "Note, that the graphs only show single points when the assessment spans a single year.",
                 "</li>")
  text <- paste0(text, "</ul>")
    return (text)
}



## SPiCT
## ------------------------------------------------------
getDataConsiderationTextForSpict <- function() {
    text <- "<b>Two data formats are accepted:</b><br>"
    text <- paste0(text, "<br>")
    text <- paste0(text, "<b>1. Wide Format:</b>")
    text <- paste0(text, "One row per time step (e.g., year), with separate columns for <i>Catch</i> and each <i>Index</i> series (e.g., <code>Index1</code>, <code>Index2</code>). Dates/years are in a dedicated columns. Columns with the relative uncertainty scaling over time for the catch and each index time series are optional.")
    text <- paste0(text, "<br>")
    text <- paste0(text, "<p> An example dataset in this format can be downloaded here (",linkAL,"). </p>")
    text <- paste0(text, "<br>")
    text <- paste0(text, "<b>2. Long format:</b>")
    text <- paste0(text, "One row per observation with columns such as <i>Time</i>, <i>Series</i> (index name), and <i>Value</i>. This is tidy and convenient when you have many indices or missing years. A variable indicating the relative uncertainty scaling for the catch and each index time series is optional.")
    text <- paste0(text, "<br>")
    text <- paste0(text, "<p> An example dataset in this format can be downloaded here (",linkLO,"). </p>")
    text <- paste0(text, "<br>")
    text <- paste0(text, "<br>")
    text <- paste0(text, "<b>Specific considerations regarding your own dataset:</b>")
    text <- paste0(text, "<ul>")
    text <- paste0(text, "<li>", "Save your dataset in 'csv' format.", "</li>")
    text <- paste0(text, "<li>", "The separator for the .csv file should be a comma ‘,’ semicolon ';' or tab. The default might differ depending on the language settings of your spreadsheet manipulation program (e.g. Excel).", "</li>")
    text <- paste0(text, "<li>", "Use a '.' to separate decimals in the data. The default might differ depending on the language settings of your spreadsheet manipulation program (e.g. Excel).", "</li>")
    text <- paste0(text, "<li>The catch is usually assumed to be representative of a calendar year, but could also span shorter periods, such as semesters, quarters or months. The corresponding time information should reflect the beginning of the catch interval. For example, for the catch in the period from 2020 to 2021, the corresponding time should be 2020.</li>")
    text <- paste0(text, "<li>By contrast, the relative abundance index observations correspond to a specific time of the year, such as 2020.25 for the first of April in 2020. Dates in various formats (e.g., 2020/04/01) or numeric times can be defined (e.g., 2020.25). If the timing is the same every year, also the input field 'Time of year (index)' can be used to specify the time of year.</li>")
    text <- paste0(text, "<li>The date format of your data file should be automatically detected, or select the date format used in your data file under <b>'Choose CSV date format'</b>, e.g. DD/MM/YYYY format to select 'Day Month Year'.</li>")
    text <- paste0(text, "</ul>")
    text <- paste0(text, "<br>")
    return (text)
}

getWorkflowConsiderationTextForSpict <- function() {
    text <- "<h4> To run this data-limited workflow in the Stock Monitoring Tool :</h4>"
    text <- paste0(text, "<ol>")
    text <- paste0(text, "<li> Upload a data set with the required information (see Data Considerations or one of the sample datasets (e.g., ",linkAL,"))</li>")
    text <- paste0(text, "<li> Adjust the Assessment Settings")
    text <- paste0(text, "<ol type='a'>")
    text <- paste0(text, "<li> Data - select the years of data to include in the analysis, adjust the timing of the index if not already done in the uploaded data set, set the Euler time step, and the catch unit. </li>")
    text <- paste0(text, "<li> Priors - modify and define priors used in the maximum likelihood estimation </li>")
    text <- paste0(text, "</ol>")
    text <- paste0(text, "</li>")
    text <- paste0(text, "<li> Check and Run the Assessment:")
    text <- paste0(text, "<ol type='a'>")
    text <- paste0(text, "<li> At any time you can press the 'Run Check' button and it will inform you if uploaded data and settings are ready for running the assessment or if anything needs to be modified or added.</li>")
    text <- paste0(text, "<li> Run the assessment ('Run Assessment' button). Note, that the model fitting can take some time (up to some minutes, depending on the time series length and complexity.</li>")
    text <- paste0(text, "</ol>")
    text <- paste0(text, "</li>")
    text <- paste0(text, "<li> Download the report as a pdf (Download Report button). The report should also automatically upload to your private workspace. </li>")
    text <- paste0(text, "<li> The Reset button removes the uploaded data and resets the settings to default</li>")
    text <- paste0(text, "</ol>")
    text <- paste0(text, "Further information can be found in the popup information buttons at each field, and in the Data, Methods, and Results Considerations tabs. Note that error messages may display in the center of the page and in place of any figures where an error has occurred.")
    text <- paste0(text, "</p>")
    return (text)
}


getMethodConsiderationTextForSpict <- function() {

    text <- "<b>Consecutive steps of the SPiCT data-limited stock assessment workflow:</b><br>"

    text <- paste0(text, "<ol>",
                   "<li><b>Data preparation</b>: Compile a time series of total catch (in weight) and at least one relative abundance index (e.g. survey index or CPUE). ",
                   "Ideally, the time series spans 10 or more years to provide sufficient information for estimation.</li>",

                   "<li><b>Model specification</b>: SPiCT fits a stochastic surplus production model in continuous time using a state–space framework. ",
                   "Separate process and observation errors are estimated, and priors may be set for key parameters such as intrinsic growth rate (<i>r</i>) and shape parameter (<i>n</i>).</li>",

                   "<li><b>Parameter estimation</b>: The model jointly estimates biological parameters (e.g. carrying capacity <i>K</i>, <i>r</i>, process variance) ",
                   "and time series of biomass (<i>B</i>) and fishing mortality (<i>F</i>) using maximum likelihood and numerical optimization.</li>",

                   "<li><b>Reference points</b>: From the fitted model, management reference points are derived, including ",
                   "biomass at MSY (<i>B</i><sub>MSY</sub>) and fishing mortality at MSY (<i>F</i><sub>MSY</sub>).</li>",

                   "<li><b>Stock status</b>: The current stock status is expressed relative to these reference points ",
                   "(e.g. ratios <i>B</i>/<i>B</i><sub>MSY</sub> and <i>F</i>/<i>F</i><sub>MSY</sub>), with confidence intervals obtained via the covariance matrix or parametric bootstrapping.</li>",

                   "<li><b>Forecasts and advice</b>: Short-term projections under alternative catch scenarios can be generated to evaluate the risk of exceeding reference points, ",
                   "informing catch advice in line with precautionary management frameworks.</li>",
                   "</ol><br>")

    text <- paste0(text,
                   "<p>SPiCT was developed to improve the estimation of stock status for data-limited situations, ",
                   "while retaining the core structure of surplus production models. ",
                   "It is particularly suited to stocks with irregular sampling intervals and noisy data, thanks to its continuous-time formulation.</p>"
                   )

    text <- paste0(text,
                   "<p>Further details on the model structure and applications can be found in: ",
                   "<a href='https://doi.org/10.1111/faf.12174' target='_blank'>Pedersen & Berg (2017)</a> and ",
                   "<a href='https://doi.org/10.1016/j.fishres.2024.107010' target='_blank'>Kokkalis et al. (2024)</a>.</p><br>"
                   )

    text <- paste0(text, spictAssumptionsHTML())

    return(text)
    }


    getResultConsiderationTextForSpict <- function() {
        text <- "<b>Data exploration:</b>"
        text <- paste0(text, "<br>")
        text <- paste0(text, "<br>")
        text <- paste0(text,
                       "Directly after the successful upload of your data set and assignment of required columns, <b>Figure 1</b> shows catch and index (indices) data over time; <b>Figure 2</b> shows the relative uncertainty scaling for the different time series. If this information is not included in the uploaded data set, equal relative uncertainty over time is assumed (equal to 1).")
        text <- paste0(text, "<br><br>")
        text <- paste0(text, "After adjusting assessment settings and running the assessment successfully (click 'Run Assessment'),
  one figure and several tables summarise the main results. Additional results and diagnostic tests are also produced.")
        text <- paste0(text, "<br>")
        text <- paste0(text, "<br>")
        text <- paste0(text,"<b>Main results:</b>")
        text <- paste0(text, "<br>")
        text <- paste0(text, "<br>")
        text <- paste0(text, "<ul>")
        text <- paste0(text, "<li><b>Figure 3</b> shows ...",
                       "</li>")
        text <- paste0(text, "<br>")
        text <- paste0(text, "<li><b>Table 1</b> includes the estimated model parameters (e.g. r,m,K,n) with 95% confidence intervals.</li><br>")
        text <- paste0(text, "<li><b>Table 2</b> includes the estimated stochastic reference points with 95% confidence intervals.</li><br>")
        text <- paste0(text, "<li><b>Table 3</b> includes the estimated states with 95% confidence intervals.</li><br>")
        text <- paste0(text, "<li><b>Table 4</b> includes the forecasted states with 95% confidence intervals.</li><br>")
        text <- paste0(text, "</ul>")
        text <- paste0(text, "<br>")
        text <- paste0(text, "<br>")
        text <- paste0(text, "<br>")
        text <- paste0(text,"<b>Additional results:</b>")
        text <- paste0(text, "<br>")
        text <- paste0(text, "<br>")
        text <- paste0(text, "<ul>")
        text <- paste0(text, "<li><b>Table 5</b> includes the estimated deterministic reference points with 95% confidence intervals.</li><br>")
        text <- paste0(text, "</ul>")
        text <- paste0(text, "<br>")
        text <- paste0(text, "<br>")
        text <- paste0(text, "<br>")
        text <- paste0(text,"<b>Diagnostics:</b>")
        text <- paste0(text, "<br>")
        text <- paste0(text, "<br>")
        text <- paste0(text, "<ul>")
        text <- paste0(text, "<li><b>Figure 6</b> shows the observation residuals.</li><br>")
        text <- paste0(text, "<li><b>Figure 7</b> shows the process residuals.</li><br>")
        text <- paste0(text, "</ul>")
        return (text)
    }




## Other
## ------------------------------

getErrorMessage <- function(forWhat) {
    if(forWhat=="CMSY"){
        return (paste0("Oops! Unfortunately the ",forWhat, " method experienced a problem with the server.<br/>Don't give up and try again in a few minutes or refresh your Stock Monitoring Tool instance.<hr/> <b>%s</b>"))
    }else{
        return (paste0("Oops! unfortunately something went wrong running the ",forWhat," method<br/>Don't give up and try again in a few minutes.<hr/> <b>%s</b>"))
    }
}




fishMethodsDataConsiderationText <- function() {
    text <- "<div>"
    text <- paste0(text, "<p>")
    text <- paste0(text, "<b>Mandatory fields to run YPR/SBPR are:</b>")
    text <- paste0(text, "<ul>")
    text <- paste0(text, "<li>age (the age of the fish)</li>")
    text <- paste0(text, "<li>ssbwgt (the spawning stock weights for each age)</li>")
    text <- paste0(text, "<li>partial (the recruitment at each age that is used to determine how much fishing mortality (F) each age group receives)</li>")
    text <- paste0(text, "<li>pmat (the proportion of mature fish at each age (used only for SBPR)</li>")
    text <- paste0(text, "</ul>")
    text <- paste0(text, "</p>")

    text <- paste0(text, "<p>")
    text <- paste0(text, "If you are creating your own dataset:<br/>")
    text <- paste0(text, "Ensure that the column names are identical to the sample dataset. Ensure your data are in .csv format. Use a “.” to separate decimals in the data.")
    text <- paste0(text, "</p>")

    text <- paste0(text, "<p>")
    text <- paste0(text, "<strong>Ensure that spawning stock weight-at-age data is representative of the full population, i.e., are all age groups sampled?</strong>")
    text <- paste0(text, "</p>")

    text <- paste0(text, "<p>")
    text <- paste0(text, "<i>**If desired, the life history parameters pulled from FishBase.org in the Supporting Tools: 'Natural Mortality Estimators' tool could be used to provide estimates of M in the Optional Parameters section.</i>")
    text <- paste0(text, "</p>")

    text <- paste0(text, "</div>")

    return (text)
}
