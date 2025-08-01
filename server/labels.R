########################### ALL LABELS ###########################
output$homeInfo <- renderText({
    session$userData$page("home")

    text <- "<h3>Stock Monitoring Tool for limited data</h3>"

    text <- paste0(text,"<p>")
    text <- paste0(text,"The Stock Monitoring Tool (SMT), developed by FAO, is an R-Shiny application, available to the global community and hosted via remote computational facilities with considerable processing resources.
This tool was designed to allow users with little to no programming experience to run methods developed for data-limited situations to evaluate and monitor the sustainability of fish stocks.  ")
    text <- paste0(text,'</p>')

    text <- paste0(text,"<h4>Navigating the Stock Monitoring Tool</h4>")

    text <- paste0(text,"<p>")

    text <- paste0(text,"<h5><b>Workflow</b></h5>")
    text <- paste0(text,"Each method is accessible in the menu to the left of the SMT, and by clicking on the desired method, further information is exposed in a dropdown
                   menu. Each method includes an <b>Introduction</b> page that gives background information on the method, including where the user can find details about the method
                   in the <a href='https://elearning.fao.org/course/view.php?id=502' target='blank_'>FAO SDG 14.4.1 eLearning module</a>. Details on the workflow specific to each method are available in the <b>Workflow</b> information tab at the top of each method's page. In general, data are uploaded, parameters are set, the method is run, and the results can be downloaded. ")
    text <- paste0(text,"</p>")

    text <- paste0(text,"<p>")
    text <- paste0(text,"<h5><b>Data</b></h5>")
    text <- paste0(text,"Advice is given on the formatting requirements and data considerations for each method on the introduction page, but also in the <b>Data</b> information tab on the top of
                   each method's page. If you would like to run the method(s) on your own dataset, we encourage you to review these considerations and align you data to the
                   required format. Each method has a sample dataset that the user can download from the <b>Sample Dataset</b> page. This can be used as a template to
                   format your own dataset. ")
    text <- paste0(text,"</p>")

    text <- paste0(text,"<p>")
    text <- paste0(text,"<h5><b>Method</b></h5>")
    text <- paste0(text,"Finally, you must navigate to the <b>method</b> page to upload either the sample dataset or your customised dataset. On the left hand side you can choose between the catch-based method, <b>CMSY</b>, or a <b>length-based workflow</b> including ELEFAN and YPR/SBPR methods. The <b>Supporting Tools</b> gives additional information on the underlying theory behind the stock monitoring methods. Additional information about the method, including key assumptions is available in the method information button at the top of each page.
                   Information and advice on <b>parameter settings</b> are available in the grey information buttons to the right of each field.
                   Once the optional parameters or priors are set for the method, <b>run</b> the method. ")
    text <- paste0(text,'</p>')

    text <- paste0(text, "<p>")
    text <- paste0(text, "The <b>CMSY</b> method is based on the 2019 9f version available on <a href='https://github.com/SISTA16/cmsy' target='_blank'> github </a> and provided by the <a href='https://i-marine.d4science.org/' target='_blank'> iMarine Infrastructure</a>. It requires internet connection to run.")
    text <- paste0(text, "</p>")

    text <- paste0(text, "<p>")
    text <- paste0(text, "The length-based workflow is based on the expert-recommended genetic algorithm of <b>ELEFAN</b> (ELEFAN_GA), and yield per recruit (YPR)/spawning biomass per recruit (SBPR) of the
                   <a href='https://cran.r-project.org/web/packages/TropFishR/index.html' target='_blank'> TropFishR </a> R package version ", packageVersion("TropFishR"))
    text <- paste0(text, "</p>")

    text <- paste0(text,"<p>")
    text <- paste0(text,"<h5><b>Results</b></h5>")
    text <- paste0(text,"The <b>results</b> will display as table and figures at the bottom of the method's page. <b>Download</b> the report to save these reports locally. They are also automatically saved to your private workspace. ")
    text <- paste0(text,'</p>')

    # text <- paste0(text, "<p>")
    # text <- paste0(text, "On the left hand side you can choose between the catch-based method, <b>CMSY</b>, or a <b>length-based workflow</b> including ELEFAN and YPR/SBPR methods. The Supporting Tools gives additional information on the underlying theory behind the stock monitoring methods.")
    # text <- paste0(text, "</p>")


    text <- paste0(text, "<p>")
    text <- paste0(text,"<h5><b>Run time</b></h5>")
    text <- paste0(text, "Run time with the sample dataset for both methods is <b>between 1-3 mins</b>. Run time for CMSY depends on the length of the catch time series analysed. Run time for the length-based workflow depends on the size of the length frequency dataset, the time aggregation, and the search space of input parameters as specified by the user. The information buttons note this effect for each relevant field.<br><br>")
    text <- paste0(text, "</p>")

   # text <- paste0(text, "<h4>Instruction to build a Docker image of this application : </h4>")
   # text <- paste0(text, "<p>")
   # text <- paste0(text, "A Dockerfile is provided that allows this tool to be run offline on any operating system. ")
   # text <- paste0(text, "To build and run the application on Linux, open a command window (terminal, bash, command prompt) on your computer and enter the following commands :")
   # text <- paste0(text, "<ul>")
   # text <- paste0(text, "<li>sudo wget https://raw.githubusercontent.com/pink-sh/StockMonitoringTool/master/Dockerfile</li>")
   # text <- paste0(text, "<li>sudo docker build -t stock_monitoring_tool . </li>")
   # text <- paste0(text, "<li>sudo docker run -p 3839:3838 stock_monitoring_tool</li>")
   # text <- paste0(text, "</ul>")
   # text <- paste0(text, "And then set your browser to http://localhost:3839")
   # text <- paste0(text, "</p>")

    text
})

output$cmsyIntroOut <- renderText({
    session$userData$page("cmsy-intro")

    sample_dataset_url = "https://data.d4science.org/shub/E_N0JSRlVEN3gwdmpjRnp5Y1BIWm5sS1QxZnUzUTRNSlp5ek50R2xlY0ZUZXVDUlFHTFFES3liblJGRSt4YWExMw=="

    text <- "<h3><b>CMSY - Catch-Maximum Sustainable Yield</b></h3>"
    text <- paste0(text, "<p>")
    # text <- paste0(text, "The <b>CMSY</b> method for data-limited stock assessment. Described in <a target='_blank' href='https://www.researchgate.net/publication/309283306_Estimating_fisheries_reference_points_from_catch_and_resilience'>Froese et al 2017</a>")
    text <- paste0(text, "The <b>CMSY</b> method for data-limited stock assessment is as described in <a target='_blank' href='https://www.researchgate.net/publication/309283306_Estimating_fisheries_reference_points_from_catch_and_resilience'>Froese et al. 2017</a>")
    text <- paste0(text, "<br/>")
    text <- paste0(text, "The algorithm can be found <a href='https://github.com/SISTA16/cmsy' target='_blank'>here</a> and the user's guide of best practices can be found <a href='https://github.com/SISTA16/cmsy/blob/master/CMSY_2019_9f_UserGuide.pdf' target='_blank'>here</a> on Github.")
    text <- paste0(text, "</p>")
    text <- paste0(text, "<p>")
    text <- paste0(text, "<br>")

    text <- paste0(text, "<h4>Method description</h4>")
    text <- paste0(text, "The Schaefer production model parameters are r and k. Different combinations of these parameters will produce different time series of biomass. In CMSY, the Schaefer model is run many times to calculate annual biomasses for r-k pairs randomly drawn from the prior distributions. The model determines which r-k pairs are valid: e.g., those pairs that result in a biomass time series that do not (1) result in a stock collapse or (2) allow the stock to exceed carrying capacity. Also, those r-k pairs that result in a final relative biomass estimate between the values specified in the inputs (the final depletion range), are accepted and used to calculate MSY (rk/4) and biomass over time. The geometric means of the resulting density distributions of r, k and MSY are taken as the most probable values.")
    text <- paste0(text, "</p>")
    text <- paste0(text, "<p>")

    text <- paste0(text, "<strong>Further information</strong> <br>")
    text <- paste0(text, "Please visit the <a href='https://elearning.fao.org/course/view.php?id=502' target='_blank'>SDG Indicator 14.4.1 - Fish stocks sustainability eLearning course</a>, <b>Lesson 4, Slides 13-26.</b>")
    text <- paste0(text, "<ul>")
    text <- paste0(text, "<li>Slides 13-17 give an overview of the method;</li>")
    text <- paste0(text, "<li>Slides 20 and 23 explain how to run the method in the Stock Monitoring Tool;</li>")
    text <- paste0(text, "<li>Slide 21 describes the required dataset format;</li>")
    text <- paste0(text, "<li>Slide 22 describes the default and optional parameters;</li>")
    text <- paste0(text, "<li>Slide 24, and its popup give examples of how to interpret the outputs;</li>")
    text <- paste0(text, "<li>Slide 25 describes the method’s caveats.</li>")
    text <- paste0(text, "</ul>")
    text <- paste0(text, "</p>")
    text <- paste0(text, "<p>")
    text <- paste0(text, "<br>")

    text <- paste0(text, "<h4>Data considerations</h4>")
    text <- paste0(text, "<strong>Mandatory fields to run CMSY: </strong>")
    text <- paste0(text, "<ul>")
    text <- paste0(text, "<li><b>Stock</b>: a unique fish stock name or identifier (e.g. “cod-2532”), repeated for each year.</li>")
    text <- paste0(text, "<li><b>yr</b>: the reporting year of the catch (e.g. 2004). One row for each year. Years have to be consecutive from the first to the last year without any missing years.</li>")
    text <- paste0(text, "<li><b>ct</b>: catch value, in tonnes (e.g. 12345). One row for each year. Gaps with no entries are not accepted and must be filled by interpolating missing or incorrect values, e.g., do not use zero as an entry if data are missing, instead use the mean of adjacent values to replace zero or to fill any gaps.</li>")
    text <- paste0(text, "<li><b>bt</b>: the value of the biomass (in tonnes, e.g. 34567), or the value of the CPUE or stock size index (e.g. 0.123), or NA if there is no information. Gaps filled with NA are acceptable for bt, i.e., abundance data can be fewer than catch data.</li>")
    text <- paste0(text, "</ul>")
    text <- paste0(text, "Other columns are identifiers that you may choose to include, but they are not necessary to run the model.<br><br>")
    text <- paste0(text, "Use the <a href=",sample_dataset_url,"> sample dataset </a> as a template to prepare your data.<br><br>")
    text <- paste0(text, "<b>Specific considerations regarding your own dataset:</b>")
    text <- paste0(text, "<ul>")
    text <- paste0(text, "<li>Save your dataset in 'csv' format.</li>")
    text <- paste0(text, "<li>The separator for the .csv file should be a comma ‘,’. The default might differ depending on the language settings of your spreadsheet manipulation program (e.g. Excel).</li>")

    text <- paste0(text, "<li>Note that years with missing biomass data should be filled with an 'NA' value.</li>")
    text <- paste0(text, "<li>Note that the column names of your dataset should exactly match those of the sample dataset.</li>")
    text <- paste0(text, "<li><b>Please ensure your time-series data are at least 15 years in length from starting year to ending year.</b></li>")
    text <- paste0(text, "</ul>")
    text <- paste0(text, "</p>")


    text <- paste0(text, "<p>")
    text <- paste0(text, "<br>")

    text <- paste0(text, "<h4>Run time </h4>")
    text <- paste0(text,'Run time varies with the length of the catch time series.')
    text <- paste0(text, "<ul>")
    text <- paste0(text, "<li>With the CMSY sample dataset run time is <b> 1-2 mins</b></li>")
    text <- paste0(text, "</ul>")
    text <- paste0(text, "</p>")
    text <- paste0(text, "<br>")

    text <- paste0(text,"<h4> Workflow Considerations </h4>")
    text <- paste0(text,"<strong> To run the CMSY method in the Stock Monitoring Tool :</strong>")
    text <- paste0(text, "<ol>")
    text <- paste0(text, "<li>Upload a csv file data set of catch time series for one or multiple stocks (see Data Considerations or the <a href='",sample_dataset_url,"'>CMSY Sample dataset</a>).</li>")
    text <- paste0(text, "<ol type='a'>")
    text <- paste0(text, "<li> Select the stock upon which to perform the analysis </li>")
    text <- paste0(text, "</ol>")
    text <- paste0(text, "<li> Adjust the Assessment Settings")
    text <- paste0(text, "<ol type='a'>")
    text <- paste0(text, "<li> Data Selection - select the years of data to include in the analysis,and over which to calculate the catchability. </li>")

    text <- paste0(text, "<li> Assessment settings - set the search space of the CMSY algorithm to estimate probable r-K pairs (i.e., set the resilience and depletion). </li>")
    text <- paste0(text, "<li> Optional information - if you have reference point values from previous assessments, you can choose to enter and compare them to the results of the present analysis.</li>")
    text <- paste0(text, "</ol>")
    text <- paste0(text, "</li>")
    text <- paste0(text, "<li> Run the Assessment:")
    text <- paste0(text, "<ol type='a'>")
    text <- paste0(text, "<li> Run the assessment (Run Assessment button). This may take some time. For example, the sample dataset takes between 1-2 mins.</li>")
    text <- paste0(text, "</ol>")
    text <- paste0(text, "</li>")
    text <- paste0(text, "<li> Download the report as a pdf (Download Report button).</li>")
    text <- paste0(text, "<li> The Reset button removes the uploaded data and resets the settings to default/</li>")
    text <- paste0(text, "</ol>")
    text <- paste0(text, "Further information can be found in the popup information buttons at each field, and in the Data, Methods, and Results Considerations tabs. Note that error messages may display in the center of the page and in place of any figures where an error has occurred.")
    text <- paste0(text, "</p>")
    text <- paste0(text, "<br>")

    text <- paste0(text, "<p>")
    text <- paste0(text, "<h4> References</h4>")
    text <- paste0(text,"Froese, Rainer & Demirel, Nazli & Coro, Gianpaolo & Kleisner, Kristin & Winker, Henning. (2017). Estimating fisheries reference points from catch and resilience. Fish and Fisheries. 18. 506-526. 10.1111/faf.12190.")
    text <- paste0(text, "</p>")
    text
})



## Length-based Methods General Introduction
## ---------------------------------
output$lbmIntroOut <- renderText({
    session$userData$page('lbm-intro')
    text <- "<h3><b> Length-based stock assessment methods </b></h3>"
    text <- paste0(text, "<br>")

    text <- paste0(text, "<p>")
    text <- paste0(text, "Length-based stock assessment methods comprise an important suit of data-limited stock assessment methods that allow to infer the stock status based on information of length measurements. There is a variety of length-based methods available. Three methods are currently implemented in the SMT: (1) length-based stock assessment with TropFishR, (2) length-based indicators, and (3) length-based spawning potential ratio. While the first method estimates all parameters from the length masurements directly, such as growth parameters or natural mortality, the other two methods require this information as input. Thus, estimated growth and mortality parameters from the first method could also be used for the other two methods.")
    text <- paste0(text, "</p>")
    text <- paste0(text, "<br>")

    text <- paste0(text, "<h4> 1. Length-based stock assessment with <b>TropFishR</b> </h4>")
    text <- paste0(text, "<p>")
    text <- paste0(text, "TropFishR is a collection of length-based methods that allow the estimation ",
                   "of life history parameters describing the growth or mortality rates of a stock as ",
                   "well as the stock status. The method requires monthly length-frequency data.")
    text <- paste0(text, " Find more information about this method in the TropFishR tab in the sidebar ",
                   "menu on the left.")
    text <- paste0(text, "</p>")
    text <- paste0(text, "<br>")

    text <- paste0(text, "<h4> 2. Length-based indicators (<b>LBIs</b>) </h4>")
    text <- paste0(text, "<p>")
    text <- paste0(text, "Length-based indicators allow to infer the stock status of a stock. ",
                   "The calculation of the indicators requires yearly length frequency data ",
                   "and information about life history parameters. These required parameters ",
                   "can be estimated with TropFishR or extracted from literature (e.g. ",
                   "<a href='http://www.fishbase.org/search.php' ",
                   "target='blank_'> FishBase</a> or <a href='https://www.sealifebase.ca' ",
                   "target='blank_'> SeaLifeBase</a> for invertebrates",
                   ").")
    text <- paste0(text, " Find more information about this method in the LBI tab in the sidebar ",
                   "menu on the left.")
    text <- paste0(text, "</p>")
    text <- paste0(text, "<br>")

    text <- paste0(text, "<h4> 3. Length-based spawning potential ratio (<b>LBSPR</b>) </h4>")
    text <- paste0(text, "<p>")
    text <- paste0(text, "The length-based spawning potential ratio (LBSPR) allows to infer the stock status of a stock. ",
                   "The calculation of the indicators requires yearly length frequency data ",
                   "and information about life history parameters. These required parameters ",
                   "can be estimated with TropFishR or extracted from literature (e.g. ",
                   "<a href='http://www.fishbase.org/search.php' ",
                   "target='blank_'> FishBase</a> or <a href='https://www.sealifebase.ca' ",
                   "target='blank_'> SeaLifeBase</a> for invertebrates",
                   ").")
    text <- paste0(text, " Find more information about this method in the LBSPR tab in the sidebar ",
                   "menu on the left.")
    text <- paste0(text, "</p>")
    text <- paste0(text, "<br>")
    text <- paste0(text, "<br>")
    text <- paste0(text, "<br>")

    text <- paste0(text, "<h4><b> Data for length-based stock assessment methods </b></h4>")
    text <- paste0(text, "<br>")

    text <- paste0(text, "<p> SMT accepts two different data formats for the length measurements:</p>")
    text <- paste0(text, "<ol>")
    text <- paste0(text, "<li> <b>Raw length measurements</b>: This format contains two columns, where one specifies the dates when a length measurement was performed and the other specifies the measured length. A third column specifying the frequency of the measured lengths at a given date is optional. Note, that when the column with dates is not provided the program assigns the current date to all length measurements. </li>")
    text <- paste0(text, "<li> <b>Length-frequency table</b>: This format has to include a column indicating the mid lengths of the length classess of measured individuals (first column of the data set), a row indicating the dates when the individuals were measured (first row of the the data set excluding the first column), and the number of individuals observed per length class (rows) and per sampling date (columns). </li>")
    text <- paste0(text, "</ol>")

    text <- paste0(text, "<p> Further information about the two data formats ",
                   "and how to convert one format to the other are described in a tutorial by Mildenberger (2020; ",
                   "<a href='https://cran.r-project.org/web/packages/TropFishR/vignettes/lfqData.html'  target='blank_'>click to open</a>). </p>")

    linkEA <- "<a href='https://data.d4science.org/shub/E_Snp1NzhlUHlFOWg0M3lUL3lZWU0yMVVTVFY5NDhaYkI1ODY4blFtSi9NYlFiMVAyamxMNGc2QXh0TlpRNmdNdQ==' target='_blank'>click to download</a>"
    linkSE <- "<a href='https://data.d4science.org/shub/E_ZEhvM2ZpSVFZd0ZOSXl5MjlGZC90eC9vOUI1NG1hQ0NEMERtRGVHR2hqbThBZGxBYS9QWG5kc1BaWTluVHNvQw==' target='_blank'>click to download</a>"
    linkOSG <- "<a href='https://data.d4science.org/shub/E_SHJJRUJ5S1lxOGhLOW1zVWpKbUJFQzE1c2wzbnltRlVBUmYrUEphbDRkN1BxQzlYOUtwQWRzZ0JMdnJrSmRhZA==' target='_blank'>click to download</a>"

    text <- paste0(text, "<br>")
    text <- paste0(text, "<b> Example data sets </b>")
    text <- paste0(text, "<p> Three example data sets are available that demonstrate the two different data formats and represent three species with different life-history parameters:</p>")
    text <- paste0(text, "<ul>")
    text <- paste0(text, "<li> A short-lived species with raw length measurements: European anchovy (", linkEA, ")</li>")  ## &nbsp;
    text <- paste0(text, "<li> A medium-lived species with a length-frequency table: Spangled Emperor (", linkSE, ")</li>")
    text <- paste0(text, "<li> A long-lived species with a length-frequency table: Orange-spotted grouper(", linkOSG, ")</li>")
    text <- paste0(text, "</ul>")
    text <- paste0(text, "<p>Note, that the date can be provided in various formats (compare the date column or header row in the example data sets). The example data was generated using an age-and length-structured population model with following life-history parameters (losely informed by the parameters provided at FishBase):</p>")

    text <- paste0(text, "<style> table, th, td { border: 1px solid black; border-collapse: collapse; }
th, td { padding-top: 3px; padding-bottom: 3px; padding-left: 8px; padding-right: 8px; } </style>")
    text <- paste0(text, "<table>")
    text <- paste0(text, "<tr>")
    text <- paste0(text, "<th> Species </th>")
    text <- paste0(text, "<th> Latin </th>")
    text <- paste0(text, "<th> L<sub>&#8734;</sub> </th>")
    text <- paste0(text, "<th> K </th>")
    text <- paste0(text, "<th> t<sub>0</sub> </th>")
    text <- paste0(text, "<th> Max. age </th>")
    text <- paste0(text, "<th> M </th>")
    text <- paste0(text, "<th> L<sub>m50</sub> </th>")
    text <- paste0(text, "<th> L<sub>m95</sub> </th>")
    text <- paste0(text, "<th> L<sub>s50</sub> </th>")
    text <- paste0(text, "<th> L<sub>s95</sub> </th>")
    text <- paste0(text, "<th> a </th>")
    text <- paste0(text, "<th> b </th>")
    text <- paste0(text, "<th> Spawning </th>")
    text <- paste0(text, "</tr>")
    text <- paste0(text, "<tr>")
    text <- paste0(text, "<td> European anchovy </td>")
    text <- paste0(text, "<td> Engraulis encrasicolus </td>")
    text <- paste0(text, "<td> 18.28 </td>")
    text <- paste0(text, "<td> 0.6 </td>")
    text <- paste0(text, "<td> -1 </td>")
    text <- paste0(text, "<td> 6 </td>")
    text <- paste0(text, "<td> 1.08 </td>")
    text <- paste0(text, "<td> 10.33 </td>")
    text <- paste0(text, "<td> 12.396 </td>")
    text <- paste0(text, "<td> 8.264 </td>")
    text <- paste0(text, "<td> 9.9168 </td>")
    text <- paste0(text, "<td> 0.0055 </td>")
    text <- paste0(text, "<td> 3.06 </td>")
    text <- paste0(text, "<td> April-August </td>")
    text <- paste0(text, "</tr>")
    text <- paste0(text, "<tr>")
    text <- paste0(text, "<td> Spangled Emperor </td>")
    text <- paste0(text, "<td> Lethrinus nebulosus </td>")
    text <- paste0(text, "<td> 64.46 </td>")
    text <- paste0(text, "<td> 0.26 </td>")
    text <- paste0(text, "<td> -1 </td>")
    text <- paste0(text, "<td> 12 </td>")
    text <- paste0(text, "<td> 0.6 </td>")
    text <- paste0(text, "<td> 28.10 </td>")
    text <- paste0(text, "<td> 33.72 </td>")
    text <- paste0(text, "<td> 25.29 </td>")
    text <- paste0(text, "<td> 32.034 </td>")
    text <- paste0(text, "<td> 0.0339 </td>")
    text <- paste0(text, "<td> 2.82 </td>")
    text <- paste0(text, "<td> February-April & August-October </td>")
    text <- paste0(text, "</tr>")
    text <- paste0(text, "<tr>")
    text <- paste0(text, "<td> Orange-spotted grouper </td>")
    text <- paste0(text, "<td> Epinephelus coioides </td>")
    text <- paste0(text, "<td> 95.45 </td>")
    text <- paste0(text, "<td> 0.15 </td>")
    text <- paste0(text, "<td> -1 </td>")
    text <- paste0(text, "<td> 18 </td>")
    text <- paste0(text, "<td> 0.27 </td>")
    text <- paste0(text, "<td> 54 </td>")
    text <- paste0(text, "<td> 64.8 </td>")
    text <- paste0(text, "<td> 48.6 </td>")
    text <- paste0(text, "<td> 61.56 </td>")
    text <- paste0(text, "<td> 0.0138 </td>")
    text <- paste0(text, "<td> 3.04 </td>")
    text <- paste0(text, "<td> March-May </td>")
    text <- paste0(text, "</tr>")
    text <- paste0(text, "</table>")

    text <- paste0(text, "<br>")
    text <- paste0(text, "<b> Further data format considerations </b>")
    text <- paste0(text, "<ul>")
    text <- paste0(text, "<li>", "Save your dataset in 'csv' format.", "</li>")
    text <- paste0(text, "<li>", "The separator for the .csv file should be a comma ‘,’ semicolon ';' or tab. The default might differ depending on the language settings of your spreadsheet manipulation program (e.g. Excel).", "</li>")
    text <- paste0(text, "<li>", "Use a '.' to separate decimals in the data. The default might differ depending on the language settings of your spreadsheet manipulation program (e.g. Excel).", "</li>")
    text <- paste0(text, "<li>", "The first column of the data set should include the mid lengths of the length classes.", "</li>")
    text <- paste0(text, "<li>The data format of your data file should be automatically detected, or select the date format used in your data file under <b>'Choose CSV date format'</b>, e.g. DD/MM/YYYY format to select 'Day Month Year'.</li>")
    text <- paste0(text, "<li>The date must include a specification of the day. If the data are aggregated or no information about the day is available, you could consider to set the day to the midpoint of the month, e.g. 15/01/2021.</li>")
    text <- paste0(text, "<li>Your data set should at least be representative of a whole year. This is particularly important if seasonally varying growth is estimated.</li>")


    ## text <- paste0(text, "<li>Ensure that your dates are given in chronological order.</li>")
    ## text <- paste0(text, "<li>", "Ensure that the 'midLength' column name is identical to the sample dataset.", "</li>")
    text <- paste0(text, "</ul>")
    text <- paste0(text, "<br>")
    text <- paste0(text, "<br>")

    text
})



## TropFishR
## ---------------------------------
output$elefanIntroOut <- renderText({
    session$userData$page('elefan-intro')
    text <- "<h3><b>Length-based stock assessment with TropFishR</b></h3>"
    text <- paste0(text, "<br>")
    text <- paste0(text, "<p>")
    text <- paste0(text, "The length-based stock assessment with TropFishR is based on the routine outlined in the Technical report ",
                   "<a href='http://www.fao.org/documents/card/en/c/9bb12a06-2f05-5dcb-a6ca-2d6dd3080f65/' target='_blank'> Introduction to Tropical Fish Stock Assessment (FAO, 1998)</a>",
                   ", and compiled into the R package ",
                   "<a href='https://cran.r-project.org/web/packages/TropFishR' target='_blank'> TropFishR </a>",
                   " by ", "<a href='https://doi.org/10.1111/2041-210X.12791' target='_blank'> Mildenberger et al. (2017).</a>",
                   " The assessment routine consists of 4 consecutive steps and methods:")
    text <- paste0(text, "</p>")
    text <- paste0(text, "<br>")
    text <- paste0(text, "<h4> Method description </h4>")
    text <- paste0(text, "<strong>1. ELEFAN: Electronic LEngth Frequency ANalysis for the estimation of the growth parameters of the von Bertlanffy growth (VBG) function.</strong>")
    text <- paste0(text, "<p style='margin-left: 20px'>")
    text <- paste0(text, "The study of fish growth involves a determination of body size as a function of age. Most stock assessment methods work essentially with age composition data. In temperate waters it is easier to acquire data to estimate age by counting year rings on hard parts of organisms such as scales and otoliths (ear bones). These rings are formed due to strong seasonal fluctuations in environmental conditions. In tropical areas such drastic changes do not occur and it is therefore very difficult, if not impossible to use these kind of seasonal rings for age determination. ELEFAN is one of a number of numerical methods that have been developed, which allow the conversion of length-frequency data into age composition. Although these methods do not require the reading of rings on hard parts, the final interpretation of the results becomes much more reliable if at least some direct age readings are available.")
    text <- paste0(text, "<br>")
    text <- paste0(text, "ELEFAN fits a seasonal version of the von Bertalanffy Growth Function (VBGF; see Supporting Tools in the sidebar menu) by:")
    text <- paste0(text, "<ol>")
    text <- paste0(text, "<li> restructuring the length-frequency data using a procedure that scores the length bins based on their deviation from a moving average (MA) across neighboring bins.</li>")
    text <- paste0(text, "<li> calculating a score value as the cumulative score for a given set of VBGF parameters based on the sum of the individual bin scores that are intersected by resulting growth curves.</li>")
    text <- paste0(text, "<li> optimising over VBGF parameters maximising the score value by a genetic algorithm (GA). Imitating the natural process of survival of the fittest, the GA simulates a population of random combinations of growth parameters in which only the individuals (or combinations) with the highest fitness value (score value; Rn) can pass on their parameters to the next generations. The combination of growth parameters with the highest score value after a set number of generations is defined as the best estimate. For more information about the  genetic algorithm please refer to <a href='https://pdfs.semanticscholar.org/8b54/b4c7f3c63efcfadac455a32f2c6d775c7184.pdf?_ga=2.102660097.1034706415.1573191124-1938330296.1570694384' target='_blank'>Scrucca (2013)</a>. </li>")
    text <- paste0(text, "</ol>")
    text <- paste0(text, "</p>")

    text <- paste0(text, "<strong> <i>2. Empirical formula</i> for the estimation of the natural mortality rate.</strong>")
    text <- paste0(text, "<p style='margin-left: 20px'>")
    text <- paste0(text, "The natural mortality rate (M) is estimated by an empirical formula based on estimated growth parameters. The options are: <br> - Then's growth formula (Then et al. 2015), <br> - Pauly's growth and temperature formula (Pauly 1980), and <br> - Then's maximum age formula (Then et al. 2015); <br> While the first option does not require any additional information, the second requires the average annual sea surface temperature (SST) in degrees Celsius and allows for a correction for schooling fish (multiplication with 0.8). The third option requires an estimate of the maximum age of the fish.")
    text <- paste0(text, "</p>")
    text <- paste0(text, "<strong> <i>3. Catch curve</i> for the estimation of the total mortality rate.</strong>")
    text <- paste0(text, "<p style='margin-left: 20px'>")
    text <- paste0(text, "The length-converted catch curve is used to estimate the total mortality rate (Z) and the fishing mortality rate (F) based on M: F = Z - M.")
    text <- paste0(text, "</p>")
    text <- paste0(text, "<strong> <i>4. Yield per recruit analysis (YPR)</i> for the estimation of the stock status.</strong>")
    text <- paste0(text, "<p style='margin-left: 20px'>")
    text <- paste0(text, "The Thompson and Bell yield per recruit model is used to estimate yield and biomass per recruit as well as the spawning potential ratio (SPR) over a range of fishing mortality rates and selectivity definitions. The stock status is based on current F relative to biological reference points, such as ",withMathJax("\\(F_{max}\\)"),", ",withMathJax("\\(F_{0.1}\\)"),", ",withMathJax("\\(F_{35\\%SPR}\\)"),". The estimation of SPR requires information about the maturity parameters.")
    text <- paste0(text, "</p>")


    text <- paste0(text, "<p style='margin-left: 20px'>")
    text <- paste0(text,
                   "This workflow requires at least one year of length composition data, and information about the length-weight ",
                   " relationship and ideally the maturity of the species.")
    text <- paste0(text, "</p>")

    text <- paste0(text, "<p>")
    text <- paste0(text, "<strong>Further information</strong><br>")
    text <- paste0(text, "Please visit the <a href='https://elearning.fao.org/course/view.php?id=502' target='_blank'>SDG Indicator 14.4.1 - Fish stocks sustainability eLearning course</a>, <b>(Lesson 4, Slides 27-34)</b> for further information:")
    text <- paste0(text, "<ul>")
    text <- paste0(text, "<li> Slides 27-29 give an overview of the method;</li>")
    text <- paste0(text, "<li> Slides 30-32 explain how to run the method in the Stock Monitoring Tool;</li>")
    text <- paste0(text, "<li> Slide 31 *popup* describes the required dataset format;</li>")
    text <- paste0(text, "<li> Slide 32 *popup* describes the default and optional parameters;</li>")
    text <- paste0(text, "<li> Slide 33 and its popup and Slide 35 give examples of how to interpret and use the outputs;</li>")
    text <- paste0(text, "<li>Slide 34 describes the method’s caveats.</li>")
    text <- paste0(text, "</ul>")
    text <- paste0(text, "</p>")

    text <- paste0(text, "<br>")
    text <- paste0(text, tropfishrAssumptionsHTML())
    text <- paste0(text, "<br>")

    text <- paste0(text,"<h4> Workflow considerations</h4>")
    text <- paste0(text,"<strong> To run this length-based workflow in the Stock Monitoring Tool :</strong>")
    text <- paste0(text, "<ol>")
    text <- paste0(text, "<li> Navigate to the 'tool' tab under TropFishR in the sidebar menu.</li>")
    text <- paste0(text, "<li> Upload a size frequency data set (for more information check the 'Data Considerations' tab in the tool or the 'Length-based methods' tab in the sidebar menu).</li>")
    text <- paste0(text, "<li> Adjust the Assessment Settings")
    text <- paste0(text, "<ol type='a'>")
    text <- paste0(text, "<li> Data Settings - select the years of data to include in the analysis, adjust the aggregation, bin size and moving average (MA) of the dataset to optimise cohort recognition by the Elefan algorithm </li>")
    text <- paste0(text, "<li> Elefan - set the search space of the Elefan algorithm to estimate growth parameters </li>")
    text <- paste0(text, "<li> Stock status - set the parameters required to estimate mortality and perform YPR (and SPR; optional) </li>")
    text <- paste0(text, "</ol>")
    text <- paste0(text, "</li>")
    text <- paste0(text, "<li> Check and Run the Assessment:")
    text <- paste0(text, "<ol type='a'>")
    text <- paste0(text, "<li> Run a check on the parameterisations prior to running the full assessment ('Run Check' button)</li>")
    text <- paste0(text, "<li> Run the assessment ('Run Assessment' button). Watch for the progress bar in the center of the screen</li>")
    text <- paste0(text, "</ol>")
    text <- paste0(text, "</li>")
    text <- paste0(text, "<li> Download the report as a pdf ('Download Report' button). The report will also be automatically uploaded to your private workspace. </li>")
    text <- paste0(text, "<li> Download the results as a zip archive ('Download Results (zip)' button).</li>")
    text <- paste0(text, "<li> The 'Reset' button removes the uploaded data and resets the settings to default values.</li>")
    text <- paste0(text, "</ol>")
    text <- paste0(text, "Further information can be found in the popup information buttons at each field, and in the Workflow, Data, Methods, and Results Considerations tabs. Note that error messages may display in the center of the page and in place of any figures where an error has occurred.")
    text <- paste0(text, "</p>")
    text <- paste0(text, "<br>")

    text <- paste0(text, "<h4>Run time </h4>")
    text <- paste0(text, "<p>")
    text <- paste0(text, "The run time varies with the size and resolution (bin size) of the length frequency dataset, ",
                   "number of time steps, time aggregation, and parameter search space. ",
                   "This is noted in the information button of these fields.",
                   "With the exsample datasets, the run time is between <b> 2-3 mins</b>.")
    text <- paste0(text, "</p>")
    text <- paste0(text, "<br>")

    text <- paste0(text, "<h4>References</h4>")
    text <- paste0(text, "<ul>")
    text <- paste0(text, "<li>Brey, T., Soriano, M., and Pauly, D. 1988. Electronic length frequency analysis: a revised and expanded user's guide to ELEFAN 0, 1 and 2. <a href='https://oceanrep.geomar.de/25801/1/IFM-BER_177.pdf' target='_blank'>https://oceanrep.geomar.de/25801/1/IFM-BER_177.pdf</a></li>")

    text <- paste0(text, "<li>Mildenberger, T. K., Taylor, M. H., Wolff, M. (2017). TropFishR: an R package for fisheries analysis with length-frequency data. Methods in Ecology and Evolution, 8(11), 1520-1527.<a href='https://besjournals.onlinelibrary.wiley.com/doi/full/10.1111/2041-210X.12791' target='_blank'>https://besjournals.onlinelibrary.wiley.com/doi/full/10.1111/2041-210X.12791</a></li>")

    text <- paste0(text, "<li>Pauly D.(1980) On the interrelationships between natural mortality, growth parameters, and mean environmental temperature in 175 fish stocks, Journal du Conseil International pour l’Exploration de la Mer, 1980, vol. 39 (pg. 175-192), <a href='https://doi.org/10.1093/icesjms/39.2.175' target='_blank'>https://doi.org/10.1093/icesjms/39.2.175</a></li>")

    text <- paste0(text, "<li>Scrucca L. (2013). GA: A Package for Genetic Algorithms in R. Journal of Statistical Software, 53(4), 1-37. <a href='http://www.jstatsoft.org/v53/i04/' target='_blank'>http://www.jstatsoft.org/v53/i04/</a></li>")

    text <- paste0(text, "<li>Taylor, M. H., Mildenberger, T. K. (2017). Extending electronic length frequency analysis in R. Fisheries Management and Ecology, 24(4), 330-338.<a href='https://doi.org/10.1111/fme.12232' target='_blank'>https://doi.org/10.1111/fme.12232</a></li>")

    text <- paste0(text, "<li>Mildenberger, T. K., Taylor, M. H., & Wolff, M. (2017). TropFishR: an R package for fisheries analysis with length-frequency data. Methods in Ecology and Evolution, 8(11), 1520-1527.<a href='https://besjournals.onlinelibrary.wiley.com/doi/full/10.1111/2041-210X.12791' target='_blank'>https://besjournals.onlinelibrary.wiley.com/doi/full/10.1111/2041-210X.12791</a></li>")

    text <- paste0(text, "<li>Then, A.Y., Hoenig, J.M., Hall, N.G., Hewitt, D.A. (2015) Evaluating the predictive performance of empirical estimators of natural mortality rate using information on over 200 fish species, ICES Journal of Marine Science, Volume 72, Issue 1, Pages 82–92, <a href='https://doi.org/10.1093/icesjms/fsu136' target='_blank'>https://doi.org/10.1093/icesjms/fsu136</a></li>")

    text <- paste0(text, "<li>Wang, K., Zhang, C., Xu, B., Xue, Y., Ren, Y. (2020). Selecting optimal bin size to account for growth variability in Electronic LEngth Frequency ANalysis (ELEFAN). Fisheries Research, 225, 105474. <a href='https://doi.org/10.1016/j.fishres.2019.105474' target='_blank'>https://doi.org/10.1016/j.fishres.2019.105474</a></li>")

    text <- paste0(text, "<li>Xiang, Y., Gubian, S., Suomela, B., Hoeng, J. (2013). Generalized Simulated Annealing for Efficient Global Optimization: the GenSA Package for R. The R Journal, Volume 5/1, June 2013. <a href='https://journal.r-project.org/archive/2013/RJ-2013-002/index.html' target='_blank'>https://journal.r-project.org/archive/2013/RJ-2013-002/index.html</a></li>")

    text <- paste0(text, "</ul>")
    text <- paste0(text, "<br>")
    text <- paste0(text, "<br>")

    text <- paste0(text, "<p>")
    text
})


## LBIs
## ---------------------------------
output$lbiIntroOut <- renderText({
    session$userData$page('lbi-intro')
    text <- "<h3><b>Length-based indicators (LBIs)</b></h3>"
    text <- paste0(text, "<br>")
    text <- paste0(text, "<p>Length-based indicators allow to estimate the stock status from annual length frequency samples from catches and life history parameters.</p>")

    text <- paste0(text, "<br>")
    text <- paste0(text, "<h4> Method description </h4>")
    text <- paste0(text, "<p>")
    text <- paste0(text,
                   "LBIs rely on the relationships between life history theory and ",
                   "length frequency data from catches and are based on ",
                   "three simple ideas: (1) the length frequency data reflects the conservation of large, mature ",
                   "individuals with arguably a high fecundity (also referred to as 'mega spawners'); (2) ",
                   "the data demonstrates the conservation of small immature individuals; (3) the length ",
                   "frequency data consists mainly of fish of the size at which the highest yield from a cohort ",
                   "or the maximum sustainable yield (MSY) is to be expected. In theory, these metrics ",
                   "are, thus, indicating how a stock is performing in terms of yield optimisation and ",
                   "conservation goals (i.e. avoiding growth and recruitment overfishing).",
                   "LBIs require at least one year of length composition data, information about the asymptotic length of ",
                   "the von Bertalanffy growth equation (",withMathJax("\\(L_\\infty\\)"),"), the ratio of natural mortality ",
                   "and the von Bertalanffy growth coefficient (",withMathJax("\\(K\\)"),"), and information about the maturity ",
                   " and ideally length-weight relationship of the species."
                   )
    text <- paste0(text, "</p>")

    text <- paste0(text, "<br>")
    text <- paste0(text, lbiAssumptionsHTML())
    text <- paste0(text, "<br>")

    text <- paste0(text,"<h4> Workflow considerations</h4>")
    text <- paste0(text,"<strong> To run this length-based workflow in the Stock Monitoring Tool :</strong>")
    text <- paste0(text, "<ol>")
    text <- paste0(text, "<li> Navigate to the 'tool' tab under LBI in the sidebar menu.</li>")
    text <- paste0(text, "<li> Upload a size frequency data set (for more information check the 'Data Considerations' tab in the tool or the 'Length-based methods' tab in the sidebar menu).</li>")
    text <- paste0(text, "<li> Specify life history parameters such as the ",withMathJax("\\(M/K\\)"),", ",withMathJax("\\(L_\\infty\\)"),", as well as ",withMathJax("\\(L_{m50}\\)")," and if available the parameters of the length-weight relationship. </li>")
    text <- paste0(text, "<li> Run the assessment ('Run Assessment' button). </li>")
    text <- paste0(text, "<li> Download an automated assessment report as a pdf document ('Download Report' button). The report will also be automatically uploaded to your private workspace. </li>")
    text <- paste0(text, "<li> Download the results as a zip archive ('Download Results (zip)' button).</li>")
    text <- paste0(text, "<li> The 'Reset' button removes the uploaded data and resets the settings to default values.</li>")
    text <- paste0(text, "</ol>")
    text <- paste0(text, "Further information can be found in the popup information buttons at each field, and in the Workflow, Data, Methods, and Results Considerations tabs. Note that error messages may display in the center of the page and in place of any figures where an error has occurred.")
    text <- paste0(text, "</p>")
    text <- paste0(text, "<br>")

    text <- paste0(text, "<h4>Run time </h4>")
    text <- paste0(text, "<p>")
    text <- paste0(text, "The run time varies with the size and resolution (bin size) of the length frequency dataset. ",
                   "With the exsample datasets, the run time is <b> <1 min</b>.")
    text <- paste0(text, "</p>")
    text <- paste0(text, "<br>")

    text <- paste0(text, "<h4>References</h4>")
    text <- paste0(text, "<ul>")
    text <- paste0(text, "<li>Cope, J. M., & Punt, A. E. (2009). Length-based reference points for data-limited situations: applications and restrictions. Marine and Coastal Fisheries: Dynamics, Management, and Ecosystem Science, 1(1), 169-186.<a href='https://doi.org/10.1577/C08-025.1' target='_blank'>https://doi.org/10.1577/C08-025.1</a></li>")

    text <- paste0(text, "<li>Froese, R. (2004). Keep it simple: three indicators to deal with overfishing. Fish and fisheries, 5(1), 86-91. <a href='https://doi.org/10.1111/j.1467-2979.2004.00144.x' target='_blank'>https://doi.org/10.1111/j.1467-2979.2004.00144.x</a></li>")

    text <- paste0(text, "<li>Froese, R., Winker, H., Coro, G., Demirel, N., Tsikliras, A.C., Dimarchopoulou, D., Scarcella, G., Probst, W.N., Dureuil, M. and Pauly, D., (2018). A new approach for estimating stock status from length frequency data. ICES Journal of Marine Science, 75(6), pp.2004-2015. <a href='https://doi.org/10.1093/icesjms/fsy078' target='_blank'>https://doi.org/10.1093/icesjms/fsy078</a></li>")

    text <- paste0(text, "<li>Hordyk, A. R., Prince, J. D., Carruthers, T. R., & Walters, C. J. (2019). Comment on “A new approach for estimating stock status from length frequency data” by Froese et al.(2018). ICES Journal of Marine Science, 76(2), 457-460. <a href='https://doi.org/10.1093/icesjms/fsy168' target='_blank'>https://doi.org/10.1093/icesjms/fsy168</a></li>")

text <- paste0(text, "<li>ICES (2018). ICES Technical guidance for providing reference points for stocks in categories
    3 and 4. ICES Technical Guidelines. <a href='https://doi.org/10.17895/ices.pub.4128' target='_blank'>https://doi.org/10.17895/ices.pub.4128</a></li>")

    text <- paste0(text, "<li>Kell, L. T., Minto, C., & Gerritsen, H. D. (2022). Evaluation of the skill of length-based indicators to identify stock status and trends. ICES Journal of Marine Science. <a href='https://doi.org/10.1093/icesjms/fsac043' target='_blank'>https://doi.org/10.1093/icesjms/fsac043</a></li>")
    text <- paste0(text, "</ul>")
    text <- paste0(text, "<br>")
    text <- paste0(text, "<br>")

    text
})




## LBSPR
## ---------------------------------
output$lbsprIntroOut <- renderText({
    session$userData$page('lbspr-intro')
    text <- "<h3><b>Length-based spawning potential ratio (LBSPR)</b></h3>"
    text <- paste0(text, "<br>")
    text <- paste0(text, "<p>This method allows to estimate the stock status from annual length frequency samples from catches and life history parameters.</p>")

    text <- paste0(text, "<br>")
    text <- paste0(text, "<h4> Method description </h4>")
    text <- paste0(text, "<p>")
    text <- paste0(text,
                   "LBSPR estimates the stock status based on the spawning potential ratio (SPR), ",
                   "that is the proportion of the unfished reproductive potential left at any given level of fishing pressure.",
                   "A SPR value of 100% indicates full spawning potential, while a value of 0 indicates a stock without any ",
                   " mature female individuals left. ",
                   "LBSPR requires at least one year of length composition data, information about the asymptotic length of ",
                   "the von Bertalanffy growth equation (",withMathJax("\\(L_\\infty\\)"),"), the ratio of natural mortality ",
                   "and the von Bertalanffy growth coefficient (",withMathJax("\\(K\\)"),"), and information about the maturity ",
                   " and length-weight relationship of the species."
                   )
    text <- paste0(text, "</p>")

    text <- paste0(text, "<br>")
    text <- paste0(text, lbsprAssumptionsHTML())
    text <- paste0(text, "<br>")


    text <- paste0(text,"<h4> Workflow considerations</h4>")
    text <- paste0(text,"<strong> To run this length-based workflow in the Stock Monitoring Tool :</strong>")
    text <- paste0(text, "<ol>")
    text <- paste0(text, "<li> Navigate to the 'tool' tab under LBSPR in the sidebar menu.</li>")
    text <- paste0(text, "<li> Upload a size frequency data set (for more information check the 'Data Considerations' tab in the tool or the 'Length-based methods' tab in the sidebar menu).</li>")
  text <- paste0(text, "<li> Specify life history parameters such as the ",withMathJax("\\(M/K\\)"),", ",withMathJax("\\(L_\\infty\\)"),", as well as maturity parameters and parameters of the length-weight relationship. </li>")
  text <- paste0(text, "<li> Run the assessment ('Run Assessment' button). </li>")
  text <- paste0(text, "<li> Download an automated assessment report as a pdf document ('Download Report' button). The report will also be automatically uploaded to your private workspace. </li>")
text <- paste0(text, "<li> Download the results as a zip archive ('Download Results (zip)' button).</li>")
  text <- paste0(text, "<li> The 'Reset' button removes the uploaded data and resets the settings to default values.</li>")
    text <- paste0(text, "</ol>")
    text <- paste0(text, "Further information can be found in the popup information buttons at each field, and in the Workflow, Data, Methods, and Results Considerations tabs. Note that error messages may display in the center of the page and in place of any figures where an error has occurred.")
    text <- paste0(text, "</p>")
    text <- paste0(text, "<br>")

    text <- paste0(text, "<h4>Run time </h4>")
    text <- paste0(text, "<p>")
    text <- paste0(text, "The run time varies with the size and resolution (bin size) of the length frequency dataset. ",
                   "With the exsample datasets, the run time is <b> <1 min</b>.")
    text <- paste0(text, "</p>")
    text <- paste0(text, "<br>")

    text <- paste0(text, "<p>")
    text <- paste0(text, "<h4>References</h4>")
    text <- paste0(text, "<ul>")

    text <- paste0(text, "<li>Brooks, E.N., Powers, J.E. and Cortés, E., 2010. Analytical reference points for age-structured models: application to data-poor fisheries. ICES Journal of Marine Science, 67(1), pp.165-175. <a href='https://doi.org/10.1093/icesjms/fsp225' target='_blank'>https://doi.org/10.1093/icesjms/fsp225</a></li>")

    text <- paste0(text, "<li>Clark, W.G., 2002. F 35% revisited ten years later. North American Journal of Fisheries Management, 22(1), pp.251-257. <a href='https://doi.org/10.1577/1548-8675(2002)022<0251:FRTYL>2.0.CO;2' target='_blank'>https://doi.org/10.1577/1548-8675(2002)022<0251:FRTYL>2.0.CO;2</a></li>")

    text <- paste0(text, "<li>Hordyk, A., Ono, K., Sainsbury, K., Loneragan, N. and Prince, J., 2015a. Some explorations of the life history ratios to describe length composition, spawning-per-recruit, and the spawning potential ratio. ICES Journal of Marine Science, 72(1), pp.204-216. <a href='https://doi.org/10.1093/icesjms/fst235' target='_blank'>https://doi.org/10.1093/icesjms/fst235</a></li>")

    text <- paste0(text, "<li>Hordyk, A., Ono, K., Valencia, S., Loneragan, N. and Prince, J., 2015b. A novel length-based empirical estimation method of spawning potential ratio (SPR), and tests of its performance, for small-scale, data-poor fisheries. ICES Journal of Marine Science, 72(1), pp.217-231. <a href='https://doi.org/10.1093/icesjms/fsu004' target='_blank'>https://doi.org/10.1093/icesjms/fsu004</a></li>")

    text <- paste0(text, "<li>Prince, J., Hordyk, A., Valencia, S.R., Loneragan, N. and Sainsbury, K., 2015. Revisiting the concept of Beverton–Holt life-history invariants with the aim of informing data-poor fisheries assessment. ICES Journal of Marine Science, 72(1), pp.194-203. <a href='https://doi.org/10.1093/icesjms/fsu011' target='_blank'>https://doi.org/10.1093/icesjms/fsu011</a></li>")

    text <- paste0(text, "</ul>")
    text <- paste0(text, "<br>")
    text <- paste0(text, "<br>")

    text
})


## SPiCT
## ---------------------------------
output$spictIntroOut <- renderText({
    session$userData$page('spict-intro')
    text <- "<h3><b>Data-limited stock assessment with SPiCT</b></h3>"
    text <- paste0(text, "<br>")
    text <- paste0(text, "<p>")
    text <- paste0(text, "The stochastic surplus production model in continuous time (SPiCT) is one of the official assessment models of the International Council for the Exploration of the Sea (ICES) and to estimate stock status and give catch quota advice for around 20 data-limited stocks in the Northeast Atlantic.")
    text <- paste0(text, "</p>")
    text <- paste0(text, "<br>")
    text <- paste0(text, "<h4> Method description </h4>")
    text <- paste0(text, "TODO")
    text <- paste0(text, "<br>")

    text <- paste0(text, "<p style='margin-left: 20px'>")
    text <- paste0(text,
                   "This workflow requires at least 10 years of catch and relative abundance information. Ideally, a fishery-independent survey is available as an index of relative abundance, but a commercial catch per unit of effort (CPUE) time series might also suffice.")
    text <- paste0(text, "</p>")

    text <- paste0(text, "<p>")
    text <- paste0(text, "<strong>Further information</strong><br>")
    text <- paste0(text, "Please visit the <a href='https://elearning.fao.org/course/view.php?id=502' target='_blank'>SDG Indicator 14.4.1 - Fish stocks sustainability eLearning course</a>, <b>(Lesson TODO, Slides TODO)</b> for further information:")
    text <- paste0(text, "</p>")

    text <- paste0(text, "<br>")
    text <- paste0(text, spictAssumptionsHTML())
    text <- paste0(text, "<br>")

    text <- paste0(text,"<h4> Workflow considerations</h4>")
    text <- paste0(text,"<strong> To run the SPiCT assessment workflow in the Stock Monitoring Tool :</strong>")
    text <- paste0(text, "<ol>")
    text <- paste0(text, "<li> Navigate to the 'tool' tab under SPiCT in the sidebar menu.</li>")
    text <- paste0(text, "<li> Upload a spict data set (for more information check the 'Data Considerations' tab in the tool).</li>")
    text <- paste0(text, "<li> Adjust the Assessment Settings")
    text <- paste0(text, "<ol type='a'>")
    text <- paste0(text, "<li> Data Settings - select the years of data to include in the analysis, adjust ... TODO </li>")
    text <- paste0(text, "<li> Priors - TODO </li>")
    text <- paste0(text, "<li> Other - TODO </li>")
    text <- paste0(text, "</ol>")
    text <- paste0(text, "</li>")
    text <- paste0(text, "<li> Run the Assessment:")
    text <- paste0(text, "<ol type='a'>")
    text <- paste0(text, "<li> Run the assessment ('Run Assessment' button). Watch for the progress bar in the center of the screen</li>")
    text <- paste0(text, "</ol>")
    text <- paste0(text, "</li>")
    text <- paste0(text, "<li> Download the report as a pdf ('Download Report' button). The report will also be automatically uploaded to your private workspace. </li>")
    text <- paste0(text, "<li> Download the results as a zip archive ('Download Results (zip)' button).</li>")
    text <- paste0(text, "<li> The 'Reset' button removes the uploaded data and resets the settings to default values.</li>")
    text <- paste0(text, "</ol>")
    text <- paste0(text, "Further information can be found in the popup information buttons at each field, and in the Workflow, Data, Methods, and Results Considerations tabs. Note that error messages may display in the center of the page and in place of any figures where an error has occurred.")
    text <- paste0(text, "</p>")
    text <- paste0(text, "<br>")

    text <- paste0(text, "<h4>Run time </h4>")
    text <- paste0(text, "<p>")
    text <- paste0(text, "The run time varies with the time series length, the number of abundance indices, and Euler time step used for estimation. ",
                   "This is noted in the information button of these fields.",
                   "With the exsample datasets, the run time is below <b> 1 min</b>.")
    text <- paste0(text, "</p>")
    text <- paste0(text, "<br>")

    text <- paste0(text, "<h4>References</h4>")
    text <- paste0(text, "<ul>")
    text <- paste0(text, "<li>Kokkalis, A., Berg, C.W., Kapur, M.S., Winker, H., Jacobsen, N.S., Taylor, M.H., Ichinokawa, M., Miyagawa, M., Medeiros-Leal, W., Nielsen, J.R. and Mildenberger, T.K., 2024. Good practices for surplus production models. Fisheries Research, 275, p.107010. <a href='https://doi.org/10.1016/j.fishres.2024.107010' target='_blank'>https://doi.org/10.1016/j.fishres.2024.107010</a></li>")

    text <- paste0(text, "<li>Mildenberger, T.K., Berg, C.W., Pedersen, M.W., Kokkalis, A. and Nielsen, J.R. 2020. Time-variant productivity in biomass dynamic models on seasonal and long-term scales. ICES Journal of Marine Science, 77(1), pp.174-187.<a href='https://doi.org/10.1093/icesjms/fsz154' target='_blank'>https://doi.org/10.1093/icesjms/fsz154</a></li>")

    text <- paste0(text, "<li>Mildenberger, T.K., Berg, C.W., Kempf, A., Rindorf, A., MacCall, A.D. and Taylor, M.H., 2025. Estimating Time-Varying Productivity and Reference Points: A Case of North Sea Demersal Fish Stocks. Fish and Fisheries. <a href='https://doi.org/10.1111/faf.12910' target='_blank'>https://doi.org/10.1111/faf.12910</a></li>")

    text <- paste0(text, "<li>Pedersen, M.W., and Berg, C.W. 2017. A stochastic surplus production model in continuous time. Fish and Fisheries, 18(2), 226-243. <a href='https://doi.org/10.1111/faf.12174' target='_blank'>https://doi.org/10.1111/faf.12174</a></li>")

    text <- paste0(text, "</ul>")
    text <- paste0(text, "<br>")
    text <- paste0(text, "<br>")

    text <- paste0(text, "<p>")
    text
})



## Fish methods
output$fishMethodsIntroOut <- renderText({
    session$userData$page('fishmethods-intro')
    text <- "<h3><b>FishMethods</b></h3>"
    text <- paste0(text, "<p>")
    text <- paste0(text, "<b>Fishmethods: </b>Fishery science methods and models from published literature and contributions from colleagues.")
    text <- paste0(text, "</p>")
    text <- paste0(text, "<br/>")
    text <- paste0(text, "<p>")
    text <- paste0(text, "In data-limited situations where long-term, comprehensive catch data do not exist, per-recruit models can be used to determine estimates of optimal fishing mortality. Yield-per-recruit (YPR) and spawning biomass-per-recruit (SBPR) models calculate the equilibrium yield per recruit and spawning stock biomass per recruit, respectively, for a given value of fishing mortality (F) and a given length or age at first capture. Since F and Tc or Lc are values that a fishery manager can control (in theory), the idea is that by focusing on YPR or SBPR, managers can maintain a stock's population by preserving its reproductive capability. These models help to determine the optimum yield to prevent overfishing by instituting management controls on effort and length or age at first capture.")
    text <- paste0(text, "<br/>")
    text <- paste0(text, "<h4>Methods used</h4>")
    text <- paste0(text, "</p>")
    text <- paste0(text, "<p>")
    text <- paste0(text, "<b>SBPR</b> Spawning stock biomass-per-recruit(SBPR) analysis is conducted following Gabriel et al. (1989). Reference points of fishing mortality (F) and SBPR for a percentage of maximum spawning potential are calculated.")
    text <- paste0(text, "</p>")
    text <- paste0(text, "<p>")
    text <- paste0(text, "<b>YPR</b> Yield-per-recruit (YPR) analysis is conducted following the modified Thompson-Bell algorithm. Reference points",  withMathJax("\\(F_{max}\\)"), "and",  withMathJax("\\(F_{0.1}\\)") ,"are calculated.")
    text <- paste0(text, "<p>")
    text <- paste0(text, "<b>",withMathJax("\\(F_{0.1}\\)"),"</b> Fishing mortality rate corresponding to 10% of the slope of the YPR curve as a function of F when F=0. This is the F at which the marginal increase in equilibrium yield has dropped to 1/10 of its value when the stock was first exploited.")
    text <- paste0(text, "<p>")
    text <- paste0(text, "<b>", withMathJax("\\(F_{max}\\)"), "</b>Fishing mortality rate that produces the maximum yield per recruit.")
    text <- paste0(text, "</p>")
    text <- paste0(text, "<p>")
    text <- paste0(text, "<b>SBPR Options:</b>")
    text <- paste0(text, "<ul>")
    text <- paste0(text, "<li><b>Input file: </b> Input data file</li>")
    text <- paste0(text, "<li><b>M: </b> Single natural mortality (M) rate if M is assumed constant over all ages</li>")
    text <- paste0(text, "<li><b>pF: </b> Proportion of fishing mortality (F) that occurs before spawning</li>")
    text <- paste0(text, "<li><b>pM: </b> Proportion of natural mortality (M) that occurs before spawning</li>")
    text <- paste0(text, "<li><b>MSP: </b> Percentage of maximum spawning potential (%MSP) for which fishing mortality (F) and SBPR should be calculated</li>")
    text <- paste0(text, "<li><b>maxF: </b> Maximum value of fishing mortality (F) range over which SBPR will be calculated</li>")
    text <- paste0(text, "<li><b>incrF: </b> Fishing mortality (F) increment for SBPR calculation</li>")
    text <- paste0(text, "</ul>")
    text <- paste0(text, "</p>")
    text <- paste0(text, "<p>")
    text <- paste0(text, "<b>YPR Options:</b>")
    text <- paste0(text, "<ul>")
    text <- paste0(text, "<li><b>Input file: </b> Input data file</li>")
    text <- paste0(text, "<li><b>M: </b> Single natural mortality (M) rate if M is assumed constant over all ages</li>")
    text <- paste0(text, "<li><b>maxF: </b> Maximum value of fishing mortality (F) range over which YPR will be calculated. YPR is calculated for F = 0 to maxF</li>")
    text <- paste0(text, "<li><b>plus: </b> logical value indicating whether the last age is a plus-group</li>")
    text <- paste0(text, "<li><b>oldest: </b> if plus is checked, a numeric value indicating the oldest age in the plus group</li>")
    text <- paste0(text, "<li><b>incrF: </b> Fishing mortality (F) increment for YPR calculation</li>")
    text <- paste0(text, "</ul>")
    text <- paste0(text, "</p>")

    text <- paste0(text, "<p>")
    text <- paste0(text, "<h4>Running time with sample dataset</h4>")
    text <- paste0(text, "<ul>")
    text <- paste0(text, "<li><b>SBPR:</b>&nbsp;10 sec</li>")
    text <- paste0(text, "<li><b>YPR:</b>&nbsp;< 10 sec</li>")
    text <- paste0(text, "</ul>")
    text <- paste0(text, "</p>")

    text <- paste0(text, "<p>")
    text <- paste0(text, "W. L. Gabriel, M. P. Sissenwine & W. J. Overholtz (1989) Analysis of Spawning Stock Biomass per Recruit: An Example for Georges Bank Haddock, North American Journal of Fisheries Management, 9:4, 383-391, DOI: <a href='https://doi.org/10.1577/1548-8675(1989)009%3C0383:AOSSBP%3E2.3.CO;2' target='_blank'>10.1577/1548-8675(1989)009<0383:AOSSBP>2.3.CO;2</a>")
    text <- paste0(text, "</p>")
    text
})

output$cmsySampleDataset <- renderText({
    session$userData$page('cmsy-sample')

    sample_dataset_url = "https://data.d4science.org/shub/E_N0JSRlVEN3gwdmpjRnp5Y1BIWm5sS1QxZnUzUTRNSlp5ek50R2xlY0ZUZXVDUlFHTFFES3liblJGRSt4YWExMw=="

    link <- paste0("<a href='",sample_dataset_url,"' target='_blank'>Click Here</a>")
    text <- paste0("<p><h4>", link,"&nbsp; to download a sample dataset that can be used with <b>CMSY</b> methods", "</h4></p>")
    text <- paste0(text, "<hr />")
    text <- paste0(text, "<p>")
    text <- paste0(text, "<h4>To run CMSY, the dataset must include:</h4>")
    text <- paste0(text, "Mandatory fields to run CMSY are:")
    text <- paste0(text, "<ul>")
    text <- paste0(text, "<li><b>Stock</b>: a unique fish stock name or identifier (e.g. “cod-2532”), repeated for each year.</li>")
    text <- paste0(text, "<li><b>yr</b>: the reporting year of the catch (e.g. 2004). One row for each year. Years have to be consecutive from the first to the last year without any missing years.</li>")
    text <- paste0(text, "<li><b>ct</b>: catch value, in tonnes (e.g. 12345). One row for each year. Gaps with no entries are not accepted and must be filled by interpolating missing or incorrect values, e.g., do not use zero as an entry if data are missing, instead use the mean of adjacent values to replace zero or to fill any gaps.</li>")
    text <- paste0(text, "<li><b>bt</b>: the value of the biomass (in tonnes, e.g. 34567), or the value of the CPUE or stock size index (e.g. 0.123), or NA if there is no information. Gaps filled with NA are acceptable for bt, i.e., abundance data can be fewer than catch data.</li>")
    text <- paste0(text, "</ul>")
    text <- paste0(text, "Other columns are identifiers that you may choose to include, but they are not necessary to run the model.<br><br>")

    text <- paste0(text, "Use the <a href='",sample_dataset_url,"' target='_blank'> sample dataset </a> as a template to prepare your data.<br><br>")
    text <- paste0(text, "<b>Specific considerations regarding your own dataset:</b>")
    text <- paste0(text, "<ul>")
    text <- paste0(text, "<li>Save your dataset in 'csv' format.</li>")
    text <- paste0(text, "<li>The separator for the .csv file should be a comma ‘,’. The default might differ depending on the language settings of your spreadsheet manipulation program (e.g. Excel).</li>")
    text <- paste0(text, "<li>Note that years with missing data should be filled with an 'NA' value.</li>")
    text <- paste0(text, "<li>Note that the column names of your dataset should exactly match those of the sample dataset.</li>")

    text <- paste0(text, "<li><b>Please ensure your time-series is at least 15 years in length from starting year to ending year.</b></li>")
    text <- paste0(text, "</ul>")
    text <- paste0(text, "</p>")
    text
})
output$elefanSampleDataset <- renderText({
    session$userData$page('elefan-sample')
    link <- "<a href='https://data.d4science.org/shub/E_Yzc0aHFhWE50WWdpaEhkMjl5TExHekdQU2NFR2FtNTM2NkRydTQ5clhMTzhVd3Y4bDJzcU16UXNSUWJzZ1NpTg==' target='_blank'>Click Here</a>"
    text <- "<b>Dataset must include:</b>"
    text <- paste0(text, "<ul>")
    text <- paste0(text,
                   "<li>A column indicating the length classes of measured individuals (first column of dataset).</li>")
    text <- paste0(text, "<li>A row indicating the dates when individuals were measured (first row of dataset excluding the first column).</li>")
    text <- paste0(text,
                   "<li>The number of individuals caught per length class (rows) and per sampling date (columns).</li>")
    text <- paste0(text, "</ul>")
    text <- paste0(text, "<br>")
    text <- paste0(text, "<p> An example dataset in this format can be downloaded <a href='https://data.d4science.org/shub/E_Yzc0aHFhWE50WWdpaEhkMjl5TExHekdQU2NFR2FtNTM2NkRydTQ5clhMTzhVd3Y4bDJzcU16UXNSUWJzZ1NpTg==' target='_blank'> here </a>. </p>")
    text <- paste0(text, "<br>")
    text <- paste0(text, "<b>Specific considerations regarding your own dataset:</b>")
    text <- paste0(text, "<ul>")
    text <- paste0(text, "<li>", "Save your dataset in 'csv' format.", "</li>")
    text <- paste0(text, "<li>", "The separator for the .csv file should be a comma ‘,’ semicolon ';' or tab. The default might differ depending on the language settings of your spreadsheet manipulation program (e.g. Excel).", "</li>")
    text <- paste0(text, "<li>", "Use a '.' to separate decimals in the data. The default might differ depending on the language settings of your spreadsheet manipulation program (e.g. Excel).", "</li>")
    text <- paste0(text, "<li>", "The first column of the data set should include the mid lengths of the length classes.", "</li>")
    text <- paste0(text, "<li>The data format of your data file should be automatically detected, or select the date format used in your data file under <b>'Choose CSV date format'</b>, e.g. DD/MM/YYYY format to select 'Day Month Year'.</li>")
    text <- paste0(text, "<li>The date must include a specification of the day. If the data are aggregated or no information about the day is available, you could consider to set the day to the midpoint of the month, e.g. 15/01/2021.</li>")
    text <- paste0(text, "<li>Your data set should at least be representative of a whole year. This is particularly important if seasonally varying growth is estimated.</li>")


    ## text <- paste0(text, "<li>Ensure that your dates are given in chronological order.</li>")
    ## text <- paste0(text, "<li>", "Ensure that the 'midLength' column name is identical to the sample dataset.", "</li>")
    text <- paste0(text, "</ul>")
    text <- paste0(text, "<br>")
    text <- paste0(text, "<p> Further information about different formats of length-frequency datasets ",
                   "and how to convert one format to the other are described in a ",
                   "<a href='https://cran.r-project.org/web/packages/TropFishR/vignettes/lfqData.html'  target='blank_'>tutorial by Mildenberger (2020)</a>. </p>")
    return(text)
})
output$lbiSampleDataset <- renderText({
    session$userData$page('lbi-sample')
    link <- "<a href='https://data.d4science.org/shub/E_Yzc0aHFhWE50WWdpaEhkMjl5TExHekdQU2NFR2FtNTM2NkRydTQ5clhMTzhVd3Y4bDJzcU16UXNSUWJzZ1NpTg==' target='_blank'>Click Here</a>"
    text <- "<b>Dataset must include:</b>"
    text <- paste0(text, "<ul>")
    text <- paste0(text,
                   "<li>A column indicating the length classes of measured individuals (first column of dataset).</li>")
    text <- paste0(text, "<li>A row indicating the dates when individuals were measured (first row of dataset excluding the first column).</li>")
    text <- paste0(text,
                   "<li>The number of individuals caught per length class (rows) and per sampling date (columns).</li>")
    text <- paste0(text, "</ul>")
    text <- paste0(text, "<br>")
    text <- paste0(text, "<p> An example dataset in this format can be downloaded <a href='https://data.d4science.org/shub/E_Yzc0aHFhWE50WWdpaEhkMjl5TExHekdQU2NFR2FtNTM2NkRydTQ5clhMTzhVd3Y4bDJzcU16UXNSUWJzZ1NpTg==' target='_blank'> here </a>. </p>")
    text <- paste0(text, "<br>")
    text <- paste0(text, "<b>Specific considerations regarding your own dataset:</b>")
    text <- paste0(text, "<ul>")
    text <- paste0(text, "<li>", "Save your dataset in 'csv' format.", "</li>")
    text <- paste0(text, "<li>", "The separator for the .csv file should be a comma ‘,’ semicolon ';' or tab. The default might differ depending on the language settings of your spreadsheet manipulation program (e.g. Excel).", "</li>")
    text <- paste0(text, "<li>", "Use a '.' to separate decimals in the data. The default might differ depending on the language settings of your spreadsheet manipulation program (e.g. Excel).", "</li>")
    text <- paste0(text, "<li>", "The first column of the data set should include the mid lengths of the length classes.", "</li>")
    text <- paste0(text, "<li>The data format of your data file should be automatically detected, or select the date format used in your data file under <b>'Choose CSV date format'</b>, e.g. DD/MM/YYYY format to select 'Day Month Year'.</li>")
    text <- paste0(text, "<li>The date must include a specification of the day. If the data are aggregated or no information about the day is available, you could consider to set the day to the midpoint of the month, e.g. 15/01/2021.</li>")
    text <- paste0(text, "<li>Your data set should at least be representative of a whole year. This is particularly important if seasonally varying growth is estimated.</li>")


    ## text <- paste0(text, "<li>Ensure that your dates are given in chronological order.</li>")
    ## text <- paste0(text, "<li>", "Ensure that the 'midLength' column name is identical to the sample dataset.", "</li>")
    text <- paste0(text, "</ul>")
    text <- paste0(text, "<br>")
    text <- paste0(text, "<p> Further information about different formats of length-frequency datasets ",
                   "and how to convert one format to the other are described in a ",
                   "<a href='https://cran.r-project.org/web/packages/TropFishR/vignettes/lfqData.html'  target='blank_'>tutorial by Mildenberger (2020)</a>. </p>")
    return(text)
})
output$lbsprSampleDataset <- renderText({
    session$userData$page('lbspr-sample')
    link <- "<a href='https://data.d4science.org/shub/E_Yzc0aHFhWE50WWdpaEhkMjl5TExHekdQU2NFR2FtNTM2NkRydTQ5clhMTzhVd3Y4bDJzcU16UXNSUWJzZ1NpTg==' target='_blank'>Click Here</a>"
    text <- "<b>Dataset must include:</b>"
    text <- paste0(text, "<ul>")
    text <- paste0(text,
                   "<li>A column indicating the length classes of measured individuals (first column of dataset).</li>")
    text <- paste0(text, "<li>A row indicating the dates when individuals were measured (first row of dataset excluding the first column).</li>")
    text <- paste0(text,
                   "<li>The number of individuals caught per length class (rows) and per sampling date (columns).</li>")
    text <- paste0(text, "</ul>")
    text <- paste0(text, "<br>")
    text <- paste0(text, "<p> An example dataset in this format can be downloaded <a href='https://data.d4science.org/shub/E_Yzc0aHFhWE50WWdpaEhkMjl5TExHekdQU2NFR2FtNTM2NkRydTQ5clhMTzhVd3Y4bDJzcU16UXNSUWJzZ1NpTg==' target='_blank'> here </a>. </p>")
    text <- paste0(text, "<br>")
    text <- paste0(text, "<b>Specific considerations regarding your own dataset:</b>")
    text <- paste0(text, "<ul>")
    text <- paste0(text, "<li>", "Save your dataset in 'csv' format.", "</li>")
    text <- paste0(text, "<li>", "The separator for the .csv file should be a comma ‘,’ semicolon ';' or tab. The default might differ depending on the language settings of your spreadsheet manipulation program (e.g. Excel).", "</li>")
    text <- paste0(text, "<li>", "Use a '.' to separate decimals in the data. The default might differ depending on the language settings of your spreadsheet manipulation program (e.g. Excel).", "</li>")
    text <- paste0(text, "<li>", "The first column of the data set should include the mid lengths of the length classes.", "</li>")
    text <- paste0(text, "<li>The data format of your data file should be automatically detected, or select the date format used in your data file under <b>'Choose CSV date format'</b>, e.g. DD/MM/YYYY format to select 'Day Month Year'.</li>")
    text <- paste0(text, "<li>The date must include a specification of the day. If the data are aggregated or no information about the day is available, you could consider to set the day to the midpoint of the month, e.g. 15/01/2021.</li>")
    text <- paste0(text, "<li>Your data set should at least be representative of a whole year. This is particularly important if seasonally varying growth is estimated.</li>")


    ## text <- paste0(text, "<li>Ensure that your dates are given in chronological order.</li>")
    ## text <- paste0(text, "<li>", "Ensure that the 'midLength' column name is identical to the sample dataset.", "</li>")
    text <- paste0(text, "</ul>")
    text <- paste0(text, "<br>")
    text <- paste0(text, "<p> Further information about different formats of length-frequency datasets ",
                   "and how to convert one format to the other are described in a ",
                   "<a href='https://cran.r-project.org/web/packages/TropFishR/vignettes/lfqData.html'  target='blank_'>tutorial by Mildenberger (2020)</a>. </p>")
    return(text)
})
output$fishMethodsSampleDataset <- renderText({
    session$userData$page('fishmethods-sample')
    link <- "<a href='https://data.d4science.org/shub/E_NnMvMjhHUjB4Q3k4SE1oWjFCamxxMm5zdUxMSEpKbFdlcjVWaHQ1U1ZoTXJJY0dqaWJzRmxHWDVFemFjYVhwcQ==' target='_blank'>Click Here</a>"
    text <- paste0("<p><h4>", link,"&nbsp; to download a sample dataset that can be used with <b>FishMethods</b>", "</h4></p>")
    text <- paste0(text, "<b>If you are creating your own dataset</b>")
    text <- paste0(text, "<ul>")
    text <- paste0(text, "<li>Mandatory fields to run YPR/SBPR are:</li>")
    text <- paste0(text, "<ul>")
    text <- paste0(text, "<li>age (the age of the fish)</li>")
    text <- paste0(text, "<li>ssbwgt (the spawning stock weights for each age)</li>")
    text <- paste0(text, "<li>partial (the recruitment at each age that is used to determine how much fishing mortality (F) each age group receives)</li>")
    text <- paste0(text, "<li>pmat (the proportion of mature fish at each age (used only for SBPR)</li>")
    text <- paste0(text, "</ul>")
    text <- paste0(text, "<li>Ensure that the column names are identical to the sample dataset.</li>")
    text <- paste0(text, "<li>Ensure your data are in .csv format.</li>")
    text <- paste0(text, "<li>Use a “.” to separate decimals in the data.</li>")
    text <- paste0(text, "</ul>")
    text
})

output$vonBertalannfyInfoText <- renderText({
    text <- "<b>The VBGF expresses the length, L, as a function of the age of the fish, t. K is a parameter that controls the curvature</b>"
    text <- paste0(text, "<h5>", withMathJax("\\(L_\\infty\\)"), "is interpreted as 'the mean length of very old (strictly: infinitely old) fish'. It is also called the 'asymptotic length'.", "</h5>")
    text <- paste0(text, "<h5>", "K is a 'curvature parameter' which determines how fast the fish approaches its ", withMathJax("\\(L_\\infty.\\)"), "</h5>")
    text <- paste0(text, "<h5>", "Some species, most of them short-lived, almost reach their ", withMathJax("\\(L_\\infty\\)"),  "in a year or two and have a high value of K.")
    text <- paste0(text, "Other species have a flat growth curve with a low K-value and need many years to reach anything like their ", withMathJax("\\(L_\\infty.\\)"),  "</h5>")
    text <- paste0(text, "<h5>", "Increasing K with the slider bar will result in a growth curve that has more 'bend'.", "</h5>")
    text <- paste0(text, "<h5>", "The third parameter, ", withMathJax("\\(t_0,\\)"),  "sometimes called 'the initial condition parameter', determines the point in time when the fish has zero length.", "</h5>")
    text <- paste0(text, "<h5>",  "Biologically, this has no meaning, because the growth begins at hatching when the larva already has a certain length, which may be called L(0) when we put t = 0 at the day of birth.", "</h5>")
    text <- paste0(text, "<h5>",  "It is easily identified by inserting t = 0 into the equation.", "</h5>")
    text <- paste0(text, "<h5>",  "Growth parameters differ from species to species, but they may also vary from stock to stock within the same species, i.e. growth parameters of a particular species may take different values in different parts of its range. Also successive cohorts may grow differently depending on environmental conditions.", "</h5>")
    text <- paste0(text, "<h5>", "Further growth parameters often take different values for the two sexes. If there are pronounced differences between the sexes in their growth parameters, the input data should be separated by sex and values of K, ", withMathJax("\\(L_\\infty,\\)"), " and ", withMathJax("\\(t_0\\)"), "should be estimated for each sex separately.", "</h5>")
})
output$seasonalVonBertalannfyInfoText <- renderText({
    text <- "Like the generalized VBGF, the seasonal VBGF expresses the length, L, as a function of the age of the fish, t. K is a parameter that controls the curvature."
    text <- paste0(text,"The addition of the term:", withMathJax("\\(\\frac{Ck}{2\\pi}sin2\\pi(t-t_s)\\)"), "produces seasonal oscillations of the growth rate, by changing ", withMathJax("\\(t_0,\\)"), "during the year. The parameter ", withMathJax("\\(t_s,\\)"))
    text <- paste0(text, "is called the 'summer point', and takes values between 0 and 1. At the time of the year when ")
    text <- paste0(text, "the fraction ", withMathJax("\\(t_s,\\)"), "has elapsed, the growth rate is the highest. At time")
    text <- paste0(text, " ", withMathJax("\\(t_w = t_s+0.5,\\)"), "which is the 'winter point', the growth rate is the lowest.")
    # text <- paste0(text, "</h4>")
    # text <- paste0(text, "<h4>")
    text <- paste0(text, "If C = 0, then the equation reduces to the generalized VBGF. In other words, C = 0 implies that there is no ")
    text <- paste0(text, "seasonality in the growth rate. The higher the value of C the more pronounced are the seasonal oscillations. ")
    text <- paste0(text, "If C = 1, the growth rate becomes zero at the winter point.")
    # text <- paste0(text, "</h4>")
    text <- paste0(text, "<br/>")
    text <- paste0(text, "<h5>")
    text <- paste0(text, "As with the generalized VBGF:")
    text <- paste0(text, "</h5>")
    text <- paste0(text, "<h5>")
    text <- paste0(text, withMathJax("\\(L_\\infty\\)"), "is is interpreted as 'the mean length of very old (strictly: infinitely old) fish'. it is also called the 'asymptotic length'.")
    text <- paste0(text, "</h5>")
    text <- paste0(text, "<h5>")
    text <- paste0(text, "K is a 'curvature parameter' which determines how fast the fish approaches its ", withMathJax("\\(L_\\infty.\\)"))
    text <- paste0(text, "</h5>")
    text <- paste0(text, "<h5>")
    text <- paste0(text, "Some species, most of them short-lived, almost reach their ", withMathJax("\\(L_\\infty\\)"), "in a year or two and have a high value of K. Other species have a flat growth curve with a low K-value and need many years to reach anything like their ", withMathJax("\\(L_\\infty.\\)"))
    text <- paste0(text, "</h5>")
    text <- paste0(text, "<h5>")
    text <- paste0(text, "Increasing K with the slider bar will result in a growth curve that has more 'bend'.")
    text <- paste0(text, "</h5>")
    text <- paste0(text, "<h5>")
    text <- paste0(text, "The third parameter, ", withMathJax("\\(t_0,\\)"), "sometimes called 'the initial condition parameter', determines the point in time when the fish has zero length.")
    text <- paste0(text, "</h5>")
    text <- paste0(text, "<h5>")
    text <- paste0(text, "Biologically, this has no meaning, because the growth begins at hatching when the larva already has a certain length, which may be called L(0) when we put t = 0 at the day of birth.")
    text <- paste0(text, "</h5>")
    text <- paste0(text, "<h5>")
    text <- paste0(text, "It is easily identified by inserting t = 0 into the equation.")
    text <- paste0(text, "</h5>")
    text
})
output$naturalMortalityInfoText <- renderText({
    text <- "<h5>This tool employs various empirical estimators of natural mortality.</h5>"
    text <- paste0(text, "<h5>", "When the user enters the scientific name for a fish species, FishBase will be queried for:", "</h5>")
    text <- paste0(text, "<ul>")
    text <- paste0(text, "<li>", "(1) Maximum age (Amax)", "</li>")
    text <- paste0(text, "<li>", "(2) Age at maturity (Amat)", "</li>")
    text <- paste0(text, "<li>", "(3) L-infinity (in cm) (Linf)", "</li>")
    text <- paste0(text, "<li>", "(4) Von Bertalanffy Growth Function (VBGF) growth coefficient (k)", "</li>")
    text <- paste0(text, "<li>", "(5) VBGF age at size 0 (t0)", "</li>")
    text <- paste0(text, "<li>", "(6) Body length in cm (Bl)", "</li>")
    text <- paste0(text, "<li>", "(7) Mean water temperature in Celsius (Temp)", "</li>")
    text <- paste0(text, "</ul>")
    text <- paste0(text, "<h5>", "Averaging of Von Bertalanffy (VBFG) parameters is done following the specifications of Pauly, D. (1991). Growth performance in fishes: rigorous description of patterns as a basis for understanding causal mechanisms. ICLARM Contribution No. 793.", "</h5>")
    text <- paste0(text, "<h5>", "Estimates will be displayed in the main panel.",  "</h5>")
    text <- paste0(text, "<h5>", "Four methods use Amax, three methods use the VBGF parameters, two methods use the VBGF parameters & Temp, one method uses the VBGF parameters & Amat, and three methods use only Amat. These groupings are indicated by the different colors in the top plot.", "</h5>")
    text <- paste0(text, "<h5>", "<em>The user can also choose to input their own parameters if they have inputs from local studies. These input values will override the FishBase calculations if all the necessary parameters for a particular method are available (e.g., all the VBGF parameters are available for those methods that require them).</em>", "</h5>")
    text <- paste0(text, "<h5>","The individual estimates of M are combined with defined weightings below (that the user can modify) and a single average M is provided. This average M can be used as input in the YPR/SBPR and ELEFAN applications.",  "</h5>")
    text <- paste0(text, "<h5>", "References for each method can be found", " <a href=\"javascript:window.open('References_M.html', '_blank','width=600,height=400')\">here</a>", "</h5>")
    text <- paste0(text, "<h5>",  "</h5>")
    text
})

output$tropFishRLibVersion1 <- renderText({
    text <- paste0("<span class='subTitle'>", "By <a href='https://cran.r-project.org/web/packages/TropFishR/index.html' target='_blank'>TropFishR</a> version ", packageVersion("TropFishR"),"</span>")
    text
})

output$tropFishRLibVersion2 <- renderText({
    text <- paste0("<span class='subTitle'>", "By <a href='https://cran.r-project.org/web/packages/TropFishR/index.html' target='_blank'>TropFishR</a> version ", packageVersion("TropFishR"),"</span>")
    text
})

output$tropFishRLibVersion3 <- renderText({
    text <- paste0("<span class='subTitle'>", "By <a href='https://cran.r-project.org/web/packages/TropFishR/index.html' target='_blank'>TropFishR</a> version ", packageVersion("TropFishR"),"</span>")
    text
})

output$fishMethodsVersion1 <- renderText({
    text <- paste0("<span class='subTitle'>", "By <a href='https://cran.r-project.org/web/packages/fishmethods/index.html' target='_blank'>Fishmethods</a> version ", packageVersion("fishmethods"),"</span>")
    text
})

output$fishMethodsVersion2 <- renderText({
    text <- paste0("<span class='subTitle'>", "By <a href='https://cran.r-project.org/web/packages/fishmethods/index.html' target='_blank'>Fishmethods</a> version ", packageVersion("fishmethods"),"</span>")
    text
})

output$basicShaeferTitle <- renderText({
    session$userData$page('basic-shaefer')
    text <- "<span><h3><b>Run surplus production model</b></h3></span>"
    text
})
output$basicVonBertalannfyTitle <- renderText({
    session$userData$page('basic-von-bertalannfy')
    text <- "<span><h3><b>Generalized Von Bertalanffy Growth Function (VBGF)</b></h3></span>"
    text
})
output$SeasonalVonBertalannfyTitle <- renderText({
    session$userData$page('seasonal-von-bertalannfy')
    text <- "<span><h3><b>Seasonal Von Bertalanffy Growth Function (soVBGF)</b></h3></span>"
    text
})
output$naturalMortalityTitle <- renderText({
    session$userData$page('natural-mortality')
    text <- "<div style='width: 100%;position: relative;height: 100px; margin-bottom:3px;'>"
    text <- paste0(text, "<div style='float: left; width: 70%;'><span><h3><b>Estimating Natural Mortality (M) from FishBase life history parameters</b></h3><br>This application is a modified version of the Barefoot Ecologist tool developed by Jason Cope: <a target='_blank' href='http://barefootecologist.com.au/shiny_m.html'>http://barefootecologist.com.au/shiny_m.html</a></span></div>")
    text <- paste0(text, "</div>")
    text
})


output$rnMax_sa <- renderText({
    if ("results" %in% names(elefan_sa)) {
        title <- paste0("<strong>Highest value of fitness function:</strong>&nbsp;", round(elefan_sa$results$data$Rn_max, 3))
        title
    } else {  "" }
})
output$rnMax <- renderText({
    if ("results" %in% names(elefan)) {
        title <- paste0("<strong>Highest value of fitness function:</strong>&nbsp;", round(elefan$results$data$Rn_max, 3))
        title
    } else {  "" }
})

output$titlePlot1_elefan <- renderText({
    if ('results' %in% names(elefan)) {
        txt <- "<p class=\"pheader_elefan\">Raw LFQ data</p>"
        txt
    }
})
output$titlePlot2_elefan <- renderText({
    if ('results' %in% names(elefan)) {
        txt <- "<p class=\"pheader_elefan\">Restructured LFQ data</p>"
        txt
    }
})
output$titlePlot3_elefan <- renderText({
    if ('results' %in% names(elefan)) {
        txt <- "<p class=\"pheader_elefan\">Thompson and Bell model with changes in F</p>"
        txt
    }
})
output$titlePlot4_elefan <- renderText({
    if ('results' %in% names(elefan)) {
        txt <- "<p class=\"pheader_elefan\">Thompson and Bell model with changes in F and Lc</p>"
        txt
    }
})
output$title_tbl1_e <- renderText({
    if ('results' %in% names(elefan)) {
        txt <- "<p class=\"pheader_elefan\">Biological reference levels:</p>"
        txt
    }
})
output$title_tbl2_e <- renderText({
    if ('results' %in% names(elefan)) {
        txt <- "<p class=\"pheader_elefan\">Current levels:</p>"
        txt
    }
})
output$titleResultsOfTheComputation_elefan <- renderText({
    if ('results' %in% names(elefan)) {
        txt <- "<h2>Results of the ELEFAN computation</h2>"
        txt
    }
})

output$shaefer_ex1 <- renderUI({
    withMathJax(helpText('Classic Schaefer (logistic) form  $$B_t = rB_t\\left(1 +
               \\frac{r}{K}\\right)\\!$$'))
})
output$shaefer_ex2 <- renderUI({
    withMathJax(helpText('Biomass giving maximum sustainable yield:  $$B_{MSY} = \\frac{K}{2}\\!$$'))
})
output$shaefer_ex3 <- renderUI({
    withMathJax(helpText('Maximum sustainable yield:  $$MSY = \\frac{rK}{4}\\!$$'))
})
output$shaefer_ex4 <- renderUI({
    withMathJax(helpText('Fishing mortality rate at MSY:  $$F_{MSY} = \\frac{r}{2}\\!$$'))
})
output$basicShaeferInfoText <- renderText({
    text <- "<p><h5>The surplus production model is an example of a 'holistic model', wherein the stock is considered as one unit of biomass and no attempt is made to model on an age- or length-base. These models deal with the entire stock, the entire fishing effort and the total yield obtained from the stock, without incorporating details such as growth and mortality-at-age or the effect of the gear on the age of fish capture.</h5></p>"
    text <- paste0(text, "<p><h5>", "The objective of the application of 'surplus production models' is to determine the optimum level of effort, that is the effort that produces the maximum yield that can be sustained without affecting the long-term productivity of the stock, the so-called maximum sustainable yield (MSY).", "</h5></p>")
})

output$cmsyLegacyWarning <- renderText({
    text <- "<span style='margin-left: 20px;'>This computation may take several minutes to complete.</span>"
    text
})



## spict -------------------------
output$spictVersion <- renderText({
    text <- paste0("<span class='subTitle'>", "By <a href='https://github.com/DTUAqua/spict/blob/master/spict/DESCRIPTION' target='_blank'>SPiCT</a> version ", packageVersion("spict"),"</span>")
    text
})



## glossary -------------------------
output$glossary <- renderText({
    session$userData$page("glossary")
    render_glossary_html(glossary_df)
})
