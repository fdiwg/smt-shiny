# smt-shiny: Stock Monitoring Tools
This is the source code a the **Stock Monitoring Tool** Shiny application.
The application has been developed on an Ubuntu 16.04 operating system and the following requirements are meant for that platform

### System Wide Dependendencies
These dependencies can be installed using the usual *sudo apt install* command

- pandoc
- pandoc-citeproc
- libcurl4-openssl-dev
- libcairo2-dev
- libxt-dev
- libssl-dev
- libssl1.0.0
- libxml2 
- libxml2-dev 
- texlive-latex-base 
- texlive-fonts-recommended 
- texlive-formats-extra 
- libv8-3.14.5 
- libv8-dev
- libsodium-dev
- libsecret-1-dev

### R libraries
**devtools** must be installed in order to install some packages from GitHub
```
install.packages("devtools")
```
The following libraries have to be installed from GitHub
```
devtools::install_github('AnalytixWare/ShinySky')
devtools::install_github('daattali/shinyjs')
devtools::install_github('jyypma/nloptr')
```
The following libraries can be installed from CRAN
```
install.packages(c('shiny', 'rmarkdown', 'shinythemes', 'shinydashboard', 'RCurl', 'devtools', 'ggplot2', 'rfishbase', 'shinyBS', 'XML', 'futile.logger'), repos='https://cloud.r-project.org/')
```

### JABBA (Bayesian surplus production model)

Since version 0.8.0 the SMT runs [JABBA](https://github.com/jabbamodel/JABBA) locally, under *Surplus production models*.
JABBA requires the JAGS sampler at the system level and the R packages `rjags` and `JABBA`:

- system: `jags` (`sudo apt install jags`, installed in the Docker image before `renv::restore()`)
- R: `rjags`, `R2jags`, `coda`, `gplots`, `randtests`, `reshape` from CRAN and `JABBA` from GitHub, all pinned in `renv.lock`
  (JABBA 2.3.7, commit `c62db86c99dc5b5a0e115fd53fdd40f7c882b7c9`).

The application starts without these packages; the JABBA tool then reports that JABBA is not installed.
The bundled sample file `www/data/jabba_sample.csv` is synthetic demonstration data illustrating the input format; it is not a validated assessment example.

After each successful run the JABBA tool renders a PDF report (`assets/SPM/jabba/markdown/jabba_report.Rmd`, rendered with the LaTeX installation already present in the image) and, when the application was opened from the VRE with a user token, uploads it once to the `StockMonitoringTool` folder of the user's workspace, as the other assessment methods do. The run status shows whether the report and the upload succeeded; a failed report or upload keeps the fitted result and the other downloads available.

### CMSY availability (DataMiner decommissioned)

CMSY was executed remotely on the D4Science DataMiner WPS service, which has been decommissioned.

- The application no longer queries the D4Science registry for a DataMiner endpoint and no longer initialises a WPS client at session startup, with or without a `gcube-token`.
- CMSY stays visible (introduction, sample dataset link, data upload) with a "CMSY temporarily unavailable" banner. Its run button is a plain disabled control that the global button-restoring helpers cannot re-enable. Execution and the CMSY report download are also refused server-side.
- All other methods (ELEFAN_GA, LBI, LBSPR, SPiCT and the supporting tools) run locally and are unaffected. VRE workspace (StorageHub) uploads behave as before.
- Session tokens are no longer written to the logs; only their presence is logged.

Local validation results and limitations for this behaviour are summarised in the relevant pull request.

### Docker

#### Pull / Run the image from DockerHub

The SMT application nows come with an automated build & publication to [Github Packages](https://github.com/fdiwg/smt-shiny/pkgs/container/smt-shiny)

This procedure intends to facilitate the installation of the application, through a simple "pull & run":

```
docker pull ghcr.io/fdiwg/smt-shiny:latest
docker run --name stock_monitoring_tool -p 3839:3838 ghcr.io/fdiwg/smt-shiny
```

And then point your browser to http://localhost:3839

Note: In case of having an existing SMT app running on docker, and in order to update the docker app, it will be required to stop and remove the container prior to run the above commands to pull & run the app:

```
docker container stop stock_monitoring_tool
docker container rm stock_monitoring_tool
```

#### Build / Run the image locally

A Dockerfile is provided and can be used to build up containers with the application.

To build and run the application issue the following commands
```
sudo docker build -t stock_monitoring_tool <Path of the Dockerfile>
sudo docker run -p 3839:3838 stock_monitoring_tool
```

And then point your browser to http://localhost:3839

