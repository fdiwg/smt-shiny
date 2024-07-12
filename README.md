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
