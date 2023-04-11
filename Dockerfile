FROM rocker/r-ver:4

MAINTAINER Enrico Anello "enrico.anello@fao.org"


# system libraries of general use
RUN apt-get update && apt-get install -y \
    sudo \
    pandoc \
    pandoc-citeproc \
    libcurl4-openssl-dev \
    libcairo2-dev \
    libxt-dev \
    libssl-dev \
    libssh2-1-dev \
    libjpeg-dev \
    default-jre \
    default-jdk \
    libxml2-dev \
    libudunits2-dev \
    libproj-dev \
    libgeos-dev \
    libgdal-dev \
    git \
    texlive-latex-base \
    texlive-fonts-recommended \
    texlive-formats-extra \
    libv8-dev \
	libsodium-dev \
    libsecret-1-dev
	


RUN apt-get update && apt-get upgrade -y

# install dependencies of the Stock monitoring tool app
RUN R -e "install.packages(c('devtools'), repos='https://cran.r-project.org/', dependencies = TRUE)"
RUN R -e "install.packages(c('XML', 'xml2','shiny','rmarkdown','shinyjs','shinythemes','shinydashboard','shinyWidgets','RCurl','ggplot2','rfishbase','shinyBS','lubridate','waiter','pracma','googleVis','stringr','R.utils','fishmethods','V8','DT','futile.logger','TropFishR','nloptr','R6','sodium','keyring'), repos='https://cran.r-project.org/')"
RUN R -e "devtools::install_github('eblondel/d4storagehub4R')"
RUN R -e "devtools::install_github('eblondel/ows4R')"
RUN R -e "devtools::install_github('AnalytixWare/ShinySky')"

#Development
RUN git -C /root/ clone https://github.com/fdiwg/smt-shiny.git && echo "OK!"
RUN git checkout branch-0.5.1 
RUN mkdir -p /srv/shiny/
RUN ln -s /root/smt-shiny /srv/shiny/smt-shiny
 
EXPOSE 3838

ENV SMT_LOG=session.log

RUN apt-get install -y curl
#Development
CMD ["R", "-e shiny::runApp('/srv/shiny/smt-shiny',port=3838,host='0.0.0.0')"]
#Deployment
#CMD ["R", "-e shiny::runApp('/srv/shiny/smt-shiny')"]
