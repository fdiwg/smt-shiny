FROM rocker/r-ver:4

MAINTAINER Emmanuel Blondel "emmanuel.blondel@fao.org"

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
    libsecret-1-dev \
    libnlopt-dev \
    libharfbuzz-dev \
    libfribidi-dev

RUN apt-get update && apt-get upgrade -y
RUN apt-get update && apt-get -y install cmake

# install core package dependencies
RUN install2.r --error --skipinstalled --ncpus -1 remotes
RUN R -e "install.packages(c('jsonlite','yaml'), repos='https://cran.r-project.org/')"
# clone app
RUN git -C /root/ clone https://github.com/fdiwg/smt-shiny.git && echo "OK!"
RUN ln -s /root/smt-shiny /srv/smt-shiny
# install R app package dependencies
RUN R -e "source('./srv/smt-shiny/install.R')"

EXPOSE 3838

ENV SMT_LOG=session.log

RUN apt-get -y update
RUN apt-get install -y curl
#Development
CMD ["R", "-e shiny::runApp('/srv/shiny/smt-shiny',port=3838,host='0.0.0.0')"]
