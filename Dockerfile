FROM rocker/shiny:4.5.1

MAINTAINER Emmanuel Blondel "emmanuel.blondel@fao.org"

LABEL org.opencontainers.image.title="smt-shiny"
LABEL org.opencontainers.image.url="https://github.com/fdiwg/smt-shiny"
LABEL org.opencontainers.image.source="https://github.com/fdiwg/smt-shiny"
LABEL org.opencontainers.image.description="A shiny app providing stock monitoring tools"
LABEL org.opencontainers.image.authors="Emmanuel Blondel <emmanuel.blondel@fao.org>"

# system libraries of general use
RUN apt-get update && apt-get install -y \
    sudo \
    pandoc \
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

#working directory
WORKDIR /srv/smt-shiny

# Set environment variables for renv cache, see doc https://docs.docker.com/build/cache/backends/
ARG RENV_PATHS_ROOT

# Make a directory in the container
RUN mkdir -p ${RENV_PATHS_ROOT}

#copy renv configuration
RUN R -e "install.packages(c('renv'), repos='https://cran.r-project.org/')"
COPY renv.lock renv.lock
COPY .Rprofile  .Rprofile
COPY renv/activate.R renv/activate.R
COPY renv/settings.json renv/settings.json

# Set renv cache location: change default location of cache to project folder
# see documentation for Multi-stage builds => https://cran.r-project.org/web/packages/renv/vignettes/docker.html
RUN mkdir renv/.cache
ENV RENV_PATHS_CACHE=renv/.cache

# Restore the R environment
RUN R -e "renv::restore()"

#copy app
COPY . /srv/smt-shiny
#etc dirs (for config)
RUN mkdir -p /etc/smt-shiny/

EXPOSE 3838

CMD ["R", "-e shiny::runApp('/srv/smt-shiny',port=3838,host='0.0.0.0')"]