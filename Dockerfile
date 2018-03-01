# shiny-server on ubuntu 16.04


# To build:
#  1.  cd to the Dockerfile directory
#  2.  docker build -t ubuntu/vod-shiny-server:latest .
#
# To run:
#
#  docker run --rm -d -p 80:3838 ubuntu/vod-shiny-server:latest
# 
# To run with local volumes:
#
#  docker run --rm -d -p 80:3838 \
#     -v /srv/shinyapps/:/srv/shiny-server/ \
#     -v /srv/shinylog/:/var/log/shiny-server/ \ 
#     ubuntu/vod-shiny-server:latest

FROM ubuntu:16.04

LABEL \
    mantainer="pg" \
    com.shiny-server.is-beta="false" \
    com.shiny-server.is-production="true" \
    com.shiny-server.version="1.0" \
    com.shiny-server.release-date="2018-01-18"

# =====================================================================
# R Server
# =====================================================================

# Don't print "debconf: unable to initialize frontend: Dialog" messages
ARG DEBIAN_FRONTED=noninteractive

# Need this to add R repo
RUN apt-get update && apt-get install -y software-properties-common

# Add R apt repository
RUN add-apt-repository "deb http://cran.r-project.org/bin/linux/ubuntu $(lsb_release -cs)/"
RUN apt-key adv --keyserver keyserver.ubuntu.com --recv-keys E084DAB9

# Install basic stuff and R
RUN apt-get update && apt-get install -y \
    git \
    vim-tiny \
    less \
    wget \
    r-base \
    r-base-dev \
    r-recommended \
    fonts-texgyre

RUN echo 'options(\n\
  repos = c(CRAN = "https://cran.r-project.org/"),\n\
  download.file.method = "libcurl",\n\
  # Detect number of physical cores\n\
  Ncpus = parallel::detectCores(logical=FALSE)\n\
)' >> /etc/R/Rprofile.site

# =====================================================================
# Shiny Server
# =====================================================================

# DEPS: Debian/Ubuntu

# libc6
# psmisc
# rrdtool
# libssl 0.9.8

RUN apt-get update && apt-get install -y \
    gdebi-core \
    pandoc \
    pandoc-citeproc \
    libcairo2-dev \
    libxt-dev \
    libssl-dev \
    libxml2-dev \
    cmake \
    libpng12-dev \
    gdal-bin \
    libgdal1-dev \
    libgdal-dev \
    libgeos-dev \
    libudunits2-dev \
    libv8-3.14-dev \
    libproj-dev \
    libcurl4-openssl-dev &&\
    rm -rf /var/lib/apt/lists/* 

# Download and install shiny server
RUN wget --no-verbose https://s3.amazonaws.com/rstudio-shiny-server-os-build/ubuntu-12.04/x86_64/VERSION -O "version.txt" && \
    VERSION=$(cat version.txt)  && \
    wget --no-verbose "https://s3.amazonaws.com/rstudio-shiny-server-os-build/ubuntu-12.04/x86_64/shiny-server-$VERSION-amd64.deb" -O ss-latest.deb && \
    gdebi -n ss-latest.deb && \
    rm -f version.txt ss-latest.deb

# Download, build and install R packages
RUN R -e "install.packages(c('shiny', 'shinydashboard', 'rmarkdown', 'shinyjs', 'scales', 'plotly', 'data.table', 'dplyr', 'htmltools', 'crosstalk', 'V8', 'RColorBrewer'))" && \
    rm -rf /tmp/*

# Install latest shiny from GitHub and copy examples
# RUN R -e "devtools::install_github('rstudio/shiny')" && \
#   cp -R /usr/local/lib/R/site-library/shiny/examples/* /srv/shiny-server/

EXPOSE 3838

RUN mkdir -p /var/log/shiny-server && chown shiny.shiny /var/log/shiny-server

COPY ./config/shiny-server.conf /etc/shiny-server/shiny-server.conf
COPY . /srv/shiny-server/

# START Copying data from S3
COPY data /srv/shiny-server/basic/data
# END Copying data from S3


RUN chmod -R 777  /srv/shiny-server/

CMD ["/opt/shiny-server/bin/shiny-server"]
 
