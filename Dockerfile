## Start with the tidyverse docker image
FROM rocker/shiny:latest

MAINTAINER "Sam Abbott" sam.abbott@bristol.ac.uk

RUN apt-get install -y \
    libnlopt0 \
    libnlopt-dev \
    libv8-3.14-dev \
    librsvg2-dev \
    libjpeg-dev \
    libssh2-1-dev \
    libcurl4-openssl-dev \
    && apt-get clean
    
## Install cran packages
RUN install2.r --error \
    --deps TRUE \
     shinydashboard \
     shinyBS \
     shinyWidgets \
     DT \
     rmarkdown \
     e1071 \
     caret \
     ggfortify \
     plotly \
     lubridate \
     wrapr \
     stringr \
     remotes

RUN rm -r /srv/shiny-server/*
ADD . /srv/shiny-server/fcdashboard
