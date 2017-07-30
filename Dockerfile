## Start with the tidyverse docker image
FROM rocker/shiny:latest

MAINTAINER "Sam Abbott" sam.abbott@bristol.ac.uk

RUN apt-get update && \
    apt-get install -y \
    libssl-dev \
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
