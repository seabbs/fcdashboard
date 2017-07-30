## Start with the tidyverse docker image
FROM rocker/shiny:latest

MAINTAINER "Sam Abbott" sam.abbott@bristol.ac.uk

RUN apt-get update && \
    apt-get install -y \
    libssl-dev \
    libnlopt0 \
    libnlopt-dev \
    && apt-get clean

## install igraph due to CRAN bug from github
RUN install2.r --error \
      --deps TRUE \
      pkgconfig \
      irlba \
      remotes

RUN installGithub.r igraph/rigraph \
&& rm -rf /tmp/downloaded_packages/

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
     stringr 

RUN rm -r /srv/shiny-server/*
ADD . /srv/shiny-server/fcdashboard
