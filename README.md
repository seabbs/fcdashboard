
<!-- README.md is generated from README.Rmd. Please edit that file -->
Funding Circle Dashboard
========================

[FC Dashboard](http://seabbs.co.uk/shiny/fcdashboard) is a shiny application that allows exploratory data analysis of the [Funding Circle](https://www.fundingcircle.com) loanbook. It is not affiliated with Funding Circle, the untransformed loanbook data is not provided.

FC Dashboard can also be used as a personal dashboard to enable you to better understand your investments. To enable this functionality download your personal loanbook from Funding Circle and upload it using the upload option in the options menu. For the best experiance it is also recommended to upload an up to date copy of the Funding Circle loanbook (as the app combines the datasets). Data is stored temporarily during your session only; the app can also be run locally if desired (see instructions below).

Installing the shiny app locally
--------------------------------

To install and run the shiny app locally on your own computer you will need to first install [R](https://www.r-project.org/), it is also suggested that you install [Rstudio](https://www.rstudio.com/products/rstudio/download/). After downloading the source code from [this repository](https://www.github.com/seabbs/fcdashboard) click on the `fcdashboard.Rprof` file, this will open an Rstudio window. Type the following code into the command line;

``` r
install.packages("shiny")
install.packages("shinydashboard")
install.packages("shinyBS")
install.packages("DT")
install.packages("tidyverse")
install.packages("rmarkdown")
install.packages("caret")
install.packages("ggfortify")
install.packages("plotly")
install.packages("lubridate")
install.packages("wrapr")
install.packages("stringr")
```

Finally save the funding circle loanbook as `loanbook.csv`, and your personal loanbook as `personal_loanbook.csv` in the fcdashboard folder. To run the app open the `ui.R` file and press run, depending on your computer this may take some time. Enjoy exploring the funding circle loanbook!
