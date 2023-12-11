#
# This docker file will help you to deploy the 
# application using the shiny proxy server on
# an ubuntu based distribution
#
# For more information about ShinyProxy, please see
# https://www.shinyproxy.io/
#
FROM openanalytics/r-base

MAINTAINER Jeffery Painter "jeff@jivecast.com"

# system libraries of general use
RUN apt-get update && apt-get install -y \
    sudo \
    pandoc pandoc-citeproc libcurl4-gnutls-dev \
    libcairo2-dev libxt-dev libxml2-dev \
    libfontconfig1-dev  \
    libcurl4-doc libgnutls28-dev libidn11-dev libkrb5-dev librtmp-dev

RUN apt-get install -y \
    apt-transport-https apt-utils ca-certificates cmake curl g++ gcc \
    gfortran git make libblas-dev libfontconfig1-dev \
    libfreetype6-dev libfribidi-dev libgit2-dev libharfbuzz-dev \
    libiodbc2-dev libjpeg-dev liblapack-dev libmariadb-dev libpng-dev \
    libpq-dev libssh-dev libssl-dev libtiff5-dev \
    locales pkg-config 

# CS6460 additional required packages
RUN apt-get install -y \
    sqlite3 libsodium-dev 

# Shiny packages
RUN R -e "install.packages(c('bench','broom','broom','callr','cli','covr','dplyr','dplyr','DT','dtplyr','feather','forcats','ggplot2','glue','googledrive','googlesheets4','haven','hms','httr','jsonlite','knitr','Lahman','lifecycle','lobstr','lubridate','magrittr','methods','microbenchmark','mockr','modelr','nycflights13','pillar','plotly','purrr','R6','ragg','readr','readr','readxl','reprex','rlang','rmarkdown','rvest','shiny','stats','datasets','stringi','stringr','stringr','testthat','tibble','tidyr','tidyselect','tidyverse','vctrs','withr','xml2', 'ggpubr', 'foreach', 'dbplyr', 'units', 's2', 'sf', 'maps', 'ggrepel', 'grDevices', 'utils'), repos='https://cloud.r-project.org/')"

# CS6460 specific packages
RUN R -e "install.packages(c('DBI', 'RSQLite', 'reactable', 'shinyalert', 'shinyauthr', 'effsize', 'pwr', 'lattice', 'grid', 'gridExtra'  ), repos='https://cloud.r-project.org/')"

# Install custom ppa to get SF to work
RUN apt-get update && apt-get install -y software-properties-common
RUN add-apt-repository ppa:ubuntugis/ubuntugis-unstable
RUN apt-get update
RUN apt-get install -y libudunits2-dev libgdal-dev libgeos-dev libproj-dev libsqlite0-dev

# copy the app to the image
RUN mkdir /root/cs6460

# our application is in the ./app directory relative to this docker file
COPY cs6460 /root/cs6460

COPY Rprofile.site /usr/lib/R/etc/

EXPOSE 3838

CMD ["R", "-e", "shiny::runApp('/root/cs6460')"]
