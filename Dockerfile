FROM rocker/shiny:4.3.3
RUN install2.r --error --skipinstalled shiny bslib plotly DT readxl data.table writexl RSQLite DBI cluster httr2 jsonlite testthat
WORKDIR /srv/floodpulse
COPY . .
EXPOSE 3838
CMD ["Rscript", "-e", "shiny::runApp('.', host = '0.0.0.0', port = 3838)"]
