FROM rocker/shiny:latest

RUN apt-get update && apt-get install -y \
    libpq-dev libssl-dev libxml2-dev libgdal-dev libproj-dev libudunits2-dev \
    && rm -rf /var/lib/apt/lists/*

RUN R -e "install.packages(c('shiny', 'DBI', 'RPostgres', 'dplyr', 'stringr', 'leaflet', 'jsonlite', 'shinycssloaders'))"

# This copies everything in HZ_App into the image
COPY . /srv/shiny-server/

RUN chown -R shiny:shiny /srv/shiny-server/

EXPOSE 3838
CMD ["R", "-e", "shiny::runApp('/srv/shiny-server', host='0.0.0.0', port=as.numeric(Sys.getenv('PORT')))"]
