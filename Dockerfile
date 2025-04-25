# Use the official R Shiny image as the base
FROM rocker/shiny:latest

# Install system dependencies
RUN apt-get update && apt-get install -y \
    libcurl4-openssl-dev \
    libssl-dev \
    libxml2-dev \
    libxt-dev \
    libfontconfig1-dev \
    zlib1g-dev \
    && rm -rf /var/lib/apt/lists/*

# Install required R packages
RUN install2.r --error \
    shiny \
    readxl \
    writexl \
    DT \
    glue \
    janitor \
    shinyjs \
    uuid \
    zip

# Copy your Shiny app files into the container
# OM do not copy, will bind-mount
#COPY . /srv/shiny-server/

# Ensure proper permissions
#RUN chown -R shiny:shiny /srv/shiny-server

# Expose the default Shiny port
EXPOSE 3838

# Run the Shiny app
CMD ["R", "-e", "shiny::runApp('/srv/shiny-server', host='0.0.0.0', port=3838)"]
