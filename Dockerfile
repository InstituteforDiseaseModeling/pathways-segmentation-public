FROM rocker/rstudio:4.4.2

ENV DEBIAN_FRONTEND=noninteractive

# Install system dependencies and gosu
RUN apt-get update && apt-get install -y --no-install-recommends \
    libcurl4-openssl-dev libssl-dev libxml2-dev \
    libudunits2-dev libgdal-dev libgeos-dev libproj-dev \
    libmagick++-dev libharfbuzz-dev libfribidi-dev \
    libfontconfig1-dev libfreetype6-dev \
    libpng-dev libtiff5-dev libjpeg-dev \
    libglpk-dev libcairo2-dev libxt-dev \
    libv8-dev libnode-dev \
    libprotobuf-dev protobuf-compiler cmake \
    curl wget git chromium-browser gosu && \
    apt-get clean && rm -rf /var/lib/apt/lists/*

# Install pacman, renv, and GitHub packages
RUN R -e "install.packages(c('renv', 'pacman', 'remotes'), repos = 'https://cloud.r-project.org')"
RUN R -e "remotes::install_github('davidsjoberg/ggsankey')"
RUN R -e "remotes::install_github('ricardo-bion/ggradar')"

# Set working directory
WORKDIR /home/rstudio/project

# Copy project files
COPY analyses/a_new_analysis_template/renv.lock renv.lock
COPY analyses/a_new_analysis_template/renv/activate.R renv/activate.R
COPY analyses/a_new_analysis_template/.Rprofile .Rprofile
COPY analyses/a_new_analysis_template/pathways-segmentation.Rproj pathways-segmentation.Rproj
COPY analyses/a_new_analysis_template/*.R ./
COPY analyses/a_new_analysis_template/*.xlsx ./
COPY analyses/a_new_analysis_template/functions/ ./functions/
COPY analyses/a_new_analysis_template/config.yml config.yml


RUN chown -R rstudio . \
 && sudo -u rstudio R -e 'source("renv/activate.R"); renv::restore()'






# FROM rocker/rstudio:latest

# ENV DEBIAN_FRONTEND=noninteractive

# # Install system dependencies and gosu
# RUN apt-get update && apt-get install -y --no-install-recommends \
#     libcurl4-openssl-dev libssl-dev libxml2-dev \
#     libudunits2-dev libgdal-dev libgeos-dev libproj-dev \
#     libmagick++-dev libharfbuzz-dev libfribidi-dev \
#     libfontconfig1-dev libfreetype6-dev \
#     libpng-dev libtiff5-dev libjpeg-dev \
#     libglpk-dev libcairo2-dev libxt-dev \
#     libv8-dev libnode-dev \
#     libprotobuf-dev protobuf-compiler cmake \
#     curl wget git chromium-browser gosu && \
#     apt-get clean && rm -rf /var/lib/apt/lists/*

# # Install pacman, renv, and GitHub packages
# RUN R -e "install.packages(c('renv', 'pacman', 'remotes'), repos = 'https://cloud.r-project.org')"
# RUN R -e "remotes::install_github('davidsjoberg/ggsankey')"
# RUN R -e "remotes::install_github('ricardo-bion/ggradar')"

# # Set working directory
# WORKDIR /home/rstudio/project

# # Copy project files
# COPY renv.lock renv.lock
# COPY renv/activate.R renv/activate.R
# COPY .Rprofile .Rprofile
# COPY pathways-segmentation.Rproj pathways-segmentation.Rproj
# COPY *.R ./
# COPY *.xlsx ./
# COPY functions/ ./functions/
# COPY config.yml config.yml


# RUN chown -R rstudio . \
#  && sudo -u rstudio R -e 'source("renv/activate.R"); renv::restore()'
