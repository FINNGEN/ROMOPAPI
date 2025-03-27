FROM --platform=linux/amd64  rocker/r-ver:4.4.1
RUN /rocker_scripts/setup_R.sh https://packagemanager.posit.co/cran/__linux__/jammy/2025-01-30

# install OS dependencies including java and python 3
RUN apt-get update && apt-get install -y openjdk-8-jdk liblzma-dev libbz2-dev libncurses5-dev curl python3-dev python3.venv git \
    # rjava
    libssl-dev libcurl4-openssl-dev  libpcre2-dev libicu-dev \
    # xml2
    libxml2-dev \
    # sodium
    libsodium-dev\
    # systemfonts
    libfontconfig1-dev \
    # textshaping
    libharfbuzz-dev libfribidi-dev\
    #ragg
    libfreetype6-dev libpng-dev libtiff5-dev libjpeg-dev\
    # inotify for pre_init.sh
    inotify-tools\
&& R CMD javareconf \
&& rm -rf /var/lib/apt/lists/*

# Install renv and restore packages
RUN Rscript -e 'install.packages("remotes")'
RUN Rscript -e 'remotes::install_github("FINNGEN/ROMOPAPI", force = TRUE)'

# Expose the port that the API will run on
EXPOSE 8585

# Create a directory for Eunomia data
RUN mkdir -p /eunomia_data
COPY eunomia_data/FinnGenR12_v5.4.sqlite /eunomia_data/FinnGenR12_v5.4.sqlite
COPY eunomia_data/FinnGenR12_v5.4.zip /eunomia_data/FinnGenR12_v5.4.zip
ENV EUNOMIA_DATA_FOLDER=/eunomia_data

# Run the API server
CMD ["Rscript", "-e", "ROMOPAPI::runApiServer(host = '0.0.0.0', port = 8585)"] 