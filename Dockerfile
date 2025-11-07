FROM --platform=linux/amd64  rocker/r-ver:4.4.1
RUN /rocker_scripts/setup_R.sh https://packagemanager.posit.co/cran/__linux__/jammy/2025-09-10

# install OS dependencies including java and python 3
RUN apt-get update && apt-get install -y openjdk-8-jdk liblzma-dev libbz2-dev libncurses5-dev curl python3-dev python3.venv git pandoc \
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
ARG ROMOPAPI_BRANCH=main
ARG BUILD_CACHE_BUSTER=2

# Install renv and restore packages
RUN --mount=type=secret,id=build_github_pat \
    cp /usr/local/lib/R/etc/Renviron /tmp/Renviron \
    && echo "GITHUB_PAT=$(cat /run/secrets/build_github_pat)" >> /usr/local/lib/R/etc/Renviron \
    && Rscript -e 'install.packages("remotes")' \
    && Rscript -e 'remotes::install_github("FINNGEN/ROMOPAPI")' \
    && cp /tmp/Renviron /usr/local/lib/R/etc/Renviron;

# Expose the port that the API will run on
EXPOSE 8585

# Run the API server
CMD ["Rscript", "-e", "ROMOPAPI::runApiServer(host = '0.0.0.0', port = 8585)"] 