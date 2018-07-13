FROM rocker/r-ver:3.5.0

RUN apt-get update && apt-get -y install \
  git \
  libimage-exiftool-perl \
  libssl-dev \
  libpq-dev \
  wget

# R package dependencies, including a few extras that we'll want handy
RUN install2.r \
  DBI \
  R6 \
  RPostgres \
  RSQLite \
  digest \
  ids \
  jsonlite \
  knitr \
  optparse \
  remotes \
  rmarkdown \
  withr \
  yaml

RUN Rscript -e 'remove.packages("BH")'

# Install Pandoc
ENV PANDOC_VERSION "1.19.2.1"
RUN wget https://github.com/jgm/pandoc/releases/download/${PANDOC_VERSION}/pandoc-${PANDOC_VERSION}-1-amd64.deb \
  && dpkg --install pandoc-${PANDOC_VERSION}-1-amd64.deb \
  && rm pandoc-${PANDOC_VERSION}-1-amd64.deb

ARG GIT_ID='UNKNOWN'
ARG GIT_BRANCH='UNKNOWN'
ENV ORDERLY_GIT_HASH $GIT_ID
ENV ORDERLY_GIT_BRANCH $GIT_BRANCH

COPY . /orderly
RUN R CMD INSTALL /orderly && \
  Rscript -e 'orderly:::write_script("/usr/bin")' && \
  cp /orderly/inst/create_orderly_demo.sh /usr/bin/ && \
  rm -rf /orderly

ENTRYPOINT ["/usr/bin/orderly"]
