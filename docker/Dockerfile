FROM r-base:3.4.0

RUN apt-get update && apt-get -y install \
  libimage-exiftool-perl \
  libssl-dev \
  libpq-dev

# R package dependencies, including a few extras that we'll want handy
RUN install.r \
  DBI \
  R6 \
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

RUN Rscript -e 'remotes::install_github("rstats-db/RPostgres@v0.1-4")' && \
  Rscript -e 'remove.packages("BH")'

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
