FROM r-base:3.4.0

RUN apt-get update && apt-get -y install \
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
  yaml

RUN Rscript -e 'remotes::install_github("rstats-db/RPostgres");' && \
  Rscript -e 'remove.packages("BH")'

# Packages that we might want to use; this list should *probably* be
# in another docker image built on top of this one but leaving here
# for now (it needs to come before the COPY or it will never be
# cached)
RUN install.r \
  ggplot2

COPY . /orderly
RUN R CMD INSTALL /orderly && \
  Rscript -e 'orderly:::write_script("/usr/bin")' && \
  rm -rf /orderly

ENTRYPOINT ["/usr/bin/orderly"]
