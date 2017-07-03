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
  docopt \
  ids \
  jsonlite \
  knitr \
  remotes \
  rmarkdown \
  yaml

RUN Rscript -e 'remotes::install_github("rstats-db/RPostgres");' && \
  Rscript -e 'remove.packages("BH")'

RUN Rscript -e 'remotes::install_github("vimc/orderly", dependencies = FALSE)' \
  && Rscript -e 'orderly:::write_script("/usr/bin")'

ENTRYPOINT ["/usr/bin/orderly"]
