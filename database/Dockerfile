FROM postgres:10.2
COPY bin /usr/local/bin

ENV POSTGRES_DB orderly
ENV POSTGRES_USER orderly
ENV POSTGRES_PASSWORD changeme
# This is needed to override the loss of data that happens if you
# don't mount a persistent volume at the mount point.
ENV PGDATA /pgdata

RUN ./docker-entrypoint.sh --version
