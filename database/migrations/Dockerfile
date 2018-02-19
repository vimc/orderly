FROM openjdk:8-jre-alpine

RUN apk --no-cache add openssl

WORKDIR /flyway

ENV FLYWAY_VERSION 4.2.0

RUN wget https://repo1.maven.org/maven2/org/flywaydb/flyway-commandline/${FLYWAY_VERSION}/flyway-commandline-${FLYWAY_VERSION}.tar.gz \
  && tar -xzf flyway-commandline-${FLYWAY_VERSION}.tar.gz \
  && mv flyway-${FLYWAY_VERSION}/* . \
  && rm flyway-commandline-${FLYWAY_VERSION}.tar.gz \
  && sed -i 's/bash/sh/' /flyway/flyway \
  && ln -s /flyway/flyway /usr/local/bin/flyway

COPY migrations/sql /sql
COPY migrations/flyway.conf /flyway/conf/flyway.conf

ENTRYPOINT ["flyway"]
CMD ["migrate"]
