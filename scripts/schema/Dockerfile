FROM openjdk:8-jre

RUN apt-get update && apt-get install -y \
        ca-certificates \
        graphviz \
        wget


ENV SCHEMASPY_VERSION 6.0.0-rc2
ENV DRIVER_VERSION 42.1.4

WORKDIR /schemaspy

RUN wget -O schemaspy.jar         https://github.com/schemaspy/schemaspy/releases/download/v$SCHEMASPY_VERSION/schemaspy-$SCHEMASPY_VERSION.jar
RUN wget -O postgresql_driver.jar https://jdbc.postgresql.org/download/postgresql-$DRIVER_VERSION.jar

ENTRYPOINT ["java", "-jar", "schemaspy.jar", \
    "-t", "pgsql", \
    "-dp", "postgresql_driver.jar", \
    "-hq"]
