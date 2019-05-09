FROM openanalytics/r-base:latest

MAINTAINER Pawel Piatkowski <pawel.piatkowski@roche.com>

RUN apt-get update && apt-get install -y libssl-dev libxml2-dev libcurl4-openssl-dev libcairo2-dev

COPY . /tmp
RUN R -e 'install.packages("devtools", dependencies = TRUE)'
RUN R -e 'devtools::install_deps("/tmp", dependencies = TRUE)'
RUN R CMD INSTALL /tmp

EXPOSE 3838
CMD ["R", "-e", "ggtips::demo()"]
