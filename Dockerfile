FROM rocker/verse
COPY . /mnt/vol
RUN R "install.packages(c('shiy'), dependencies = TRUE)"
RUN R -e "devtools::install('/mnt/vol', dependencies = TRUE)"
CMD ["R", "-e", "TestApp::run()"]
