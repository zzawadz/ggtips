FROM rocker/verse
COPY . /mnt/vol
RUN R "install.packages(c('shiy'), dependencies = TRUE)"
RUN R CMD INSTALL /mnt/vol
CMD ["R", "-e", "TestApp::run()"]