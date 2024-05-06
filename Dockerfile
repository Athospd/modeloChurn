FROM rstudio/plumber

RUN git clone https://github.com/azeloc/modeloChurn /tmp/api/
WORKDIR /tmp/api/

RUN Rscript -e "install.packages('tidymodels')"
RUN Rscript -e "install.packages('randomForest')"

CMD ["/tmp/api/modeloChurn/inst/apiModelo/plumber.R"]
