FROM rstudio/plumber

RUN git clone https://github.com/azeloc/modeloChurn /home/api/
WORKDIR /home/api/

RUN Rscript -e "install.packages('tidymodels')"
RUN Rscript -e "install.packages('devtools')"
RUN Rscript -e "install.packages('glmnet')"

CMD ["/home/api/inst/apiModelo/plumber.R"]
