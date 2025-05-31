FROM rstudio/plumber

RUN git clone https://github.com/azeloc/modeloChurn /home/api/
WORKDIR /home/api/

RUN Rscript -e "install.packages('tidymodels')"
RUN Rscript -e "install.packages('devtools')"
RUN Rscript -e "install.packages('glmnet')"

EXPOSE 8080

CMD ["Rscript", "-e", "r <- plumb('/home/api/inst/apiModelo/plumber.R'); r$run(host = '0.0.0.0', port = 8080)"]
