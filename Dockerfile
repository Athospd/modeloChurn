FROM rstudio/plumber

RUN git clone https://github.com/curso-r/modeloChurn /tmp/api/
WORKDIR /tmp/api/

RUN apt-get update -qq && apt-get install -y \
  git-core \
  libssl-dev \
  curl \
  libsodium-dev \
  libxml2-dev \
  libglpk-dev \
  libxml2-dev

RUN Rscript -e "install.packages('tidymodels')"
RUN Rscript -e "install.packages('randomForest')"

CMD ["/tmp/api/inst/apiModelo/plumber.R"]
