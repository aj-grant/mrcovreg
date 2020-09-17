FROM rocker/tidyverse:3.6.3
COPY . /app
WORKDIR /app

RUN R -e "install.packages('rjson')"
RUN R -e "install.packages('MendelianRandomization')"
RUN R -e "devtools::install_github('phenoscanner/phenoscanner')"
RUN R -e "devtools::install_local('/app')"
