FROM rocker/r-ver:3.4.0
LABEL maintainer="bmarwick"
RUN export DEBIAN_FRONTEND=noninteractive; apt-get -y update \
 && apt-get install -y pandoc \
	pandoc-citeproc
RUN ["install2.r", "-r 'https://cloud.r-project.org'", "Rcpp", "digest", "withr", "rprojroot", "futile.options", "backports", "magrittr", "evaluate", "stringi", "futile.logger", "fortunes", "rmarkdown", "devtools", "lambda.r", "stringr", "yaml", "memoise", "htmltools", "knitr"]
WORKDIR /payload/
CMD ["R"]
