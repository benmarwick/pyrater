<!-- README.md is generated from README.Rmd. Please edit that file -->
pyrater
=======

[![Last-changedate](https://img.shields.io/badge/last%20change-2017--06--10-brightgreen.svg)](https://github.com/benmarwick/mjbtramp/commits/master) [![minimal R version](https://img.shields.io/badge/R%3E%3D-3.4.0-brightgreen.svg)](https://cran.r-project.org/) [![Licence](https://img.shields.io/github/license/mashape/apistatus.svg)](http://choosealicense.com/licenses/mit/) [![Travis-CI Build Status](https://travis-ci.org/benmarwick/pyrater.png?branch=master)](https://travis-ci.org/benmarwick/pyrater) [![ORCiD](https://img.shields.io/badge/ORCiD-0000--0001--7879--4531-green.svg)](http://orcid.org/0000-0001-7879-4531)

The goal of pyrater is to give R users access to Daniele Silvestro's [PyRate](https://github.com/dsilvestro/PyRate), a Python program to estimate speciation, extinction, and preservation rates from fossil occurrence data using a Bayesian framework.

How to install
--------------

You can install this package by running this line:

``` r
devtools::install_github("benmarwick/pyrater")
```

Prerequisites
-------------

You need to have Python 2.7 installed on your machine to use the functions in PyRate. You can download Python 2.7 from <https://www.python.org/download/releases/2.7/>

Relevant publications
---------------------

Gjesfjeld, E., Chang, J., Silvestro, D., Kelty, C., & Alfaro, M. (2016). Competition and extinction explain the evolution of diversity in American automobiles. PALGRAVE COMMUNICATIONS 2, Article number: 16019 (2016) <doi:10.1057/palcomms.2016.19>

Silvestro, D., Schnitzler, J., Liow, L.H., Antonelli, A. and Salamin, N. (2014) Bayesian Estimation of Speciation and Extinction from Incomplete Fossil Occurrence Data. Systematic Biology, 63, 349-367.

Silvestro, D., Salamin, N., Schnitzler, J. (2014) PyRate: A new program to estimate speciation and extinction rates from incomplete fossil record. Methods in Ecology and Evolution, 5, 1126-1131.

Silvestro D., Cascales-Minana B., Bacon C. D., Antonelli A. (2015) Revisiting the origin and diversification of vascular plants through a comprehensive Bayesian analysis of the fossil record. New Phytologist, <doi:10.1111/nph.13247>.
