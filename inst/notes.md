

# laser package no longer on CRAN, so we can get it like this:
devtools::install_github("cran/laser")

# input data looks like this:

> head(car_models)
             make           model    first_year  last_year
1        Allstate           A-230       1952      1953
2        Allstate           A-240       1952      1953
3      AM_General             DJ5       1969      1974
4      AM_General             FJ8       1973      1974
5      AM_General          Hummer       1992      2001
6 American_Austin American_Austin       1930      1935


> head(pottery_data)
    Ware             most_recent_age..BP. oldest_age..BP. trait
1 Brown_Ware_A                   66             265         1
2 Brown_Ware_A                   66             265         5
3 Brown_Ware_A                   66             265         3
4 Brown_Ware_A                 1130            1265         5
5 Brown_Ware_A                 1130            1265         1
6 Brown_Ware_A                 1165            1365         2

# edit the lineage_pyrate
#- don't do global assignment
#- don't write to csv
#- don't use fixed paths

# couldn't find preservation_pyrate_data_1BD_marginal.rates.log
# couln't find PDF files for plots

# output plotting functions have backslash problems

library(reticulate)
use_python("C:/python27/")
py_available(initialize = TRUE)
py_available()
