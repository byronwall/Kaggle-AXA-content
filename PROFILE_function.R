library("lineprof")
library("shiny")
library("Rcpp")
library("httpuv")


source("ANALYZE logistic regression.R")
l = lineprof(do_randomForest(1003))
shine(l)