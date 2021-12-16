# Manage packages ----

#' Install required packages from CRAN
#' 
#' @param required_packages a character vector with the package names for required CRAN packages.
install_required_packages <- function(required_packages) {
  lapply(required_packages, function(package) {
    if (package %in% installed.packages()[,1] == FALSE) {
      install.packages(package)
    } 
  })    
}

req_etl <- c("readxl", "stringr", "tidyr", "dplyr") 
install_required_packages(req_etl)

req_eda <- c("tableone", "corrplot", "gridExtra", "scales", "ggplot2", "survival", "cmprsk","survminer") 
install_required_packages(req_eda)

req_model <- c("riskRegression", "xfun", "caret", "prodlim", "pec")
install_required_packages(req_model)

# Clean-up ----

rm(list = ls())
