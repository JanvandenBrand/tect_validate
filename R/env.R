# Manage packages ----
library(here)
req <- read.table(here("requirements.txt"))
req <- unlist(c(req))
  
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
install_required_packages(req)
lapply(req, library, character.only=TRUE)


# Clean-up ----

rm(list = ls())
