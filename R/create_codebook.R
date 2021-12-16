# Create codebook --------
#' Get variable labels
#' 
#' Check is variable labels exist in attributes and assign them to a vector
#' 
#' @param d A data.frame
#' 
#' @returns varlabel: a vector with the label attribute
get_label <- function(d) {
  varlabel <- attributes(d)$label
  if (!is.null(varlabel)) {
    varlabel
  } else {
    ""
  }
}  
#' Get the class
#' 
#' Check if class labels exist and assign them to a vector
#' 
#' @param d A data.frame
#' 
#' @returns classlabel: a vector with the class attribute

get_class <- function(d) {
  classlabel <- class(d)
  if (!is.null(classlabel)) {
    classlabel
  } else {
    ""
  }
}

#' Get summary
#' 
#' Get the summary for each variable in the data and return it as a character vector
#' 
#' @param d A data.frame
#' 
#' @returns A character vector with the summary statistics for d
get_summary <- function(d) {
  varsummary <- summary(d)
  if (!is.null(varsummary)) {
    varsummary
  } else {
    ""
  }
}

