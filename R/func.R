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


#' Plot AUC
#' 
#' Plot the area under the received characteristics curve over time
#' 
#' @param score the score object created with get_score
#' 
#' @returns a ggplot object
plot_auc <- function(score) {
  plot <- ggplot(data=score$AUC$score, aes(x=times, y=AUC)) +
    geom_line() +
    geom_hline(aes(yintercept=0.5),
               color="gray62",
               lty=2) +
    theme_classic() +
    labs(title="") +
    xlab("Follow-up time (months)") + 
    scale_x_continuous(limits=c(0, 36),
                       breaks=seq(0, 36, 6)) +
    ylab("Area under the ROC curve") + 
    scale_y_continuous(limits=c(0.0, 1),
                       breaks=seq(0.0, 1, 0.2)) +
    theme(axis.title  = element_text(size = 16),
          axis.text = element_text(size = 12))
  
}

#' Plot calibration
#' 
#' Plot the calibration curves for all times in plot
#' 
#' @param score is score object created with get_score
#' @param ... optional args to overwrite defaults in ggplot.
plot_calibration <- function(score, ...) {
  plt_data <- score$Calibration$plotframe
  plt_data <- plt_data %>% 
    dplyr::mutate(times = as.factor(times))
  ggplot(plt_data, aes(x=risk, y=pseudovalue, color=times)) +
    geom_smooth(method="loess",
                span=1.0) +
    colorspace::scale_color_discrete_qualitative(palette="Dark 3") +
    theme_classic() + 
    labs(caption=paste(
      "Calibration between predicted and observed risk"
    )) +
    ylab("Observed Risk") + 
    xlab("Predicted Risk") +
    scale_y_continuous(limits=c(0, 1),
                       breaks=seq(0, 1, 0.1)) +
    scale_x_continuous(limits=c(0, 1),
                       breaks=seq(0, 1, 0.1)) +
    geom_abline(color="gray62",
                lty=2) +
    theme(axis.title  = element_text(size = 16),
          axis.text = element_text(size = 12)) +
    coord_cartesian(xlim=c(0, 1),
                    ylim=c(0, 1))

}
