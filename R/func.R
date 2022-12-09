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


#' Plot Cumulative Incidence
#' 
#' Plot the cumulative incidence of the competing outcome
#' 
#' @param data the dataframe used to obtain cuminc
#' @param ci_estimate a ci object returned by cmprsk::cuminc
#' @param censored a character value with the label for the censored category
#' 
#' @returns a ggplot object
get_ci_plot <- function(data, ci_estimate, censored) {
  ci <- lapply(ci_estimate, as.data.frame)
  for (i in seq_along(ci)) {
    ci[[i]] <- cbind(ci[[i]], levels(data$event)[i])
  }
  ci <- bind_rows(ci)
  ci <- ci %>%  rename(outcome = `levels(data$event)[i]`)
  ci_plot <- ggplot(data = ci %>% dplyr::filter(outcome != censored), 
                    aes(x=time, y=est, color=outcome)) + 
    geom_step() +
    colorspace::scale_color_discrete_qualitative(palette="Dark 3") +
    theme_classic() +
    ggtitle("") +
    xlab("Follow-up time (months)") + 
    scale_x_continuous(breaks=seq(0, 120, 12)) +
    ylab("Cumulative incidence") + 
    scale_y_continuous(limits=c(0, 1),
                       breaks=seq(0, 1, 0.2)) +
    theme(legend.position=c(0.8, 0.9),
          axis.title=element_text(size=16),
          axis.text=element_text(size=12)) +
    coord_cartesian(xlim=c(0, 120))
}

#' Plot Stacked Cumulative Incidence
#' 
#' Plot stacked areas of cumulative incidence of the competing outcomes
#' 
#' @param ci_estimate a ci object returned by cmprsk::cuminc
#' @param start 1L (default=0) an integer value to indicate the start time for the cuminc estimates
#' @param end 1L (default=12) an integer value to indicate the end time for the cuminc estimates
#' @param step 1L (default=1) an integer value to indicate the step size for the cuminc estimates
#' @param x_interval 1L (default=3) an integer value to indicate the major ticks on the time axis
#' @param outcome_order character vector with the ordering of the outcome labels
#' 
#' @returns a ggplot object
get_stacked_cuminc_plot <- function(ci_estimate, start=0, end=12, step=1, x_interval=3, outcome_order) {
  # obtain estimated cuminc at fixed time points for all competing events so that we have a data point for every time
  ci_estimates_fixed <- timepoints(
    ci_estimate, 
    times=seq(start, end, step)
  )$est
  # wrangle the data in long format for easier plotting
  ci_estimates_fixed <- t(ci_estimates_fixed) %>% 
    as.data.frame() %>%
    mutate(time = seq(start,end,step))
  names(ci_estimates_fixed) <- str_remove_all(names(ci_estimates_fixed), "1 ")
  ci_estimates_fixed <- ci_estimates_fixed %>%  
    pivot_longer(!time, names_to="outcome", values_to="estimate")
  ci_estimates_fixed <- ci_estimates_fixed %>%
    mutate(
      outcome = forcats::fct_relevel(outcome,
                                     outcome_order)
    )
  # The actual plot
  stacked_ci_plot <- ggplot(data = ci_estimates_fixed, 
                            aes(x=time, y=estimate, fill=outcome)) + 
    geom_area() +
    # horizonal line at y=1
    geom_segment(aes(x=3, xend=end, y=1, yend=1), linetype=1, color="black") +
    # vertical line at x = end
    geom_segment(aes(x=end, xend=end, y=0, yend=1), linetype=1, color="black") +
    # vertical line at x = 3
    geom_segment(aes(x=3, xend=3, y=0, yend=1), linetype=1, color="black") +
    colorspace::scale_color_discrete_qualitative(palette="Dark 3") +
    theme_classic() +
    ggtitle("") +
    xlab("Follow-up time (months)") + 
    scale_x_continuous(breaks=seq(start, end, x_interval)) +
    ylab("Cumulative incidence") + 
    scale_y_continuous(limits=c(0, 1),
                       breaks=seq(0, 1, 0.2)) +
    theme(legend.position="bottom",
          axis.title=element_text(size=16),
          axis.text=element_text(size=12),
          legend.title=element_text(size=12),
          legend.text=element_text(size=10)) +
    guides(fill=guide_legend(
      title="Outcome",
      nrow=2)) +
    coord_cartesian(xlim=c(start, end)) 
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
