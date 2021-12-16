# Preliminaries -------------------------------------------------------------------------------

req <- c("gridExtra", "scales", "ggplot2", "survival", "cmprsk", "prodlim", "riskRegression",
         "pec") 
lapply(req, library, character.only=TRUE)
rm(req)
source("R/eda.R")

# Performance original model ------------------------------------------------------------------
times <- c(6, 12, 18, 24, 36, 60, 120)
d <- d %>% 
  dplyr::mutate(event = as.integer(event)-1)

## ---- fgr-model
fgr_model <- FGR(Hist(time_event, as.integer(event)) ~ donor_age + graft_survival + aantal_rejecties,
                 cause=1,
                 init=c(0.027, 0.011, 0.336),
                 maxiter=0,
                 data=d)

## ---- score function
fgr_model_score <- riskRegression::Score(list(fgr_model),
        formula=Hist(time_event, as.integer(event)) ~ 1,
        cause=1,
        metrics="auc",
        plot="calibration",
        data=d, 
        se.fit=0L,
        conf.int=TRUE,
        times = times)

# ---- plot ROC auc
ggplot(data=fgr_model_score$AUC$score, aes(x=times, y=AUC)) +
  geom_line() +
  geom_hline(aes(yintercept=0.5),
             color="gray62",
             lty=2) +
  theme_classic() +
  labs(title="") +
  xlab("Follow-up time (months)") + 
  scale_x_continuous(limits=c(0, 60),
                     breaks=seq(0, 60, 12)) +
  ylab("Area under the ROC curve") + 
  scale_y_continuous(limits=c(0.0, 1),
                     breaks=seq(0.0, 1, 0.2)) +
  theme(axis.title  = element_text(size = 16),
        axis.text = element_text(size = 12))
ggsave(filename="plots/AUC-orginal-model.tif", 
       device="tiff",
       width=15, 
       height=12,
       units="cm",
       dpi=300)

## ---- Plot calibration original model
plt_data <- fgr_model_score$Calibration$plotframe
plt_data <- plt_data %>% 
  dplyr::mutate(times = as.factor(times))
ggplot(plt_data, aes(x=risk, y=pseudovalue, color=times)) +
  geom_smooth(method="loess",
              span=0.5) +
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
  coord_cartesian(xlim=c(0, 0.4),
                  ylim=c(0, 0.4))
ggsave(filename="plots/calibration-orginal-model.tif", 
       device="tiff",
       width=15, 
       height=12,
       units="cm",
       dpi=300)


# Refit the model ----
## ---- adjust calibration in the large
d <- d %>% mutate(
  prog_index = 0.027 * donor_age + 0.011 * graft_survival + 0.336 * aantal_rejecties
)

fgr_model_refit <- FGR(Hist(time_event, event) ~ prog_index,
                       cause=1,
                       data=d)

# refitted model performance ------
fgr_model_refit_score <- riskRegression::Score(list(fgr_model_refit),
                                         formula=Hist(time_event, event) ~ 1,
                                         cause=1,
                                         metrics="auc",
                                         plot="calibration",
                                         data=d, 
                                         se.fit=1L,
                                         conf.int=FALSE,
                                         times = times)

## ---- Plot ROC-AUC
ggplot(data=fgr_model_refit_score$AUC$score, aes(x=times, y=AUC)) +
  geom_line() +
  geom_hline(aes(yintercept=0.5),
             color="gray62",
             lty=2) +
  theme_classic() +
  labs(title="") +
  xlab("Follow-up time (months)") + 
  scale_x_continuous(limits=c(0, 60),
                     breaks=seq(0, 60, 12)) +
  ylab("Area under the ROC curve") + 
  scale_y_continuous(limits=c(0.0, 1),
                     breaks=seq(0.0, 1, 0.2)) +
  theme(axis.title  = element_text(size = 16),
        axis.text = element_text(size = 12))
ggsave(filename="plots/AUC-recalibrated-model.tif", 
       device="tiff",
       width=15, 
       height=12,
       units="cm",
       dpi=300)

## ---- Plot calibration
plt_data <- fgr_model_refit_score$Calibration$plotframe
plt_data <- plt_data %>% 
  dplyr::mutate(times = as.factor(times))
ggplot(plt_data, aes(x=risk, y=pseudovalue, color=times)) +
  geom_smooth(method="loess",
              span=0.5) +
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
  coord_cartesian(xlim=c(0, 0.2),
                  ylim=c(0, 0.2))
ggsave(filename="plots/calibration-recalibrated-model.tif", 
       device="tiff",
       width=15, 
       height=12,
       units="cm",
       dpi=300)


# ---- sessionInfo


writeLines(capture.output(sessionInfo()), "output/sessioninfo.txt")
