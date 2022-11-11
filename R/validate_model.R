# Preliminaries -------------------------------------------------------------------------------
## Rescale and recode
d <- d %>% 
  dplyr::mutate(
    event = as.integer(event) - 1,
    rej_any = ifelse(aantal_rejecties==0, 0, 1),
    hs = ifelse(recipient_age > 40, 1, 0),
    prog_index = 0.027 * donor_age - 0.01126 * graft_survival + 0.336 * aantal_rejecties
)

# Models ------------------------------------------------------------------

load(file="R/fgr_model_original.RData", .GlobalEnv)
load(file="R/fgr_model_recalibrated.RData", .GlobalEnv)
load(file="R/fgr_model_final.RData", .GlobalEnv)
load(file="R/fgr_model_donorage.RData", .GlobalEnv)


# Model performance in the validation data ----------------------------------------------------


## Original model ------------------------------------------------------------------------------

times <- c(6, 12, 18, 24, 30, 36)
### get score functions ----
score_model_original <- riskRegression::Score(list(fgr_model_original),
                      formula=Hist(time_event, as.integer(event)) ~ 1,
                      cause=1,
                      metrics="auc",
                      plot="calibration",
                      data=d, 
                      se.fit=0L,
                      conf.int=TRUE,
                      times = times)

### plot AUC ----
ggsave(plot=plot_auc(score_model_original),
      filename="plots/AUC-original-validation-data.tif", 
       device="tiff",
       width=15, 
       height=12,
       units="cm",
       dpi=300)
write.csv2(score_model_original$AUC$score, file="output/AUC-original-validation-data.csv")
### Plot calibration original model ---- 
ggsave(plot=plot_calibration(score_model_original), 
       filename="plots/calibration-original-validation-data.tif", 
       device="tiff",
       width=15, 
       height=12,
       units="cm",
       dpi=300)


## Performance of recalibrated model -----------------------------------------------------------

### score function ----
score_model_recalibrated <- riskRegression::Score(list(fgr_model_recalibrated),
                                                  formula=Hist(time_event, as.integer(event)) ~ 1,
                                                  cause=1,
                                                  metrics="auc",
                                                  plot="calibration",
                                                  data=d, 
                                                  se.fit=0L,
                                                  conf.int=TRUE,
                                                  times = times)
### plot ROC auc -----
ggsave(plot=plot_auc(score_model_recalibrated),
       filename="plots/AUC-recalibrated-validation-data.tif", 
       device="tiff",
       width=15, 
       height=12,
       units="cm",
       dpi=300)
write.csv2(score_model_recalibrated$AUC$score, file="output/AUC-recalibrated-validation-data.csv")
### Plot calibration original model ---- 
ggsave(plot=plot_calibration(score_model_recalibrated),
       filename="plots/calibration-recalibrated-validation-data.tif", 
       device="tiff",
       width=15, 
       height=12,
       units="cm",
       dpi=300)

## Model performance in the re-trained model ---------------------------------------------------
### score-function ----
fgr_train_score <- riskRegression::Score(list(fgr_model_final),
                                         formula=Hist(time_event, as.integer(event)) ~ 1,
                                         cause=1,
                                         metrics="auc",
                                         plot="calibration",
                                         data=d, 
                                         se.fit=0L,
                                         conf.int=TRUE,
                                         times = times)
### plot ROC auc -----
ggsave(plot=plot_auc(fgr_train_score),
       filename="plots/AUC-retrained-validation-data.tif", 
       device="tiff",
       width=15, 
       height=12,
       units="cm",
       dpi=300)
write.csv2(fgr_train_score$AUC$score, file="output/AUC-retrained-validation-data.csv")
### Plot calibration original model ---- 
ggsave(plot=plot_calibration(fgr_train_score),
       filename="plots/calibration-retrained-validation-data.tif", 
       device="tiff",
       width=15, 
       height=12,
       units="cm",
       dpi=300)

## Model performance of the donor age model ----
### score-function ----
fgr_donorage_score <- riskRegression::Score(list(fgr_model_donorage),
                                         formula=Hist(time_event, as.integer(event)) ~ 1,
                                         cause=1,
                                         metrics="auc",
                                         plot="calibration",
                                         data=d, 
                                         se.fit=0L,
                                         conf.int=TRUE,
                                         times = times)
### plot ROC auc -----
ggsave(plot=plot_auc(fgr_donorage_score),
       filename="plots/AUC-donorage-validation-data.tif", 
       device="tiff",
       width=15, 
       height=12,
       units="cm",
       dpi=300)
write.csv2(fgr_donorage_score$AUC$score, file="output/AUC-donorage-validation-data.csv")
### Plot calibration original model ---- 
ggsave(plot=plot_calibration(fgr_donorage_score),
       filename="plots/calibration-donorage-validation-data.tif", 
       device="tiff",
       width=15, 
       height=12,
       units="cm",
       dpi=300)


# ---- Export data
write.csv2(d, "data/tect-data.csv")


# ---- sessionInfo

writeLines(capture.output(sessionInfo()), "output/sessioninfo.txt")
