

# Models ------------------------------------------------------------

# Original model
summary(fgr_model_original)

# Recalibrated model
summary(fgr_model_recalibrated)

# Retrained model
summary(fgr_model_final)

# Model performance orginal model -----------------------------------------------------------
times <- c(6, 12, 18, 24, 30, 36)

## score function ----
score_model_original <- riskRegression::Score(list(fgr_model_original),
                      formula=Hist(time_event, as.integer(event)) ~ 1,
                      cause=1,
                      metrics="auc",
                      plot="calibration",
                      data=d, 
                      se.fit=0L,
                      conf.int=TRUE,
                      times = times)

## plot ROC auc -----
ggsave(plot=plot_auc(score_model_original),
       filename="plots/AUC-original-training-data.tif", 
       device="tiff",
       width=15, 
       height=12,
       units="cm",
       dpi=300)
write.csv2(score_model_original$AUC$score, file="output/AUC-orginal-training-data.csv")
## Plot calibration original model ---- 
ggsave(plot=plot_calibration(score_model_original), 
       filename="plots/calibration-original-training-data.tif", 
       device="tiff",
       width=15, 
       height=12,
       units="cm",
       dpi=300)


# Performance of recalibrated model -----------------------------------------------------------

## score function ----
score_model_recalibrated <- riskRegression::Score(list(fgr_model_recalibrated),
                      formula=Hist(time_event, as.integer(event)) ~ 1,
                      cause=1,
                      metrics="auc",
                      plot="calibration",
                      data=d, 
                      se.fit=0L,
                      conf.int=TRUE,
                      times = times)

## plot ROC auc -----
ggsave(plot=plot_auc(score_model_recalibrated),
       filename="plots/AUC-recalibrated-training-data.tif", 
       device="tiff",
       width=15, 
       height=12,
       units="cm",
       dpi=300)
write.csv2(score_model_recalibrated$AUC$score, file="output/AUC-recalibrated-training-data.csv")
## Plot calibration original model ---- 
ggsave(plot=plot_calibration(score_model_recalibrated),
       filename="plots/calibration-recalibrated-training-data.tif", 
       device="tiff",
       width=15, 
       height=12,
       units="cm",
       dpi=300)

# Model performance in the re-trained model ---------------------------------------------------
fgr_train_score <- riskRegression::Score(list(fgr_model_final),
                      formula=Hist(time_event, as.integer(event)) ~ 1,
                      cause=1,
                      metrics="auc",
                      plot="calibration",
                      data=d, 
                      se.fit=0L,
                      conf.int=TRUE,
                      times = times)

## plot ROC auc -----
ggsave(plot=plot_auc(fgr_train_score),
       filename="plots/AUC-retrained-training-data.tif", 
       device="tiff",
       width=15, 
       height=12,
       units="cm",
       dpi=300)

write.csv2(fgr_train_score$AUC$score, file="output/AUC-retrained-training-data.csv")
## Plot calibration original model ---- 
ggsave(plot_calibration(fgr_train_score),
       filename="plots/calibration-retrained-training-data.tif", 
       device="tiff",
       width=15, 
       height=12,
       units="cm",
       dpi=300)
