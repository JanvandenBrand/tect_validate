
# recode the event and rescale variables
d <- d %>% 
  dplyr::mutate(
    event = as.integer(event) - 1,
    donor_age = ifelse(
      is.na(donor_age),
      round(median(donor_age, na.rm=TRUE)),
      donor_age
    )
  )


# Original model ------------------------------------------------------------------------------

fgr_model_original <- FGR(Hist(time_event, as.integer(event)) ~ donor_age + graft_survival + aantal_rejecties,
                          init=c(0.027, -0.011, 0.336),
                          cause=1,
                          maxiter=0,
                          data=d)



# Adjust calibration in the large -------------------------------------------------------------
d <- d %>% 
  dplyr::mutate(
    prog_index = 0.027 * donor_age - 0.011 * graft_survival + 0.336 * aantal_rejecties
  )

fgr_model_recalibrated <- FGR(Hist(time_event, event) ~ prog_index,
                              cause=1,
                              data=d)


# Re-train the model with recipient age ----
## check non linear dose response relations  ----
# cause specific cox for graft intolerance
d <- d %>% 
  dplyr::mutate(
    gi = as.integer(as.integer(event) == 1)
  )

gam_age <- gam(time_event ~ s(recipient_age) + graft_survival + aantal_rejecties,
               data=d,
               family=cox.ph(),
               weights=gi
               )
tiff(filename="plots/gam_age.tiff")
plot(gam_age)
dev.off()


# possible non-linear effect for age after 40 years

mfp(Surv(time_event, gi) ~ fp(recipient_age, df=4) + graft_survival + aantal_rejecties,
    family=cox,
    data=d
    )
# suggests linear

gam_graftsurv <- gam(time_event ~ recipient_age + s(graft_survival) + aantal_rejecties,
                     data=d,
                     family=cox.ph(),
                     weights=gi
                    )
tiff(filename="plots/gam_graftsurv.tiff")
plot(gam_graftsurv)
dev.off()
# linear

# create heaviside function for age
# stack rejection into any vs none
d <- d %>% 
  dplyr::mutate(
    hs = ifelse(recipient_age > 40, 1, 0),
    rej_any = ifelse(aantal_rejecties==0, 0, 1)
  )


## ---- fgr-model -----
# fit the model
fgr_model_base <- FGR(Hist(time_event, as.integer(event)) ~ recipient_age + graft_survival + aantal_rejecties,
                 cause=1,
                 data=d)
fgr_model_base_summary <- summary(fgr_model_base)

# heavside function for age
fgr_model_hs <- FGR(Hist(time_event, as.integer(event)) ~ I(hs*recipient_age) + graft_survival + aantal_rejecties,
                 cause=1,
                 data=d)
fgr_model_hs_summary <- summary(fgr_model_hs)

# Most parsimonious model after retaining on original data
fgr_model_final <- FGR(Hist(time_event, as.integer(event)) ~ I(hs*recipient_age) + graft_survival + rej_any,
                 cause=1,
                 data=d)
fgr_model_final_summary <- summary(fgr_model_final)
# Model with age <40 and donorage
fgr_model_donorage <- FGR(Hist(time_event, as.integer(event)) ~ I(hs*recipient_age) + graft_survival + rej_any + donor_age,
                          cause=1,
                          data=d)
fgr_model_donorage_summary <- summary(fgr_model_donorage)

result_table <- list(
  base_model = list("loglik" = fgr_model_base_summary$loglik, "subHR" = fgr_model_base_summary$conf.int[,c(1, 3, 4)]),
  heaviside_model = list("loglik" = fgr_model_hs_summary$loglik, "subHR" = fgr_model_hs_summary$conf.int[,c(1, 3, 4)]),
  fgr_model_final = list("loglik" = fgr_model_final_summary$loglik, "subHR" = fgr_model_final_summary$conf.int[,c(1, 3, 4)]),
  fgr_model_donorage = list("loglik" = fgr_model_donorage_summary$loglik, "subHR" = fgr_model_donorage_summary$conf.int[,c(1, 3, 4)])
)
result_table <- do.call(rbind, unlist(result_table, recursive=FALSE))
write.csv2(result_table,"output/retrained-models.csv")

# Save the models for predictions
save(fgr_model_original, file="R/fgr_model_original.RData")
save(fgr_model_recalibrated, file="R/fgr_model_recalibrated.RData")
save(fgr_model_final, file="R/fgr_model_final.RData")
save(fgr_model_donorage, file="R/fgr_model_donorage.RData")
