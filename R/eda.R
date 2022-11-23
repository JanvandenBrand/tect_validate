
# Import SPSS data ----------------------------------------------------------------------------

d <- read.spss("data/oorspronkelijke cohort.sav",
               use.value.labels=TRUE,
               to.data.frame=TRUE)

d <- d %>% 
  dplyr::mutate(
    event = forcats::fct_relevel(event, "end of follow up", after=0)
  )

d <- d %>% 
  dplyr::rename(
    graft_survival = graftsurv,
    recipient_age = age,
    aantal_rejecties = rej_total,
    time_event = fu
  )


# Transform data ----
# Check if factor levels are similar across all datasets

# Summarize data
var_label <- lapply(d, get_label)
class_label <- lapply(d, get_class)
var_summary <- lapply(d, get_summary)
codebook <- data.frame(variable = names(d),
           variable_label = unlist(var_label),
           class = unlist(class_label),
           summary = paste(var_summary)
)
write.table(codebook, file = "output/codebook.txt", sep = "\t", row.names=FALSE)

##  ---- eda-factor-plots 
factors <- names(class_label)[class_label=="factor"]
# factors <- c(factors, "event")
lapply(factors, function(variable) {
  ggplot(data=d, 
         aes(x=get(variable), fill=get(variable))) +
    geom_bar() +
    scale_fill_discrete(labels=function(x) stringr::str_trunc(x, width=24)) +
    labs(x="",
         y="") +
    guides(fill=guide_legend(title=paste(variable))) +
    theme_minimal() + 
    theme(axis.text.x=element_blank())
  ggsave(filename=str_c("plots/", variable, ".png"), device="png")
})

## ---- eda-continuous-plots 
continuous <- names(class_label)[class_label=="numeric"]
# continuous <- c(continuous[-which(continuous %in% "event")])
lapply(continuous, function(variable) {
  ggplot(data=d,
         aes(x=get(variable))) +
    geom_histogram(bins=10) +
    labs(y="",
         x=stringr::str_trunc(paste(variable), width=24)) +
    guides(fill=guide_legend(title=paste(variable))) +
    theme_minimal() 
  ggsave(filename=str_c("plots/", variable, ".png"), device="png")
})

## ---- correlations
correlations <- d %>% 
  dplyr::select(all_of(c(factors,continuous))) %>% 
  mutate(across(all_of(c(factors,continuous)), ~ as.numeric(.x)))
correlations <- stats::cor(correlations, use="pairwise.complete.obs")
corrplot(correlations,
         tl.pos="td",
         tl.cex=0.8,
         tl.col="black",
         type="upper",
         method="ellipse",
         diag=TRUE)
ggsave(filename="plots/corrplot.png", device="png")

## ---- table 1 
table1 <- CreateTableOne(data = d,
               factorVars=factors[-which(factors %in% "event")],
               strata="event")
table1 <- print(table1, printToggle=FALSE)

write.csv2(table1, file="output/table1.csv")

# Plot cumulative incidence curves ----
# Panel A: creat a plot where all transplantectomies are in a single category.
d_death <- d %>% 
  mutate(event = factor(
           case_when(
              event == "graft nephrectomy because other reason" ~ "nephrectomy",
              event == "graft nephrectomy because of graft intolerance" ~ "nephrectomy",
              event == "retransplantation with previous graft in situ" ~ "no nephrectomy",
              event == "death with graft in situ" ~ "death with graft in situ",
              event == "end of follow up" ~ "end of follow up"
              )
  )
)
ci_tect <- with(d_death, 
                cuminc(ftime=time_event,
                       fstatus=event,
                       cencode=2)
                )
ci_plot <- get_ci_plot(data=d_death, ci_estimate=ci_tect, censored="end of follow up")
ggsave(ci_plot,
       filename="plots/ci_curve_panel_A.tif", 
       device="tiff",
       width=15, 
       height=12,
       units="cm",
       dpi=300)


# Panel B: create a plot where all competing events are in a single category
d_tect <- d %>% 
  mutate(event = factor(
    case_when(
      event == "graft nephrectomy because other reason" ~ "graft nephrectomy because other reason",
      event == "graft nephrectomy because of graft intolerance" ~ "graft nephrectomy because of graft intolerance",
      event == "retransplantation with previous graft in situ" ~ "competing event",
      event == "death with graft in situ" ~ "competing event",
      event == "end of follow up" ~ "end of follow up"
    )
  )
)
ci_tect <- with(d_tect, 
                cuminc(ftime=time_event,
                       fstatus=event,
                       cencode=2)
)
ci_plot <- get_ci_plot(data=d_tect, ci_estimate=ci_tect, censored="end of follow up")
ggsave(ci_plot,
       filename="plots/ci_curve_panel_B.tif", 
       device="tiff",
       width=15, 
       height=12,
       units="cm",
       dpi=300)
# stacked cumulative incidence plot
ci_estimate <- with(d,
                    cuminc(ftime=time_event,
                           fstatus=event,
                           cencode=1)
)
stacked_cuminc <- get_stacked_cuminc_plot(ci_estimate, start=0, end=36, step=1) 
ggsave(stacked_cuminc,
       filename="plots/stacked_cuminc.tif", 
       device="tiff",
       width=15, 
       height=12,
       units="cm",
       dpi=300)


# get a risk table
km <- survfit(Surv(time=time_event, event=event) ~ 1, data=d)
risktable <- data.frame(
  time = km$time,
  n = km$n.risk[,1]
)
# select times closest to seq(0,120,12)
list_risk_table <- vector("list", length=length(seq(0, 120, 12)))
for (t in c(3, seq(12, 120, 12))) {
  list_risk_table[[t]] <- risktable[which.min(abs(risktable$time-t)),]
}
risk_table <- bind_rows(list_risk_table)
risk_table <- risk_table %>% mutate(
  time = round(time)
)
rt <- t(as.matrix(risk_table$n))
colnames(rt) <- round(risk_table$time)
rt <- tableGrob(rt, 
                theme = ttheme_minimal(base_size = 10, 
                                       rowhead = list(hjust=0, x=0.3)
                ))
rt$widths <- unit(rep(1/(ncol(rt)+1), ncol(rt)), "npc")
write.csv2(risk_table, file="output/risk_table.csv")


# clean up
rm(list = setdiff(ls(),c("d", "ci_plot", "risk_table", "factors", "continuous", "table1")))

save(d, file="data/training-data.Rdata")
