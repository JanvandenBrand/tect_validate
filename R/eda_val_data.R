

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
write.table(codebook, file = "output/codebook_validation.txt", sep = "\t", row.names=FALSE)

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
ggsave(filename="plots/corrplot_validate.png", device="png")

## ---- table 1 
table1 <- CreateTableOne(data = d %>% dplyr::select(-et_recipient_number),
               factorVars=factors[-which(factors %in% "event")],
               strata="event")
table1 <- print(table1, printToggle=FALSE)
write.csv2(table1, file="output/table1_validation.csv")

# Plot cumulative incidence curves ----
## ---- ci plot
ci_tect <- with(d, 
                cuminc(ftime=time_event,
                       fstatus=event,
                       cencode=0)
                )
# cast into a data.frame
ci <- lapply(ci_tect, as.data.frame)
for (i in seq_along(ci)) {
  ci[[i]] <- cbind(ci[[i]], levels(d$event)[i])
}
ci <- bind_rows(ci)
ci <- ci %>%  rename(outcome = `levels(d$event)[i]`)
ci_plot <- ggplot(data = ci %>% dplyr::filter(outcome != "censored"), 
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
# gridExtra::grid.arrange(ci_plot, rt)

ci_plot
ggsave(filename="plots/ci_curve_validation.tif", 
       device="tiff",
       width=15, 
       height=12,
       units="cm",
       dpi=300)


write.csv2(risk_table, file="output/risk_table_validation.csv")

# clean up
rm(list = setdiff(ls(),c("d", "ci_plot", "risk_table", "factors", "continuous", "table1")))


