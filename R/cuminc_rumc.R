

load("data/validation-data.RData")
d_validatie <- d
load("data/training-data.RData")

# ---- configurations ---

max_follow_up <- 60
breaks <- 6 

# ---- code starts here ---- 

d <- d %>% 
  dplyr::select(time_event, event, dec)
d <- d %>%
  mutate(center = "rumc")

d_validatie <- d_validatie %>%
  dplyr::select(time_event, event, dec, center)

d <- d %>% 
  tibble::add_row(d_validatie)

d <- d %>% 
  dplyr::filter(center=="rumc")

d <- d %>%
  dplyr::mutate(dec = as.factor(dec))

ci_tect <- with(d, 
                cuminc(ftime=time_event,
                       group=dec,
                       fstatus=event,
                       cencode=0)
)
# cast into a data.frame
ci <- lapply(ci_tect, as.data.frame)
for (i in c(6:10)) {
  ci[[i]] <- cbind(ci[[i]], levels(d$dec)[i-5])
}
ci <- bind_rows(ci[6:10]) 
ci <- ci %>% 
  rename(dec = `levels(d$dec)[i - 5]`)
ci_plot <- ggplot(data = ci, 
                  aes(x=time, y=est, color=dec)) + 
  geom_step() +
  colorspace::scale_color_discrete_qualitative(palette="Dark 3") +
  theme_classic() +
  ggtitle("") +
  xlab("Follow-up time (months)") + 
  scale_x_continuous(breaks=seq(0, max_follow_up, breaks)) +
  ylab("Cumulative incidence") + 
  scale_y_continuous(limits=c(0, 1),
                     breaks=seq(0, 1, 0.2)) +
  theme(legend.position=c(0.8, 0.9),
        axis.title=element_text(size=16),
        axis.text=element_text(size=12)) +
  coord_cartesian(xlim=c(0, max_follow_up))
# get a risk table
km <- survfit(Surv(time=time_event, event=event) ~ 1, data=d)
risktable <- data.frame(
  time = km$time,
  n = km$n.risk[,1]
)
# select times closest to seq(0,max_follow_up,breaks)
list_risk_table <- vector("list", length=length(seq(0, max_follow_up, breaks)))
for (t in c(3, seq(breaks, max_follow_up, breaks))) {
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

ggsave(ci_plot,
       filename="plots/ci_curve_rumc.tif", 
       device="tiff",
       width=15, 
       height=12,
       units="cm",
       dpi=300)

