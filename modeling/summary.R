library(tidyverse)
library(brms)
library(MASS)
library(ggthemes)

options(mc.cores = parallel::detectCores())
parallel:::setDefaultClusterOptions(setup_strategy = "sequential")
options(contrasts = c("contr.sdif","contr.sdif"))

setwd("/Users/adkinsty/Box/side_projects/covid/Forecasting/")

data <- read_csv("data/prepped/all_filtered_data.csv") %>%
  mutate(day = factor(day,levels = c(1,2,3)))

# FORECASTS
# Group effect summary
data %>% filter(exp==1) %>%
  group_by(group) %>% summarise(est = mean(est)) %>% ungroup() %>%
  pivot_wider(names_from=group,values_from = est) %>% mutate(ta - g)
data %>% filter(exp==2) %>%
  group_by(group) %>% summarise(est = mean(est)) %>% ungroup() %>%
  pivot_wider(names_from=group,values_from = est) %>% mutate(ta - g)
data %>% filter(exp==3.1) %>%
  group_by(group) %>% summarise(est = mean(est)) %>% ungroup() %>%
  pivot_wider(names_from=group,values_from = est) %>% mutate(ta - g)
data %>% filter(exp==3.2) %>%
  group_by(group) %>% summarise(est = mean(est)) %>% ungroup() %>%
  pivot_wider(names_from=group,values_from = est) %>% mutate(ta - g)

# day summary
data %>% 
  mutate(over=est > truth,
         under=est < truth) %>%
  group_by(exp,day) %>%
  summarise(est = mean(est),
            truth=mean(truth),
            over=mean(over),
            under=mean(under))


# CONFIDENCE
data %>% filter(exp==1) %>%
  group_by(group) %>% summarise(conf = mean(confidence)) %>% ungroup() %>%
  pivot_wider(names_from=group,values_from = conf) %>% mutate(ta - g)
data %>% filter(exp==2) %>%
  group_by(group) %>% summarise(conf = mean(confidence)) %>% ungroup() %>%
  pivot_wider(names_from=group,values_from = conf) %>% mutate(ta - g)
data %>% filter(exp==3.1) %>%
  group_by(group) %>% summarise(conf = mean(confidence)) %>% ungroup() %>%
  pivot_wider(names_from=group,values_from = conf) %>% mutate(ta - g)
data %>% filter(exp==3.2) %>%
  group_by(group) %>% summarise(conf = mean(confidence)) %>% ungroup() %>%
  pivot_wider(names_from=group,values_from = conf) %>% mutate(ta - g)

# day summary
data %>% 
  group_by(exp,day) %>%
  summarise(conf = mean(confidence))


# plots
pal <- c("#000000", # Black
         "#cc79a7", # Reddish purple
         "#f0e442", # Yellow
         "#009e73", # Bluish green
         "#e69f00", # Orange
         "#d55e00", # Vermillion
         "#56b4e9", # Sky blue
         "#0072b2") # Blue
plt_est_conf <- function(data) {
  exp.labs <- c("US-framing\nFaster prior growth",
                "US-framing\nSlower prior growth",
                "Neutral framing\nFaster prior growth",
                "Neutral framing\nSlower prior growth")
  names(exp.labs) <- c(1,2,3.1, 3.2)
  p <- data %>%
    group_by(exp) %>%
    mutate(z_conf = scale(confidence),
           z_est = scale(est)) %>% ungroup() %>%
    ggplot(aes(x=confidence,y=z_est)) +
    # geom_point(alpha=.25) +
    stat_density2d_filled(aes(group=1),colour=NA,adjust = 1.5) +
    # stat_summary_bin(bins=20,position = position_nudge(-5),size=.25) +
    geom_smooth(method="lm",se=F,size=2,colour="black") +
    facet_wrap(.~exp,labeller = labeller(exp=exp.labs)) +
    coord_cartesian(xlim=c(0,100),ylim=c(-1,1)) +
    scale_x_continuous("Confidence") +
    scale_y_continuous("Forecast z-score") +
    theme_tufte(base_size = 15,base_family = "sans") +
    # scale_colour_manual("",values = c("#e69f00","#0072b2","grey"),
    #                     labels=c("Graph forecast","Table forecast","True growth")) +
    theme(legend.position = "top",
          axis.line=element_line(size=.25))
  return(p)
}
plt_conf <- function(data) {
  exp.labs <- rep(c("More exponential",
                    "Less exponential"),2)
  names(exp.labs) <- c(1,2,3.1, 3.2)
  p <- data %>%
    mutate(day = factor(day,levels=c(1,2,3))) %>%
    group_by(day,group,exp) %>%
    summarise(se = sd(confidence)/sqrt(n()),mu=mean(confidence)) %>%
    ggplot(aes(x=day,y=mu,ymin=mu-se,ymax=mu+se,group=group,fill=group)) +
    geom_col(position=position_dodge(.5),width=.5,colour="white") +
    geom_errorbar(position=position_dodge(.5),width=.1) +
    facet_wrap(exp~.,nrow=1,labeller = labeller(exp=exp.labs)) +
    theme_tufte(base_size = 15,base_family = "sans") +
    scale_x_discrete("Forecast horizon (days)",labels=c("3", "6", "9")) +
    scale_y_continuous("Confidence in forecast") +
    scale_fill_manual("",values = c("#d55e00","#0072b2"),
                      labels=c("Graph forecast","Table forecast")) +
    theme(legend.position = c(.4,.95),
          axis.line=element_line(size=.25),
          legend.key.size = unit(5,"mm"),
          legend.text = element_text(size=10),
          plot.title = element_text(hjust = 0.5),
          plot.subtitle = element_text(hjust = 0.5))
  return(p)
}
plt_est <- function(data) {
  exp.labs <- rep(c("More exponential",
                    "Less exponential"),2)
  names(exp.labs) <- c(1,2,3.1, 3.2)
  p <- data %>% 
    mutate(day = factor(day,levels=c(1,2,3))) %>%
    group_by(day,group,exp) %>%
    summarise(se = sd(est/1e5)/sqrt(n()),mu=mean(est/1e5),truth=mean(truth/1e5)) %>%
    ggplot(aes(x=day,y=mu,ymax=mu+se,ymin=mu-se,group=group,colour=group)) +
    geom_point() +
    geom_line(alpha=.75) +
    geom_errorbar(width=.05) +
    geom_line(aes(y=truth,colour="z"),linetype="dashed") +
    geom_point(aes(y=truth,colour="z")) +
    facet_wrap(exp~.,nrow=1,labeller = labeller(exp=exp.labs),scales="free_y") +
    theme_tufte(base_size = 15,base_family = "sans") +
    scale_x_discrete("Forecast horizon (days)",labels=c("3", "6", "9")) +
    scale_y_continuous("Total Cases (100k)",breaks=seq(0,100,1)) +
    scale_colour_manual("",values = c("#d55e00","#0072b2","black"),
                        labels=c("Graph forecast","Table forecast","True growth")) +
    theme(legend.position = c(.15,.8),
          axis.line=element_line(size=.25),
          legend.key.size = unit(5,"mm"),
          legend.text = element_text(size=10),
          plot.title = element_text(hjust = 0.5),
          plot.subtitle = element_text(hjust = 0.5))
  return(p)
}

data %>% filter(exp %in% c(1,2)) %>% plt_est()
ggsave("visualization/figures/fig1CD.pdf",units = "in",height=3,width=6,dpi=150)
data %>% filter(exp %in% c(1,2)) %>% plt_conf()
ggsave("visualization/figures/fig1EF.pdf",units = "in",height=3,width=6,dpi=150)
data %>% filter(exp %in% c(3.1,3.2)) %>% plt_est()
ggsave("visualization/figures/fig2CD.pdf",units = "in",height=3,width=6,dpi=150)
data %>% filter(exp %in% c(3.1,3.2)) %>% plt_conf()
ggsave("visualization/figures/fig2EF.pdf",units = "in",height=3,width=6,dpi=150)


social <- data %>% 
  mutate(f_iso = futur_social_iso_1,
         p_iso = past_social_iso_1,
         tsd = time_to_stop_dist) %>%
  filter(exp %in% c(1,2)) %>%
  group_by(exp,day) %>%
  mutate(z_est = (est - mean(est)) / sd(est)) %>% ungroup() %>%
  group_by(exp) %>%
  mutate(z_f_iso = (f_iso - mean(f_iso)) / sd(f_iso),
         z_p_iso = (p_iso - mean(p_iso)) / sd(p_iso),
         z_tsd = (tsd - mean(tsd)) / sd(tsd)) %>% ungroup() %>%
  group_by(exp,MTurk_ID,group) %>%
  summarise(est = mean(z_est),
            f_iso = mean(z_f_iso),
            p_iso = mean(z_p_iso),
            tsd = mean(z_tsd)) %>% ungroup() %>%
  group_by(exp) %>%
  mutate(r_est = rank(est),
         r_f_iso = rank(f_iso),
         r_p_iso = rank(p_iso),
         r_tsd = rank(tsd))


social %>% 
  ggplot(aes(x=r_est,y=r_f_iso)) +
  stat_summary_bin(bins=100,size=.25,alpha=.5) +
  geom_smooth(method="lm",se=F,colour="#009e73",size=1.5) +
  theme_tufte(base_size = 12,base_family = "sans") +
  coord_fixed(ylim=c(0,500),xlim=c(0,500)) +
  scale_y_continuous("Social Distancing Adherence (rank)",breaks=seq(0,500,100)) +
  scale_x_continuous("Forecasted Total Cases (rank)",breaks=seq(0,500,100)) +
  theme(legend.position = c(.25,.9),
        axis.line=element_line(size=.25),
        legend.key.size = unit(5,"mm"),
        legend.text = element_text(size=10),
        plot.title = element_text(hjust = 0.5),
        plot.subtitle = element_text(hjust = 0.5))
ggsave("visualization/figures/fig3B.pdf",units="in",height=3,width=3)
social %>% 
  ggplot(aes(x=r_est,y=r_tsd)) +
  stat_summary_bin(bins=100,size=.25,alpha=.5) +
  geom_smooth(method="lm",se=F,colour="#009e73",size=1.5) +
  theme_tufte(base_size = 12,base_family = "sans") +
  coord_fixed(ylim=c(0,500),xlim=c(0,500)) +
  scale_y_continuous("Social Distancing Duration (rank)",breaks=seq(0,500,100)) +
  scale_x_continuous("Forecasted Total Cases (rank)",breaks=seq(0,500,100)) +
  theme(legend.position = c(.25,.9),
        axis.line=element_line(size=.25),
        legend.key.size = unit(5,"mm"),
        legend.text = element_text(size=10),
        plot.title = element_text(hjust = 0.5),
        plot.subtitle = element_text(hjust = 0.5))
ggsave("visualization/figures/fig3C.pdf",units="in",height=3,width=3)


exp1 <- social %>% filter(exp == 1)
cor.test(x = exp1$est, y = exp1$f_iso,method = "kendall")
cor.test(x = exp1$est, y = exp1$p_iso,method = "kendall")
cor.test(x = exp1$est, y = exp1$tsd,method = "kendall")

exp2 <- social %>% filter(exp == 2)
cor.test(x = exp2$est, y = exp2$f_iso,method = "kendall")
cor.test(x = exp2$est, y = exp2$p_iso,method = "kendall")
cor.test(x = exp2$est, y = exp2$tsd,method = "kendall")



# separate for returning subjects, by reviewer request
returns1 <- data %>% filter(new == 0) 
return_ids <- unique(returns1$MTurk_ID)
returns0 <- data %>% filter(exp == 1 & MTurk_ID %in% return_ids)


return_soc <- returns0 %>% bind_rows(returns1) %>%
  mutate(f_iso = futur_social_iso_1,
         p_iso = past_social_iso_1,
         tsd = time_to_stop_dist) %>%
  filter(exp %in% c(1,2)) %>%
  group_by(exp,day) %>%
  mutate(z_est = (est - mean(est)) / sd(est)) %>% ungroup() %>%
  group_by(exp) %>%
  mutate(z_f_iso = (f_iso - mean(f_iso)) / sd(f_iso),
         z_p_iso = (p_iso - mean(p_iso)) / sd(p_iso),
         z_tsd = (tsd - mean(tsd)) / sd(tsd)) %>% ungroup() %>%
  group_by(exp,MTurk_ID,group) %>%
  summarise(est = mean(z_est),
            f_iso = mean(z_f_iso),
            p_iso = mean(z_p_iso),
            tsd = mean(z_tsd)) %>% ungroup() %>%
  group_by(exp) %>%
  mutate(r_est = rank(est),
         r_f_iso = rank(f_iso),
         r_p_iso = rank(p_iso),
         r_tsd = rank(tsd))

exp1r <- return_soc %>% filter(exp == 1)
cor.test(x = exp1r$est, y = exp1r$f_iso,method = "kendall")
cor.test(x = exp1r$est, y = exp1r$p_iso,method = "kendall")
cor.test(x = exp1r$est, y = exp1r$tsd,method = "kendall")

exp2r <- return_soc %>% filter(exp == 2)
cor.test(x = exp2r$est, y = exp2r$f_iso,method = "kendall")
cor.test(x = exp2r$est, y = exp2r$p_iso,method = "kendall")
cor.test(x = exp2r$est, y = exp2r$tsd,method = "kendall")