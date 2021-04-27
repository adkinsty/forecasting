library(tidyverse)
library(brms)
library(MASS)
library(bayestestR)

options(mc.cores = parallel::detectCores())
parallel:::setDefaultClusterOptions(setup_strategy = "sequential")
options(contrasts = c("contr.sdif","contr.sdif"))

bm_summary <- function(bm) {
  describe_posterior(bm,effects = 'fixed',component = 'all',ci = 0.95,test = c("p_direction"),centrality = 'all')
}

setwd("/Users/adkinsty/Box/side_projects/covid/Forecasting/")

dat <- read_csv("data/prepped/all_filtered_data_all.csv") %>%
  filter(outcome %in% c("aCases","deaths") & exp %in% c(1,2)) %>%
  mutate(day = factor(day,levels=1:3))

prob <- function(x) {
  # convert odds to probability
  return(x/(1+x))
}

# DEATHS
ddat <- dat %>% filter(outcome == "deaths")
ddat1 <- ddat %>% filter(exp == 1)
ddat2 <- ddat %>% filter(exp == 2)

# models of forecasts
mf_formula <- bf(z_est ~ 1 + day*group + (1 | state_id))
mf_priors <- c(set_prior("student_t(3, 0, 2.5)",class="b"),
              set_prior("student_t(3, 0, 2.5)",class="sd"),
              set_prior("gamma(0.01, 0.01)",class = "shape"))
mfd1 <- brm(mf_formula,
           data=ddat1,
           family = Gamma(link="log"),
           prior = mf_priors,
           iter = 5e3,
           warmup = 1e3)
mfd2   <- update(mfd1,newdata=ddat2)

bm_summary(mfd1)
bm_summary(mfd2)
plot(mfd1,pars = c("taMg","day2","day3"))
plot(mfd2,pars = c("taMg","day2","day3"))

ddat %>% 
  mutate(exp = ifelse(exp==1,"Study 1","Study 2")) %>%
  ggplot(aes(x=day,group=group,colour=group)) + 
  stat_summary(aes(y=est)) + stat_summary(aes(y=est),geom="line") + 
  stat_summary(aes(y=truth,group=1,colour="Truth")) +
  facet_wrap(exp~.,scales="free_y") +
  scale_y_continuous("Deaths") + 
  scale_x_discrete("Horizon (days)",labels=c(3,6,9)) +
  scale_colour_manual("",values=c("red2","blue3","black"),labels=c("Graph est.","Table est.","Truth"))

# models of confidence
mc_formula <- bf(z_conf ~ 1 + day*group + (1 | state_id))
mc_priors <- c(set_prior("student_t(3, 0, 2.5)",class="b"),
               set_prior("student_t(3, 0, 2.5)",class="sd"))
mcd1 <- brm(mc_formula,
           data=ddat1,
           family = gaussian(),
           prior = mc_priors,
           iter = 5e3,
           warmup = 1e3)
mcd2   <- update(mcd1,newdata=ddat2)

bm_summary(mcd1)
bm_summary(mcd2)
plot(mcd1,pars = c("taMg","day2","day3"))
plot(mcd2,pars = c("taMg","day2","day3"))

ddat %>% 
  mutate(exp = ifelse(exp==1,"Study 1","Study 2")) %>%
  ggplot(aes(x=day,y=confidence,group=group,fill=group)) + 
  stat_summary(geom="col",width=.8,position=position_dodge(.8)) + stat_summary(position=position_dodge(.8),size=.25) + 
  facet_wrap(exp~.,scales="free_y") +
  scale_y_continuous("Confidence") + 
  scale_x_discrete("Horizon (days)",labels=c(3,6,9)) +
  scale_fill_manual("",values=c("red2","blue3"),labels=c("Graph est.","Table est."))




# ACTUAL CASES

adat <- dat %>% filter(outcome == "aCases")
adat1 <- adat %>% filter(exp == 1)
adat2 <- adat %>% filter(exp == 2)

# models of forecasts
mfa1 <- brm(mf_formula,
           data=adat1,
           family = Gamma(link="log"),
           prior = mf_priors,
           iter = 5e3,
           warmup = 1e3)
mfa2   <- update(mfa1,newdata=adat2)

bm_summary(mfa1)
bm_summary(mfa2)
plot(mfa1,pars = c("taMg","day2","day3"))
plot(mfa2,pars = c("taMg","day2","day3"))

adat %>% 
  mutate(exp = ifelse(exp==1,"Study 1","Study 2")) %>%
  ggplot(aes(x=day,group=group,colour=group)) + 
  stat_summary(aes(y=est)) + stat_summary(aes(y=est),geom="line") + 
  stat_summary(aes(y=truth,group=1,colour="Truth")) +
  facet_wrap(exp~.,scales="free_y") +
  scale_y_continuous("'Actual' Cases") + 
  scale_x_discrete("Horizon (days)",labels=c(3,6,9)) +
  scale_colour_manual("",values=c("red2","blue3","black"),labels=c("Graph est.","Table est.","Truth"))


# models of confidence
mca1 <- brm(mc_formula,
           data=adat1,
           family = gaussian(),
           prior = mc_priors,
           iter = 5e3,
           warmup = 1e3)
mca2   <- update(mca1,newdata=adat2)

bm_summary(mca1)
bm_summary(mca2)

plot(mca1,pars = c("taMg","day2","day3"))
plot(mca2,pars = c("taMg","day2","day3"))

adat %>% 
  mutate(exp = ifelse(exp==1,"Study 1","Study 2")) %>%
  ggplot(aes(x=day,y=confidence,group=group,fill=group)) + 
  stat_summary(geom="col",width=.8,position=position_dodge(.8)) + stat_summary(position=position_dodge(.8),size=.25) + 
  facet_wrap(exp~.,scales="free_y") +
  scale_y_continuous("Confidence") + 
  scale_x_discrete("Horizon (days)",labels=c(3,6,9)) +
  scale_fill_manual("",values=c("red2","blue3"),labels=c("Graph est.","Table est."))
