library(tidyverse)
library(brms)
library(MASS)

options(mc.cores = parallel::detectCores())
parallel:::setDefaultClusterOptions(setup_strategy = "sequential")
options(contrasts = c("contr.sdif","contr.sdif"))

setwd("/Users/adkinsty/Box/side_projects/covid/Forecasting/")

data <- read_csv("data/prepped/all_filtered_data.csv") %>%
  mutate(over = est > truth,
         day = factor(day,levels=1:3))

prob <- function(x) {
  # convert odds to probability
  return(x/(1+x))
}

# models of forecasts
mf_formula <- bf(z_est ~ 1 + day*group + (1 | state_id))
mf_priors <- c(set_prior("student_t(3, 0, 2.5)",class="b"),
              set_prior("student_t(3, 0, 2.5)",class="sd"),
              set_prior("gamma(0.01, 0.01)",class = "shape"))
mf1F <- brm(mf_formula,
           data=data%>%filter(exp==1),
           family = Gamma(link="log"),
           prior = mf_priors,
           iter = 5e3,
           warmup = 1e3)
mf2F   <- update(mf1F,newdata=data%>%filter(exp==2))
mf3.1F <- update(mf1F,newdata=data%>%filter(exp==3.1))
mf3.2F <- update(mf1F,newdata=data%>%filter(exp==3.2))

plot(mf1F,pars = c("taMg","day2","day3"))
plot(mf2F,pars = c("taMg","day2","day3"))
plot(mf3.1F,pars = c("taMg","day2","day3"))
plot(mf3.2F,pars = c("taMg","day2","day3"))

# models of confidence
mc_formula <- bf(z_conf ~ 1 + day*group + (1 | state_id))
mc_priors <- c(set_prior("student_t(3, 0, 2.5)",class="b"),
               set_prior("student_t(3, 0, 2.5)",class="sd"))
mc1F <- brm(mc_formula,
           data=data%>%filter(exp==1),
           family = gaussian(),
           prior = mc_priors,
           iter = 5e3,
           warmup = 1e3)
mc2F   <- update(mc1F,newdata=data%>%filter(exp==2))
mc3.1F <- update(mc1F,newdata=data%>%filter(exp==3.1))
mc3.2F <- update(mc1F,newdata=data%>%filter(exp==3.2))

plot(mc1F,pars = c("taMg","day2","day3"))
plot(mc2F,pars = c("taMg","day2","day3"))
plot(mc3.1F,pars = c("taMg","day2","day3"))
plot(mc3.2F,pars = c("taMg","day2","day3"))

# overestimation prob models
est <- tibble()
for (e in sort(unique(data$exp))){ 
  for (d in sort(unique(data$day))) {
    tmp <- data%>%filter(exp==e & day ==d)
    if (e == 1 & d == 1) {
      m <- brm(formula=bf(over ~ 1 + (1|state_id)),
               family = bernoulli(link="logit"),
               data = tmp,
               iter = 7500,
               warmup = 2500,control = list(adapt_delta=0.9))
    } else {
      m <- update(m,newdata=tmp)
    }
    est <- as_tibble(round(prob(exp(fixef(m))),digits=2)) %>% 
      mutate(exp=e,day=d) %>%
      bind_rows(est)
    }
}
