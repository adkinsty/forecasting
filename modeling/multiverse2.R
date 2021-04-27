# Multiverse analysis to examine the impact of our extreme value critereon

library(tidyverse)
library(brms)
library(MASS)

options(mc.cores = parallel::detectCores())
parallel:::setDefaultClusterOptions(setup_strategy = "sequential")
options(contrasts = c("contr.sdif","contr.sdif"))

setwd("/Users/adkinsty/Box/side_projects/covid/Forecasting/")


get_m <- function(m0=c(),dat,m,f,first_m) {
  if (first_m) {
    if (m == "f") {
      f_formula <- bf(z_est ~ 1 + day*group + (1 | state_id))
      f_priors <- c(set_prior("student_t(3, 0, 2.5)",class="b"),
                    set_prior("student_t(3, 0, 2.5)",class="sd"),
                    set_prior("gamma(0.01, 0.01)",class = "shape"))
      m1 <- brm(f_formula,data=tmp%>%filter(exp==1),
                family = Gamma(link="log"),prior = f_priors,
                iter = 5e3,warmup = 1e3)
      m2   <- update(m1,newdata=tmp%>%filter(exp==2))
    } else {
      c_formula <- bf(z_conf ~ 1 + day*group + (1 | state_id))
      c_priors <- c(set_prior("student_t(3, 0, 2.5)",class="b"),
                    set_prior("student_t(3, 0, 2.5)",class="sd"))
      m1 <- brm(c_formula,data=tmp%>%filter(exp==1),
                family = gaussian(),prior = c_priors,
                iter = 5e3,warmup = 1e3)
      m2   <- update(m1,newdata=tmp%>%filter(exp==2))
    }
  } else {
    m1 <- update(m0,newdata=dat%>%filter(exp==1))
    m2 <- update(m0,newdata=dat%>%filter(exp==2))
  }
  write_rds(m1,sprintf("modeling/multiverse_models/%s1_%s.rds",m,f))
  write_rds(m2,sprintf("modeling/multiverse_models/%s2_%s.rds",m,f))
  
  return(list(m1 = m1, m2 = m2))
}
get_eff <- function(m) {
  m1 = m$m1
  m2 = m$m2
  e1 <- sprintf("%s (%s)",
                round(fixef(m1)['grouptaMg','Estimate'],2),
                round(fixef(m1)['grouptaMg','Est.Error'],2))
  e2 <- sprintf("%s (%s)",
                round(fixef(m2)['grouptaMg','Estimate'],2),
                round(fixef(m2)['grouptaMg','Est.Error'],2))
  return(c(e1,e2))
}

# apply initial filters
criteria <- c("eff","att","pres","rt")
tmp_cols <- c()
for (i in criteria) {tmp_cols <- append(tmp_cols,sprintf("%s_filter",i))}

data <- read_csv("data/prepped/all_unfiltered_data.csv")  %>%
  filter(!decrease) %>%
  mutate(day = factor(day,levels = c(1,2,3))) %>%
  filter_at(.vars = tmp_cols,all_vars(.==TRUE))

first_m = TRUE
thresholds <- 10:20
results <- tibble()
for (f in thresholds) {
  
  tmp <- data %>% filter(est < lb*f)
  
  # get results
  if (first_m) {
    fm0 <- get_m(NA,tmp,"f",f,first_m)
    cm0 <- get_m(NA,tmp,"c",f,first_m)
    fm <- fm0
    cm <- cm0
  } else {
    fm <- get_m(fm0$m1,tmp,"f",f,first_m)
    cm <- get_m(cm0$m1,tmp,"c",f,first_m)
  }
  
  f_eff <- get_eff(fm)
  c_eff <- get_eff(cm)
  
  results <- tibble(filters = f, f1 = f_eff[1], f2 = f_eff[2],
                    c1 = c_eff[1], c2 = c_eff[2]) %>%
    bind_rows(results)
  
  first_m = FALSE
}
write_csv(results,"modeling/multiverse_models/multiverse2_results.csv")
