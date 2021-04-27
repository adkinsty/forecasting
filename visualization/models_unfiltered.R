library(tidyverse)
library(brms)

options(mc.cores = parallel::detectCores())
parallel:::setDefaultClusterOptions(setup_strategy = "sequential")

# individual experiment data
data1 <- read.csv("data/prepped/data_long_study1.csv") %>% 
  mutate(exp=1,
         rt = ifelse(group=='ta',rt_ta,rt_g),
         est1 = cCases_1_est,est2=cCases_2_est,est3=cCases_3_est) %>%
  dplyr::select(est,est1,est2,est3,
                MTurk_ID,exp,group,delay,
                rt,attention,president,outcome,age)
data2 <- read.csv("data/prepped/data_long_study2.csv") %>% 
  filter(new==1) %>%
  mutate(exp=2,
         rt = ifelse(group=='ta',rt_ta,rt_g),
         est1 = cCases_1_est,est2=cCases_2_est,est3=cCases_3_est) %>%
  dplyr::select(est,est1,est2,est3,
                MTurk_ID,exp,group,delay,
                rt,attention,president,outcome,age)
data3 <- read.csv("data/prepped/data_long_study3.csv") %>% 
  mutate(exp = ifelse(study==1,3.1,3.2),
         rt = ifelse(study==1, ifelse(group=='ta',rt_ta_1,rt_g_1),
                     ifelse(group=='ta',rt_ta_2,rt_g_2)),
         est1 = cCases_1_est,est2=cCases_2_est,est3=cCases_3_est) %>%
  dplyr::select(est,est1,est2,est3,
                MTurk_ID,exp,group,delay,
                rt,attention,president,outcome,age)


# all data
all_data <- bind_rows(data1,data2,data3) %>%
  filter(!is.na(est)) %>%
  filter(outcome == 'cCases' & group != 't') %>%
  mutate(lb = ifelse(exp %in% c(1,3.1),96968,393782),
         day = factor(delay,c(1,2,3)),
         group = factor(group,c("g","ta"))) %>%
  as_tibble()

# exclusion breakdown
all_data %>%
  group_by(exp) %>%
  summarise(n = n(),
            n_att = sum(attention != 6 | is.na(attention)),
            n_pres = sum(!grepl("trump",president,ignore.case = TRUE) | !grepl("don",president,ignore.case = TRUE)),
            n_rt = sum(rt < 30),
            n_age = sum(age < 18 | age > 100 | is.na(age)),
            n_ub = sum(est > lb*10),
            n_lb = sum(est < lb),
            n_dec = sum(ifelse(day==2,est<=est1,
                               ifelse(day==3,est<=est2,0))))
# apply exclusions
data = all_data %>%
  filter(attention == 6) %>%
  filter(grepl("trump",president,ignore.case = TRUE)) %>%
  filter(rt > 30) %>%
  filter(age >= 18) %>%
  filter(est <= lb*10) %>%
  filter(est >= lb) %>%
  mutate(decrease = ifelse(day==2,est<est1,
                           ifelse(day==3,est<est2,0))) %>%
  filter(!decrease) %>%
  group_by(exp) %>%
  mutate(z_est = est / sd(est)) %>% ungroup()
# models of forecasts
m_formula <- bf(z_est ~ 1 + day*group + (1 | MTurk_ID))
m_priors <- c(set_prior("student_t(3, 0, 2.5)",class="b"),
              set_prior("student_t(3, 0, 2.5)",class="sd"),
              set_prior("gamma(0.01, 0.01)",class = "shape"))
m1 <- brm(m_formula,
          data=data%>%filter(exp==1),
          family = Gamma(link="log"),
          prior = m_priors,
          iter = 1e5,
          warmup = 5e4,
          thin = 10)
m2 <- update(m1,newdata=data%>%filter(exp==2))
m3.1 <- update(m1,newdata=data%>%filter(exp==3.1))
m3.2 <- update(m1,newdata=data%>%filter(exp==3.2))

plot(m1,pars = c("taMg","day2","day3"))
plot(m2,pars = c("taMg","day2","day3"))
plot(m3.1,pars = c("taMg","day2","day3"))
plot(m3.2,pars = c("taMg","day2","day3"))

pal <- c("#000000", # Black
         "#cc79a7", # Reddish purple
         "#f0e442", # Yellow
         "#009e73", # Bluish green
         "#e69f00", # Orange
         "#d55e00", # Vermillion
         "#56b4e9", # Sky blue
         "#0072b2") # Blue

true1 <- tibble(day=1:3,est=c(165851,248079,340775))
true2 <- tibble(day=1:3,est=c(505959,590573,677570))

data %>% 
  filter(exp %in% c(1,3.1)) %>%
  mutate(exp = ifelse(exp==1,"Experiment 1","Replication 1")) %>%
  ggplot(aes(x=day,y=est/1e5,group=group,colour=group)) +
  stat_summary(size=.4) +
  stat_summary(geom="line",alpha=.75) +
  geom_point(data=true1,aes(x=day,y=est/1e5,colour="z"),inherit.aes = F) +
  geom_line(data=true1,aes(x=day,y=est/1e5,colour="z"),inherit.aes = F,linetype="dashed") +
  facet_wrap(exp~.,nrow=1) +
  theme_tufte(base_size = 15,base_family = "sans") +
  scale_x_discrete("Time horizon (days)",labels=c("3", "6", "9")) +
  scale_y_continuous("Total Cases (100k)",breaks=seq(0,10,.5)) +
  scale_colour_manual("",values = c("#e69f00","#0072b2","grey"),
                      labels=c("Graph forecast","Table forecast","True growth")) +
  theme(legend.position = "top",
        axis.line=element_line(size=.25))

data %>% 
  filter(exp %in% c(2,3.2)) %>%
  mutate(exp = ifelse(exp==2,"Experiment 2","Replication 2")) %>%
  ggplot(aes(x=day,y=est/1e5,group=group,colour=group)) +
  stat_summary(size=.4) +
  stat_summary(geom="line",alpha=.75) +
  geom_point(data=true2,aes(x=day,y=est/1e5,colour="z"),inherit.aes = F) +
  geom_line(data=true2,aes(x=day,y=est/1e5,colour="z"),inherit.aes = F,linetype="dashed") +
  facet_wrap(exp~.,nrow=1) +
  theme_tufte(base_size = 15,base_family = "sans") +
  scale_x_discrete("Time horizon (days)",labels=c("3", "6", "9")) +
  scale_y_continuous("Total Cases (100k)",breaks=seq(0,10,1)) +
  scale_colour_manual("",values = c("#e69f00","#0072b2","grey"),
                      labels=c("Graph forecast","Table forecast","True growth")) +
  theme(legend.position = "top",
        axis.line=element_line(size=.25))







