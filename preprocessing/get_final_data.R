library(tidyverse)

options(mc.cores = parallel::detectCores())
parallel:::setDefaultClusterOptions(setup_strategy = "sequential")

setwd("/Users/adkinsty/Box/side_projects/covid/Forecasting1/")

# true COVID growth
true1 <- tibble(day=1:3,est=c(165851,248079,340775))
true2 <- tibble(day=1:3,est=c(505959,590573,677570))

# location data
loc_data <- read.csv("data/extra/loc/uszips.csv") %>% dplyr::select(c(zip,state_id))

# individual experiment data
data1 <- read.csv("data/prepped/data_long_study1.csv") %>% 
  mutate(exp=1,
         id = id * 1e4,
         day=as.numeric(day),
         group=group_id,
         est=estimate,
         est1 = cCases_1_est,est2=cCases_2_est,est3=cCases_3_est) %>%
  dplyr::select(est,est1,est2,est3,confidence,decrease,
                MTurk_ID,exp,group,day,zip,id,
                past_social_iso_1,futur_social_iso_1,
                time_to_stop_dist,
                rt,attention,president,outcome,age,effort)
data2 <- read.csv("data/prepped/data_long_study2.csv") %>% 
  mutate(exp=2,
         id = id / 1e4,
         group=group_id,
         day=as.numeric(day),
         est=estimate,
         est1 = cCases_1_est,est2=cCases_2_est,est3=cCases_3_est) %>%
  dplyr::select(est,est1,est2,est3,confidence,decrease,
                MTurk_ID,exp,group,day,zip,id,
                past_social_iso_1,futur_social_iso_1,
                time_to_stop_dist,
                rt,attention,president,outcome,age,effort,new)
data3 <- read.csv("data/prepped/data_long_study3.csv") %>% 
  mutate(study=as.numeric(study_id),
         exp = ifelse(study==1,3.1,3.2),
         group=ifelse(group_id=="ta_1" | group_id=="ta_2","ta","g"),
         day=as.numeric(day),
         est=estimate,
         est1 = cCases_1_est,est2=cCases_2_est,est3=cCases_3_est) %>%
  dplyr::select(est,est1,est2,est3,confidence,decrease,
                MTurk_ID,exp,group,day,zip,id,
                rt,attention,president,outcome,age,effort)

# combined data
data <- bind_rows(data1,data2,data3) %>%
  filter(!is.na(est)) %>%
  filter(outcome == 'cCases') %>%
  mutate(lb = ifelse(exp %in% c(1,3.1),96968,393782),
         # day = factor(day,c(1,2,3)),
         group = factor(group,c("g","ta", "t")),
         truth = ifelse(exp %in% c(1,3.1),true1[day,]$est,true2[day,]$est),
         n_start = length(unique(MTurk_ID))) %>%
  merge(loc_data,by="zip") %>%
  as_tibble()


# filtered data
dataF <- data %>%
  filter(!decrease) %>%
  filter(age >= 18) %>%
  filter(attention == 6) %>%
  filter(grepl("trump",president,ignore.case = TRUE) |
           grepl("don",president,ignore.case = TRUE)) %>%
  filter(effort > 5) %>%
  filter(rt > 30) %>%
  filter(est > lb) %>%
  filter(est < lb*10) %>%
  group_by(exp) %>%
  mutate(z_est = est / sd(est),
         z_conf = confidence / sd(confidence)) %>% ungroup()

write.csv(dataF,"data/prepped/filtered_data.csv",row.names = F)


dataF %>%
  group_by(exp,group,new) %>%
  summarise(N = length(unique(MTurk_ID)))


# unfiltered, winsorized data for Reviewer 3
dataU <- data %>%
  group_by(exp) %>%
  mutate(z_est = est / sd(est),
         z_conf = confidence / sd(confidence)) %>% ungroup() %>%
  mutate(eff_filter = effort > 5,
         att_filter = attention == 6,
         pres_filter = grepl("trump",president,ignore.case = TRUE) |
           grepl("don",president,ignore.case = TRUE),
         rt_filter = rt > 30,
         est_filter = est < 10*lb)
# write.csv(dataU,"data/prepped/all_unfiltered_data.csv",row.names = F)


dataU <- data %>%
  group_by(exp) %>%
  mutate(z_est = est / sd(est),
         z_conf = confidence / sd(confidence)) %>% ungroup() %>%
  mutate(eff_filter = effort > 5,
         att_filter = attention == 6,
         pres_filter = grepl("trump",president,ignore.case = TRUE) |
           grepl("don",president,ignore.case = TRUE),
         rt_filter = rt > 30,
         est_filter = est < 10*lb)
# write.csv(dataU,"data/prepped/all_unfiltered_data.csv",row.names = F)

