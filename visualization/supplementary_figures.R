library(tidyr)
library(tidyverse)
library(ggthemes)
library(ggplot2)
library(plotly)

setwd("/Users/adkinsty/Box/side_projects/covid/")
#************************************************
# GET DATA
#***********************************************
# TRUE COVID DATA (from worldometers.info)
start_date = as.Date("2020-02-29")
date = seq(start_date, by="day",length.out=48)
est = c(
    68,75,100,124,158,221,319,435,541,704,
    994,1301,1630,2183,2770,3617,4604,6357,
    9317,13898,19551,24418,
    33840,44189,55398,
    68905,86379,105217,124788,144980,165851,
    190930,217771,248079,280730,315141,340775,
    372252,406000,438210,472016,505959,536145,
    563753,590573,617661,648003,677570)
covid_data = tibble(date,est)

# CENSUS DATA
loc_data = read.csv("data/extra/loc/uszips.csv")


# STUDY 3.1
data_in3.1 = read_csv("data/prepped/data_long_study3.csv") %>%
    filter(study == 1) %>%
    mutate(pc_est = est/96968, # scale by lb
           rt = ifelse(!is.na(rt_g_1),rt_g_1,rt_ta_1), # response time
           date = as.Date(
               ifelse(delay==1,"2020-03-30",
                      ifelse(delay==2,"2020-04-02","2020-04-05")))) %>%
    filter(outcome == "cCases" & pc_est >= 1 & pc_est <= 10 & rt >= 30 & rt <= 600)
data3.1 = merge(data_in3.1,loc_data,by="zip") %>% 
    mutate(state = state_id)
baseline3.1 = data3.1 %>% group_by(MTurk_ID,group) %>%
    summarise(n=n()) %>% ungroup() %>%
    mutate(est = rep(96968,length(unique(data3.1$id))),
           date = as.Date(rep("2020-03-27",length(unique(data3.1$id)))))
pltdata3.1 = data3.1 %>% bind_rows(baseline3.1)
covid_data3.1 = tibble(
    date = as.Date(c("2020-02-29","2020-03-06","2020-03-13","2020-03-20","2020-03-27","2020-03-30","2020-04-02","2020-04-05")),
    est = c(68,319,2183,19367,96968,165851,248079,340775))

# STUDY 3.2
data_in3.2 = read_csv("data/prepped/data_long_study3.csv") %>%
    filter(study == 2) %>%
    mutate(pc_est = est/393782, # scale by lb
           rt = ifelse(!is.na(rt_g_2),rt_g_2,rt_ta_2), # response time
           date = as.Date(
               ifelse(delay==1,"2020-04-10",
                      ifelse(delay==2,"2020-04-13","2020-04-16")))) %>%
    filter(outcome == "cCases" & pc_est >= 1 & pc_est <= 10 & rt >= 30 & rt <= 600)
data3.2 = merge(data_in3.2,loc_data,by="zip") %>% 
    mutate(state = state_id)
baseline3.2 = data3.2 %>% group_by(MTurk_ID,group) %>%
    summarise(n=n()) %>% ungroup() %>%
    mutate(est = rep(393782,length(unique(data3.2$id))),
           date = as.Date(rep("2020-04-07",length(unique(data3.2$id)))))
pltdata3.2 = data3.2 %>% bind_rows(baseline3.2)
covid_data3.2 = tibble(
    date = as.Date(c("2020-03-11","2020-03-18","2020-03-25","2020-04-01","2020-04-07","2020-04-10","2020-04-13","2020-04-16")),
    est = c(1301,9197,68211,215003,393782,505959,590573,677570))

# STUDY 4.1
data_in4.1 = read_csv("data/prepped/data_long_study4.csv")   %>%
    filter(shape == 'lin') %>%
    mutate(pc_est = est/97361, # scale by lb
           rt = ifelse(!is.na(rt_g_lin),rt_g_lin,rt_ta_lin), # response time
           date = as.Date(
               ifelse(delay==1,"2020-04-26",
                      ifelse(delay==2,"2020-04-29","2020-05-02")))) %>%
    filter(pc_est >= 1 & pc_est <= 10 & rt >= 30 & rt <= 600)
data4.1 = merge(data_in4.1,loc_data,by="zip") %>% 
    mutate(state = state_id)
baseline4.1 = data4.1 %>% group_by(MTurk_ID,group) %>%
    summarise(n=n()) %>% ungroup() %>%
    mutate(est = rep(97361,length(unique(data4.1$id))),
           date = as.Date(rep("2020-04-23",length(unique(data4.1$id)))))
pltdata4.1 = data4.1 %>% bind_rows(baseline4.1)
covid_data4.1 = tibble(
    date = as.Date(c("2020-03-26","2020-04-02","2020-04-09","2020-04-16","2020-04-23","2020-04-26","2020-04-29","2020-05-02")),
    est = c(19312,38767,58950,78011,97361,105857,114214,122571))

# STUDY 4.2
data_in4.2 = read_csv("data/prepped/data_long_study4.csv")   %>%
    filter(shape == 'exp') %>%
    mutate(pc_est = est/97625, # scale by lb
           rt = ifelse(!is.na(rt_g_exp),rt_g_exp,rt_ta_exp), # response time
           date = as.Date(
               ifelse(delay==1,"2020-04-26",
                      ifelse(delay==2,"2020-04-29","2020-05-02")))) %>%
    filter(pc_est >= 1 & pc_est <= 10 & rt >= 30 & rt <= 600)
data4.2 = merge(data_in4.2,loc_data,by="zip") %>% 
    mutate(state = state_id)
baseline4.2 = data4.2 %>% group_by(MTurk_ID,group) %>%
    summarise(n=n()) %>% ungroup() %>%
    mutate(est = rep(97625,length(unique(data4.2$id))),
           date = as.Date(rep("2020-04-23",length(unique(data4.2$id)))))
pltdata4.2 = data4.2 %>% bind_rows(baseline4.2)
covid_data4.2 = tibble(
    date = as.Date(c("2020-03-26","2020-04-02","2020-04-09","2020-04-16","2020-04-23","2020-04-26","2020-04-29","2020-05-02")),
    est = c(1647,4201,11209,33021,97625,155649,249245,399124))

# STUDY 5
data_in5 = read_csv("data/prepped/data_long_study5.csv") %>%
    mutate(pc_est = est/393782, # scale by lb
           date = as.Date(
               ifelse(delay==1,"2020-04-10",
                      ifelse(delay==2,"2020-04-13","2020-04-16")))) %>%
    filter(outcome == "cCases" & pc_est >= 1 & pc_est <= 10)
data5 = merge(data_in5,loc_data,by="zip") %>% 
    mutate(state = state_id)
baseline5 = data5 %>% group_by(MTurk_ID,group) %>%
    summarise(n=n()) %>% ungroup() %>%
    mutate(est = rep(393782,length(unique(data5$id))),
           date = as.Date(rep("2020-04-07",length(unique(data5$id)))))
pltdata5 = data5 %>% bind_rows(baseline5)
covid_data5 = tibble(
    date = as.Date(c("2020-03-11","2020-03-18","2020-03-25","2020-04-01","2020-04-07","2020-04-10","2020-04-13","2020-04-16")),
    est = c(1301,9197,68211,215003,393782,505959,590573,677570))



# FIGURE S1A
ggplot() +
    geom_line(data=pltdata3.1,aes(x=date,y=est,group=MTurk_ID),size=.5,alpha=.1,colour="gray") +
    geom_line(data=covid_data3.1,aes(x=date,y=est),size=1) +
    stat_summary(data=pltdata3.1,aes(x=date,y=est,group=group,colour=group),
                 geom="line",size=1) + 
    scale_y_continuous("Total COVID-19 Cases",
                       labels=scales::number) +  
    scale_color_manual(values=c("red2","blue2")) +
    scale_x_date(name = "Date in 2020",
                 breaks=as.Date(c("2020-02-29","2020-03-06","2020-03-13","2020-03-20","2020-03-27","2020-03-30","2020-04-02","2020-04-05")),
                 labels=c("Feb. 29","Mar. 6","Mar. 13","Mar. 20","Mar. 27","Mar. 30","Apr. 2","Apr. 5")) +
    coord_cartesian(ylim=c(0,5e5)) +
    ggtitle("Study 3: Replication of Study 1") +
    theme_clean(base_size = 12,base_family = 'Times') +
    theme(plot.background = element_blank(),
          axis.title = element_text(face = "bold"),
          legend.position = 'none',
          plot.title = element_text(hjust = .5))
ggsave("visualization/figures/final/figS1A.pdf",height=4,width=7,units="in",dpi=150)
ggplot() +
    stat_summary(data=pltdata3.1,aes(x=date,y=est,group=group,colour=group)) + 
    stat_summary(data=pltdata3.1,aes(x=date,y=est,group=group,colour=group),geom="line") + 
    scale_y_continuous("Total COVID-19 Cases in US",
                       labels=scales::number) + 
    scale_color_manual(values=c("red2","blue2")) +
    scale_x_date(limits=as.Date(c("2020-03-29","2020-04-06")),
                 breaks=as.Date(c("2020-03-30","2020-04-02","2020-04-05")),
                 labels=c("Mar. 30","Apr. 2","Apr. 5")) +
    theme_clean(base_size = 10,base_family = 'Times') +
    theme(plot.background = element_blank(),
          panel.grid = element_blank(),
          axis.title = element_blank(),
          legend.position = 'none',
          plot.title = element_text(hjust = .5))
ggsave("visualization/figures/final/figS1A_inset.pdf",height=2,width=2,units="in",dpi=150)
# FIGURE S1B
ggplot() +
    geom_line(data=pltdata3.2,aes(x=date,y=est,group=MTurk_ID),size=.5,alpha=.1,colour="gray") +
    geom_line(data=covid_data3.2,aes(x=date,y=est),size=1) +
    stat_summary(data=pltdata3.2,aes(x=date,y=est,group=group,colour=group),
                 geom="line",size=1) + 
    scale_y_continuous("Total COVID-19 Cases",
                       breaks = c(0,200000,400000,600000,800000,1000000),
                       labels=scales::number) +  
    scale_color_manual(values=c("red2","blue2")) +
    scale_x_date(name = "Date in 2020",
                 breaks=as.Date(c("2020-03-11","2020-03-18","2020-03-25","2020-04-01","2020-04-07","2020-04-10","2020-04-13","2020-04-16")),
                 labels=c("Mar. 11","Mar. 18","Mar. 25","Apr. 1","Apr. 7","Apr. 10","Apr. 13","Apr. 16")) +
    coord_cartesian(ylim=c(0,1e6)) +
    ggtitle("Study 3: Replication of Study 2") +
    theme_clean(base_size = 12,base_family = 'Times') +
    theme(plot.background = element_blank(),
          axis.title = element_text(face = "bold"),
          legend.position = 'none',
          plot.title = element_text(hjust = .5))
ggsave("visualization/figures/final/figS1B.pdf",height=4,width=7,units="in",dpi=150)
ggplot() +
    stat_summary(data=pltdata3.2,aes(x=date,y=est,group=group,colour=group)) + 
    stat_summary(data=pltdata3.2,aes(x=date,y=est,group=group,colour=group),geom="line") + 
    scale_y_continuous("Total COVID-19 Cases in US",
                       labels=scales::number) + 
    scale_color_manual(values=c("red2","blue2")) +
    scale_x_date(limits=as.Date(c("2020-04-09","2020-04-17")),
                 breaks=as.Date(c("2020-04-10","2020-04-13","2020-04-16")),
                 labels=c("Apr. 10","Apr. 13","Apr. 16")) +
    theme_clean(base_size = 10,base_family = 'Times') +
    theme(plot.background = element_blank(),
          panel.grid = element_blank(),
          axis.title = element_blank(),
          legend.position = 'none',
          plot.title = element_text(hjust = .5))
ggsave("visualization/figures/final/figS1B_inset.pdf",height=2,width=2,units="in",dpi=150)
# FIGURE S2A
ggplot() +
    geom_line(data=pltdata4.1,aes(x=date,y=est,group=MTurk_ID),size=.5,alpha=.1,colour="gray") +
    geom_line(data=covid_data4.1,aes(x=date,y=est),size=1) +
    stat_summary(data=pltdata4.1,aes(x=date,y=est,group=group,colour=group),
                 geom="line",size=1) + 
    scale_y_continuous("Total COVID-19 Cases",
                       labels=scales::number) + 
    scale_color_manual(values=c("red2","blue2")) +
    scale_x_date(name = "Date in 2020",
                 breaks=as.Date(c("2020-03-26","2020-04-02","2020-04-09","2020-04-16","2020-04-23","2020-04-26","2020-04-29","2020-05-02")),
                 labels=c("Mar. 26","Apr. 2","Apr. 9","Apr. 16","Apr. 23","Apr. 26","Apr. 29","May 2")) +
    coord_cartesian(ylim=c(0,2.5e5)) +
    ggtitle("Study 4: Linear") +
    theme_clean(base_size = 12,base_family = 'Times') +
    theme(plot.background = element_blank(),
          axis.title = element_text(face = "bold"),
          legend.position = 'none',
          plot.title = element_text(hjust = .5))
ggsave("visualization/figures/final/figS2A.pdf",height=4,width=7,units="in",dpi=150)
ggplot() +
    stat_summary(data=pltdata4.1,aes(x=date,y=est,group=group,colour=group)) + 
    stat_summary(data=pltdata4.1,aes(x=date,y=est,group=group,colour=group),geom="line") + 
    scale_y_continuous("Total COVID-19 Cases in US",
                       labels=scales::number) + 
    scale_color_manual(values=c("red2","blue2")) +
    scale_x_date(limits=as.Date(c("2020-04-25","2020-05-03")),
                 breaks=as.Date(c("2020-04-26","2020-04-29","2020-05-02")),
                 labels=c("Apr. 10","Apr. 13","Apr. 16")) +
    theme_clean(base_size = 10,base_family = 'Times') +
    theme(plot.background = element_blank(),
          panel.grid = element_blank(),
          axis.title = element_blank(),
          legend.position = 'none',
          plot.title = element_text(hjust = .5))
ggsave("visualization/figures/final/figS2A_inset.pdf",height=2,width=2,units="in",dpi=150)

# FIGURE S2B
ggplot() +
    geom_line(data=pltdata4.2,aes(x=date,y=est,group=MTurk_ID),size=.5,alpha=.1,colour="gray") +
    geom_line(data=covid_data4.2,aes(x=date,y=est),size=1) +
    stat_summary(data=pltdata4.2,aes(x=date,y=est,group=group,colour=group),
                 geom="line",size=1) + 
    scale_y_continuous("Total COVID-19 Cases",
                       labels=scales::number) + 
    scale_color_manual(values=c("red2","blue2")) +
    scale_x_date(name = "Date in 2020",
                 breaks=as.Date(c("2020-03-26","2020-04-02","2020-04-09","2020-04-16","2020-04-23","2020-04-26","2020-04-29","2020-05-02")),
                 labels=c("Mar. 26","Apr. 2","Apr. 9","Apr. 16","Apr. 23","Apr. 26","Apr. 29","May 2")) +
    coord_cartesian(ylim=c(0,5e5)) +
    ggtitle("Study 4: Exponential") +
    theme_clean(base_size = 12,base_family = 'Times') +
    theme(plot.background = element_blank(),
          axis.title = element_text(face = "bold"),
          legend.position = 'none',
          plot.title = element_text(hjust = .5))
ggsave("visualization/figures/final/figS2B.pdf",height=4,width=7,units="in",dpi=150)
ggplot() +
    stat_summary(data=pltdata4.2,aes(x=date,y=est,group=group,colour=group)) + 
    stat_summary(data=pltdata4.2,aes(x=date,y=est,group=group,colour=group),geom="line") + 
    scale_y_continuous("Total COVID-19 Cases in US",
                       labels=scales::number) + 
    scale_color_manual(values=c("red2","blue2")) +
    scale_x_date(limits=as.Date(c("2020-04-25","2020-05-03")),
                 breaks=as.Date(c("2020-04-26","2020-04-29","2020-05-02")),
                 labels=c("Apr. 10","Apr. 13","Apr. 16")) +
    theme_clean(base_size = 10,base_family = 'Times') +
    theme(plot.background = element_blank(),
          panel.grid = element_blank(),
          axis.title = element_blank(),
          legend.position = 'none',
          plot.title = element_text(hjust = .5))
ggsave("visualization/figures/final/figS2B_inset.pdf",height=2,width=2,units="in",dpi=150)

# FIGURE S3
ggplot() +
    geom_line(data=pltdata5,aes(x=date,y=est,group=MTurk_ID),size=.5,alpha=.1,colour="gray") +
    geom_line(data=covid_data5,aes(x=date,y=est),size=1) +
    stat_summary(data=pltdata5,aes(x=date,y=est,group=group,colour=group),
                 geom="line",size=1) + 
    scale_y_continuous("Total COVID-19 Cases in US",
                       breaks = c(0,200000,400000,600000,800000,1000000),
                       labels=scales::number) +  
    scale_color_manual(values=c("red2","blue2")) +
    scale_x_date(name = "Date in 2020",
                 breaks=as.Date(c("2020-03-11","2020-03-18","2020-03-25","2020-04-01","2020-04-07","2020-04-10","2020-04-13","2020-04-16")),
                 labels=c("Mar. 11","Mar. 18","Mar. 25","Apr. 1","Apr. 7","Apr. 10","Apr. 13","Apr. 16")) +
    coord_cartesian(ylim=c(0,1e6)) +
    ggtitle("Study 5") +
    theme_clean(base_size = 12,base_family = 'Times') +
    theme(plot.background = element_blank(),
          axis.title = element_text(face = "bold"),
          legend.position = 'none',
          plot.title = element_text(hjust = .5))
ggsave("visualization/figures/final/figS3A.pdf",height=4,width=7,units="in",dpi=150)
ggplot() +
    stat_summary(data=pltdata5,aes(x=date,y=est,group=group,colour=group)) + 
    stat_summary(data=pltdata5,aes(x=date,y=est,group=group,colour=group),geom="line") + 
    scale_y_continuous("Total COVID-19 Cases in US",
                       labels=scales::number) + 
    scale_color_manual(values=c("red2","blue2")) +
    scale_x_date(limits=as.Date(c("2020-04-09","2020-04-17")),
                 breaks=as.Date(c("2020-04-10","2020-04-13","2020-04-16")),
                 labels=c("Apr. 10","Apr. 13","Apr. 16")) +
    theme_clean(base_size = 10,base_family = 'Times') +
    theme(plot.background = element_blank(),
          panel.grid = element_blank(),
          axis.title = element_blank(),
          legend.position = 'none',
          plot.title = element_text(hjust = .5))
ggsave("visualization/figures/final/figS3A_inset.pdf",height=2,width=2,units="in",dpi=150)
