library(tidyr)
library(tidyverse)
library(ggthemes)
library(ggplot2)
library(plotly)

setwd("/Users/adkinsty/Box/side_projects/covid/")

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
loc_data = read_csv("data/extra/loc/uszips.csv")

# STUDY 1 DATA
data_in1 = read_csv("data/prepped/data_long_study1.csv")  %>%
    mutate(pc_est = est/96968,
           date = as.Date(
               ifelse(delay==1,"2020-03-30",
                      ifelse(delay==2,"2020-04-02","2020-04-05"))),
           rt = ifelse(!is.na(rt_g),rt_g,ifelse(!is.na(rt_t),rt_t,rt_ta))) %>%
    filter(outcome == "cCases" & group != "t" & pc_est >= 1 & pc_est <= 10 & rt >= 30 & rt <= 600)
pltdata1 = merge(data_in1,loc_data,by="zip") %>% 
    mutate(state = state_id)
covid_data1 = tibble(
    date = as.Date(c("2020-02-29","2020-03-06","2020-03-13","2020-03-20","2020-03-27","2020-03-30","2020-04-02","2020-04-05")),
    est = c(68,319,2183,19367,96968,165851,248079,340775))

# Figure 1A
covid_data1 %>% ggplot(aes(x = date, y = est/1e3, label = est, group=1)) + 
    stat_summary(data=pltdata1,aes(x=date,y=est/1e3,group=group,colour=group),size=.2,position=position_dodge(.25)) + 
    stat_summary(data=pltdata1,aes(x=date,y=est/1e3,group=group,colour=group),geom="line",position=position_dodge(.25)) + 
    geom_point(size = 1) +
    geom_line() +
    # geom_text(nudge_y = 8e3, nudge_x = -1,size=3,family="Times") +
    scale_y_continuous("Total COVID-19 Cases in US (thousand)",
                       labels=scales::number) + 
    scale_x_date(name = "Date in 2020",
                 limits=as.Date(c("2020-02-28","2020-03-28")),
                 breaks=as.Date(c("2020-02-29","2020-03-06","2020-03-13","2020-03-20","2020-03-27")),
                 labels=c("Feb. 29","Mar. 6","Mar. 13","Mar. 20","Mar. 27")) +
    scale_color_manual(values=c("red3","blue3")) +
    coord_cartesian(ylim=c(0,100)) +
    theme_clean(base_size = 12,base_family = 'Times') +
    theme(plot.background = element_blank(),
          panel.grid = element_blank(),
          axis.line = element_line(),
          axis.title = element_text(face = "bold"),
          legend.position = 'none',
          plot.title = element_text(hjust = .5))
ggsave("visualization/figures/final/fig1A.pdf",height=3,width=3,units="in",dpi=150)


# Figure 1B
covid_data1 %>% ggplot(aes(x = date, y = est/1e3, label = est, group=1)) + 
    stat_summary(data=pltdata1,aes(x=date,y=est/1e3,group=group,colour=group),size=.2,position=position_dodge(.25)) + 
    stat_summary(data=pltdata1,aes(x=date,y=est/1e3,group=group,colour=group),geom="line",position=position_dodge(.25)) + 
    geom_point(size = 1) +
    geom_line() +
    scale_y_continuous("Total COVID-19 Cases in US (thousand)",
                       labels=scales::number) + 
    scale_x_date(name = "Date in 2020",
                 limits=as.Date(c("2020-03-29","2020-04-06")),
                 breaks=as.Date(c("2020-03-30","2020-04-02","2020-04-05")),
                 labels=c("Mar. 30","Apr. 2","Apr. 5")) +
    scale_color_manual(values=c("red3","blue3")) +
    coord_cartesian(ylim=c(100,400)) +
    theme_clean(base_size = 12,base_family = 'Times') +
    theme(plot.background = element_blank(),
          panel.grid = element_blank(),
          axis.line = element_line(),
          axis.title = element_text(face = "bold"),
          legend.position = 'none',
          plot.title = element_text(hjust = .5))
ggsave("visualization/figures/final/fig1B.pdf",height=3,width=3,units="in",dpi=150)


# STUDY 2 DATA
data_in2 = read_csv("data/prepped/data_long_study2.csv")  %>%
    mutate(pc_est = est/393782,
           date = as.Date(
               ifelse(delay==1,"2020-04-10",
                      ifelse(delay==2,"2020-04-13","2020-04-16"))),
           rt = ifelse(!is.na(rt_g),rt_g,ifelse(!is.na(rt_t),rt_t,rt_ta))) %>%
    filter(group != "t" & outcome == "cCases" & pc_est >= 1 & pc_est <= 10 & rt >= 30 & rt <= 600)
pltdata2 = merge(data_in2,loc_data,by="zip") %>% 
    mutate(state = state_id)
covid_data2 = tibble(
    date = as.Date(c("2020-03-11","2020-03-18","2020-03-25","2020-04-01","2020-04-07","2020-04-10","2020-04-13","2020-04-16")),
    est = c(1301,9197,68211,215003,393782,505959,590573,677570))

# Figure 1C
covid_data2 %>% ggplot(aes(x = date, y = est/1e3, label = est, group=1)) + 
    geom_point(size = 1) +
    geom_line() +
    stat_summary(data=pltdata2,aes(x=date,y=est/1e3,group=group,colour=group),size=.2,position=position_dodge(.25)) + 
    stat_summary(data=pltdata2,aes(x=date,y=est/1e3,group=group,colour=group),geom="line",position=position_dodge(.25)) + 
    scale_y_continuous("Total COVID-19 Cases in US (thousand)",
                       labels=scales::number) + 
    scale_x_date(name = "Date in 2020",
                 limits=as.Date(c("2020-03-10","2020-04-08")),
                 breaks=as.Date(c("2020-03-11","2020-03-18","2020-03-25","2020-04-01","2020-04-07")),
                 labels=c("Mar. 11","Mar. 18","Mar. 25","Apr. 1","Apr. 7")) +
    scale_color_manual(values=c("red3","blue3")) +
    coord_cartesian(ylim=c(0,400)) +
    theme_clean(base_size = 12,base_family = 'Times') +
    theme(plot.background = element_blank(),
          panel.grid = element_blank(),
          axis.line = element_line(),
          axis.title = element_text(face = "bold"),
          legend.position = 'none',
          plot.title = element_text(hjust = .5))
ggsave("visualization/figures/final/fig1C.pdf",height=3,width=3,units="in",dpi=150)

# Figure 1D
covid_data2 %>% ggplot(aes(x = date, y = est/1e3, label = est, group=1)) + 
    geom_point(size = 1) +
    geom_line() +
    stat_summary(data=pltdata2,aes(x=date,y=est/1e3,group=group,colour=group),size=.2,position=position_dodge(.25)) + 
    stat_summary(data=pltdata2,aes(x=date,y=est/1e3,group=group,colour=group),geom="line",position=position_dodge(.25)) + 
    scale_y_continuous("Total COVID-19 Cases in US (thousand)",
                       labels=scales::number) + 
    scale_x_date(name = "Date in 2020",
                 limits=as.Date(c("2020-04-09","2020-04-17")),
                 breaks=as.Date(c("2020-04-10","2020-04-13","2020-04-16")),
                 labels=c("Apr. 10","Apr. 13","Apr. 16")) +
    scale_color_manual(values=c("red3","blue3")) +
    coord_cartesian(ylim=c(400,900)) +
    theme_clean(base_size = 12,base_family = 'Times') +
    theme(plot.background = element_blank(),
          panel.grid = element_blank(),
          axis.line = element_line(),
          axis.title = element_text(face = "bold"),
          legend.position = 'none',
          plot.title = element_text(hjust = .5))
ggsave("visualization/figures/final/fig1D.pdf",height=3,width=3,units="in",dpi=150)




# STUDY 3.1
data_in3.1 = read_csv("data/prepped/data_long_study3.csv") %>%
    filter(study == 1) %>%
    mutate(pc_est = est/96968, # scale by lb
           rt = ifelse(!is.na(rt_g_1),rt_g_1,rt_ta_1), # response time
           date = as.Date(
               ifelse(delay==1,"2020-03-30",
                      ifelse(delay==2,"2020-04-02","2020-04-05")))) %>%
    filter(outcome == "cCases" & pc_est >= 1 & pc_est <= 10 & rt >= 30 & rt <= 600)
pltdata3.1 = merge(data_in3.1,loc_data,by="zip") %>% 
    mutate(state = state_id)
covid_data3.1 = tibble(
    date = as.Date(c("2020-02-29","2020-03-06","2020-03-13","2020-03-20","2020-03-27","2020-03-30","2020-04-02","2020-04-05")),
    est = c(68,319,2183,19367,96968,165851,248079,340775))

# Figure 3A
covid_data3.1 %>% ggplot(aes(x = date, y = est/1e3, label = est, group=1)) + 
    stat_summary(data=pltdata3.1,aes(x=date,y=est/1e3,group=group,colour=group),size=.2,position=position_dodge(.25)) + 
    stat_summary(data=pltdata3.1,aes(x=date,y=est/1e3,group=group,colour=group),geom="line",position=position_dodge(.25)) + 
    geom_point(size = 1) +
    geom_line() +
    # geom_text(nudge_y = 8e3, nudge_x = -1,size=3,family="Times") +
    scale_y_continuous("Total COVID-19 Cases in US (thousand)",
                       labels=scales::number) + 
    scale_x_date(name = "Date in 2020",
                 limits=as.Date(c("2020-02-28","2020-03-28")),
                 breaks=as.Date(c("2020-02-29","2020-03-06","2020-03-13","2020-03-20","2020-03-27")),
                 labels=c("Feb. 29","Mar. 6","Mar. 13","Mar. 20","Mar. 27")) +
    scale_color_manual(values=c("red3","blue3")) +
    coord_cartesian(ylim=c(0,100)) +
    theme_clean(base_size = 12,base_family = 'Times') +
    theme(plot.background = element_blank(),
          panel.grid = element_blank(),
          axis.line = element_line(),
          axis.title = element_text(face = "bold"),
          legend.position = 'none',
          plot.title = element_text(hjust = .5))
ggsave("visualization/figures/final/fig3A.pdf",height=3,width=3,units="in",dpi=150)

# Figure 3B
covid_data3.1 %>% ggplot(aes(x = date, y = est/1e3, label = est, group=1)) + 
    stat_summary(data=pltdata3.1,aes(x=date,y=est/1e3,group=group,colour=group),size=.2,position=position_dodge(.25)) + 
    stat_summary(data=pltdata3.1,aes(x=date,y=est/1e3,group=group,colour=group),geom="line",position=position_dodge(.25)) + 
    geom_point(size = 1) +
    geom_line() +
    scale_y_continuous("Total COVID-19 Cases in US (thousand)",
                       labels=scales::number) + 
    scale_x_date(name = "Date in 2020",
                 limits=as.Date(c("2020-03-29","2020-04-06")),
                 breaks=as.Date(c("2020-03-30","2020-04-02","2020-04-05")),
                 labels=c("Mar. 30","Apr. 2","Apr. 5")) +
    scale_color_manual(values=c("red3","blue3")) +
    coord_cartesian(ylim=c(100,400)) +
    theme_clean(base_size = 12,base_family = 'Times') +
    theme(plot.background = element_blank(),
          panel.grid = element_blank(),
          axis.line = element_line(),
          axis.title = element_text(face = "bold"),
          legend.position = 'none',
          plot.title = element_text(hjust = .5))
ggsave("visualization/figures/final/fig3B.pdf",height=3,width=3,units="in",dpi=150)



# STUDY 3.2
data_in3.2 = read_csv("data/prepped/data_long_study3.csv") %>%
    filter(study == 2) %>%
    mutate(pc_est = est/393782, # scale by lb
           rt = ifelse(!is.na(rt_g_2),rt_g_2,rt_ta_2), # response time
           date = as.Date(
               ifelse(delay==1,"2020-04-10",
                      ifelse(delay==2,"2020-04-13","2020-04-16")))) %>%
    filter(outcome == "cCases" & pc_est >= 1 & pc_est <= 10 & rt >= 30 & rt <= 600)
pltdata3.2 = merge(data_in3.2,loc_data,by="zip") %>% 
    mutate(state = state_id)
covid_data3.2 = tibble(
    date = as.Date(c("2020-03-11","2020-03-18","2020-03-25","2020-04-01","2020-04-07","2020-04-10","2020-04-13","2020-04-16")),
    est = c(1301,9197,68211,215003,393782,505959,590573,677570))

# Figure 3C
covid_data3.2 %>% ggplot(aes(x = date, y = est/1e3, label = est, group=1)) + 
    geom_point(size = 1) +
    geom_line() +
    stat_summary(data=pltdata3.2,aes(x=date,y=est/1e3,group=group,colour=group),size=.2,position=position_dodge(.25)) + 
    stat_summary(data=pltdata3.2,aes(x=date,y=est/1e3,group=group,colour=group),geom="line",position=position_dodge(.25)) + 
    scale_y_continuous("Total COVID-19 Cases in US (thousand)",
                       labels=scales::number) + 
    scale_x_date(name = "Date in 2020",
                 limits=as.Date(c("2020-03-10","2020-04-08")),
                 breaks=as.Date(c("2020-03-11","2020-03-18","2020-03-25","2020-04-01","2020-04-07")),
                 labels=c("Mar. 11","Mar. 18","Mar. 25","Apr. 1","Apr. 7")) +
    scale_color_manual(values=c("red3","blue3")) +
    coord_cartesian(ylim=c(0,400)) +
    theme_clean(base_size = 12,base_family = 'Times') +
    theme(plot.background = element_blank(),
          panel.grid = element_blank(),
          axis.line = element_line(),
          axis.title = element_text(face = "bold"),
          legend.position = 'none',
          plot.title = element_text(hjust = .5))
ggsave("visualization/figures/final/fig3C.pdf",height=3,width=3,units="in",dpi=150)

# Figure 1D
covid_data3.2 %>% ggplot(aes(x = date, y = est/1e3, label = est, group=1)) + 
    geom_point(size = 1) +
    geom_line() +
    stat_summary(data=pltdata3.2,aes(x=date,y=est/1e3,group=group,colour=group),size=.2,position=position_dodge(.25)) + 
    stat_summary(data=pltdata3.2,aes(x=date,y=est/1e3,group=group,colour=group),geom="line",position=position_dodge(.25)) + 
    scale_y_continuous("Total COVID-19 Cases in US (thousand)",
                       labels=scales::number) + 
    scale_x_date(name = "Date in 2020",
                 limits=as.Date(c("2020-04-09","2020-04-17")),
                 breaks=as.Date(c("2020-04-10","2020-04-13","2020-04-16")),
                 labels=c("Apr. 10","Apr. 13","Apr. 16")) +
    scale_color_manual(values=c("red3","blue3")) +
    coord_cartesian(ylim=c(400,900)) +
    theme_clean(base_size = 12,base_family = 'Times') +
    theme(plot.background = element_blank(),
          panel.grid = element_blank(),
          axis.line = element_line(),
          axis.title = element_text(face = "bold"),
          legend.position = 'none',
          plot.title = element_text(hjust = .5))
ggsave("visualization/figures/final/fig3D.pdf",height=3,width=3,units="in",dpi=150)













stopdata1 = pltdata1 %>%
    # filter(delay==3) %>%
    mutate(
        ttsd = time_to_stop_dist,
        stop_dist = as.Date.character(
            tryFormats = c("%m-%d-%Y"),
            ifelse(ttsd==1,"04-03-2020",
                   ifelse(ttsd==2,"04-10-2020",
                          ifelse(ttsd==3,"04-17-2020",
                                 ifelse(ttsd==4,"04-24-2020",
                                        ifelse(ttsd==5,"05-27-2020",
                                               ifelse(ttsd==6,"06-27-2020","07-27-2020"))))))))
    
stopdata1 %>%
    group_by(MTurk_ID) %>% 
    summarise(est=mean(est),ttsd=mean(ttsd)) %>% 
    mutate(rank_est = rank(est)) %>%
    ggplot() +
    geom_smooth(aes(x=ttsd,y=rank_est),method="lm",se=T,colour="green3") +
    stat_summary(aes(x=ttsd,y=rank_est)) +
    scale_x_continuous("Anticipated End to Social-Distancing",
                 breaks=1:7,
                 labels=c("1 week", "2 weeks", "3 weeks","1 month","2 months","3 months", "4+ months")) +
    scale_y_continuous("Ranked Forecast of COVID-19 Cases",
                       labels=scales::number) +      
    theme_clean(base_family = "Times",base_size=11) +
    theme(plot.background = element_blank(),
          legend.position = 'none',
          axis.title = element_text(face="bold"),
          plot.title = element_text(hjust = .5))
ggsave("visualization/figures/final/fig2A.pdf",height=4,width=4,units="in",dpi=150)
stopdata1 %>%
    group_by(MTurk_ID) %>% 
    summarise(est=mean(est),futur_social_iso_1=mean(futur_social_iso_1)) %>% 
    mutate(rank_est = rank(est)) %>%
    ggplot() +
    geom_smooth(aes(x=futur_social_iso_1,y=est),method="lm",se=T,colour="green3") +
    stat_summary_bin(aes(x=futur_social_iso_1,y=est),bins=100,geom="point") +
    scale_x_continuous("Anticipated Adherence to Social-Distancing") +    
    scale_y_continuous("Forecast of COVID-19 Cases",
                       labels=scales::number) +      
    theme_clean(base_family = "Times",base_size=11) +
    theme(plot.background = element_blank(),
          legend.position = 'none',
          axis.title = element_text(face="bold"),
          plot.title = element_text(hjust = .5))
ggsave("visualization/figures/final/fig2B.pdf",height=4,width=4,units="in",dpi=150)
summ_stopdata1 = stopdata1 %>%
    mutate(stop = ttsd) %>%
    group_by(MTurk_ID) %>%
    summarise(est = mean(est,na.rm=T),
              stop=mean(stop,na.rm=T),
              iso = mean(futur_social_iso_1,na.rm=T)) %>% ungroup()
cor.test(summ_stopdata1$est,summ_stopdata1$stop,method = "kendall")
cor.test(summ_stopdata1$est,summ_stopdata1$iso)

# Figure 2B
stopdata2 = pltdata2 %>%
    mutate(
        rank_est = rank(est),
        ttsd = time_to_stop_dist,
        stop_dist = as.Date.character(
            tryFormats = c("%m-%d-%Y"),
            ifelse(ttsd==1,"04-14-2020",
                   ifelse(ttsd==2,"04-21-2020",
                          ifelse(ttsd==3,"04-28-2020",
                                 ifelse(ttsd==4,"05-05-2020",
                                        ifelse(ttsd==5,"06-07-2020",
                                               ifelse(ttsd==6,"07-07-2020","08-07-2020"))))))),
        dtsd = ifelse(ttsd==1,7,
                   ifelse(ttsd==2,14,
                          ifelse(ttsd==3,21,
                                 ifelse(ttsd==4,28,
                                        ifelse(ttsd==5,60,
                                               ifelse(ttsd==6,90,120)))))))

stopdata2 %>%
    group_by(MTurk_ID) %>% 
    summarise(est=mean(est),ttsd=mean(ttsd)) %>% 
    mutate(rank_est = rank(est)) %>%
    ggplot() +
    geom_smooth(aes(x=ttsd,y=rank_est),method="lm",se=T,colour="green3") +
    stat_summary(aes(x=ttsd,y=rank_est)) +
    scale_x_continuous("Anticipated End to Social-Distancing",
                       breaks=1:7,
                       labels=c("1 week", "2 weeks", "3 weeks","1 month","2 months","3 months", "4+ months")) +
    scale_y_continuous("Ranked Forecast of COVID-19 Cases",
                       labels=scales::number) +      
    theme_clean(base_family = "Times",base_size=11) +
    theme(plot.background = element_blank(),
          legend.position = 'none',
          axis.title = element_text(face="bold"),
          plot.title = element_text(hjust = .5))
ggsave("visualization/figures/final/fig2C.pdf",height=4,width=4,units="in",dpi=150)
stopdata2 %>%
    group_by(MTurk_ID) %>% 
    summarise(est=mean(est),futur_social_iso_1=mean(futur_social_iso_1)) %>% 
    mutate(rank_est = rank(est)) %>%
    ggplot() +
    geom_smooth(aes(x=futur_social_iso_1,y=est),method="lm",se=T,colour="green3") +
    stat_summary_bin(aes(x=futur_social_iso_1,y=est),geom="point",bins=100) +
    scale_x_continuous("Anticipated Adherence to Social-Distancing") +    
    scale_y_continuous("Forecast of COVID-19 Cases",
                       labels=scales::number) +  
    coord_cartesian(ylim=c(400000,1000000)) +
    theme_clean(base_family = "Times",base_size=11) +
    theme(plot.background = element_blank(),
          legend.position = 'none',
          axis.title = element_text(face="bold"),
          plot.title = element_text(hjust = .5))
ggsave("visualization/figures/final/fig2D.pdf",height=4,width=4,units="in",dpi=150)

summ_stopdata2 = stopdata2 %>%
    mutate(stop = ttsd) %>%
    group_by(MTurk_ID) %>%
    summarise(est = mean(est,na.rm=T),
              stop=mean(stop,na.rm=T),
              iso = mean(futur_social_iso_1,na.rm=T)) %>% ungroup()
cor.test(summ_stopdata2$est,summ_stopdata2$stop,method = "kendall")
cor.test(summ_stopdata2$est,summ_stopdata2$iso)














d1 = pltdata1 %>%
    group_by(MTurk_ID,group) %>%
    summarise_all(mean)
d1 %>%
    ggplot(aes(x=group,y=past_social_iso_1,colour=group)) +
    stat_summary_bin() +
    scale_color_manual(values=c("red3","blue3")) +
    ggtitle("Study 1") +
    theme_clean(base_family = "Times",base_size=12) +
    theme(plot.background = element_blank(),
          legend.position = 'none',
          axis.title = element_text(face="bold"),
          plot.title = element_text(hjust = .5))
t.test(d1[d1$group=="ta",]$past_social_iso_1,d1[d1$group=="g",]$past_social_iso_1,conf.level = .89)

d1 %>%
    ggplot(aes(x=group,y=futur_social_iso_1,colour=group)) +
    stat_summary_bin() +
    scale_color_manual(values=c("red3","blue3")) +
    ggtitle("Study 1") +
    theme_clean(base_family = "Times",base_size=12) +
    theme(plot.background = element_blank(),
          legend.position = 'none',
          axis.title = element_text(face="bold"),
          plot.title = element_text(hjust = .5))
t.test(d1[d1$group=="ta",]$futur_social_iso_1,d1[d1$group=="g",]$futur_social_iso_1)


d2 = pltdata2 %>%
    group_by(MTurk_ID,group) %>%
    summarise_all(mean)
d2 %>%
    ggplot(aes(x=group,y=past_social_iso_1,colour=group)) +
    stat_summary_bin() +
    scale_color_manual(values=c("red3","blue3")) +
    ggtitle("Study 2") +
    theme_clean(base_family = "Times",base_size=12) +
    theme(plot.background = element_blank(),
          legend.position = 'none',
          axis.title = element_text(face="bold"),
          plot.title = element_text(hjust = .5))
t.test(d2[d2$group=="ta",]$past_social_iso_1,d2[d2$group=="g",]$past_social_iso_1)

d2 %>%
    ggplot(aes(x=group,y=futur_social_iso_1,colour=group)) +
    stat_summary_bin() +
    scale_color_manual(values=c("red3","blue3")) +
    ggtitle("Study 2") +
    theme_clean(base_family = "Times",base_size=12) +
    theme(plot.background = element_blank(),
          legend.position = 'none',
          axis.title = element_text(face="bold"),
          plot.title = element_text(hjust = .5))
t.test(d2[d2$group=="ta",]$futur_social_iso_1,d2[d2$group=="g",]$futur_social_iso_1)


d2 %>%
    mutate(ttsd = time_to_stop_dist,
        dtsd = ifelse(ttsd==1,7,
                         ifelse(ttsd==2,14,
                                ifelse(ttsd==3,21,
                                       ifelse(ttsd==4,28,
                                              ifelse(ttsd==5,60,
                                                     ifelse(ttsd==6,90,120))))))) %>%
    ggplot(aes(x=group,y=dtsd,colour=group)) +
    stat_summary_bin() +
    scale_color_manual(values=c("red3","blue3")) +
    ggtitle("Study 2") +
    theme_clean(base_family = "Times",base_size=12) +
    theme(plot.background = element_blank(),
          legend.position = 'none',
          axis.title = element_text(face="bold"),
          plot.title = element_text(hjust = .5))

