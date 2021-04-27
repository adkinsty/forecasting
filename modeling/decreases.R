library(tidyverse)

setwd("/users/adkinsty/Box/side_projects/covid/Forecasting/")

dat <- read_csv("data/prepped/all_unfiltered_data.csv") %>% 
    mutate(decrease = est1 < lb | est2 < est1 | est3 < est2) %>%
    filter(est < 10*lb & !is.na(decrease))

colnames(dat)

dat %>% 
    filter(decrease) %>%
    ggplot(aes(x=factor(day),y=est,group=id)) +
    geom_line(alpha=.5) +
    geom_hline(aes(yintercept=lb),colour="red") +
    facet_wrap(exp~.,scales="free")

dat %>% filter(decrease & exp == 3.2 & (est2 < 1e6 & est2>750000)) %>%
    ggplot(aes(x=factor(day),y=est,group=MTurk_ID,colour=MTurk_ID)) +
    geom_line(alpha=.5) +
    # geom_hline(aes(yintercept=lb),colour="red") +
    facet_wrap(exp~.,scales="free")

dat %>% 
    ggplot(aes(x=est,fill=decrease)) +
    geom_histogram() +
    facet_wrap(.~exp,ncol=4,scales="free")

dat %>% 
    filter(decrease) %>%
    ggplot(aes(x=est,fill=est<100)) +
    geom_histogram() +
    facet_wrap(.~exp,ncol=4,scales="free")


dat %>% 
    filter(decrease) %>%
    mutate(n = n()) %>%
    mutate(x = ifelse(est < 10, 0, ifelse(est > 10 & est < 100, 2, ifelse(est > 100 & est < 1000, 3, ifelse(est > 1000 & est < 100000, 4, 5))))) %>% 
    group_by(x) %>%
    summarise(n_group = n()/ mean(n))

dat %>%
    ggplot(aes(x=decrease,fill=group)) +
    geom_bar(position=position_dodge(1)) +
    facet_wrap(.~exp)