# Tyler Adkins

library(ggplot2)
library(dplyr)
library(tidyr)
library(ggthemes)
library(gganimate)
library(transformr)
library(extraDistr)

setwd("/Users/adkinsty/Box/side_projects/covid/Forecasting/")


lin_pred = function(x) {
    b0 = 0
    b1 = 19500
    sigma = 500
    y = b0 + b1*x #+ rdnorm(1,0,sigma)
    return(y)
}
exp_pred = function(x) {
    b0 = 1200
    b1 = 3
    sigma = 500
    y = b0*b1^{x-1} #+ rdnorm(1,0,sigma)
    return(y)
}
x = c(1,2,3,4,5)#,5+(3/7),5+(6/7),5+(9/7))
data = tibble(x = x) %>% 
    rowwise() %>%
    mutate(lin = ceiling(lin_pred(x)),
           exp = ceiling(exp_pred(x))) %>% ungroup() %>%
    mutate(date = as.Date(c("2020-03-26",
                            "2020-04-02",
                            "2020-04-09",
                            "2020-04-16",
                            "2020-04-23")))
require(scales)
data %>% ggplot(aes(x = date, y = exp, label = exp, group=1)) + 
    geom_point(size = 2, color = "#377BB5") +
    geom_text(nudge_y = 6000,nudge_x=-1, color = "#377BB5",size=3.5) +
    geom_line(color = "#377BB5") +
    scale_x_date("Date",
                 breaks=as.Date(c("2020-03-26",
                                  "2020-04-02",
                                  "2020-04-09",
                                  "2020-04-16",
                                  "2020-04-23")),
                 labels=c("Mar.26","Apr.2","Apr.9","Apr.16","Apr.23")) +
    scale_y_continuous("Total Cases",
                       labels = comma, 
                       breaks=c(0,2e4,4e4,6e4,8e4,1e5),
                       limits=c(0,1.2e5)) +    
    theme_wsj(base_size = 9, base_family = "sans", color = "gray") +
    theme(axis.line.y = element_line(size=.5),
          axis.text = element_text(face = "plain"),
          axis.title = element_text(size = 10, family = "sans",face="bold"),
          panel.grid = element_blank())
ggsave("materials/graph_stimuli/total_cases_by_date_study4_exp.pdf",units="in",height=4,width=5,dpi=96)
data %>% ggplot(aes(x = date, y = lin, label = lin, group=1)) + 
    geom_point(size = 2, color = "#377BB5") +
    geom_text(nudge_y = 7000,nudge_x=.2, color = "#377BB5",size=3.5) +
    geom_line(color = "#377BB5") +
    scale_x_date("Date",
                 breaks=as.Date(c("2020-03-26",
                                  "2020-04-02",
                                  "2020-04-09",
                                  "2020-04-16",
                                  "2020-04-23")),
                 labels=c("Mar.26","Apr.2","Apr.9","Apr.16","Apr.23")) +
    scale_y_continuous("Total Cases",
                       labels = comma, 
                       breaks=c(0,2e4,4e4,6e4,8e4,1e5),
                       limits=c(0,1.2e5)) +
    theme_wsj(base_size = 9, base_family = "sans", color = "gray") +
    theme(axis.line.y = element_line(size=.5),
          axis.text = element_text(face = "plain"),
          axis.title = element_text(size = 10, family = "sans",face="bold"),
          panel.grid = element_blank())
ggsave("materials/graph_stimuli/total_cases_by_date_study4_lin.pdf",units="in",height=4,width=5,dpi=96)

