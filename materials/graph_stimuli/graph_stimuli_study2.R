library(ggplot2)
library(dplyr)
library(tidyr)
library(ggthemes)
# library(gganimate)
# library(transformr)

setwd("/Users/adkinsty/Box/side_projects/covid/Forecasting/")

data = tibble(
            date = c(1,2,3,4,5),
            deaths = c(38, 150, 1027, 5102, 12692),
            cases = c(1301, 9197, 68211, 215003, 393782),
            c = c(1, 1, 1, 1, 1))

deaths.plt = data %>% ggplot(aes(x = factor(date), y = deaths, label = deaths, group=c)) + 
    geom_point(size = 2, color = "#377BB5") +
    geom_text(nudge_y = 500,nudge_x=-.25, color = "#377BB5",size=3.5) +
    geom_line(color = "#377BB5") +
    scale_x_discrete("Date",
                     labels = c(expression(paste("Mar. ",11)), 
                                expression(paste("Mar. ",18)),
                                expression(paste("Mar. ",25)),
                                expression(paste("Apr. ",1)),
                                expression(paste("Apr. ",7)))) +
    scale_y_continuous("Total Deaths") +
    theme_wsj(base_size = 10, base_family = "sans", color = "gray") +
    theme(axis.line.y = element_line(size=.5),
          axis.text = element_text(face = "plain"),
          axis.title = element_text(size = 11, family = "sans",face="bold"),
          panel.grid.major.y = element_blank())
ggsave("materials/graph_stimuli/total_deaths_by_date_study2.pdf",height = 3,width=4,units="in")



require(scales)
cases.plt = data %>% ggplot(aes(x = factor(date), y = cases, label = cases, group=c)) + 
    geom_point(size = 2, color = "black") +
    geom_text(nudge_y = 15000, nudge_x = -.38, color = "black", size=3) +
    geom_line(color = "black") +
    scale_x_discrete("Date",
                     labels = c(expression(paste("Mar. ",11)), 
                                expression(paste("Mar. ",18)),
                                expression(paste("Mar. ",25)),
                                expression(paste("Apr. ",1)),
                                expression(paste("Apr. ",7)))) +
    scale_y_continuous("Total Cases",labels = comma) +
    theme_wsj(base_size = 10, base_family = "sans", color = "white") +
    theme(axis.line.y = element_line(size=.5),
          axis.text = element_text(face = "plain"),
          axis.title = element_text(size = 11, family = "sans",face="bold"),
          panel.grid.major.y = element_blank())
ggsave("materials/graph_stimuli/total_cases_by_date_study2.pdf",height = 3,width=4,units="in")








ml <- lm(cases ~ date,data)
me <- nls(formula = cases ~ a*exp(b*date),start = list('a'=1301,'b'=1),data=data)
summary(ml)
summary(me)
AIC(ml)
AIC(me)

future = data %>% 
    bind_rows(tibble(date = c(5.428571429,5.857143,6.285714)))
future %>%
    mutate(epred = predict(me,newdata=future),
           lpred = predict(ml,newdata=future)) %>%
    ggplot(aes(x=date)) +
    geom_point(aes(y=cases),size=2) +
    geom_line(aes(y=lpred),colour="green4") +
    geom_line(aes(y=epred),colour="red") +
    theme_bw(base_size=15) +
    scale_x_continuous(breaks=c(sort(unique(future$date))),
                       labels=1:8)

