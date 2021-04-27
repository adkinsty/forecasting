library(tidyverse)
library(ggthemes)

# Colors:        Black      Orange     Skyblue   BluGreen    Yellow      Blue    Vermillion  RedPurple
all_colors <- c("#000000", "#e69f00", "#56b4e9", "#009e73", "#f0e442", "#0072b2", "#d55e00", "#cc79a7")


setwd("/Users/adkinsty/Box/side_projects/covid/Forecasting/")

dat <- read_csv("data/prepped/all_unfiltered_data.csv") 


dat %>%
  filter(exp %in% 1:2 & est < 4e7) %>%
  mutate(exp = ifelse(exp==1,"Study 1","Study 2")) %>%
  ggplot(aes(x=est)) + 
  geom_histogram(bins=100) +
  scale_x_continuous("Forecast") +
  geom_vline(aes(xintercept=lb*10),colour="red",linetype="dotted") +
  theme_tufte(base_size=10,base_family="sans") +
  facet_wrap(exp~.,scales="free") +
  theme(axis.line.x = element_line(size=.5,colour="grey"))
ggsave("visualization/figures/figS1A.pdf",height=2,width=5,dpi=150)

dat %>%
  filter(exp %in% 1:2 & est < (lb*10) & !is.na(decrease)) %>%
  mutate(exp = ifelse(exp==1,"Study 1","Study 2")) %>%
  ggplot(aes(x=est,fill=decrease)) + 
  geom_histogram(bins=45) +
  scale_x_continuous("Forecast") +
  theme_tufte(base_size=10,base_family="sans") +
  scale_fill_manual("No impossible\nforecasts criterion",values=c("#009e73","#cc79a7"),labels=c("Passed","Failed")) +
  facet_wrap(exp~.,scales="free") +
  theme(legend.position="none",
        axis.line.x = element_line(size=.5,colour="grey"))
ggsave("visualization/figures/figS1B.pdf",height=2,width=5,dpi=150)


dat2 <- read_csv("data/prepped/all_filtered_data.csv") 

dat2 %>%
  filter(exp %in% 1:2 & est < (lb*10) & !decrease) %>%
  mutate(exp = ifelse(exp==1,"Study 1","Study 2")) %>%
  ggplot(aes(x=est)) + 
  geom_histogram(bins=40) +
  scale_x_continuous("Forecast") +
  theme_tufte(base_size=10,base_family="sans") +
  facet_wrap(exp~.,scales="free") +
  theme(axis.line.x = element_line(size=.5,colour="grey"))
ggsave("visualization/figures/figS1C.pdf",height=2,width=5,dpi=150)

# dat %>%
#   filter(exp %in% c(1,2)) %>%
#   ggplot(aes(x=est/1e5)) +
#   geom_density() +
#   scale_x_continuous("Forecasted total cases (100k)", 
#                      limits=c(-1,10),
#                      breaks=0:20) +
#   facet_wrap(exp~group,nrow=4) +
#   theme_tufte(base_size=20,base_family = "sans")


# dat %>% 
#   mutate(exp=factor(exp,levels=c(1,2,3.1,3.2))) %>%
#   ggplot(aes(x=exp,fill=group,group=group)) +
#   geom_bar(position=position_dodge(.5),width=.5) + 
#   scale_x_discrete() +
#   theme_minimal(base_size=20)


# dat %>% 
#   filter(exp%in%c(1,2)) %>%
#   mutate(day=factor(day,1:3)) %>%
#   ggplot(aes(x=day,y=est/1e5,colour=group,group=group)) +
#   geom_violin(aes(group=day),colour="grey2",fill="grey",alpha=.5) +
#   geom_point(alpha=.5,position=position_dodge(width=.5)) +
#   facet_wrap(.~exp,labeller = labeller(exp=c("1"="Study 1","2"="Study 2"))) +
#   scale_x_discrete("Forecast Horizon (days)") +
#   scale_y_continuous("Forecast (100k)",breaks=seq(0,20,2)) +
#   scale_colour_manual("",values = c("#d55e00","#0072b2"),
#                     labels=c("Graph forecast","Table forecast")) +  
#   theme_tufte(base_size=12,base_family = "sans") +
#   theme(legend.position = c(.2,.8),
#         axis.line=element_line(size=.25),
#         legend.key.size = unit(5,"mm"),
#         legend.text = element_text(size=10),
#         plot.title = element_text(hjust = 0.5),
#         plot.subtitle = element_text(hjust = 0.5))
# ggsave("visualization/figures/goober_forecasts.pdf",height=4,width=5,dpi=150)
