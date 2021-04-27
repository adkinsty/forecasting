library(tidyverse)

setwd("/Users/adkinsty/Box/side_projects/covid/Forecasting/")
data <- read_csv("data/prepped/all_filtered_data.csv")


new_data <- data %>%
  filter(exp < 3) %>%
  mutate(esim_value = ifelse(exp == 1 & day == 1, 196210,
                      ifelse(exp == 1 & day == 2, 396777,
                             ifelse(exp == 1 & day == 3, 802364,
                                    ifelse(exp == 2 & day == 1, 568222,
                                           ifelse(exp == 2 & day == 2, 800699, 1128288))))),
        misest = (abs(est - esim_value)/esim_value)*100)

new_data1 <- new_data %>%
  filter (exp == 1)

new_data2 <- new_data %>%
  filter (exp == 2)


data1 = tibble(
  date = c(1,2,3,4,5),
  deaths = c(1, 15, 48, 255, 1477),
  cases = c(68, 319, 2183, 19367, 96968),
  c = c(1, 1, 1, 1, 1))

ml <- lm(cases ~ date,data1)
me <- nls(formula = cases ~ a*exp(b*date),start = list('a'=1301,'b'=1),data=data1)
summary(ml)
summary(me)
AIC(ml)
AIC(me)

future = data1 %>% 
  bind_rows(tibble(date = c(5.428571429,5.857143,6.285714)))

future1 %>%
  mutate(epred = predict(me,newdata=future1),
         lpred = predict(ml,newdata=future1)) 

  
  ##study 2 data

data2 = tibble(
  date = c(1,2,3,4,5),
  deaths = c(38, 150, 1027, 5102, 12692),
  cases = c(1301, 9197, 68211, 215003, 393782),
  c = c(1, 1, 1, 1, 1))


ml <- lm(cases ~ date,data2)
me <- nls(formula = cases ~ a*exp(b*date),start = list('a'=1301,'b'=1),data=data2)
summary(ml)
summary(me)
AIC(ml)
AIC(me)


future2 = data2 %>% 
  bind_rows(tibble(date = c(5.428571429,5.857143,6.285714)))
future2 %>%
  mutate(epred = predict(me,newdata=future2),
         lpred = predict(ml,newdata=future2))


new_data %>%
  group_by(state_id,exp, group) %>%
  summarise(err = mean(misest)) %>%
  mutate(gmu = mean(err)) %>%
  group_by(state_id) %>% 
  mutate(smu = mean(err)) %>% ungroup() %>%
  mutate(new_err = err - smu + gmu) %>%
  group_by(exp,group) %>%
  summarise(err = mean(err), se = sd(new_err)/sqrt(n())) %>%
  mutate(exp = factor(exp,levels=c(1,2))) %>%
  ggplot(aes(x=exp,y=err,ymax=err+se,ymin=err-se,colour=group,group=interaction(group))) +
  geom_point(position=position_dodge(0),size=1.5) + 
  geom_errorbar(position=position_dodge(0),width=.1,linetype="solid") +
  geom_line(position=position_dodge(0),alpha=.75) +
  scale_x_discrete("Study") +
  scale_y_continuous("Forecasting Error (%)") +
  #facet_wrap(group~.,labeller = labeller(group=c("ta"="Table","g"="Graph"))) +
  scale_colour_manual("",values = c("#0072b2","#d55e00"),labels=c("Table","Graph")) +
  coord_cartesian(ylim=c(0,60)) +
  theme_tufte(base_family = "sans", base_size=12) +
  theme(axis.line = element_line(size=.25),
        legend.position = c(.25,.25))
ggsave("visualization/figures/trend_error.pdf",units="in",height=2.5,width=2.5,dpi=150)

#calculate mean forecasting error for each day. 
data1.1 <- new_data %>%
  filter (exp == 1 & day == 1)
data1.2 <- new_data %>%
  filter (exp == 1 & day == 2)
data1.3 <- new_data %>%
  filter (exp == 1 & day == 3)
data2.1 <- new_data %>%
  filter (exp == 2 & day == 1)
data2.2 <- new_data %>%
  filter (exp == 2 & day == 2)
data2.3 <- new_data %>%
  filter (exp == 2 & day == 3)

mean(data1.1$misest)
mean(data1.2$misest)
mean(data1.3$misest)
mean(data2.1$misest)
mean(data2.2$misest)
mean(data2.3$misest)
sd(data1.1$misest)
sd(data1.2$misest)
sd(data1.3$misest)
sd(data2.1$misest)
sd(data2.2$misest)
sd(data2.3$misest)
  
   