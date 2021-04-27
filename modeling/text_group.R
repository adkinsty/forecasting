library(tidyverse)

pal <- c("#000000", # Black
         "#cc79a7", # Reddish purple
         "#f0e442", # Yellow
         "#009e73", # Bluish green
         "#e69f00", # Orange
         "#d55e00", # Vermillion
         "#56b4e9", # Sky blue
         "#0072b2") # Blue

setwd("/Users/adkinsty/Box/side_projects/covid/Forecasting/")

dat <- read_csv("data/prepped/filtered_data.csv") %>%
  filter(outcome == "cCases" & exp %in% c(1,2)) %>%
  mutate(day = factor(day,levels=1:3))


plt_est <- function(data) {
  exp.labs <- rep(c("More exponential",
                    "Less exponential"),2)
  names(exp.labs) <- c(1,2,3.1, 3.2)
  p <- data %>% 
    mutate(day = factor(day,levels=c(1,2,3))) %>%
    mutate(group = factor(group, levels=c("g", "ta", "t"))) %>%
    group_by(day,group,exp) %>%
    summarise(se = sd(est/1e5)/sqrt(n()),mu=mean(est/1e5),truth=mean(truth/1e5)) %>%
    ggplot(aes(x=day,y=mu,ymax=mu+se,ymin=mu-se,group=group,colour=group)) +
    geom_point() +
    geom_line(alpha=.75) +
    geom_errorbar(width=.05) +
    geom_line(aes(y=truth,colour="z"),linetype="dashed") +
    geom_point(aes(y=truth,colour="z")) +
    facet_wrap(exp~.,nrow=1,labeller = labeller(exp=exp.labs),scales="free_y") +
    theme_tufte(base_size = 15,base_family = "sans") +
    scale_x_discrete("Forecast horizon (days)",labels=c("3", "6", "9")) +
    scale_y_continuous("Total Cases (100k)",breaks=seq(0,100,1)) +
    scale_colour_manual("",values = c("#d55e00","#f0e442", "#0072b2", "black"),
                        labels=c("Graph forecast","Text forecast","Table forecast", "True growth")) +
    theme(legend.position = c(.15,.85),
          axis.line=element_line(size=.25),
          legend.key.size = unit(5,"mm"),
          legend.text = element_text(size=10),
          plot.title = element_text(hjust = 0.5),
          plot.subtitle = element_text(hjust = 0.5))
  return(p)
}
dat %>% plt_est()
ggsave("visualization/figures/text_forecasts.jpg", units = "in", height = 3, width = 6)


