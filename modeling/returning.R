library(tidyverse)
library(brms)
library(MASS)
library(ggthemes)

options(mc.cores = parallel::detectCores())
parallel:::setDefaultClusterOptions(setup_strategy = "sequential")
options(contrasts = c("contr.sdif","contr.sdif"))

setwd("/Users/adkinsty/Box/side_projects/covid/Forecasting/")

all_data <- read_csv("data/prepped/all_filtered_data.csv") 

exp1 <- all_data %>% filter(exp == 1)
exp2 <- all_data %>% filter(exp == 2)

data <- all_data %>% 
  filter(exp %in% c(1,2)) %>%
  mutate(return = ifelse(MTurk_ID %in% exp1$MTurk_ID & MTurk_ID %in% exp2$MTurk_ID, TRUE, FALSE),
         err = est - truth,
         p_err = err / truth,
         abs_p_err = abs(err) / truth,
         day = factor(day,1:3),
         exp = factor(exp,1:2),
         group = factor(group,c("ta","g")))

data %>% 
  ggplot(aes(x=abs_p_err,colour=interaction(exp,group),group=interaction(exp,group))) +
  geom_freqpoly()

formula <- bf(abs_p_err ~ 1 + day + group*exp*return + (1 | state_id))
priors <- c(set_prior("student_t(3, 0, 2.5)",class="b"),
              set_prior("student_t(3, 0, 2.5)",class="sd"),
              set_prior("gamma(0.01, 0.01)",class = "shape"))
model <- brm(formula,data,family = Gamma(link="log"),prior = priors,
             iter = 1e4, warmup = 5e3)
# write_rds(model,"modeling/returning.rds")


data %>%
  group_by(state_id,exp,return,group) %>%
  summarise(err = mean(abs_p_err)*100) %>%
  mutate(gmu = mean(err)) %>%
  group_by(state_id) %>% 
  mutate(smu = mean(err)) %>% ungroup() %>%
  mutate(new_err = err - smu + gmu) %>%
  group_by(exp,return,group) %>%
  summarise(err = mean(err), se = sd(new_err)/sqrt(n())) %>%
  mutate(return = factor(return,levels=c(TRUE,FALSE))) %>%
  ggplot(aes(x=exp,y=err,ymax=err+se,ymin=err-se,colour=group,shape=return,
             linetype=return,group=interaction(group,return))) +
  geom_point(position=position_dodge(.25),size=1.5) + 
  geom_errorbar(position=position_dodge(.25),width=.1,linetype="solid") +
  geom_line(position=position_dodge(.25),alpha=.75) +
  scale_x_discrete("Study") +
  scale_y_continuous("Forecasting Error (%)") +
  facet_wrap(group~.,labeller = labeller(group=c("ta"="Table","g"="Graph"))) +
  scale_linetype_manual("",labels=c("Returning","New"),values=c("solid","blank")) +
  scale_shape_manual("",labels=c("Returning","New"),values=c("circle","triangle")) +
  scale_colour_manual("",values = c("#0072b2","#d55e00"),labels=c("Table","Graph"),guide=F) +
  coord_cartesian(ylim=c(0,60)) +
  theme_tufte(base_family = "sans", base_size=12) +
  theme(axis.line = element_line(size=.25),
        legend.position = "top")
ggsave("visualization/figures/fig3A.pdf",units="in",height=3,width=3,dpi=150)
