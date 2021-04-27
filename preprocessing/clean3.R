library(tidyverse)
library(tidyselect)

setwd("/Users/adkinsty/Box/side_projects/covid/Forecasting1/")

# exclude first 2 'junk' rows (e.g., pilot subjects)
data_input = read_csv(file = "data/raw_data/raw_data_study3.csv")[-c(1:2),]

num_center <- 3.5
risk_center <- 4

data_renamed = data_input %>%
  mutate(id = 1:n(),
         total_time = data_input$`Duration (in seconds)`) %>%
  rowwise() %>%
  mutate(
    trump = trump_approval_1,
    fox = ifelse(news == 3, 1, 0),
    # FILTERS
    president = Q129,
    effort = as.numeric(Effort_1),
    attention = Q122_7,
    start_en = Q165,
    prim_lng = Q166,
    
    # group
    group_id = ifelse(!is.na(Confirm_case_est_1),
                      "ta_1",
                      ifelse(!is.na(Q252_1),
                             "g_1",
                             ifelse(!is.na(Q261_1),
                                    "ta_2",
                                    "g_2"))),
    study_id = ifelse(group_id%in%c("ta_1","g_1"),"1","2"),

    cCases_1_est_ta_1 = as.numeric(gsub(",","",x =Confirm_case_est_1)),
    cCases_2_est_ta_1 = as.numeric(gsub(",","",x =Confirm_case_est_2)),
    cCases_3_est_ta_1 = as.numeric(gsub(",","",x =Confirm_case_est_3)),
    cCases_1_conf_ta_1 = Confirm_C_Conf_1,
    cCases_2_conf_ta_1 = Confirm_C_Conf_2,
    cCases_3_conf_ta_1 = Confirm_C_Conf_3,
    aCases_1_est_ta_1 = as.numeric(gsub(",","",x =Actual_case_est_1)),
    aCases_2_est_ta_1 = as.numeric(gsub(",","",x =Actual_case_est_2)),
    aCases_3_est_ta_1 = as.numeric(gsub(",","",x =Actual_case_est_3)),
    aCases_1_conf_ta_1 = Act_case_conf_1,
    aCases_2_conf_ta_1 = Act_case_conf_2,
    aCases_3_conf_ta_1 = Act_case_conf_3,
    deaths_1_est_ta_1 = as.numeric(gsub(",","",x =Deaths_est_2)),
    deaths_2_est_ta_1 = as.numeric(gsub(",","",x =Deaths_est_3)),
    deaths_3_est_ta_1 = as.numeric(gsub(",","",x =Deaths_est_4)),
    deaths_1_conf_ta_1 = Deaths_conf_1,
    deaths_2_conf_ta_1 = Deaths_conf_2,
    deaths_3_conf_ta_1 = Deaths_conf_3,
    cCases_1_est_g_1 = as.numeric(gsub(",","",x =Q252_1)),
    cCases_2_est_g_1 = as.numeric(gsub(",","",x =Q252_2)),
    cCases_3_est_g_1 = as.numeric(gsub(",","",x =Q252_3)),
    cCases_1_conf_g_1 = Q253_1,
    cCases_2_conf_g_1 = Q253_2,
    cCases_3_conf_g_1 = Q253_3,
    aCases_1_est_g_1 = as.numeric(gsub(",","",x =Q255_1)),
    aCases_2_est_g_1 = as.numeric(gsub(",","",x =Q255_2)),
    aCases_3_est_g_1 = as.numeric(gsub(",","",x =Q255_3)),
    aCases_1_conf_g_1 = Q256_1,
    aCases_2_conf_g_1 = Q256_2,
    aCases_3_conf_g_1 = Q256_3,
    deaths_1_est_g_1 = as.numeric(gsub(",","",x =Q257_2)),
    deaths_2_est_g_1 = as.numeric(gsub(",","",x =Q257_3)),
    deaths_3_est_g_1 = as.numeric(gsub(",","",x =Q257_4)),
    deaths_1_conf_g_1 = Q258_1,
    deaths_2_conf_g_1 = Q258_2,
    deaths_3_conf_g_1 = Q258_3,
    cCases_1_est_ta_2 = as.numeric(gsub(",","",x =Q261_1)),
    cCases_2_est_ta_2 = as.numeric(gsub(",","",x =Q261_2)),
    cCases_3_est_ta_2 = as.numeric(gsub(",","",x =Q261_3)),
    cCases_1_conf_ta_2 = Q262_1,
    cCases_2_conf_ta_2 = Q262_2,
    cCases_3_conf_ta_2 = Q262_3,
    aCases_1_est_ta_2 = as.numeric(gsub(",","",x =Q264_1)),
    aCases_2_est_ta_2 = as.numeric(gsub(",","",x =Q264_2)),
    aCases_3_est_ta_2 = as.numeric(gsub(",","",x =Q264_3)),
    aCases_1_conf_ta_2 = Q265_1,
    aCases_2_conf_ta_2 = Q265_2,
    aCases_3_conf_ta_2 = Q265_3,
    deaths_1_est_ta_2 = as.numeric(gsub(",","",x =Q266_2)),
    deaths_2_est_ta_2 = as.numeric(gsub(",","",x =Q266_3)),
    deaths_3_est_ta_2 = as.numeric(gsub(",","",x =Q266_4)),
    deaths_1_conf_ta_2 = Q267_1,
    deaths_2_conf_ta_2 = Q267_2,
    deaths_3_conf_ta_2 = Q267_3,
    cCases_1_est_g_2 = as.numeric(gsub(",","",x =Q296_1)),
    cCases_2_est_g_2 = as.numeric(gsub(",","",x =Q296_2)),
    cCases_3_est_g_2 = as.numeric(gsub(",","",x =Q296_3)),
    cCases_1_conf_g_2 = Q297_1,
    cCases_2_conf_g_2 = Q297_2,
    cCases_3_conf_g_2 = Q297_3,
    aCases_1_est_g_2 = as.numeric(gsub(",","",x =Q299_1)),
    aCases_2_est_g_2 = as.numeric(gsub(",","",x =Q299_2)),
    aCases_3_est_g_2 = as.numeric(gsub(",","",x =Q299_3)),
    aCases_1_conf_g_2 = Q300_1,
    aCases_2_conf_g_2 = Q300_2,
    aCases_3_conf_g_2 = Q300_3,
    deaths_1_est_g_2 = as.numeric(gsub(",","",x =Q301_2)),
    deaths_2_est_g_2 = as.numeric(gsub(",","",x =Q301_3)),
    deaths_3_est_g_2 = as.numeric(gsub(",","",x =Q301_4)),
    deaths_1_conf_g_2 = Q302_1,
    deaths_2_conf_g_2 = Q302_2,
    deaths_3_conf_g_2 = Q302_3,
    
    rt_ta_1 = `Q155_Page Submit`,
    rt_g_1 = `Q156_Page Submit`,
    rt_ta_2 = `Q157_Page Submit`,
    rt_g_2 = `Q303_Page Submit`,
    
    prob_of_case = prob_of_contract_1,
    prob_of_hosp = Q133_1,
    prob_of_death = Q132_1,
    time_to_stop_dist = Q167_1,
    max_new_cases = as.numeric(gsub(",","",x =Q168)),
    know_someone = ifelse(know_someone==2,0,1),
    zip = as.numeric(Zip_Code),
    age = as.numeric(Age),
    age_bin = ifelse(age > 60, 'old', ifelse(age < 30, 'young', 'middle')),
    edu = Ed_Level,
    edu_mom = Ed_Level_Mom,
    gen_health = ifelse(general_health_1=="Very Good",5,
                        ifelse(general_health_1 == "Quite Good",4,
                               ifelse(general_health_1=="Neither good nor poor",3,
                                      ifelse(general_health_1=="Quite Poor",2,1)))),
    gen_anxiety = ifelse(gen_anxiety_1=="Nearly every day",4,
                         ifelse(gen_anxiety_1=="More than half the days",3,
                                ifelse(gen_anxiety_1=="Several days",2,1))),
    cons1 = as.numeric(Conserv_scale_1), 
    cons2 = -as.numeric(Conserv_scale_2), 
    cons3 = -as.numeric(Conserv_scale_3), 
    cons4 = as.numeric(Conserv_scale_4),
    cons5 = -as.numeric(Conserv_scale_5), 
    cons6 = as.numeric(Conserv_scale_6),
    cons7 = as.numeric(Conserv_scale_7),
    conserv_mu = mean(c(cons1,cons2,cons3,cons4,cons5,cons6,cons7),na.rm=T),
    
    risk1 = as.numeric(substr(Q122_1,1,1)) - risk_center,
    risk2=as.numeric(substr(Q122_2,1,1)) - risk_center,
    risk3=as.numeric(substr(Q122_3,1,1)) - risk_center,
    risk4=as.numeric(substr(Q122_4,1,1)) - risk_center,
    risk5= -(as.numeric(substr(Q122_5,1,1)) - risk_center),
    risk6=as.numeric(substr(Q122_6,1,1)) - risk_center,
    risk_mu = mean(c(risk1,risk2,risk3,risk4,risk5,risk6),na.rm=T),
    num1 = as.numeric(substr(Numeracy_1,1,1)) - num_center,
    num2=as.numeric(substr(Numeracy_2,1,1))- num_center,
    num3=as.numeric(substr(Numeracy_3,1,1))-num_center,
    num4=as.numeric(substr(Numeracy_4,1,1))- num_center, 
    num5=as.numeric(substr(Q124_1,1,1))- num_center, 
    num6=as.numeric(substr(Q125_1,1,1))-num_center,
    num7= -(as.numeric(substr(Q126_1,1,1))-num_center), 
    num8=as.numeric(substr(Q127_1,1,1))-num_center,
    num_mu = mean(c(num1,num2,num3,num4, num5,num6,num7,num8))) %>% ungroup() %>%
  mutate(cCases_1_est = ifelse(group_id=="ta_1",cCases_1_est_ta_1,
                               ifelse(group_id=="g_1",cCases_1_est_g_1,
                                      ifelse(group_id=="ta_2",cCases_1_est_ta_2,
                                             cCases_1_est_g_2))),
         cCases_2_est = ifelse(group_id=="ta_1",cCases_2_est_ta_1,
                               ifelse(group_id=="g_1",cCases_2_est_g_1,
                                      ifelse(group_id=="ta_2",cCases_2_est_ta_2,
                                             cCases_2_est_g_2))),
         cCases_3_est = ifelse(group_id=="ta_1",cCases_3_est_ta_1,
                               ifelse(group_id=="g_1",cCases_3_est_g_1,
                                      ifelse(group_id=="ta_2",cCases_3_est_ta_2,
                                             cCases_3_est_g_2))),
         aCases_1_est = ifelse(group_id=="ta_1",aCases_1_est_ta_1,
                               ifelse(group_id=="g_1",aCases_1_est_g_1,
                                      ifelse(group_id=="ta_2",aCases_1_est_ta_2,
                                             aCases_1_est_g_2))),
         aCases_2_est = ifelse(group_id=="ta_1",aCases_2_est_ta_1,
                               ifelse(group_id=="g_1",aCases_2_est_g_1,
                                      ifelse(group_id=="ta_2",aCases_2_est_ta_2,
                                             aCases_2_est_g_2))),
         aCases_3_est = ifelse(group_id=="ta_1",aCases_3_est_ta_1,
                               ifelse(group_id=="g_1",aCases_3_est_g_1,
                                      ifelse(group_id=="ta_2",aCases_3_est_ta_2,
                                             aCases_3_est_g_2))),
         deaths_1_est = ifelse(group_id=="ta_1",deaths_1_est_ta_1,
                               ifelse(group_id=="g_1",deaths_1_est_g_1,
                                      ifelse(group_id=="ta_2",deaths_1_est_ta_2,
                                             deaths_1_est_g_2))),
         deaths_2_est = ifelse(group_id=="ta_1",deaths_2_est_ta_1,
                               ifelse(group_id=="g_1",deaths_2_est_g_1,
                                      ifelse(group_id=="ta_2",deaths_2_est_ta_2,
                                             deaths_2_est_g_2))),
         deaths_3_est = ifelse(group_id=="ta_1",deaths_3_est_ta_1,
                               ifelse(group_id=="g_1",deaths_3_est_g_1,
                                      ifelse(group_id=="ta_2",deaths_3_est_ta_2,
                                             deaths_3_est_g_2))),
         cCases_1_conf = ifelse(group_id=="ta_1",cCases_1_conf_ta_1,
                               ifelse(group_id=="g_1",cCases_1_conf_g_1,
                                      ifelse(group_id=="ta_2",cCases_1_conf_ta_2,
                                             cCases_1_conf_g_2))),
         cCases_2_conf = ifelse(group_id=="ta_1",cCases_2_conf_ta_1,
                               ifelse(group_id=="g_1",cCases_2_conf_g_1,
                                      ifelse(group_id=="ta_2",cCases_2_conf_ta_2,
                                             cCases_2_conf_g_2))),
         cCases_3_conf = ifelse(group_id=="ta_1",cCases_3_conf_ta_1,
                               ifelse(group_id=="g_1",cCases_3_conf_g_1,
                                      ifelse(group_id=="ta_2",cCases_3_conf_ta_2,
                                             cCases_3_conf_g_2))),
         aCases_1_conf = ifelse(group_id=="ta_1",aCases_1_conf_ta_1,
                               ifelse(group_id=="g_1",aCases_1_conf_g_1,
                                      ifelse(group_id=="ta_2",aCases_1_conf_ta_2,
                                             aCases_1_conf_g_2))),
         aCases_2_conf = ifelse(group_id=="ta_1",aCases_2_conf_ta_1,
                               ifelse(group_id=="g_1",aCases_2_conf_g_1,
                                      ifelse(group_id=="ta_2",aCases_2_conf_ta_2,
                                             aCases_2_conf_g_2))),
         aCases_3_conf = ifelse(group_id=="ta_1",aCases_3_conf_ta_1,
                               ifelse(group_id=="g_1",aCases_3_conf_g_1,
                                      ifelse(group_id=="ta_2",aCases_3_conf_ta_2,
                                             aCases_3_conf_g_2))),
         deaths_1_conf = ifelse(group_id=="ta_1",deaths_1_conf_ta_1,
                               ifelse(group_id=="g_1",deaths_1_conf_g_1,
                                      ifelse(group_id=="ta_2",deaths_1_conf_ta_2,
                                             deaths_1_conf_g_2))),
         deaths_2_conf = ifelse(group_id=="ta_1",deaths_2_conf_ta_1,
                               ifelse(group_id=="g_1",deaths_2_conf_g_1,
                                      ifelse(group_id=="ta_2",deaths_2_conf_ta_2,
                                             deaths_2_conf_g_2))),
         deaths_3_conf = ifelse(group_id=="ta_1",deaths_3_conf_ta_1,
                               ifelse(group_id=="g_1",deaths_3_conf_g_1,
                                      ifelse(group_id=="ta_2",deaths_3_conf_ta_2,
                                             deaths_3_conf_g_2))),
         rt = ifelse(group_id=="ta_1",rt_ta_1,
                     ifelse(group_id=="g_1",rt_g_1,
                            ifelse(group_id=="ta_2",rt_ta_2,rt_g_2))))


data_select = data_renamed %>%
  dplyr::select(Finished,id,age,age_bin,MTurk_ID,group_id,study_id,
                zip,president,attention,effort,
    cCases_1_est,cCases_2_est,cCases_3_est,
    cCases_1_conf,cCases_2_conf,cCases_3_conf,
    aCases_1_est,aCases_2_est,aCases_3_est,
    aCases_1_conf,aCases_2_conf,aCases_3_conf,
    deaths_1_est,deaths_2_est,deaths_3_est,
    deaths_1_conf,deaths_2_conf,deaths_3_conf,rt,
    past_social_iso_1,futur_social_iso_1, time_to_stop_dist) %>%
  filter(Finished == 1) %>%
  mutate(lb = ifelse(study_id == "1",96968,393782)) %>%
  mutate(decrease = lb > cCases_1_est | 
           cCases_1_est > cCases_2_est | cCases_2_est > cCases_3_est |
           aCases_1_est > aCases_2_est | aCases_2_est > aCases_3_est |
           deaths_1_est > deaths_2_est | deaths_2_est > deaths_3_est)

data_long_est = data_select %>%
  pivot_longer(
    cols = c("cCases_1_est","cCases_2_est","cCases_3_est"),
    names_to = c("outcome","day"),
    names_pattern = "(.*)_(.*)_est",
    values_to = "estimate")
data_long_conf = data_select %>%
  pivot_longer(
    cols = c("cCases_1_conf","cCases_2_conf","cCases_3_conf"),
    names_to = c("outcome","day"),
    names_pattern = "(.*)_(.*)_conf",
    values_to = "confidence")
data_long = data_long_est %>%
  cbind(data_long_conf[c('confidence',"cCases_1_est","cCases_2_est","cCases_3_est")])


write_csv(data_long,"data/prepped/data_long_study3.csv")


zip_data <- read.csv("data/extra/loc/uszips.csv") %>% dplyr::select(c(zip,state_id))

filtered_wide = data_select %>%
  mutate(lb = ifelse(study_id == "1",96968,393782)) %>%
  mutate(N = n()) %>%
  filter(age > 18) %>%
  mutate(N1 = n()) %>%
  merge(zip_data,by="zip") %>%
  mutate(N2 = n()) %>%
  filter(!(decrease)) %>%
  mutate(N3 = n()) %>%
  filter(attention == 6) %>%
  mutate(N4 = n()) %>%
  filter(grepl("trump",president,ignore.case = TRUE) |
           grepl("don",president,ignore.case = TRUE)) %>%
  mutate(N5 = n()) %>%
  filter(effort > 5) %>%
  mutate(N6 = n()) %>%
  filter(as.numeric(rt) > 30) %>%
  mutate(N7 = n())

data_long_est = filtered_wide %>%
  pivot_longer(
    cols = c("cCases_1_est","cCases_2_est","cCases_3_est"),
    names_to = c("outcome","day"),
    names_pattern = "(.*)_(.*)_est",
    values_to = "estimate")
data_long_conf = filtered_wide %>%
  pivot_longer(
    cols = c("cCases_1_conf","cCases_2_conf","cCases_3_conf"),
    names_to = c("outcome","day"),
    names_pattern = "(.*)_(.*)_conf",
    values_to = "confidence")
data_long = data_long_est %>% cbind(data_long_conf['confidence'])

filtered_long = data_long %>%
  mutate(N7.5 = n()) %>%
  filter(estimate <= lb*10) %>%
  mutate(N8 = n(),
         N9 = length(unique(id)))

filtered_long %>% dplyr::select(contains("N",F))

  
filtered_long %>% group_by(group_id) %>% summarise(n=length(unique(id)))
filtered_long %>% group_by(study_id) %>% summarise(n=length(unique(id)))




