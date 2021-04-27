library(tidyverse)
library(tidyselect)

setwd("/Users/adkinsty/Box/side_projects/covid/Forecasting1/")

# exclude first 9 'junk' rows (e.g., pilot subjects)
data_input = read_csv(file = "data/raw_data/raw_data_study1.csv")[-c(1:9),] 

num_center <- 3.5
risk_center <- 4

data_renamed <- data_input %>%
  mutate(id = 1:n()) %>%
  rowwise() %>%
  mutate(
    president = Q129,
    effort = as.numeric(Effort_1),
    attention = Q122_7,
    start_en = Q165,
    prim_lng = Q166,
    
    group_id = ifelse(!is.na(Confirm_case_est_1),"t",
                      ifelse(!is.na(Q252_1),"ta","g")),

    cCases_1_est_t = as.numeric(gsub(",","",x =Confirm_case_est_1)),
    cCases_2_est_t = as.numeric(gsub(",","",x =Confirm_case_est_2)),
    cCases_3_est_t = as.numeric(gsub(",","",x =Confirm_case_est_3)),
    cCases_1_conf_t = Confirm_C_Conf_1,
    cCases_2_conf_t = Confirm_C_Conf_2,
    cCases_3_conf_t = Confirm_C_Conf_3,
    aCases_1_est_t = as.numeric(gsub(",","",x =Actual_case_est_1)),
    aCases_2_est_t = as.numeric(gsub(",","",x =Actual_case_est_2)),
    aCases_3_est_t = as.numeric(gsub(",","",x =Actual_case_est_3)),
    aCases_1_conf_t = Act_case_conf_1,
    aCases_2_conf_t = Act_case_conf_2,
    aCases_3_conf_t = Act_case_conf_3,
    deaths_1_est_t = as.numeric(gsub(",","",x =Deaths_est_2)),
    deaths_2_est_t = as.numeric(gsub(",","",x =Deaths_est_3)),
    deaths_3_est_t = as.numeric(gsub(",","",x =Deaths_est_4)),
    deaths_1_conf_t = Deaths_conf_1,
    deaths_2_conf_t = Deaths_conf_2,
    deaths_3_conf_t = Deaths_conf_3,


    cCases_1_est_ta = as.numeric(gsub(",","",x =Q252_1)),
    cCases_2_est_ta = as.numeric(gsub(",","",x =Q252_2)),
    cCases_3_est_ta = as.numeric(gsub(",","",x =Q252_3)),
    cCases_1_conf_ta = Q253_1,
    cCases_2_conf_ta = Q253_2,
    cCases_3_conf_ta = Q253_3,
    aCases_1_est_ta = as.numeric(gsub(",","",x =Q255_1)),
    aCases_2_est_ta = as.numeric(gsub(",","",x =Q255_2)),
    aCases_3_est_ta = as.numeric(gsub(",","",x =Q255_3)),
    aCases_1_conf_ta = Q256_1,
    aCases_2_conf_ta = Q256_2,
    aCases_3_conf_ta = Q256_3,
    deaths_1_est_ta = as.numeric(gsub(",","",x =Q257_2)),
    deaths_2_est_ta = as.numeric(gsub(",","",x =Q257_3)),
    deaths_3_est_ta = as.numeric(gsub(",","",x =Q257_4)),
    deaths_1_conf_ta = Q258_1,
    deaths_2_conf_ta = Q258_2,
    deaths_3_conf_ta = Q258_3,
    
    cCases_1_est_g = as.numeric(gsub(",","",x =Q261_1)),
    cCases_2_est_g = as.numeric(gsub(",","",x =Q261_2)),
    cCases_3_est_g = as.numeric(gsub(",","",x =Q261_3)),
    cCases_1_conf_g = Q262_1,
    cCases_2_conf_g = Q262_2,
    cCases_3_conf_g = Q262_3,
    aCases_1_est_g = as.numeric(gsub(",","",x =Q264_1)),
    aCases_2_est_g = as.numeric(gsub(",","",x =Q264_2)),
    aCases_3_est_g = as.numeric(gsub(",","",x =Q264_3)),
    aCases_1_conf_g = Q265_1,
    aCases_2_conf_g = Q265_2,
    aCases_3_conf_g = Q265_3,
    deaths_1_est_g = as.numeric(gsub(",","",x =Q266_2)),
    deaths_2_est_g = as.numeric(gsub(",","",x =Q266_3)),
    deaths_3_est_g = as.numeric(gsub(",","",x =Q266_4)),
    deaths_1_conf_g = Q267_1,
    deaths_2_conf_g = Q267_2,
    deaths_3_conf_g = Q267_3,
  
    rt_t = `Q155_Page Submit`,
    rt_ta = `Q156_Page Submit`,
    rt_g = `Q157_Page Submit`,
    
    prob_of_case = prob_of_contract_1,
    prob_of_hosp = Q133_1,
    prob_of_death = Q132_1,
    time_to_stop_dist = Q167_1,
    max_new_cases = as.numeric(gsub(",","",x =Q168)),
    
    know_someone = ifelse(know_someone=="2",0,1),
    
    zip = as.numeric(Zip_Code),
    age = as.numeric(Age),
    age_bin = ifelse(age > 60, 'old', ifelse(age < 30, 'young', 'middle')),
    
    edu = Ed_Level,
    edu_mom = Ed_Level_Mom,
    
    gen_health = as.numeric(general_health_1),
    gen_anxiety = as.numeric(gen_anxiety_1),
    
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
  mutate(cCases_1_est = ifelse(group_id=="ta",cCases_1_est_ta,ifelse(group_id=="t", cCases_1_est_t, cCases_1_est_g)),
         cCases_2_est = ifelse(group_id=="ta",cCases_2_est_ta,ifelse(group_id=="t", cCases_2_est_t, cCases_2_est_g)),
         cCases_3_est = ifelse(group_id=="ta",cCases_3_est_ta,ifelse(group_id=="t", cCases_3_est_t, cCases_3_est_g)),
         aCases_1_est = ifelse(group_id=="ta",aCases_1_est_ta,ifelse(group_id=="t", aCases_1_est_t, aCases_1_est_g)),
         aCases_2_est = ifelse(group_id=="ta",aCases_2_est_ta,ifelse(group_id=="t", aCases_2_est_t, aCases_2_est_g)),
         aCases_3_est = ifelse(group_id=="ta",aCases_3_est_ta,ifelse(group_id=="t", aCases_3_est_t, aCases_3_est_g)),
         deaths_1_est = ifelse(group_id=="ta",deaths_1_est_ta,ifelse(group_id=="t", deaths_1_est_t, deaths_1_est_g)),
         deaths_2_est = ifelse(group_id=="ta",deaths_2_est_ta,ifelse(group_id=="t", deaths_2_est_t, deaths_2_est_g)),
         deaths_3_est = ifelse(group_id=="ta",deaths_3_est_ta,ifelse(group_id=="t", deaths_3_est_t, deaths_3_est_g)),
         cCases_1_conf = ifelse(group_id=="ta",cCases_1_conf_ta,ifelse(group_id=="t", cCases_1_conf_t, cCases_1_conf_g)),
         cCases_2_conf = ifelse(group_id=="ta",cCases_2_conf_ta,ifelse(group_id=="t", cCases_2_conf_t, cCases_2_conf_g)),
         cCases_3_conf = ifelse(group_id=="ta",cCases_3_conf_ta,ifelse(group_id=="t", cCases_3_conf_t, cCases_3_conf_g)),
         aCases_1_conf = ifelse(group_id=="ta",aCases_1_conf_ta,ifelse(group_id=="t", aCases_1_conf_t, aCases_1_conf_g)),
         aCases_2_conf = ifelse(group_id=="ta",aCases_2_conf_ta,ifelse(group_id=="t", aCases_2_conf_t, aCases_2_conf_g)),
         aCases_3_conf = ifelse(group_id=="ta",aCases_3_conf_ta,ifelse(group_id=="t", aCases_3_conf_t, aCases_3_conf_g)),
         deaths_1_conf = ifelse(group_id=="ta",deaths_1_conf_ta,ifelse(group_id=="t", deaths_1_conf_t, deaths_1_conf_g)),
         deaths_2_conf = ifelse(group_id=="ta",deaths_2_conf_ta,ifelse(group_id=="t", deaths_2_conf_t,deaths_2_conf_g)),
         deaths_3_conf = ifelse(group_id=="ta",deaths_3_conf_ta,ifelse(group_id=="t", deaths_3_conf_t,deaths_3_conf_g)),
         rt = ifelse(group_id=="ta",rt_ta,ifelse(group_id=="t", rt_t,rt_g)))

data_select = data_renamed %>%
  dplyr::select(
    Finished,id,age,age_bin,MTurk_ID,group_id,zip,president,attention,effort,
    cCases_1_est,cCases_2_est,cCases_3_est,
    cCases_1_conf,cCases_2_conf,cCases_3_conf,
    aCases_1_est,aCases_2_est,aCases_3_est,
    aCases_1_conf,aCases_2_conf,aCases_3_conf,
    deaths_1_est,deaths_2_est,deaths_3_est,
    deaths_1_conf,deaths_2_conf,deaths_3_conf,
    rt,
    past_social_iso_1,futur_social_iso_1, time_to_stop_dist) %>%
  #filter(group_id !='t') %>%
  filter(Finished == 1) %>%
  mutate(lb = 96968) %>%
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

write_csv(data_long,"data/prepped/data_long_study1.csv")


zip_data <- read.csv("data/extra/loc/uszips.csv") %>% dplyr::select(c(zip,state_id))

filtered_wide = data_select %>%
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

