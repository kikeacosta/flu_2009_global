rm(list=ls())
source("code/00_functions.R")

dt <- 
  read_rds("data_inter/stmf.rds")



dt %>% 
  ggplot()+
  geom_point(aes(date, deaths))