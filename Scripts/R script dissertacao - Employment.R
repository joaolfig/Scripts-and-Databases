library(mFilter)
library(ggplot2)
library(tidyverse)
library(dplyr)
library(stringr)
library(zoo)
library(plm)

#rm(list=ls())

#setwd("C:/Users/Joao arthur/OneDrive - Fundacao Getulio Vargas - FGV/Dissertação/Scripts-and-Databases")

employment <- read.csv("Databases/BLS Data/Employment/Not Adjusted 39-2025/Monthly/employment.csv", header = TRUE, sep = ";")

employment$observation_date <- as.Date(employment$observation_date, format = "%d/%m/%Y")

employment <- employment %>%
  pivot_longer(
    cols = -c(observation_date)
    ,names_to = "state"
    ,values_to = "employment"
  )

#convert date to yearqtr
employment$quarter_year <-as.yearqtr(employment$observation_date)

employment_qtr <- employment

#Aggregate employment data in employment_qtr to quarterly frequency by mean
employment_qtr <- employment_qtr %>%
  group_by(state, quarter_year) %>%
  summarise(employment = mean(employment, na.rm = TRUE), .groups = 'drop') %>%
  filter(!is.na(employment))


employment_qtr <- pdata.frame(employment_qtr, index = c("state", "quarter_year"))

employment_qtr$quarter_year <-as.yearqtr(employment_qtr$quarter_year)

# Eu não sei se multiplica ou não por 100 aqui na fórmula de log change
employment_qtr <- employment_qtr %>%
  arrange(state, quarter_year) %>%
  group_by(state) %>%
  mutate(employment_logchange = log(employment) - dplyr::lag(log(employment)) 
         ,employment_logchange_l1 = dplyr::lag(employment_logchange, 1)
         ,employment_logchange_l2 = dplyr::lag(employment_logchange, 2)
         ,employment_logchange_l3 = dplyr::lag(employment_logchange, 3)
         ,employment_logchange_l4 = dplyr::lag(employment_logchange, 4)) %>%
  ungroup()

# 
# # Spillover effect
# employment_qtr <- employment_qtr %>%
#   #select out the column of US employment
#   dplyr::filter(state != "US") %>%
#   group_by(quarter_year) %>% 
#   mutate(
#       employment_share = employment / sum(employment, na.rm = TRUE)
#       ,employment_spillover = sum(employment_logchange * employment_share, na.rm = TRUE) 
#       - (employment_logchange * employment_share)
#                    ) %>%
#   ungroup()


  
