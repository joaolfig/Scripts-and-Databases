library(haven)
library(dplyr)
library(tidyverse)

rm(list=ls())

setwd('C:/Users/Joao arthur/OneDrive - Fundacao Getulio Vargas - FGV/Dissertação/Scripts and Databases')
source('Scripts/Garro (2021).R')
setwd('C:/Users/Joao arthur/OneDrive - Fundacao Getulio Vargas - FGV/Dissertação/Scripts and Databases')
source('Scripts/R script dissertacao - Oil production.R')


df_garro_lean <- df_garro[,c('st','year','oilprod','oilprice','brent'
                             ,'dubai','gsp','oil_gsp','oil_gspt')]

#rename st to state
df_garro_lean <- df_garro_lean %>%
  rename(state = st)

#convert state and Year to factor in both datasets
df_garro_lean$state <- as.factor(df_garro_lean$state)
df_garro_lean$year <- as.factor(df_garro_lean$year)

oil_data$State <- as.factor(oil_data$State)
oil_data$Year <- as.factor(oil_data$Year)

#left join the two datasets by df_garro_lean by State and Year
df_iv <- oil_data %>%
  left_join(df_garro_lean, by = c("State" = "state", "Year" = "year"))

df_iv <- df_garro_lean %>%
  left_join(oil_data, by = c("state" = "State", "year" = "Year"))

