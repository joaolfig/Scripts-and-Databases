library(haven)
library(dplyr)
library(tidyverse)

rm(list=ls())

setwd('C:/Users/Joao arthur/OneDrive - Fundacao Getulio Vargas - FGV/Dissertação/Scripts-and-Databases')
source('Scripts/Garro (2021).R')
setwd('C:/Users/Joao arthur/OneDrive - Fundacao Getulio Vargas - FGV/Dissertação/Scripts-and-Databases')
source('Scripts/R script dissertacao - Oil production.R')
source('Scripts/R script dissertacao - Employment gap.R')

rm(list=setdiff(ls(), c("df_garro","employment_states_gap","oil_data")))

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

# multiply GSP by 1,000,000 and difference by 1,000
df_iv <- df_iv %>%
  mutate(gsp = gsp * 1000000,
         Difference = Difference * 1000)

df_iv$IV <- (df_iv$Difference * df_iv$oilprice) / df_iv$gsp
  
colnames(df_iv)


#left join df_iv with employment_states_gap by state and year
df_iv <- df_iv %>%
  left_join(employment_states_gap, by = c("state" = "state", "year" = "year"))


summary(lm(competence_employment ~ lag(oilprice):state , data = df_iv))

summary(lm(competence_employment ~ state + oilprice , data = df_iv))

# Count number of observations by UF
df_iv %>%
  group_by(state) %>%
  summarise(n = n()) %>%
  arrange(desc(n))

#Check min and max values for year using summary
summary(as.numeric(df_iv$year))

