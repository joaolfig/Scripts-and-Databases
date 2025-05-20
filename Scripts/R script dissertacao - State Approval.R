library(haven)
library(dplyr)
library(tidyr)

#rm(list=ls())

setwd("C:/Users/Joao arthur/OneDrive - Fundacao Getulio Vargas - FGV/Dissertação/Scripts-and-Databases")

df_approval_qtr_raw <- read_dta("Databases/SEAD 1 State Executive Approval Dataset/SEAD governor quarterly v1.dta")

df_approval_qtr <- df_approval_qtr_raw[,c('state', 'year', 'quarter','quarter_year','qtr',
'Approval_Smoothed','Approval_Not_Smoothed')]

df_approval_annual_raw <- read_dta("Databases/SEAD 1 State Executive Approval Dataset/SEAD governor annual v1.dta")

df_approval_annual <- df_approval_annual_raw[,c('state', 'year','Approval_Smoothed','Approval_Not_Smoothed')]

us_2letters_code <- read.csv("Databases/US state 2 letter codes/2_letter_codes.csv", sep = ",", encoding = "UTF-8")



# Use us_2letters_code to merge columns and add a new column with the code
# State name in both columns is in column "State", and the code is in column "State_Code"
df_approval_qtr <- df_approval_qtr %>%
  left_join(us_2letters_code, by = c("state" = "State")) %>%
  dplyr::select(-state) %>%
  rename(state = State_Code)

df_approval_annual <- df_approval_annual %>%
  left_join(us_2letters_code, by = c("state" = "State")) %>%
  dplyr::select(-state) %>%
  rename(state = State_Code)


#create _lag1 and _lag2 variables for approval smoothed
df_approval_qtr <- df_approval_qtr %>%
  group_by(state) %>%
  mutate(Approval_Not_Smoothed_lag1 = lag(Approval_Not_Smoothed, 1),
         Approval_Not_Smoothed_lag2 = lag(Approval_Not_Smoothed, 2)) %>%
  ungroup()

df_approval_annual <- df_approval_annual %>%
  group_by(state) %>%
  mutate(Approval_Not_Smoothed_lag1 = lag(Approval_Not_Smoothed, 1),
         Approval_Not_Smoothed_lag2 = lag(Approval_Not_Smoothed, 2)) %>%
  ungroup()



