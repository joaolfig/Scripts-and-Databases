library(haven)
library(dplyr)
library(tidyr)
library(zoo)

#rm(list=ls())

setwd("C:/Users/Joao arthur/OneDrive - Fundacao Getulio Vargas - FGV/Dissertação/Scripts-and-Databases")
#setwd("C:/Users/b435097/OneDrive - Fundacao Getulio Vargas - FGV/Dissertação/Scripts-and-Databases")

source('Scripts/R script dissertacao - Electoral Data Gubernatorial.R')

#rm all variables but vote_state

df_incumbent <- vote_state[,c('state','year','incumbent')]

#rm(list=setdiff(ls(), c('df_incumbent')))

setwd("C:/Users/Joao arthur/OneDrive - Fundacao Getulio Vargas - FGV/Dissertação/Scripts-and-Databases")
#setwd("C:/Users/b435097/OneDrive - Fundacao Getulio Vargas - FGV/Dissertação/Scripts-and-Databases")

df_approval_qtr_raw <- read_dta("Databases/SEAD 1 State Executive Approval Dataset/SEAD governor quarterly v1.dta")

df_approval_qtr <- df_approval_qtr_raw

df_approval_qtr$quarter_year <- as.yearqtr(gsub("\\.", " Q", round(df_approval_qtr$quarter_year,1)))

# Use us_2letters_code to merge columns and add a new column with the code
# State name in both columns is in column "State", and the code is in column "State_Code"
us_2letters_code <- read.csv("Databases/US state 2 letter codes/2_letter_codes.csv", sep = ",", encoding = "UTF-8")

df_approval_qtr <- df_approval_qtr %>%
  left_join(us_2letters_code, by = c("state" = "State")) %>%
  dplyr::select(-state) %>%
  rename(state = State_Code)

# Create lag variables for approval, disapproval and relative approval
df_approval_qtr <- df_approval_qtr %>%
  group_by(state) %>%
  mutate(Approval_Not_Smoothed_l1 = dplyr::lag(Approval_Not_Smoothed, 1)
         ,Disapproval_Not_Smoothed_l1 = dplyr::lag(Approval_Not_Smoothed, 2)
         ,Relative_Not_Smoothed_l1 = dplyr::lag(Approval_Not_Smoothed, 1)) %>%
  ungroup()

#Add the governors into the approval datasets

df_approval_qtr <- merge(df_approval_qtr, df_incumbent, by = c('state','year'), all.x = TRUE)

# Fill down the incumbent variable
df_approval_qtr <- df_approval_qtr %>%
  group_by(state) %>%
  fill(incumbent, .direction = "up") %>%
  ungroup() 

# Drop columns in years before 1944, bc this I don't have data on incumbents before that

df_approval_qtr <- df_approval_qtr %>%
  filter(year >= 1944)


####### Annual Data
#
# df_approval_annual_raw <- read_dta("Databases/SEAD 1 State Executive Approval Dataset/SEAD governor annual v1.dta")
#
#
# df_approval_annual <- df_approval_annual %>%
#   group_by(state) %>%
#   fill(incumbent, .direction = "up") %>%
#   ungroup() 
# df_approval_annual <- df_approval_annual %>%
#   left_join(us_2letters_code, by = c("state" = "State")) %>%
#   dplyr::select(-state) %>%
#   rename(state = State_Code)
# df_approval_annual <- df_approval_annual %>%
#   group_by(state) %>%
#   mutate(Approval_Not_Smoothed_lag1 = lag(Approval_Not_Smoothed, 1),
#          Approval_Not_Smoothed_lag2 = lag(Approval_Not_Smoothed, 2)) %>%
#   ungroup()
# df_approval_annual <- df_approval_annual_raw[,c('state', 'year','Approval_Smoothed','Approval_Not_Smoothed')]
# df_approval_annual <- df_approval_annual %>%
#   filter(year >= 1944)
#df_approval_annual <- merge(df_approval_annual, df_incumbent, by = c('state','year'), all.x = TRUE)

####### Calculate volatility by using SD of lead approval not smoothed
# 
# df_approval_qtr$fst_diff_Not_Smoothed <- abs(df_approval_qtr$Approval_Not_Smoothed_lag1 - df_approval_qtr$Approval_Not_Smoothed)
# 
# df_approval_qtr$fst_diff_Smoothed <- abs(df_approval_qtr$Approval_Smoothed_lag1 - df_approval_qtr$Approval_Smoothed)
# 
# colnames(df_approval_qtr)
# 
# df_approval_qtr <- df_approval_qtr %>%
#   arrange(state, year, quarter_year) %>% # Ensures correct order for lead()
#   group_by(state) %>% # Processes each state independently
#   mutate(
#     # Temporary columns lead observations
#     Approval_next_1 = dplyr::lead(Approval_Not_Smoothed, n = 1),
#     Approval_next_2 = dplyr::lead(Approval_Not_Smoothed, n = 2),
#     Approval_next_3 = dplyr::lead(Approval_Not_Smoothed, n = 3),
#   ) %>%
#   ungroup() %>%
#   rowwise() %>% # This tells dplyr to perform operations row by row
#   mutate(
#     sd_approval_next_year = sd(c(Approval_Not_Smoothed, Approval_next_1, Approval_next_2, Approval_next_3), na.rm = FALSE)
#   ) %>%
#   ungroup()  %>%
#   dplyr::select(-Approval_next_1, -Approval_next_2, -Approval_next_3)
# 
# 
# df_approval_qtr <- df_approval_qtr %>%
#   arrange(state, year, quarter_year) %>% # Ensures correct order for lead()
#   group_by(state) %>% # Processes each state independently
#   mutate(
#     # Temporary columns lead observations
#     Approval_next_1 = dplyr::lead(Approval_Not_Smoothed, n = 1),
#     Approval_next_2 = dplyr::lead(Approval_Not_Smoothed, n = 2),
#     Approval_next_3 = dplyr::lead(Approval_Not_Smoothed, n = 3),
#     Approval_next_4 = dplyr::lead(Approval_Not_Smoothed, n = 3),
#     Approval_next_5 = dplyr::lead(Approval_Not_Smoothed, n = 3),
#     Approval_next_6 = dplyr::lead(Approval_Not_Smoothed, n = 3),
#     Approval_next_7 = dplyr::lead(Approval_Not_Smoothed, n = 3),
#   ) %>%
#   ungroup() %>%
#   rowwise() %>% # This tells dplyr to perform operations row by row
#   mutate(
#     sd_approval_next_2years = sd(c(Approval_Not_Smoothed, Approval_next_1, Approval_next_2, Approval_next_3,
#                                    Approval_next_4, Approval_next_5, Approval_next_6, Approval_next_7), na.rm = FALSE)
#   ) %>%
#   ungroup()  %>%
#   dplyr::select(-Approval_next_1, -Approval_next_2, -Approval_next_3,
#          -Approval_next_4, -Approval_next_5, -Approval_next_6, -Approval_next_7)

  
  