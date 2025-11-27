library(haven)
library(dplyr)
library(tidyr)
library(zoo)


setwd("C:/Users/Joao arthur/OneDrive - Fundacao Getulio Vargas - FGV/Dissertação/Scripts-and-Databases")

df_approval <- read_dta("Databases/SEAD 1 State Executive Approval Dataset/SEAD governor annual v1.dta")

us_2letters_code <- read.csv("Databases/US state 2 letter codes/2_letter_codes.csv", sep = ",", encoding = "UTF-8")

df_approval <- df_approval %>%
  left_join(us_2letters_code, by = c("state" = "State")) %>%
  dplyr::select(-state) %>%
  rename(state = State_Code)

# Filter only the following states AK, LA, ND, NM, MT, MT, TX, WY
df_approval <- df_approval %>%
  filter(state %in% c("AK", "LA", "ND", "NM", "MT", "TX", "WY"))

source('Scripts/R script dissertacao - Oil prices.R')

oil_deflated <- oil_deflated %>%
  mutate(
    oil_deflated_std = (oil_deflated - mean(oil_deflated, na.rm = TRUE)) / sd(oil_deflated, na.rm = TRUE)
    ,oil_deflated_std = round(oil_deflated_std,2)
  )

df_approval <- df_approval %>%
  left_join(oil_deflated, by = c("year" = "year"))

m <- lm(Approval_Smoothed ~ oil_deflated_std + state
        + dplyr::lag(Approval_Smoothed,1), data = df_approval) 
screenreg(m)

#unique states
unique(df_approval$state)
