library(haven)
library(dplyr)
library(zoo)

setwd("C:/Users/Joao arthur/OneDrive - Fundacao Getulio Vargas - FGV/Dissertação/Scripts-and-Databases")

controls_singer <- read_dta("Databases/Replication Files for Singer (2023)/controls for table 3.dta")

controls_singer$quarter_year <- as.yearqtr(gsub("\\.", " Q", round(controls_singer$quarter_year,1)))

controls_singer <- controls_singer %>%
  arrange(state, quarter_year) %>%
  group_by(state) %>%
  mutate(unemployment_state_l1 = dplyr::lag(unemployment_state, 1)
         ,unemployment_state_l2 = dplyr::lag(unemployment_state, 2)
         ,governorresignedthatquarter_l1 = dplyr::lag(governorresignedthatquarter, 1)
         ,governorresignedthatquarter_l2 = dplyr::lag(governorresignedthatquarter, 2)
         ,governordiedthatquarter_l1 = dplyr::lag(governordiedthatquarter, 1)
         ,governordiedthatquarter_l2 = dplyr::lag(governordiedthatquarter, 2)) %>%
  ungroup()

# Use us_2letters_code to merge columns and add a new column with the code
# State name in both columns is in column "State", and the code is in column "State_Code"
us_2letters_code <- read.csv("Databases/US state 2 letter codes/2_letter_codes.csv", sep = ",", encoding = "UTF-8")

controls_singer <- controls_singer %>%
  left_join(us_2letters_code, by = c("state" = "State")) %>%
  dplyr::select(-state) %>%
  rename(state = State_Code)