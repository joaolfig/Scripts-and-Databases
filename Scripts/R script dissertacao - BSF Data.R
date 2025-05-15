library(readxl)

#rm(list=ls())

setwd("C:/Users/Joao arthur/OneDrive - Fundacao Getulio Vargas - FGV/Dissertação/Scripts-and-Databases")

#Read CSV Databases/BSF Data/BSF strictness Wagner 2005.xlsx
BSF_rules <- read_excel("Databases/BSF Data/BSF strictness Wagner 2005.xlsx", sheet = "BSF strictness data")

BSF_rules$Deposit_strictness <- ifelse(BSF_rules$`Deposit Rule` > 2, 1, 0)
BSF_rules$Withdrawal_strictness <- ifelse(BSF_rules$`Withdrawal Rule` > 2, 1, 0)

colnames(BSF_rules)[1:2] <- c('state','year')
colnames(BSF_rules)[2] <- "year"

#Create a dataframe with the combination of each state in unique(BSF$state)
# And each year from 1948 to 1997 
state_year <- expand.grid(state = unique(BSF_rules$state), year = 1948:2020)


state_year <- merge(state_year, BSF_rules, by = c('state','year'), all.x = TRUE)

#group by state year and fill down NA
state_year <- state_year %>% group_by(state) %>% fill(everything())
state_year$BSF_implementation <- ifelse(is.na(state_year$Deposit_strictness), 0, 1)

BSF_dataset <- state_year[,c('state','year','BSF_implementation','Deposit_strictness','Withdrawal_strictness')]

#group by state year and fill NA upward
BSF_dataset <- BSF_dataset %>% group_by(state) %>% fill(everything(), .direction = "up")
