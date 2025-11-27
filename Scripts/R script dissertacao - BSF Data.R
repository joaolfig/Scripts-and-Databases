library(dplyr)
library(tidyr)
library(readxl)

#rm(list=ls())

setwd("C:/Users/Joao arthur/OneDrive - Fundacao Getulio Vargas - FGV/Dissertação/Scripts-and-Databases")
#setwd("C:/Users/b435097/OneDrive - Fundacao Getulio Vargas - FGV/Dissertação/Scripts-and-Databases")


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
state_year$BSF_implemented <- ifelse(is.na(state_year$Deposit_strictness), 0, 1)

BSF_dataset <- state_year #[,c('state','year','BSF_implemented','Deposit_strictness','Withdrawal_strictness')]

#group by state year and fill NA upward
BSF_dataset <- BSF_dataset %>% group_by(state) %>% fill(everything(), .direction = "up")

# Make NA in any column into 0
BSF_dataset[is.na(BSF_dataset)] <- 0

# Make variables only account for when the BSF is in fact implemented

BSF_dataset$BSF_deposit_3level <- BSF_dataset$BSF_implemented + (BSF_dataset$BSF_implemented * BSF_dataset$Deposit_strictness)
BSF_dataset$BSF_withdrawal_3level <- BSF_dataset$BSF_implemented + (BSF_dataset$BSF_implemented * BSF_dataset$Withdrawal_strictness)

BSF_dataset$BSF_deposit_5level <- BSF_dataset$BSF_implemented * BSF_dataset$`Deposit Rule`
BSF_dataset$BSF_withdrawal_5level <- BSF_dataset$BSF_implemented * BSF_dataset$`Withdrawal Rule`


# Make both them into factors
BSF_dataset$BSF_deposit_3level <- factor(BSF_dataset$BSF_deposit_3level, levels = c(0, 1, 2)
                                         ,labels = c("0.No BSF", "1.Lenient rule", "2.Strict rule"))
BSF_dataset$BSF_withdrawal_3level <- factor(BSF_dataset$BSF_withdrawal_3level, levels = c(0, 1, 2)
                                            ,labels = c("0.No BSF", "1.Lenient rule", "2.Strict rule"))

BSF_dataset$BSF_deposit_5level <- factor(BSF_dataset$BSF_deposit_5level, levels = c(0, 1, 2, 3, 4)
                                         ,labels = c("0.No BSF"
                                                     ,"1.by legislative appropriation"
                                                     ,"2.in the event of a budget surplus"
                                                     ,"3.if revenue growth is positive"
                                                     ,"4.following a mathematical formula"))

BSF_dataset$BSF_withdrawal_5level <- factor(BSF_dataset$BSF_withdrawal_5level, levels = c(0, 1, 2, 3, 4)
                                            ,labels = c("0.No BSF"
                                                        ,"1.by legislative appropriation"
                                                        ,"2.in the event of a budget deficit"
                                                        ,"3.if supermajority legislative approval"
                                                        ,"4.following a mathematical formula"))


# I want to left join BSF_dataset to BSF_rules so the column BSF_rules$year based on the column state
BSF_dataset <- BSF_dataset %>% 
  left_join(BSF_rules[,c(1:2)], by = c('state')) %>%
  rename(year = year.x
    ,first_year_BSF = year.y) 



