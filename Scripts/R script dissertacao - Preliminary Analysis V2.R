library(haven)
library(stringr)
library(dplyr)
library(texreg) # Screenreg
library(ivreg) # IV
library(AER) # Mostra se a IV é boa
library(QuantPsyc) # LM com standardized beta coefficients

setwd("C:/Users/Joao arthur/OneDrive - Fundacao Getulio Vargas - FGV/Dissertação/Scripts-and-Databases")

rm(list=ls())

############### Load Data ################
source('Scripts/R script dissertacao - Electoral Data Gubernatorial.R')
source('Scripts/R script dissertacao - Electoral Data Presidential.R')
source('Scripts/R script dissertacao - Employment gap.R')
source('Scripts/R script dissertacao - Oil prices.R')
source('Scripts/R script dissertacao - BSF Data.R')
source('Scripts/R script dissertacao - State Approval.R')

df_garro <- read_dta("Databases/Replication Files for Garro (2021)/polar_main_dataset.dta")

rm(list=setdiff(ls(), c("vote_state",'president_incumbent_ts'
                        ,"employment_states_gap","oil_deflated"
                        ,"BSF_rules",'BSF_dataset','df_garro'
                        ,'df_approval_qtr','df_approval_annual')))

############### Merge Column and create variables ###############
# Merge all data into one dataframe
str(vote_state)
df_analysis <- vote_state[,c('state','year','incumbent_party','winner_party'
                             ,'dem_pct_votes_2pty','rep_pct_votes_2pty'
                             ,'dem_pct_2pty_change','rep_pct_2pty_change'
                             ,'incumbent_pct_2pty','incumbent_pct_2pty_change'
                             ,'reelection_party','reelection_candidate'
                             ,'incumbent_running','challenger_running')]

#left join with president_incumbent_ts
df_analysis <- merge(df_analysis, president_incumbent_ts[,c('year','president_party','presidential_election_dm')], by = c('year'), all.x = TRUE)

# Create a dummy for midterm punishment
df_analysis$midterm_punishment <- ifelse(df_analysis$presidential_election_dm == 0 &
                                           df_analysis$incumbent_party == df_analysis$president_party, 1, 0)

# Variable to check if governor and president are from the same party 1 if yes, -1 if no
df_analysis$gov_presi_same_party <- ifelse(df_analysis$incumbent_party == df_analysis$president_party, 1, -1)

#left join with employment_states_gap
df_analysis <- merge(df_analysis, employment_states_gap[,c('state','year','employment_gap','employment_gap_change','employment_gap_national','employment_gap_change_national','competence_employment')], by = c('state','year'), all.x = TRUE)

#left join with Oil data
df_analysis <- merge(df_analysis, oil_deflated[,c('year','oil','oil_deflated','log_oil_deflated_change')], by = c('year'), all.x = TRUE)

#left join with BSF data
df_analysis <- merge(df_analysis, BSF_dataset[,c('state','year','BSF_implementation'
                                                 ,'Deposit_strictness','Withdrawal_strictness')]
                     , by = c('state','year'), all.x = TRUE)

df_analysis$incumbent_party <- ifelse(df_analysis$incumbent_party == 'Democratic', 1, 0)

#rename st to state in df_garro
df_garro <- df_garro %>%
  rename(state = st)
df_garro <- df_garro[,c('state','year','oilprice','reserves','gsp','gsppc')]
df_garro$oilprice_lag1 <- lag(df_garro$oilprice, 1)
df_garro$oilprice_lag2 <- lag(df_garro$oilprice, 2)
df_garro$gsp_lag1 <- lag(df_garro$gsp, 1)
df_garro$gsp_lag2 <- lag(df_garro$gsp, 2)

df_analysis <- merge(df_analysis, df_garro, by = c('state','year'), all.x = TRUE)

#merge with df_approval_annual
df_approval_annual$year <- as.numeric(df_approval_annual$year)

df_analysis <- merge(df_analysis, df_approval_annual[,c('state','year','Approval_Not_Smoothed','Approval_Not_Smoothed_lag1','Approval_Not_Smoothed_lag2')], by = c('state','year'), all.x = TRUE)

colnames(df_analysis)
############### Analysis #######################################

screenreg(lm(incumbent_running ~
       + Approval_Not_Smoothed_lag1
       + year + state,
     data = df_analysis), omit = "state|year",)


screenreg(lm(reelection_candidate ~
             + incumbent_party
             + Approval_Not_Smoothed_lag1
             + midterm_punishment
             + year + state,
             data = df_analysis[df_analysis$incumbent_party == 1,]
             ), omit = "state|year",)


screenreg(lm(Approval_Not_Smoothed ~
             + Approval_Not_Smoothed_lag1  
             + incumbent_party
             + gov_presi_same_party
#             + midterm_punishment
             + oilprice_lag1
             + year + state
             ,data = df_analysis), omit = "state|year",)



