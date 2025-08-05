library(haven)
library(stringr)
library(dplyr)
library(texreg) # Screenreg
library(ivreg) # IV
library(AER) # Mostra se a IV é boa
library(QuantPsyc) # LM com standardized beta coefficients
library(tableone)
library(marginaleffects)

setwd("C:/Users/Joao arthur/OneDrive - Fundacao Getulio Vargas - FGV/Dissertação/Scripts-and-Databases")

rm(list=ls())

############### Load Data ################
source('Scripts/R script dissertacao - Electoral Data Gubernatorial.R')
source('Scripts/R script dissertacao - Electoral Data Presidential.R')
source('Scripts/R script dissertacao - Employment gap.R')
source('Scripts/R script dissertacao - Oil prices.R')
source('Scripts/R script dissertacao - BSF Data.R')
source('Scripts/R script dissertacao - State Approval.R')
source('Scripts/R script dissertacao - Garro (2021).R')

rm(list=setdiff(ls(), c("vote_state",'president_incumbent_ts'
                        ,"employment_states_gap","oil_deflated"
                        ,"BSF_rules",'BSF_dataset','df_garro'
                        ,'df_approval_qtr','df_approval_annual')))

############### Merge Column and create variables ###############
# Merge all data into one dataframe
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

#left join with df_approval_annual
df_analysis <- merge(df_analysis, df_approval_annual[,c('state','year','Approval_Not_Smoothed','Approval_Not_Smoothed_lag1','Approval_Not_Smoothed_lag2')], by = c('state','year'), all.x = TRUE)

#left join with df_garro
df_analysis <- merge(df_analysis, df_garro[,c('state','year','oilprice','lagloggsppc'
                                              
                                              
                                              ,'lag2logoil_iv_two')], by = c('state','year'), all.x = TRUE)


#df_analysis <- merge(df_analysis, df_garro, by = c('state','year'), all.x = TRUE)
#where df_analysis$dm_garro == NA, 0
#df_analysis$dm_garro[is.na(df_analysis$dm_garro)] <- 0


# Create vars
df_analysis$incumbent_party <- ifelse(df_analysis$incumbent_party == 'Democratic', 1, 0)

############### Analysis #######################################

m1 <- lm(reelection_candidate~
   log_oil_deflated_change*state + factor(year)
  ,data=df_analysis[df_analysis$incumbent_running==1,]
)

m2 <- lm(reelection_candidate~
           +log_oil_deflated_change*BSF_implementation
           +log_oil_deflated_change*state + factor(year)
         ,data=df_analysis[df_analysis$incumbent_running==1,]
)

m3 <- lm(reelection_candidate~
           +log_oil_deflated_change*Deposit_strictness
         +log_oil_deflated_change*state + factor(year)
         ,data=df_analysis[df_analysis$incumbent_running==1,]
)

m4 <- lm(reelection_candidate~
           +log_oil_deflated_change*Withdrawal_strictness
         +log_oil_deflated_change*state + factor(year)
         ,data=df_analysis[df_analysis$incumbent_running==1,]
)

screenreg(list(m1,m2,m3,m4),omit.coef = "state|year",digits=3)

plot_predictions(m1,condition = c("log_oil_deflated_change"))
plot_predictions(m2,condition = c("log_oil_deflated_change"
                                  ,"BSF_implementation"))
plot_predictions(m3,condition = c("log_oil_deflated_change"
                                  ,"Deposit_strictness"))
plot_predictions(m4,condition = c("log_oil_deflated_change"
                                  ,"Withdrawal_strictness"))

colnames(df_analysis)
