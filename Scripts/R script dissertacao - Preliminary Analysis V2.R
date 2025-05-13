library(stringr)
library(dplyr)
library(estimatr)
library(texreg)
library(stargazer)

setwd("C:/Users/Joao arthur/OneDrive - Fundacao Getulio Vargas - FGV/Dissertação/Scripts and Databases")

rm(list=ls())

############### Load Data ################
source('Scripts/R script dissertacao - Electoral Data Gubernatorial.R')
source('Scripts/R script dissertacao - Electoral Data Presidential.R')
source('Scripts/R script dissertacao - Employment gap.R')
source('Scripts/R script dissertacao - Oil prices.R')
source('Scripts/R script dissertacao - BSF Data.R')

rm(list=setdiff(ls(), c("vote_state",'president_incumbent_ts'
                        ,"employment_states_gap","oil_deflated"
                        ,"BSF_rules",'BSF_dataset')))

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
df_analysis <- merge(df_analysis, oil_deflated[,c('year','log_oil_deflated_change')], by = c('year'), all.x = TRUE)


#left join with BSF data
df_analysis <- merge(df_analysis, BSF_dataset[,c('state','year','BSF_implementation'
                                                 ,'Deposit_strictness','Withdrawal_strictness')]
                     , by = c('state','year'), all.x = TRUE)

df_analysis$incumbent_party <- ifelse(df_analysis$incumbent_party == 'Democratic', 1, 0)

############### Summary Statistics #######################################





############### EXOGENOUS OIL EFFECT ON POLITICAL OUTCOMES ############### 

m_oil_on_running <- lm(incumbent_running~log_oil_deflated_change:state + lag(log_oil_deflated_change):state,data=df_analysis)

m_oil_on_voteshare <- lm(incumbent_pct_2pty_change ~ log_oil_deflated_change + lag(log_oil_deflated_change),data=df_analysis[df_analysis$incumbent_running == 1,])
m_oil_on_reelection <- lm(reelection_candidate~log_oil_deflated_change + lag(log_oil_deflated_change),data=df_analysis[df_analysis$incumbent_running == 1,])
m_oil_on_reelection_pty <- lm(reelection_party~log_oil_deflated_change + lag(log_oil_deflated_change),data=df_analysis[df_analysis$incumbent_running == 1,])

# With Fixed Effects
m_oil_on_running_FE <- lm(incumbent_running~log_oil_deflated_change + lag(log_oil_deflated_change) + state + year,data=df_analysis)

m_oil_on_voteshare_FE <- lm(incumbent_pct_2pty_change ~ log_oil_deflated_change + lag(log_oil_deflated_change) + state + year,data=df_analysis[df_analysis$incumbent_running == 1,])
m_oil_on_reelection_FE <- lm(reelection_candidate~log_oil_deflated_change + lag(log_oil_deflated_change) + state + year,data=df_analysis[df_analysis$incumbent_running == 1,])
m_oil_on_reelection_pty_FE <- lm(reelection_party~log_oil_deflated_change + lag(log_oil_deflated_change) + state + year,data=df_analysis[df_analysis$incumbent_running == 1,])

screenreg(list(m_oil_on_running, m_oil_on_voteshare, m_oil_on_reelection, m_oil_on_reelection_pty,
                  m_oil_on_running_FE, m_oil_on_voteshare_FE, m_oil_on_reelection_FE, m_oil_on_reelection_pty_FE),
          omit.coef = "lag|state|year", stars = c(0.001, 0.01, 0.05))

############### Analysis of the Data #####################################

screenreg(list(m1, h1sca,h1scb,h1scc), omit.coef = "year|state")



