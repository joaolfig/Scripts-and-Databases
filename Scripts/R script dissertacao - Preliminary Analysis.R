library(stringr)
library(dplyr)
library(estimatr)
library(texreg)
library(stargazer)

setwd("C:/Users/Joao arthur/OneDrive - Fundacao Getulio Vargas - FGV/Dissertação/Scripts-and-Databases")
getwd()

rm(list=ls())

############### Load Data ################
source('Scripts/R script dissertacao - Electoral Data Gubernatorial.R')
source('Scripts/R script dissertacao - Electoral Data Presidential.R')
source('Scripts/R script dissertacao - Employment gap.R')
source('Scripts/R script dissertacao - Oil.R')
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

############### Analysis of the Data ############### 
# filter df_analysis only till 1997
#df_analysis <- df_analysis %>% filter(year <= 1997)

# Turn variables into factor
# df_analysis[,1:4] <- lapply(df_analysis[,1:4], factor)
# df_analysis[,11:17] <- lapply(df_analysis[,11:17], factor)
# df_analysis[,24:26] <- lapply(df_analysis[,24:26], factor)


# df_analysis[,11:14] <- lapply(df_analysis[,11:14], as.numeric)
# df_analysis[,16:17] <- lapply(df_analysis[,16:17], as.numeric)
# df_analysis[,24:26] <- lapply(df_analysis[,24:26], as.numeric)

# Summary statistics for selected columns Wolfers (2007) Appendix A
# Political variables (w/ full sample)
stargazer(df_analysis[, c(9,10,12,3,18,20,23,22,24)], 
          type = "text",  # Options: "text", "html", "latex"
          summary.stat = c("mean", "sd", "min", "max",'n'),
          title = "Summary Statistics",
          digits = 3)

summary(df_analysis[, c(25,26,27)])

colnames(df_analysis)
# Table 1 
w1 <- lm(incumbent_pct_2pty_change ~ employment_gap_change, 
         data = df_analysis)

# Table 2
# Panel A
w2A <- lm(incumbent_pct_2pty_change ~ competence_employment + employment_gap_change_national, data = df_analysis)

# Panel B
w2B <- glm(reelection_candidate ~ competence_employment + employment_gap_change_national
           ,data = df_analysis[df_analysis$incumbent_running == 1,]
           , family = binomial(link = "probit"))



screenreg(list(w1, w2A, w2B),omit.coef = "year|state")
#          ,custom.model.names = c("OLS","OLS FE",'OLS competence FE','Probit', 'Probit FE')
          
          # ,custom.cpp = list(
          #   "employment_gap_change" = "Employment Gap Change"
          # )
#          ,custom.gof.rows = list(
#            'State FE' = c(F,T,T,F,T)
#            ,'Time FE' = c(F,T,T,F,T))


# Table 3
w3.1 <- lm(incumbent_pct_2pty_change ~ competence_employment 
+ employment_gap_change_national, data = df_analysis)

w3.2 <- lm(incumbent_pct_2pty_change ~ competence_employment 
+ employment_gap_change_national*gov_presi_same_party, data = df_analysis)

w3.3 <- lm(incumbent_pct_2pty_change ~ competence_employment
+ employment_gap_change_national + gov_presi_same_party 
+ gov_presi_same_party:year, data = df_analysis)

w3.4 <- lm(incumbent_pct_2pty_change ~ competence_employment 
+ employment_gap_change_national + gov_presi_same_party 
+ gov_presi_same_party:year + state, data = df_analysis)

w3.5 <- lm(incumbent_pct_2pty_change ~ competence_employment
           + employment_gap_change_national + gov_presi_same_party 
           + gov_presi_same_party:year + state + incumbent_party
           + incumbent_party:employment_gap
           , data = df_analysis)

w3.6 <- lm(incumbent_pct_2pty_change ~ competence_employment
           + employment_gap_change_national + gov_presi_same_party*year 
           + state + incumbent_party + incumbent_party:employment_gap 
           , data = df_analysis)

screenreg(list(w3.1, w3.2, w3.3, w3.4, w3.5, w3.6),omit.coef = "year|state")

# Linear probability models

m1 <- lm(incumbent_running ~ employment_gap_change 
         + competence_employment 
         + employment_gap_change_national 
         + gov_presi_same_party
         + gov_presi_same_party:employment_gap_change_national
         + state 
         + year, data = df_analysis)


m2 <- lm(reelection_party ~ employment_gap_change 
         + competence_employment 
         + employment_gap_change_national 
         + gov_presi_same_party
         + gov_presi_same_party:employment_gap_change_national
         + state 
         + year, data = df_analysis)

m3 <- lm(reelection_candidate ~ employment_gap_change 
         + competence_employment 
         + employment_gap_change_national 
         + gov_presi_same_party
         + gov_presi_same_party:employment_gap_change_national
         + state 
         + year, data = df_analysis)

m4 <- lm(incumbent_running ~ 
         + gov_presi_same_party
         +  gov_presi_same_party:log_oil_deflated_change
         + log_oil_deflated_change 
         + log_oil_deflated_change:state
         + state 
         + year, data = df_analysis)

m5 <- lm(reelection_party ~ 
         gov_presi_same_party
         + gov_presi_same_party:log_oil_deflated_change
         + log_oil_deflated_change 
         + log_oil_deflated_change:state
         + state 
         + year, data = df_analysis)

m6 <- lm(reelection_candidate ~ 
         gov_presi_same_party
         + gov_presi_same_party:log_oil_deflated_change
         + log_oil_deflated_change 
         + log_oil_deflated_change:state
         + state 
         + year, data = df_analysis)


screenreg(list(m1,m2,m3),omit.coef = "year|state")

screenreg(list(m4,m5,m6),omit.coef = "year|state")



# Checking the correlation between the variables and oil prices

summary(lm(employment_gap_change ~ log_oil_deflated_change + lag(log_oil_deflated_change),data=df_analysis))
summary(lm(employment_gap_change_national ~ log_oil_deflated_change + lag(log_oil_deflated_change),data=df_analysis))
summary(lm(competence_employment ~ log_oil_deflated_change + lag(log_oil_deflated_change),data=df_analysis))


colnames(df_analysis)
summary(lm(incumbent_pct_2pty_change ~ log_oil_deflated_change + lag(log_oil_deflated_change),data=df_analysis))
summary(lm(incumbent_running~log_oil_deflated_change + lag(log_oil_deflated_change),data=df_analysis))
summary(lm(reelection_party~log_oil_deflated_change + lag(log_oil_deflated_change),data=df_analysis))
summary(lm(reelection_candidate~log_oil_deflated_change + lag(log_oil_deflated_change),data=df_analysis))


# m_oil2 <- lm(log_oil_deflated_change ~

# oil <- lm(incumbent_pct_2pty_change ~ competence_employment
#            + employment_gap_change_national + gov_presi_same_party*year 
#           
#           + state + incumbent_party + incumbent_party:employment_gap
#            , data = df_analysis)
# 
# 
# screenreg(w3.6, omit.coef = "year|state")
# 
# summary(lm(incumbent_pct_2pty_change ~ competence_employment
#    + employment_gap_change_national + 
#    + log_oil_deflated_change + incumbent_party + incumbent_party:employment_gap
#    , data = df_analysis))
# 
# 
# m <- lm(reelection_party ~ 
#            + employment_gap_change 
#            + gov_presi_same_party:employment_gap_change_national
# #           + incumbent_party
#            + state + year
#            , data = df_analysis)
# 
# m2 <- lm(reelection_party ~ 
#            + competence_employment
#          + employment_gap_change_national
#          + gov_presi_same_party:employment_gap_change_national
# #         + incumbent_party
#          + state + year
#          , data = df_analysis)
# 
# m3 <- lm(reelection_party ~ 
#          + competence_employment
#          + employment_gap_change_national
#          + gov_presi_same_party:employment_gap_change_national
# #         + incumbent_party
#          + state + year
#          , data = df_analysis[df_analysis$incumbent_running == 1,])
# 
# m4 <- lm(reelection_party ~ 
#          + competence_employment
#          + employment_gap_change_national
#          + gov_presi_same_party:employment_gap_change_national
# #         + incumbent_party
# #         + BSF_implementation*Deposit_strictness
# #         + BSF_implementation*Withdrawal_strictness
#          + BSF_implementation:Deposit_strictness
#          + state + year
#          , data = df_analysis)
# 
# m4 <- lm(reelection_candidate ~ 
#            + competence_employment
#          + employment_gap_change_national
#          + gov_presi_same_party:employment_gap_change_national
#          + BSF_implementation*log_oil_deflated_change
#          + state + year
#          , data = df_analysis)
# 
# m5 <- lm(
#   employment_gap_change ~
#     + BSF_implementation*log_oil_deflated_change*state
#     + year
#   , data = df_analysis
#   
# )
# 
# screenreg(list(m5), omit.coef = "year|state")  


screenreg(list(m,m2,m3,m4), omit.coef = "year|state")

#colnames(df_analysis)
#   
#   w3.3 <-
#   
#   w3.4 <- 
#   
#   w3.5 <-
#   
#   w3.6 <- 

# IV: Oil*State_FE
# m6 <- lm(incumbent_pct_2pty_change ~ employment_gap_change + competence_employment + state+year, data = df_analysis)
# m7 <- lm(competence_employment ~ log_oil_deflated_change, data = df_analysis)
# 
# 
# screenreg(list(m6,m7)
#           ,omit.coef = "year|state")
# 
# 
# colnames(df_analysis)

# Wolfers uses "two years ended gap"
# Search for "two years" in Wolfers


# vale a pena ver a correlação entre o desempenho da economia do estado, sem a nacional, com o petróleo * estado
