library(stringr)
library(dplyr)
library(estimatr)
library(texreg)
library(stargazer)

suppressPackageStartupMessages({
  library(tidyr);library(dplyr);library(texreg);library(estimatr);library(marginaleffects);
  library(AER);library(lmtest);library(sandwich);library(flextable);library(mediation);
  library(equatiomatic)
})

setwd("C:/Users/Joao arthur/OneDrive - Fundacao Getulio Vargas - FGV/Dissertação/Scripts-and-Databases")
getwd()

rm(list=ls())

############### Load Data ################
source('Scripts/R script dissertacao - Electoral Data Gubernatorial.R')
source('Scripts/R script dissertacao - Electoral Data Presidential.R')
source('Scripts/R script dissertacao - Oil prices.R')
source('Scripts/R script dissertacao - BSF Data.R')

rm(list=setdiff(ls(), c("vote_state",'president_incumbent_ts'
                        ,"employment_states_gap"
                        ,"oil_deflated"
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

#left join with Oil data
df_analysis <- merge(df_analysis, oil_deflated[,c('year','oil_deflated','log_oil_deflated_change')], by = c('year'), all.x = TRUE)

#left join with BSF data
df_analysis <- merge(df_analysis, BSF_dataset[,c('state','year','BSF_implemented'
                                                 ,'Deposit_strictness','Withdrawal_strictness')]
                     , by = c('state','year'), all.x = TRUE)

df_analysis$incumbent_party <- ifelse(df_analysis$incumbent_party == 'Democratic', 1, 0)

# I want to limit my data to 2007
# df_analysis <- df_analysis[df_analysis$year >= 1958,]
# df_analysis <- df_analysis[df_analysis$year <= 2007,]


# I want to use only the following states: LA,MT,NM,ND,OK,TX,WY
oil_states <- c("LA","MT","NM","ND","OK","TX","WY")

df_analysis <- df_analysis[df_analysis$state %in% oil_states,]

#standardize oil_deflated
df_analysis <- df_analysis %>%
  mutate(
    oil_deflated_std = (oil_deflated - mean(oil_deflated, na.rm = TRUE)) / sd(oil_deflated, na.rm = TRUE)
    ,oil_deflated_std = round(oil_deflated_std,2)
  )


#check unique states
unique(df_analysis$state)

#Check unique years
unique(df_analysis$year) %>%
  sort()

############### Analysis of the Data ############### 
# Linear probability model:
colnames(df_analysis)

m1a <- lm_robust(reelection_party ~ oil_deflated_std*BSF_implemented
                 + incumbent_party + president_party + gov_presi_same_party
                 + incumbent_running
                 + state 
                 ,data = df_analysis)
m1b <- lm(reelection_party ~ oil_deflated_std*BSF_implemented
          + incumbent_party + president_party + gov_presi_same_party
          + incumbent_running
          + state 
          ,data = df_analysis ,se_type="HC2")

m2a <- lm(incumbent_running ~ oil_deflated_std*BSF_implemented
          + incumbent_party + president_party + gov_presi_same_party
          + state 
          ,data = df_analysis)
m2b <- lm_robust(incumbent_running ~ oil_deflated_std*BSF_implemented
                 + incumbent_party + president_party + gov_presi_same_party
                 + state 
                 ,data = df_analysis ,se_type="HC2")

m3a <- lm(reelection_candidate ~ oil_deflated_std*BSF_implemented
          + incumbent_party + president_party + gov_presi_same_party
          + state 
          ,data = subset(df_analysis, incumbent_running == 1))
m3b <- lm_robust(reelection_candidate ~ oil_deflated_std*BSF_implemented
                 + incumbent_party + president_party + gov_presi_same_party
                 + state 
                 ,data = subset(df_analysis, incumbent_running == 1)
                 ,se_type="HC2")

screenreg(
  list(m1a, m1b, m2a, m2b, m3a, m3b),
  omit.coef = "(Intercept)|state",
  ci.force = TRUE,
  custom.header = list(
    "Party re-election" = 1:2,
    "Candidate re-run" = 3:4,
    "Candidate re-election" = 5:6
  )
)  

plot_slopes(m1a, variables = "oil_deflated_std", by = "BSF_implemented") +
  labs(
    title = "Marginal effect of resrev_i on savings_i",
    subtitle = "Separate slopes by election year dummy (0 = non-election, 1 = election)",
    x = "Resource revenues (resrev_i)",
    y = "Marginal effect on savings_i (dY/dX)",
    caption = "95% CIs shown. From slopes(): non-election dy/dx = 0.69 [0.34; 1.03], p<0.001;
election dy/dx = 0.37 [−0.05; 0.79], p=0.081."
  ) +
  theme_minimal()

# 2) Predicted values across resrev_i, colored by election group
plot_predictions(m1a, condition = c("oil_deflated_std", "BSF_implemented")) +
  labs(
    title = "Predicted savings_i across resrev_i",
    subtitle = "Lines and 95% CI ribbons by election year dummy (0 vs 1)",
    x = "Resource revenues (resrev_i)",
    y = "Predicted savings_i",
    caption = "From predictions(): mean predicted savings_i — non-election = 2.05 [1.65; 2.45],
election = 1.43 [0.88; 1.99]. Other covariates held at typical values."
  ) +
  theme_minimal()





# Plot real oil prices
library(ggplot2)
ggplot(df_analysis, aes(x=year, y=oil_deflated_std)) +
  geom_line() +
  geom_point() +
  labs(title="Real Oil Prices Over Time",
       x="Year",
       y="Real Oil Price (Deflated)") +
  theme_minimal()





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
