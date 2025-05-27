library(haven)
library(dplyr)
library(tidyverse)
library(estimatr)
library(texreg)
library(ivreg) # IV
library(AER) # Mostra se a IV é boa
library(QuantPsyc) # LM com standardized beta coefficients
library(fastDummies) # para criar dummies
library(sandwich)   # para estimar erros robustos
library(clubSandwich)
library(lmtest)     # para coeftest com erros robustos

#rm(list=ls())

setwd("C:/Users/Joao arthur/OneDrive - Fundacao Getulio Vargas - FGV/Dissertação/Scripts-and-Databases")


df_garro <- read_dta("Databases/Replication Files for Garro (2021)/polar_main_dataset.dta")

#rename st to state in df_garro
df_garro <- df_garro %>%
  rename(state = st)
#df_garro <- df_garro[,c('state','year','oilprice','reserves','gsp','gsppc')]
# df_garro$oilprice_lag1 <- lag(df_garro$oilprice, 1)
# df_garro$oilprice_lag2 <- lag(df_garro$oilprice, 2)
# df_garro$gsp_lag1 <- lag(df_garro$gsp, 1)
# df_garro$gsp_lag2 <- lag(df_garro$gsp, 2)

df_garro$dm_garro <- 1

############### Analysis #######################################
# 
# # Table 1 Least Squares
# t1m1 <- lm_robust( pol_polar~ lagloggsppc
#                    +factor(state_ch) +factor(year)
#                    ,clusters=state
#                    ,se_type='stata',data=df_garro[df_garro$year>=1996,])
# 
# t1m2 <- lm_robust( pol_polar~ lagloggsppc
#                    +factor(state_ch) +factor(year)
#                    +lagcompet +laggovdem +lagsplitleg +lagdemcontrol
#                    ,clusters=state
#                    ,se_type='stata',data=df_garro[df_garro$year>=1996,])
# 
# t1m3 <- lm_robust( pol_polar~ lagloggsppc
#                    +factor(state_ch) +factor(year)
#                    +lagcompet +laggovdem +lagsplitleg +lagdemcontrol
#                    +laglogpop
#                    ,clusters=state
#                    ,se_type='stata',data=df_garro[df_garro$year>=1996,])
# 
# screenreg(list(t1m1,t1m2,t1m3)
#           ,omit.coef = "Intercept|state|year|compet|dem|leg|pop",digits=3
#           ,custom.gof.rows=list('Chamber FE'=c('Yes','Yes','Yes')
#                                 ,'Year FE'=c('Yes','Yes','Yes')
#                                 ,'Party Controls'=c('No','Yes','Yes')
#                                 ,'Population Control'=c('No','No','Yes')))
# 
# # Table 2 Reduced Form
# t2m1 <- lm_robust( pol_polar~ lag2logoil_iv_two
#                    +factor(state_ch) +factor(year)
#                    ,clusters=state
#                    ,se_type='stata',data=df_garro[df_garro$year>=1996,])
# 
# t2m2 <- lm_robust( pol_polar~ lag2logoil_iv_two
#                    +factor(state_ch) +factor(year)
#                    +lagcompet +laggovdem +lagsplitleg +lagdemcontrol
#                    ,clusters=state
#                    ,se_type='stata',data=df_garro[df_garro$year>=1996,])
# 
# t2m3 <- lm_robust( pol_polar~ lag2logoil_iv_two
#                    +factor(state_ch) +factor(year)
#                    +lagcompet +laggovdem +lagsplitleg +lagdemcontrol
#                    +laglogpop
#                    ,clusters=state
#                    ,se_type='stata',data=df_garro[df_garro$year>=1996,])
# 
# screenreg(list(t2m1,t2m2,t2m3)
#           ,omit.coef = "Intercept|state|year|compet|dem|leg|pop",digits=3
#           ,custom.gof.rows=list('Chamber FE'=c('Yes','Yes','Yes')
#                                 ,'Year FE'=c('Yes','Yes','Yes')
#                                 ,'Party Controls'=c('No','Yes','Yes')
#                                 ,'Population Control'=c('No','No','Yes')))
# 
# # Table 3 First Stage
# t3m1 <- lm_robust( loggsppc~ laglogoil_iv_two
#                    +state +factor(year)
#                    ,clusters=state
#                    ,se_type='stata',data=df_garro[df_garro$year>=1996,])
# 
# t3m2 <- lm_robust( loggsppc~ laglogoil_iv_two
#                    +state +factor(year)
#                    +lagcompet +laggovdem +lagsplitleg +lagdemcontrol
#                    ,clusters=state
#                    ,se_type='stata',data=df_garro[df_garro$year>=1996,])
# 
# t3m3 <- lm_robust( loggsppc~ laglogoil_iv_two
#                    +state +factor(year)
#                    +lagcompet +laggovdem +lagsplitleg +lagdemcontrol
#                    +laglogpop
#                    ,clusters=state
#                    ,se_type='stata',data=df_garro[df_garro$year>=1996,])
# 
# screenreg(list(t3m1,t3m2,t3m3)
#           ,omit.coef = "Intercept|state|year|compet|dem|leg|pop",digits=3
#           ,custom.gof.rows=list('Chamber FE'=c('Yes','Yes','Yes')
#                                 ,'Year FE'=c('Yes','Yes','Yes')
#                                 ,'Party Controls'=c('No','Yes','Yes')
#                                 ,'Population Control'=c('No','No','Yes')))
# 
# # Table 4 Second Stage IV
# t4m1 <- ivreg(pol_polar~ lagloggsppc + factor(state_ch) +factor(year) 
#               | laglogoil_iv_two + factor(state_ch) +factor(year)
#               ,data=df_garro[df_garro$year>=1996,]
#               )
# 
# t4m2 <- ivreg(pol_polar~ factor(state_ch) +factor(year)+lagloggsppc
#               +lagcompet +laggovdem +lagsplitleg +lagdemcontrol
#               | factor(state_ch) +factor(year) + laglogoil_iv_two
#               +lagcompet +laggovdem +lagsplitleg +lagdemcontrol
#               ,data=df_garro[df_garro$year>=1996,]
# )
# 
# t4m3 <- ivreg(pol_polar~ factor(state_ch) +factor(year)+lagloggsppc
#               +lagcompet +laggovdem +lagsplitleg +lagdemcontrol
#               +laglogpop
#               | factor(state_ch) +factor(year) + laglogoil_iv_two
#               +lagcompet +laggovdem +lagsplitleg +lagdemcontrol
#               +laglogpop
#               ,data=df_garro[df_garro$year>=1996,]
# )
# 
# screenreg(list(t4m1,t4m2,t4m3)
#           ,omit.coef = "Intercept|state|year|compet|dem|leg|pop", digits=3
#           # ,custom.gof.rows=list('Chamber FE'=c('Yes','Yes','Yes')
#           #                       ,'Year FE'=c('Yes','Yes','Yes')
#           #                       ,'Party Controls'=c('No','Yes','Yes')
#           #                       ,'Population Control'=c('No','No','Yes'))
#           )
# 
# # Table 4 with robust errors
# t4m1_re <- coeftest(t4m1,vcov. = vcovCL(t4m1, cluster = ~ state))
# 
# summary(t4m1, diagnostics = TRUE)
# 
# # Table 5
# t5m1 <- ivreg(dem_polar ~ lagloggsppc
#               + factor(state_ch) +factor(year)
#               | factor(state_ch) +factor(year) + laglogoil_iv_two
#               , data=df_garro[df_garro$year>=1996,])
# 
# t5m4 <- ivreg(rep_polar ~ lagloggsppc
#               + factor(state_ch) +factor(year)
#               | factor(state_ch) +factor(year) + laglogoil_iv_two
#               , data=df_garro[df_garro$year>=1996,])
# 
# screenreg(list(t5m1,t5m4)
#           ,omit.coef = "Intercept|state|year|compet|dem|leg|pop", digits=3)
# 
# 
# ############### Analysis Garro (2021) + BSF ###################################
# 
# source('Scripts/R script dissertacao - BSF Data.R')
# 
# df_garro_BSF <- merge(df_garro, BSF_dataset[,c('state','year','BSF_implementation'
#                                                  ,'Deposit_strictness','Withdrawal_strictness')]
#                      , by = c('state','year'), all.x = TRUE)
# 
# t3m1_BSF_d <- lm_robust( loggsppc~ laglogoil_iv_two*Deposit_strictness
#                        +state +factor(year)
#                        ,clusters=state
#                        ,se_type='stata',data=df_garro_BSF[df_garro_BSF$year>=1996,])
# 
# t3m1_BSF_w <- lm_robust( loggsppc~ laglogoil_iv_two*Withdrawal_strictness
#                        +state +factor(year)
#                        ,clusters=state
#                        ,se_type='stata',data=df_garro_BSF[df_garro_BSF$year>=1996,])
# 
# screenreg(list(t3m1,t3m1_BSF_d,t3m1_BSF_w)
#           ,omit.coef = "Intercept|state|year|compet|dem|leg|pop",digits=3)
# 
# t4m1_BSF <- ivreg(pol_polar~ lagloggsppc + Deposit_strictness + lagloggsppc:Deposit_strictness
#                   +factor(state_ch) +factor(year)
#                   | laglogoil_iv_two + factor(state_ch) +factor(year) 
#                   ,data=df_garro_BSF[df_garro_BSF$year>=1996,])
# 
# screenreg(list(t4m1,t4m1_BSF)
#           ,omit.coef = "Intercept|state|year|compet|dem|leg|pop",digits=3)
# 
# 
# oil_on_polar_BSF_d <- lm_robust( pol_polar~ laglogoil_iv_two*Deposit_strictness
#                          +state +factor(year)
#                          ,clusters=state
#                          ,se_type='stata',data=df_garro_BSF[df_garro_BSF$year>=1996,])
# 
# oil_on_polar_BSF_w <- lm_robust( pol_polar~ laglogoil_iv_two*Withdrawal_strictness
#                          +state +factor(year)
#                          ,clusters=state
#                          ,se_type='stata',data=df_garro_BSF[df_garro_BSF$year>=1996,])
# 
# screenreg(list(oil_on_polar_BSF_d,oil_on_polar_BSF_w)
#           ,omit.coef = "Intercept|state|year|compet|dem|leg|pop",digits=3)
# 
# 
# 
# 
# 
# 
# 
# 
