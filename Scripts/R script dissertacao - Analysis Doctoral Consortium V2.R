library(readxl)
library(dplyr)
library(tidyverse)
library(zoo)  # para yearqtr
library(plm)
library(lmtest) # Para os clustered standard errors
library(sandwich) # Para os clustered standard errors
library(texreg) 
library(panelAR) # Base teórica: Beck & Katz (1995)
library(vars) # Para modelos VAR como em Engemann et al. (2013)
library(dfms) # Para o Modelo de Fator Comun Dinâmico # Usar dps em conjunto com a PCA, para robustês
# install.packages("C:/Users/Joao arthur/Downloads/panelAR_0.1.tar.gz", 
#                  type= "source", 
#                  repos= NULL)
library(marginaleffects)


setwd("C:/Users/Joao arthur/OneDrive - Fundacao Getulio Vargas - FGV/Dissertação/Scripts-and-Databases")

source('Scripts/R script dissertacao - State Approval.R')
source('Scripts/R script dissertacao - Employment.R')
source('Scripts/R script dissertacao - Oil prices.R')
source('Scripts/R script dissertacao - BSF Data.R')
oil_shock_heterogeneity <- read_excel("Databases/Oil Heterogeneity Engemann et al. (2013)/Oil Heterogeneity Engemann Coefficients.xlsx")
source('Scripts/R script dissertacao - Controls from Singer (2023).R')

rm(list=setdiff(ls(), c('df_approval_qtr','employment_qtr','oil_qtr'
                        ,'BSF_dataset', 'oil_shock_heterogeneity', 'controls_singer')))


# Data for main analysis
df_analysis <- df_approval_qtr[,c(1,12,2,4,5,19,7,9,11,14:15,16:18)] %>%
  mutate(incumbent = paste(incumbent, state)) %>%
  left_join(employment_qtr[,c(1,2,4:6)], by = c("state", "quarter_year")) %>%
  left_join(oil_qtr[,c(6,9:18,19:22)], by = c( "quarter_year")) %>%
  left_join(oil_shock_heterogeneity[,c(1,7)], by = c("state")) %>%
  left_join(BSF_dataset[,c(1,2,6,7,8)], by = c("state", "year")) %>%
  left_join(controls_singer[,c(25,5,6:24)], by = c("state", "quarter_year"))

df_analysis <- pdata.frame(df_analysis, index = c("state", "quarter_year"))

df_analysis$quarter_year <- as.yearqtr(as.character(df_analysis$quarter_year))


pdim(df_analysis)$n$n # How balanced my panel is

############### Plots to sow the data

plot(subset(oil_qtr, year > 1971 & year < 2009)$quarter_year
     ,subset(oil_qtr, year > 1971 & year < 2009)$oil_shock_positive
     ,type='l')

plot(subset(df_analysis,significant_negative_effect_from_positive_shock ==1 & year > 1971 & year < 2009)$oil_shock_positive)

plot(subset(df_analysis,significant_negative_effect_from_positive_shock == 1 & year > 1971 & year < 2009)$Approval_Not_Smoothed)

#subset(df_analysis,significant_negative_effect_from_positive_shock == 1 & year > 1971 & year < 2009)

# from panel data to wide dataframe of columns Approval_Not_Smoothed, Disapproval_Not_Smoothed and Relative Approval_Not_Smoothed
approval_wide <- subset(df_analysis, quarter_year >= as.yearqtr("1973 Q4") & quarter_year <= as.yearqtr("2008 Q4") & !state %in% c("HI", "ID", "ND", "OK", "SD", "VT","AK","DE","ME","NH","UT","WA")) %>%
  dplyr::select(state, quarter_year, Approval_Not_Smoothed) %>%
  tidyr::pivot_wider(
    names_from = state,
    values_from = c(Approval_Not_Smoothed)
  ) %>%
  dplyr::arrange(quarter_year)

#drop rows with any NA values
approval_wide_clean <- approval_wide %>%
  dplyr::filter(complete.cases(.))

approval_wide_scaled <- scale(approval_wide_clean[,-c(1)])

pca <- prcomp(approval_wide_scaled)

df_pca <- data.frame(quarter_year = approval_wide_clean$quarter_year, pca1 = pca$x[,1])
df_pca <- left_join(df_pca, oil_qtr[,c(3,6,9)], by = c("quarter_year"))

df_pca[,-c(1)] <- scale(df_pca[,-c(1)]) # Scale the PCA and oil average qtr columns


plot(df_pca$quarter_year, df_pca$pca1, type = 'l', col = 'blue', xlab = 'Quarter Year', ylab = 'PCA and Oil Average Qtr')
lines(df_pca$quarter_year, df_pca$oil_avg_qtr, col = 'red')

library(ggplot2)
ggplot(df_pca, aes(x = quarter_year)) +
  geom_line(aes(y = pca1, color = "PCA")) +
  geom_line(aes(y = oil_qtr_avg, color = "Oil Average Qtr")) +
  geom_line(aes(y = oil_shock_positive, color = "Positive Oil Shock")) +
  labs(title = "PCA and Oil Average Qtr Over Time",
       y = "Values",
       x = "Quarter Year") +
  scale_color_manual(values = c("PCA" = "blue", "Oil Average Qtr" = "red", "Positive Oil Shock" = "green")) +
  theme_minimal()
# 
# summary(plm(pca1 ~ 
#    #+ oil_qtr_avg   
#    + dplyr::lag(oil_qtr_avg,1)
#    + dplyr::lag(oil_qtr_avg,2)
#    #+ lag(oil_qtr_avg,3)
#    #+ lag(oil_qtr_avg,4)
#    #+ oil_shock_positive 
#    + dplyr::lag(oil_shock_positive,1)
#    + dplyr::lag(oil_shock_positive,2)
#    #+ lag(oil_shock_positive,3)
#    #+ lag(oil_shock_positive,4)
#    ,data = df_pca,model="pooling"))
# glimpse(df_pca)

############### Filter only certain states

#Filter to have only states in AK, LA, ND, NM, MT, OK, TX, WV
df_analysis <- subset(df_analysis, state %in% c("AK", "LA", "ND", "NM", "MT", "OK", "TX", "WV"))

############### Tests for Stationarity of the Dependent variables

purtest(na.omit(df_analysis[,c(1,2,7)])[,c('Approval_Not_Smoothed')],test='ips'
        ,exo='intercept',lags=2)

purtest(na.omit(df_analysis[,c(1,2,8)])[,c('Disapproval_Not_Smoothed')],test='ips'
        ,exo='intercept',lags=2)

purtest(na.omit(df_analysis[,c(1,2,9)])[,c('Relative_Not_Smoothed')],test='ips'
        ,exo='intercept',lags=2)

purtest(na.omit(df_analysis[,c(1,2,15)])[,c('employment_logchange')],test='ips'
        ,exo='intercept',lags=2)

############### Subset data to account for only years Engemann et al. (2013) tested the effect of oil on the economy

# df_analysis <- subset(df_analysis, year => 1976 & year <= 2019)

############### Subset data to account only for valid surveys, as suggested by Singer (2023)

# df_analysis <- subset(df_analysis, valid_surveys == 1)

pdim(df_analysis) # How balanced my panel is
pdim(df_analysis)$T$Ti # Nº observations by state

############### Aggregating variables for the models:

dependent_var_empl <- "employment_logchange"
dependent_var_app <- "Approval_Not_Smoothed"
dependent_var_disapp <- "Disapproval_Not_Smoothed"
dependent_var_relapp <- "Relative_Not_Smoothed"


controls_singer <- c(
  "quarter1_first_valid"
  ,"quarter2_first_valid"
  ,"quarter3_first_valid"
  ,"quarter1_repeat_valid"
  ,"quarter2_repeat_valid"
  ,"quarter3_repeat_valid"
  ,"electionquarter"
  ,"female"
  ,"not_elected"
  ,"governorresignedthatquarter"
  ,"governorresignedthatquarter_l1"
  ,"governorresignedthatquarter_l2"
  ,"governordiedthatquarter"
  ,"governordiedthatquarter_l1"
  ,"governordiedthatquarter_l2"
  ,"factor(gov_party)"
)



m <- lm(as.formula(paste('Approval_Not_Smoothed'
                         ,'~ oil_shock_positive_l1 +'
                         # ,'oil_shock_positive_l2 +' 
                         ,'oil_shock_negative_l1 +'
                         # , 'oil_shock_negative_l2 +'
                         , 'dplyr::lag(Approval_Not_Smoothed,1) +'
                         ,'oil_qtr_avg_l1 +'
                         , 'oil_qtr_avg_l2 +'
                         ,'factor(state)'
)),data=df_analysis)

screenreg(m,omit.coef="state",ci=TRUE)


colnames(df_analysis)










