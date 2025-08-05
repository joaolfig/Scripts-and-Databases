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
library(ggplot2)


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
  left_join(BSF_dataset[,c(1,2,6,7,8,9,10)], by = c("state", "year")) %>%
  left_join(controls_singer[,c(25,5,6:24)], by = c("state", "quarter_year"))

df_analysis <- pdata.frame(df_analysis, index = c("state", "quarter_year"))

df_analysis$quarter_year <- as.yearqtr(as.character(df_analysis$quarter_year))


############### Plots to sow the data

plot(subset(oil_qtr, year > 1971 & year < 2009)$quarter_year
     ,subset(oil_qtr, year > 1971 & year < 2009)$oil_shock_positive
     ,type='l')

plot(subset(df_analysis,significant_negative_effect_from_positive_shock == 1 & year > 1971 & year < 2009)$Approval_Not_Smoothed)


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


ggplot(df_pca, aes(x = quarter_year)) +
  geom_line(aes(y = oil_qtr_avg, color = "Oil Average Qtr")) +
  geom_line(aes(y = oil_shock_positive, color = "Positive Oil Shock")) +
  labs(title = "PCA and Oil Average Qtr Over Time",
       y = "Values",
       x = "Quarter Year") +
  scale_color_manual(values = c("PCA" = "blue", "Oil Average Qtr" = "red", "Positive Oil Shock" = "green")) +
  theme_minimal()


############### Tests for Stationarity of the Dependent variables

purtest(na.omit(df_analysis[,c(1,2,7)])[,c('Approval_Not_Smoothed')],test='ips'
        ,exo='intercept',lags=2)

purtest(na.omit(df_analysis[,c(1,2,8)])[,c('Disapproval_Not_Smoothed')],test='ips'
        ,exo='intercept',lags=2)

purtest(na.omit(df_analysis[,c(1,2,9)])[,c('Relative_Not_Smoothed')],test='ips'
        ,exo='intercept',lags=2)

purtest(na.omit(df_analysis[,c(1,2,15)])[,c('employment_logchange')],test='ips'
        ,exo='intercept',lags=2)

############### Aggregating variables for the models:

dependent_var_empl <- "employment_logchange"
dependent_var_app <- "Approval_Not_Smoothed"
dependent_var_disapp <- "Disapproval_Not_Smoothed"
dependent_var_relapp <- "Relative_Not_Smoothed"

# Variables measured as in Engemann et al. (2013)
oil_variables <- c(
                  "oil_shock_positive_l1"
                   ,"oil_shock_positive_l2"
                   ,"oil_shock_positive_l3"
                   ,"oil_shock_positive_l4"
                   #,"oil_shock_negative_l1"
                   #,"oil_shock_negative_l2"
                   #,"oil_shock_negative_l3"
                   #,"oil_shock_negative_l4"
                   ,"significant_negative_effect_from_positive_shock"
                   ,"oil_shock_positive_l1:significant_negative_effect_from_positive_shock"
                   ,"oil_shock_positive_l2:significant_negative_effect_from_positive_shock"
                   ,"oil_shock_positive_l3:significant_negative_effect_from_positive_shock"
                   ,"oil_shock_positive_l4:significant_negative_effect_from_positive_shock"
                   )

oil_variables_avgs <- c(
  "oil_avg_qtr_l1"
  ,"oil_avg_qtr_l2"
  ,"oil_avg_qtr_l3"
  ,"oil_avg_qtr_l4"
  ,"significant_negative_effect_from_positive_shock"
  ,"oil_avg_qtr_l1:significant_negative_effect_from_positive_shock"
  ,"oil_avg_qtr_l2:significant_negative_effect_from_positive_shock"
  ,"oil_avg_qtr_l3:significant_negative_effect_from_positive_shock"
  ,"oil_avg_qtr_l4:significant_negative_effect_from_positive_shock"
  
)
    

empl_growth <- c("employment_logchange"
                       ,"employment_logchange_l1"
                       ,"employment_logchange_l2")


# Controls from Singer (2023)
unemployment <- c("unemployment_state"
                  ,"unemployment_state_l1"
                  ,"unemployment_state_l2")

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

# Fixed effects
fixed_effects <- c("factor(quarter_year)",
                   "factor(incumbent)")

# BSF variables
BSF_withdrawal_variables <- c(
  'BSF_withdrawal_3level'
  ,'BSF_withdrawal_3level:oil_shock_positive_l1'
  ,'BSF_withdrawal_3level:oil_shock_positive_l2'
  ,'BSF_withdrawal_3level:oil_shock_positive_l3'
  ,'BSF_withdrawal_3level:oil_shock_positive_l4'
)


############### Subset data to account for only years Engemann et al. (2013) tested the effect of oil on the economy

# df_analysis <- subset(df_analysis, year => 1976 & year <= 2019)

############### Subset data to account only for valid surveys, as suggested by Singer (2023)

# df_analysis <- subset(df_analysis, valid_surveys == 1)

# subset(df_analysis, 
#        quarter_year >= as.yearqtr("1973 Q4") & 
#          quarter_year <= as.yearqtr("2008 Q4") & 
#          valid_surveys == 1 & 
#          !state %in% c("HI", "ID", "ND", "OK", "SD", "VT"))


unique(subset(df_analysis, quarter_year >= as.yearqtr("1973 Q4") & quarter_year <= as.yearqtr("2008 Q4")& valid_surveys == 1 & !state %in% c("HI", "ID", "ND", "OK", "SD", "VT"))$state)

############### Models

### Replication of Singer (2023)

models_empl_app <- list()
models_empl_app_psar1 <- list()

for (dv in c(dependent_var_app, dependent_var_disapp,dependent_var_relapp)) {
  model_formula <- as.formula(paste(dv
                                    ,'~',paste(unemployment, collapse = " + ")
                                    ,'+',paste(controls_singer, collapse = " + ")
                                    ))
  models_empl_app[[dv]] <- plm(formula = model_formula
                            ,data = subset(df_analysis, year >= 1976 & year <= 2019 & valid_3qtr == 1 & !state %in% c("HI", "ID", "ND", "OK", "SD", "VT")),
                            model = "pooling")
  
  models_empl_app_psar1[[dv]] <- panelAR( # Base teórica: Beck & Katz (1995)
                            formula = model_formula
                            ,data = subset(df_analysis, year >= 1976 & year <= 2019 & valid_3qtr == 1 & !state %in% c("HI", "ID", "ND", "OK", "SD", "VT"))%>%
                            filter(!is.na(Relative_Not_Smoothed),
                                   !is.na(Approval_Not_Smoothed),
                                   !is.na(Disapproval_Not_Smoothed))
  ,panelVar = "state"
  ,timeVar = "qtr"
  ,autoCorr = "psar1" # Panel Specific AR(1) correction
  ,panelCorrMethod = "phet" # Panel-corrected standard errors (PCSE)
) 
}

### Models of oil on approval

models_oil_app_psar1 <- list()

for (dv in c(dependent_var_app, dependent_var_disapp, dependent_var_relapp)) {
  model_formula <- as.formula(paste(dv
                                    ,'~',paste(oil_variables, collapse = " + ")
                                    #,'+',paste(controls_singer, collapse = " + ")
                                    #,'+',paste(fixed_effects, collapse = " + ")
                                    ))
  models_oil_app_psar1[[dv]] <- panelAR( # Base teórica: Beck & Katz (1995)
    formula = model_formula
    ,data = subset(df_analysis, quarter_year >= as.yearqtr("1973 Q4") & quarter_year <= as.yearqtr("2008 Q4")& !state %in% c("HI", "ID", "ND", "OK", "SD", "VT")) %>%
      filter(!is.na(Relative_Not_Smoothed),
             !is.na(Approval_Not_Smoothed),
             !is.na(Disapproval_Not_Smoothed),
             valid_surveys == 1)
    ,panelVar = "state"
    ,timeVar = "qtr"
    ,autoCorr = "psar1" # Panel Specific AR(1) correction
    ,panelCorrMethod = "phet" # Panel-corrected standard errors (PCSE)
  ) 
}

### Models of oil on approval with Singer's controls


models_oil_controls_app_psar1 <- list()

for (dv in c(dependent_var_app, dependent_var_disapp, dependent_var_relapp)) {
  model_formula <- as.formula(paste(dv
                                    ,'~',paste(oil_variables, collapse = " + ")
                                    ,'+',paste(controls_singer, collapse = " + ")
                                    #,'+',paste(fixed_effects, collapse = " + ")
  ))
  models_oil_controls_app_psar1[[dv]] <- panelAR( # Base teórica: Beck & Katz (1995)
    formula = model_formula
    ,data = subset(df_analysis, quarter_year >= as.yearqtr("1973 Q4") & quarter_year <= as.yearqtr("2008 Q4")& !state %in% c("HI", "ID", "ND", "OK", "SD", "VT")) %>%
      filter(!is.na(Relative_Not_Smoothed),
             !is.na(Approval_Not_Smoothed),
             !is.na(Disapproval_Not_Smoothed),
             valid_surveys == 1)
    ,panelVar = "state"
    ,timeVar = "qtr"
    ,autoCorr = "psar1" # Panel Specific AR(1) correction
    ,panelCorrMethod = "phet" # Panel-corrected standard errors (PCSE)
  ) 
}


### Models BSF withdrawal

models_BSF_withdrawal_psar1 <- list()

for (dv in c(dependent_var_app, dependent_var_disapp, dependent_var_relapp)) {
  model_formula <- as.formula(paste(dv
                                    ,'~',paste(oil_variables, collapse = " + ")
                                    ,'+',paste(BSF_variables, collapse = " + ")
                                    #,'+',paste(controls_singer, collapse = " + ")
                                    #,'+',paste(fixed_effects, collapse = " + ")
  ))
  models_BSF_withdrawal_psar1[[dv]] <- panelAR( # Base teórica: Beck & Katz (1995)
    formula = model_formula
    ,data = subset(df_analysis, quarter_year >= as.yearqtr("1973 Q4") & quarter_year <= as.yearqtr("2008 Q4")&!state %in% c("HI", "ID", "ND", "OK", "SD", "VT")) %>%
      filter(!is.na(Relative_Not_Smoothed),
             !is.na(Approval_Not_Smoothed),
             !is.na(Disapproval_Not_Smoothed),
             valid_surveys == 1,
             significant_negative_effect_from_positive_shock == 1)
    ,panelVar = "state"
    ,timeVar = "qtr"
    ,autoCorr = "psar1" # Panel Specific AR(1) correction
    ,panelCorrMethod = "phet" # Panel-corrected standard errors (PCSE)
  ) 
}

### Models BSF deposit





############### Tables


### Models Singer (2023) - Table 3
Reduce(function(x, y) merge(x, y, by = "term", all = TRUE),
       lapply(names(models_empl_app_psar1), function(name) {
         s <- summary(models_empl_app_psar1[[name]])$coefficients
         stars <- cut(s[, 4], breaks = c(-Inf, 0.001, 0.01, 0.05, 0.1, Inf),
                      labels = c("***", "**", "*", ".", ""))
         out <- data.frame(term = rownames(s),
                           value = sprintf("%.3f (%.3f)%s", s[, 1], s[, 2], stars),
                           row.names = NULL)
         names(out)[2] <- name
         out
       }))

### Models oil on approval
Reduce(function(x, y) merge(x, y, by = "term", all = TRUE),
       lapply(names(models_oil_app_psar1), function(name) {
         s <- summary(models_oil_app_psar1[[name]])$coefficients
         stars <- cut(s[, 4], breaks = c(-Inf, 0.001, 0.01, 0.05, 0.1, Inf),
                      labels = c("***", "**", "*", ".", ""))
         out <- data.frame(term = rownames(s),
                           value = sprintf("%.3f (%.3f)%s", s[, 1], s[, 2], stars),
                           row.names = NULL)
         names(out)[2] <- name
         out
       }))

### Models oil on approval with Singer's controls
Reduce(function(x, y) merge(x, y, by = "term", all = TRUE),
       lapply(names(models_oil_controls_app_psar1), function(name) {
         s <- summary(models_oil_controls_app_psar1[[name]])$coefficients
         stars <- cut(s[, 4], breaks = c(-Inf, 0.001, 0.01, 0.05, 0.1, Inf),
                      labels = c("***", "**", "*", ".", ""))
         out <- data.frame(term = rownames(s),
                           value = sprintf("%.3f (%.3f)%s", s[, 1], s[, 2], stars),
                           row.names = NULL)
         names(out)[2] <- name
         out
       }))

### Models BSF withdrawal
Reduce(function(x, y) merge(x, y, by = "term", all = TRUE),
       lapply(names(models_BSF_withdrawal_psar1), function(name) {
         s <- summary(models_BSF_withdrawal_psar1[[name]])$coefficients
         stars <- cut(s[, 4], breaks = c(-Inf, 0.001, 0.01, 0.05, 0.1, Inf),
                      labels = c("***", "**", "*", ".", ""))
         out <- data.frame(term = rownames(s),
                           value = sprintf("%.3f (%.3f)%s", s[, 1], s[, 2], stars),
                           row.names = NULL)
         names(out)[2] <- name
         out
       }))

############### Marginal Effects

### Models oil on approval

m <- models_oil_app_psar1$Relative_Not_Smoothed
vc <- vcov(m)

lags <- paste0("oil_shock_positive_l", 1:4)

df <- do.call(rbind, lapply(lags, function(X) {
  XZ <- grep(paste0(X, ".*significant_negative_effect"), names(coef(m)), value = TRUE)
  me <- c(coef(m)[X], coef(m)[X] + coef(m)[XZ])
  se <- c(sqrt(vc[X, X]), sqrt(vc[X, X] + vc[XZ, XZ] + 2 * vc[X, XZ]))
  data.frame(
    Lag = X,
    Z = c("Z = 0", "Z = 1"),
    me = me,
    low = me - 1.96 * se,
    up = me + 1.96 * se
  )
}))

ggplot(df, aes(x = Lag, y = me, color = Z, group = Z)) +
  geom_point(position = position_dodge(0.3)) +
  geom_errorbar(aes(ymin = low, ymax = up), width = 0.2, position = position_dodge(0.3)) +
  geom_hline(yintercept = 0, linetype = "dashed") +
  labs(y = "Marginal Effect", x = "Lag", color = "Condition on Z") +
  theme_minimal()


### Models oil on approval with singer's controls

m <- models_oil_controls_app_psar1$Relative_Not_Smoothed
vc <- vcov(m)

lags <- paste0("oil_shock_positive_l", 1:4)

df <- do.call(rbind, lapply(lags, function(X) {
  XZ <- grep(paste0(X, ".*significant_negative_effect"), names(coef(m)), value = TRUE)
  me <- c(coef(m)[X], coef(m)[X] + coef(m)[XZ])
  se <- c(sqrt(vc[X, X]), sqrt(vc[X, X] + vc[XZ, XZ] + 2 * vc[X, XZ]))
  data.frame(
    Lag = X,
    Z = c("Z = 0", "Z = 1"),
    me = me,
    low = me - 1.96 * se,
    up = me + 1.96 * se
  )
}))

ggplot(df, aes(x = Lag, y = me, color = Z, group = Z)) +
  geom_point(position = position_dodge(0.3)) +
  geom_errorbar(aes(ymin = low, ymax = up), width = 0.2, position = position_dodge(0.3)) +
  geom_hline(yintercept = 0, linetype = "dashed") +
  labs(y = "Marginal Effect", x = "Lag", color = "Condition on Z") +
  theme_minimal()


### Models BSF

m <- models_BSF_withdrawal_psar1$Relative_Not_Smoothed
vc <- vcov(m)

lags <- paste0("oil_shock_positive_l", 1:4)
levels_Z <- c("No BSF", "Lenient rule", "Strict rule")

df <- do.call(rbind, lapply(lags, function(X) {
  
  coefs_Z <- c(
    paste0(X), # Base category (No BSF, assuming it as baseline)
    paste0(X, ":BSF_withdrawal_3levelLenient rule"),
    paste0(X, ":BSF_withdrawal_3levelStrict rule")
  )
  
  me <- c(
    coef(m)[coefs_Z[1]], # No BSF (baseline)
    coef(m)[coefs_Z[1]] + coef(m)[coefs_Z[2]], # Lenient rule
    coef(m)[coefs_Z[1]] + coef(m)[coefs_Z[3]]  # Strict rule
  )
  
  se <- c(
    sqrt(vc[coefs_Z[1], coefs_Z[1]]), # No BSF
    sqrt(vc[coefs_Z[1], coefs_Z[1]] + vc[coefs_Z[2], coefs_Z[2]] + 2 * vc[coefs_Z[1], coefs_Z[2]]), # Lenient rule
    sqrt(vc[coefs_Z[1], coefs_Z[1]] + vc[coefs_Z[3], coefs_Z[3]] + 2 * vc[coefs_Z[1], coefs_Z[3]])  # Strict rule
  )
  
  data.frame(
    Lag = X,
    Z = factor(levels_Z, levels = levels_Z),
    me = me,
    low = me - 1.96 * se,
    up = me + 1.96 * se
  )
}))

ggplot(df, aes(x = Lag, y = me, color = Z, group = Z)) +
  geom_point(position = position_dodge(0.5), size = 2.5) +
  geom_errorbar(aes(ymin = low, ymax = up), width = 0.2, position = position_dodge(0.5)) +
  geom_hline(yintercept = 0, linetype = "dashed") +
  labs(y = "Marginal Effect", x = "Lag", color = "BSF withdrawal") +
  theme_minimal(base_size = 14)





