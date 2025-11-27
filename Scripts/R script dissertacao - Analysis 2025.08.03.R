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
library(car)

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
  left_join(oil_qtr[,c(6,9:20,21:24)], by = c( "quarter_year")) %>%
  left_join(oil_shock_heterogeneity[,c(1,7)], by = c("state")) %>%
  left_join(BSF_dataset[,c(1,2,6,7,8,9,10,11,12)], by = c("state", "year")) %>%
  left_join(controls_singer[,c(25,5,6:24)], by = c("state", "quarter_year"))

df_analysis <- pdata.frame(df_analysis, index = c("state", "quarter_year"))

df_analysis$quarter_year <- as.yearqtr(as.character(df_analysis$quarter_year))

############### Plots to sow the data

subset_analysis <- subset(df_analysis, 
                      year >= 1973 & year  <= 2009 &
                        valid_surveys == 1 & 
                        #valid_3qtr == 1 &
                        #!state %in% c("HI", "ID", "ND", "OK", "SD", "VT") &
                        !is.na(Relative_Not_Smoothed) &
                        !is.na(Approval_Not_Smoothed) &
                        !is.na(Disapproval_Not_Smoothed)
                      )
pdim(subset_analysis)

#Select only panels where BSF_implementation mean is between 0 and 1

#select subset_analysis calculate the mean BSF_implementation and drop states where
# it is 0 or 1
# 
# subset_analysis <- subset_analysis %>%
#   group_by(state) %>%
#   filter(mean(BSF_implementation, na.rm = TRUE) > 0 & mean(BSF_implementation, na.rm = TRUE) < 1) %>%
#   ungroup()
# 
# pdim(subset_analysis)
# 
# print(unique(subset_analysis$state))
# 
# subset_analysis <- pdata.frame(subset_analysis, index = c("state", "quarter_year"))
# 
# subset_analysis$quarter_year <- as.yearqtr(as.character(subset_analysis$quarter_year))



plot(subset_analysis$Approval_Not_Smoothed)

table(oil_shock_heterogeneity$state,oil_shock_heterogeneity$significant_negative_effect_from_positive_shock)

#make it into a barplot of sum of BSF_implementation by year

par(mfrow=c(2,2), xpd=TRUE) # permite legenda fora da área do gráfico

barplot(table(subset_analysis$BSF_deposit_5level),
        col=rainbow(5),
        legend.text=TRUE,
        args.legend=list(x="topright", cex=0.7, bty="n"),
        main="BSF Deposit Rules 5 levels")

barplot(table(subset_analysis$BSF_deposit_3level),
        col=rainbow(3),
        main="BSF Deposit Rules 3 levels")

barplot(table(subset_analysis$BSF_withdrawal_5level),
        col=rainbow(5),
        legend.text=TRUE,
        args.legend=list(x="topright", cex=0.7, bty="n"),
        main="BSF Withdrawal Rules 5 levels")

barplot(table(subset_analysis$BSF_withdrawal_3level),
        col=rainbow(3),
        main="BSF Withdrawal Rules 3 levels")



par(mfrow=c(2,2)) # 2 rows and 2 columns for the boxplots

barplot(table(subset_analysis$BSF_deposit_3level,subset_analysis$year),
        main="BSF Deposit Rules by Quarter",
        beside=FALSE,col=rainbow(3), legend.text=TRUE,
        args.legend=list(x="topleft", cex=0.7, bty="n"))

barplot(table(subset_analysis$BSF_deposit_5level,subset_analysis$year),
        main="BSF Deposit Rules by Quarter",
        beside=FALSE,col=rainbow(5), legend.text=TRUE,
        args.legend=list(x="topleft", cex=0.7, bty="n"))

barplot(table(subset_analysis$BSF_withdrawal_3level,subset_analysis$year),
        main="BSF Withdrawal Rules by Quarter",
        beside=FALSE,col=rainbow(3), legend.text=TRUE,
        args.legend=list(x="topleft", cex=0.7, bty="n"))

barplot(table(subset_analysis$BSF_withdrawal_5level,subset_analysis$year),
        main="BSF Withdrawal Rules by Quarter",
        beside=FALSE,col=rainbow(5), legend.text=TRUE,
        args.legend=list(x="topleft", cex=0.7, bty="n"))

table(subset_analysis$BSF_deposit_3level)
table(subset_analysis$BSF_deposit_5level)
table(subset_analysis$BSF_withdrawal_3level)
table(subset_analysis$BSF_withdrawal_5level)

# Filtrar os dados para o período de interesse
oil_filtered <- oil_qtr %>%
  filter(quarter_year >= as.yearqtr("1976 Q1") & quarter_year <= as.yearqtr("2009 Q4")) %>%
  dplyr::select(quarter_year, oil_qtr_avg, oil_shock_positive,oil_shock_negative)

# Escalar ambas as variáveis
oil_filtered <- oil_filtered %>%
  mutate(across(c(oil_qtr_avg, oil_shock_positive,oil_shock_negative), scale))

# Plotar com ggplot
ggplot(oil_filtered, aes(x = quarter_year)) +
  geom_line(aes(y = oil_qtr_avg, color = "Preço Médio do Petróleo (Escalado)")) +
  geom_line(aes(y = oil_shock_positive, color = "Choque Positivo no Petróleo (Escalado)")) +
  geom_line(aes(y = oil_shock_negative, color = "Choque Negativo no Petróleo (Escalado)")) +
  scale_color_manual(values = c("Preço Médio do Petróleo (Escalado)" = "blue",
                                "Choque Positivo no Petróleo (Escalado)" = "green",
                                "Choque Negativo no Petróleo (Escalado)" = "red")) +
  scale_x_yearqtr(format = "%Y", n = 10) +
  labs(title = "Preço e Choques de Preço do Petróleo (Escalados)",
       x = "Trimestre",
       y = "Valor (Escalado)",
       color = "Variáveis") +
  theme_minimal()


############### Tests for Stationarity of the Dependent variables

purtest(na.omit(df_analysis[,c(1,2,7)])[,c('Approval_Not_Smoothed')],test='ips'
        ,exo='intercept',lags=2)

purtest(na.omit(df_analysis[,c(1,2,8)])[,c('Disapproval_Not_Smoothed')],test='ips'
        ,exo='intercept',lags=2)

purtest(na.omit(df_analysis[,c(1,2,9)])[,c('Relative_Not_Smoothed')],test='ips'
        ,exo='intercept',lags=2)

############### Aggregating variables for the models:

dependent_var_app <- "Approval_Not_Smoothed"
dependent_var_disapp <- "Disapproval_Not_Smoothed"
dependent_var_relapp <- "Relative_Not_Smoothed"

# Variables measured as in Engemann et al. (2013)
oil_variables <- c(
                  "oil_shock_positive_l1"
                   ,"oil_shock_positive_l2"
                   ,"oil_shock_positive_l3"
                   ,"oil_shock_positive_l4"
                   #,"oil_shock_positive_l5"
                   ,"oil_shock_negative_l1"
                   ,"oil_shock_negative_l2"
                   ,"oil_shock_negative_l3"
                   ,"oil_shock_negative_l4"
                   # ,"oil_shock_negative_l5"
                  )

oil_variables_sig <- c(
  "significant_negative_effect_from_positive_shock"
  ,"oil_shock_positive_l1:significant_negative_effect_from_positive_shock"
  ,"oil_shock_positive_l2:significant_negative_effect_from_positive_shock"
  ,"oil_shock_positive_l3:significant_negative_effect_from_positive_shock"
  ,"oil_shock_positive_l4:significant_negative_effect_from_positive_shock"
#  ,"oil_shock_positive_l5:significant_negative_effect_from_positive_shock"
  ,"oil_shock_negative_l1:significant_negative_effect_from_positive_shock"
  ,"oil_shock_negative_l2:significant_negative_effect_from_positive_shock"
  ,"oil_shock_negative_l3:significant_negative_effect_from_positive_shock"
  ,"oil_shock_negative_l4:significant_negative_effect_from_positive_shock"
#  ,"oil_shock_negative_l5:significant_negative_effect_from_positive_shock"
)

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
fixed_effects <- c( "factor(quarter_year)",
                   "factor(state)")

# BSF variables
BSF_withdrawal_variables <- c(
  'BSF_withdrawal_3level'
  ,'BSF_withdrawal_3level:oil_shock_positive_l1'
  ,'BSF_withdrawal_3level:oil_shock_positive_l2'
  ,'BSF_withdrawal_3level:oil_shock_positive_l3'
  ,'BSF_withdrawal_3level:oil_shock_positive_l4'
  ,'BSF_withdrawal_3level:oil_shock_negative_l1'
  ,'BSF_withdrawal_3level:oil_shock_negative_l2'
  ,'BSF_withdrawal_3level:oil_shock_negative_l3'
  ,'BSF_withdrawal_3level:oil_shock_negative_l4'
)

BSF_deposit_variables <- c(
  'BSF_deposit_3level'
  ,'BSF_deposit_3level:oil_shock_positive_l1'
  ,'BSF_deposit_3level:oil_shock_positive_l2'
  ,'BSF_deposit_3level:oil_shock_positive_l3'
  ,'BSF_deposit_3level:oil_shock_positive_l4'
  ,'BSF_deposit_3level:oil_shock_negative_l1'
  ,'BSF_deposit_3level:oil_shock_negative_l2'
  ,'BSF_deposit_3level:oil_shock_negative_l3'
  ,'BSF_deposit_3level:oil_shock_negative_l4'
)


############### Models
# 
# ### Models of oil on approval 
models_oil_app_psar1 <- list()

for (dv in c(dependent_var_app)) {#, dependent_var_disapp, dependent_var_relapp)) {
  model_formula <- as.formula(paste(dv
                                    ,'~',paste(oil_variables, collapse = " + ")
                                    #,'+',paste(unemployment, collapse = " + ")
                                    #,'+',paste(controls_singer, collapse = " + ")
                                    #,'+',paste(fixed_effects, collapse = " + ")
                                    ))
  models_oil_app_psar1[[dv]] <- panelAR( # Base teórica: Beck & Katz (1995)
    formula = model_formula
    ,data = subset_analysis
    ,panelVar = "state"
    ,timeVar = "qtr"
    ,autoCorr = "psar1" # Panel Specific AR(1) correction
    ,panelCorrMethod = "phet" # Panel-corrected standard errors (PCSE)
  )
}
# 
# 
# 
# ### Models of oil on approval with shock significance variable
# 
# #models_oil_sig_app_lm <- list()
# models_oil_sig_app_psar1 <- list()
# 
# for (dv in c(dependent_var_app )) {#, dependent_var_disapp, dependent_var_relapp)) {
#   model_formula <- as.formula(paste(dv
#                                     ,'~',paste(oil_variables, collapse = " + ")
#                                     #,'+',paste(oil_variables_sig, collapse = " + ")
#                                     ,'+',paste(unemployment, collapse = " + ")
#                                     ,'+',paste(controls_singer, collapse = " + ")
#                                     #,'+',paste(fixed_effects, collapse = " + ")
#   ))
#   #models_oil_sig_app_lm[[dv]] <- lm(model_formula,subset_analysis)
#   
#   models_oil_sig_app_psar1[[dv]] <- panelAR( # Base teórica: Beck & Katz (1995)
#     formula = model_formula
#     ,data = subset_analysis
#     ,panelVar = "state"
#     ,timeVar = "qtr"
#     ,autoCorr = "psar1" # Panel Specific AR(1) correction
#     ,panelCorrMethod = "phet" # Panel-corrected standard errors (PCSE)
#   ) 
# }
# 
# ### Models of oil on approval with Singer's controls
# models_oil_controls_app_psar1 <- list()
# 
# for (dv in c(dependent_var_app, dependent_var_disapp, dependent_var_relapp)) {
#   model_formula <- as.formula(paste(dv
#                                     ,'~',paste(oil_variables, collapse = " + ")
#                                     #,'+',paste(oil_variables_sig, collapse = " + ")
#                                     ,'+',paste(unemployment, collapse = " + ")
#                                     ,'+',paste(controls_singer, collapse = " + ")
#                                     ,'+',paste(fixed_effects, collapse = " + ")
#   ))
#   models_oil_controls_app_psar1[[dv]] <- panelAR( # Base teórica: Beck & Katz (1995)
#     formula = model_formula
#     ,data = subset_analysis
#     ,panelVar = "state"
#     ,timeVar = "qtr"
#     ,autoCorr = "psar1" # Panel Specific AR(1) correction
#     ,panelCorrMethod = "phet" # Panel-corrected standard errors (PCSE)
#   ) 
# }

### Models BSF withdrawal
# models_BSF_withdrawal_psar1 <- list()
# 
# for (dv in c(dependent_var_app, dependent_var_disapp, dependent_var_relapp)) {
#   model_formula <- as.formula(paste(dv
#                                     ,'~',paste(oil_variables, collapse = " + ")
#                                     ,'+',paste(BSF_withdrawal_variables, collapse = " + ")
#                                     #,'+',paste(controls_singer, collapse = " + ")
#                                     #,'+',paste(fixed_effects, collapse = " + ")
#   ))
#   models_BSF_withdrawal_psar1[[dv]] <- panelAR( # Base teórica: Beck & Katz (1995)
#     formula = model_formula
#     ,data = subset_analysis
#     ,panelVar = "state"
#     ,timeVar = "qtr"
#     ,autoCorr = "psar1" # Panel Specific AR(1) correction
#     ,panelCorrMethod = "phet" # Panel-corrected standard errors (PCSE)
#   ) 
# }
# 
### Models BSF deposit
models_BSF_deposit_psar1 <- list()

for (dv in c(dependent_var_app, dependent_var_disapp, dependent_var_relapp)) {
  model_formula <- as.formula(paste(dv
                                    ,'~',paste(oil_variables, collapse = " + ")
                                    ,'+',paste(BSF_deposit_variables, collapse = " + ")
                                    #,'+',paste(controls_singer, collapse = " + ")
                                    #,'+',paste(fixed_effects, collapse = " + ")
  ))
  models_BSF_deposit_psar1[[dv]] <- panelAR( # Base teórica: Beck & Katz (1995)
    formula = model_formula
    ,data = subset_analysis
    ,panelVar = "state"
    ,timeVar = "qtr"
    ,autoCorr = "psar1" # Panel Specific AR(1) correction
    ,panelCorrMethod = "phet" # Panel-corrected standard errors (PCSE)
  )
}

############### lm.beta and Mediation through employment

# linear_models <- list()
# 
# for (dv in 
#      c(dependent_var_app)) { #,dependent_var_disapp, dependent_var_relapp)) {
#   #   c('unemployment_state')) {
#   model_formula <- as.formula(paste(dv
#                                     ,'~',paste(oil_variables, collapse = " + ")
#                                     ,'+',paste(unemployment, collapse = " + ")
#                                     #,'+',paste(oil_variables_sig, collapse = " + ")
#                                     ,'+',paste(controls_singer, collapse = " + ")
#                                     ,'+',paste(fixed_effects, collapse = " + ")
#   ))
#   linear_models[[dv]] <- lm(model_formula,subset_analysis)
# 
# }
# 
# summary(linear_models$Approval_Not_Smoothed)
############### Tables

# ### Models oil on approval
# Reduce(function(x, y) merge(x, y, by = "term", all = TRUE),
#        lapply(names(models_oil_app_psar1), function(name) {
#          s <- summary(models_oil_app_psar1[[name]])$coefficients
#          stars <- cut(s[, 4], breaks = c(-Inf, 0.001, 0.01, 0.05, 0.1, Inf),
#                       labels = c("***", "**", "*", ".", ""))
#          out <- data.frame(term = rownames(s),
#                            value = sprintf("%.3f (%.3f)%s", s[, 1], s[, 2], stars),
#                            row.names = NULL)
#          names(out)[2] <- name
#          out
#        }))




### Tables of Models
model_lists <- list(
models_oil_app_psar1,
# models_oil_sig_app_psar1,
  # models_oil_controls_app_psar1,
#  models_BSF_withdrawal_psar1
  models_BSF_deposit_psar1
)

for (model in model_lists) {
  print(Reduce(function(x, y) merge(x, y, by = "term", all = TRUE),
         lapply(names(model), function(name) {
           s <- summary(model[[name]])$coefficients
           stars <- cut(s[, 4], breaks = c(-Inf, 0.001, 0.01, 0.05, 0.1, Inf),
                        labels = c("***", "**", "*", ".", ""))
           out <- data.frame(term = rownames(s),
                             value = sprintf("%.3f (%.3f)%s", s[, 1], s[, 2], stars),
                             row.names = NULL)
           names(out)[2] <- name
           out
         })))
  
}

for (model in model_lists){
  print(summary(model$Approval_Not_Smoothed))
}

# ############### Marginal Effects
m    <- models_BSF_deposit_psar1$Approval_Not_Smoothed
vc   <- vcov(m)
b    <- coef(m)
mod  <- "BSF_deposit_3level"
lags <- paste0("oil_shock_positive_l", 1:4)
levs <- levels(model.frame(m)[[mod]])

# Função para encontrar termo de interação
int_term <- function(X, lvl, bnames) {
  cand <- c(paste0(X, ":", mod, lvl),
            paste0(mod, lvl, ":", X))
  match <- cand[cand %in% bnames]
  if (length(match)) match[1] else NA_character_
}

# Calcula ME e IC para cada lag e nível
one_me <- function(X, lvl) {
  if (!(X %in% names(b))) return(NULL)
  t_int <- int_term(X, lvl, names(b))
  if (is.na(t_int)) {
    me  <- b[X]
    var <- vc[X, X]
  } else {
    me  <- b[X] + b[t_int]
    var <- vc[X, X] + vc[t_int, t_int] + 2 * vc[X, t_int]
  }
  se <- sqrt(max(var, 0))
  data.frame(Lag = X,
             Moderator_Level = lvl,  # agora é o nome direto do nível
             me  = me,
             low = me - 1.96 * se,
             up  = me + 1.96 * se,
             row.names = NULL)
}

# Juntar tudo
df <- do.call(rbind, lapply(lags, function(X) {
  do.call(rbind, lapply(levs, function(lvl) one_me(X, lvl)))
}))

df

ggplot(df, aes(x = Lag, y = me, color = Moderator_Level)) +
  geom_point(position = position_dodge(0.3)) +
  geom_errorbar(aes(ymin = low, ymax = up),
                width = 0.2,
                position = position_dodge(0.3)) +
  geom_hline(yintercept = 0, linetype = "dashed") +
  labs(y = "Marginal Effect", x = "Lag", color = "Moderator") +
  theme_minimal()

# ############### Marginal Effects com a Bilbioteca Marginal Effects
# 
# lags <- paste0("oil_shock_positive_l", 1:4)
# fml <- as.formula(paste("Relative_Not_Smoothed ~",
#                         paste(c(lags, "significant_negative_effect_from_positive_shock",
#                                 paste0(lags, ":significant_negative_effect_from_positive_shock")), collapse = " + ")))
# model <- lm(fml, df_analysis)
# 
# plot_comparisons(model,
#                  variables = lags,
#                  condition = list(significant_negative_effect_from_positive_shock = c(0, 1)))
# 
# # Estimando o modelo com interações
# lags <- paste0("oil_shock_positive_l", 1:4)
# fml <- as.formula(paste("Relative_Not_Smoothed ~",
#                         paste(c(lags, "significant_negative_effect_from_positive_shock",
#                                 paste0(lags, ":significant_negative_effect_from_positive_shock")), collapse = " + ")))
# model <- lm(fml, df_analysis)
# 
# 
# ############### Wald Test
# 
# lags <- paste0("oil_shock_positive_l", 1:4)
# interaction_vars <- paste0(lags, ":significant_negative_effect_from_positive_shock")
# hypotheses <- paste0(lags, " + ", paste0(lags, ":significant_negative_effect_from_positive_shock"), " = 0")
# interactions <- paste0(paste0(lags, ":significant_negative_effect_from_positive_shock"), " = 0")
# 
# # Testa se os efeitos marginais para Z=1 (coef + interação) são todos nulos
# linearHypothesis(models_oil_controls_app_psar1$Approval_Not_Smoothed, hypotheses)
# 
# # Testa se as interações são todas nulas (ou seja, sem modificação de efeito)
# linearHypothesis(models_oil_controls_app_psar1$Relative_Not_Smoothed, interactions)
# 
# m <- models_oil_app_psar1$Relative_Not_Smoothed
# 
# coefs <- coef(m)
# vc <- vcov(m)
# 
# linearHypothesis(, hypotheses)




