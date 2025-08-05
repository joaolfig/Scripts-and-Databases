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

# Data for replicating "Where is an oil shock?" by Engemann et al. (2013)
df_wos <- employment_qtr[,c(1,2,3,4)] %>% #3,4:8
  left_join(oil_qtr[,c(6,9:10)], by = c( "quarter_year")) %>% #9:19
  # Set a dm_katrina where quarter_year is between 2005 Q4 and 2006 Q4
  #mutate(dm_katrina = ifelse(quarter_year >= as.yearqtr("2005 Q4") & quarter_year <= as.yearqtr("2006 Q4"), 1, 0))
  mutate(dm_katrina = ifelse(quarter_year == as.yearqtr("2005 Q3"),1,0)
  #        ,dm_katrina_l1 = dplyr::lag(dm_katrina, 1)
  #        ,dm_katrina_l2 = dplyr::lag(dm_katrina, 2)
  #        ,dm_katrina_l3 = dplyr::lag(dm_katrina, 3)
  #        ,dm_katrina_l4 = dplyr::lag(dm_katrina, 4)
         )

df_wos <- subset(df_wos, quarter_year >= as.yearqtr("1973 Q4") & quarter_year <= as.yearqtr("2008 Q4")) 


screenreg(lm(employment_logchange~ 
  dplyr::lag(employment_logchange,1) + dplyr::lag(employment_logchange,2) + dplyr::lag(employment_logchange,3) + dplyr::lag(employment_logchange,4)
  + dplyr::lag(oil_shock_positive,1) + dplyr::lag(oil_shock_positive,2) + dplyr::lag(oil_shock_positive,3) + dplyr::lag(oil_shock_positive,4)
  + dplyr::lag(oil_shock_negative,1) + dplyr::lag(oil_shock_negative,2) + dplyr::lag(oil_shock_negative,3) + dplyr::lag(oil_shock_negative,4)
#  + dplyr::lag(dm_katrina,1) + dplyr::lag(dm_katrina,2) + dplyr::lag(dm_katrina,3) + dplyr::lag(dm_katrina,4)
    ,data=df_wos[df_wos$state=="US",]),digits=3)

VARselect(df_wos[df_wos$state=="US",c(4,5:7)], lag.max = 10, type = "const")

screenreg(VAR(df_wos[df_wos$state=="US",c(4,5:7)], p = 4, type = "const")$varresult$employment_logchange)




df_wos[df_wos$state=="CA",c(4,5:7)]

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

############### Models
# Replication of Engemann et al. 2013
# model_oil_empl <- plm(formula = model_formula,
#                       data = df_analysis,
#                       model = "pooling")
# 

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
### Singer but using employment growth as independent variable
models_empl_app_emplgrowth <- list()
models_empl_app_psar1_emplgrowth <- list()

for (dv in c(dependent_var_app, dependent_var_disapp, dependent_var_relapp)) {
  model_formula <- as.formula(paste(dv
                                    ,'~',paste(empl_growth, collapse = " + ")
                                    ,'+',paste(controls_singer, collapse = " + ")
  ))
  models_empl_app_emplgrowth[[dv]] <- plm(formula = model_formula
                               ,data = subset(df_analysis, year >= 1976 & year <= 2019 & valid_3qtr == 1 & !state %in% c("HI", "ID", "ND", "OK", "SD", "VT")),
                               model = "pooling")
  
  models_empl_app_psar1_emplgrowth[[dv]] <- panelAR( # Base teórica: Beck & Katz (1995)
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

models_oil_app <- list()
models_oil_app_psar1 <- list()

for (dv in c(dependent_var_app, dependent_var_disapp, dependent_var_relapp)) {
  model_formula <- as.formula(paste(dv
                                    ,'~',paste(oil_variables, collapse = " + ")
                                    #,'+',paste(controls_singer, collapse = " + ")
                                    #,'+',paste(fixed_effects, collapse = " + ")
                                    ))
  models_oil_app_psar1[[dv]] <- panelAR( # Base teórica: Beck & Katz (1995)
    formula = model_formula
    ,data = subset(df_analysis, quarter_year >= as.yearqtr("1973 Q4") & quarter_year <= as.yearqtr("2008 Q4")& valid_surveys == 1 & !state %in% c("HI", "ID", "ND", "OK", "SD", "VT")) %>%
      filter(!is.na(Relative_Not_Smoothed),
             !is.na(Approval_Not_Smoothed),
             !is.na(Disapproval_Not_Smoothed))
    ,panelVar = "state"
    ,timeVar = "qtr"
    ,autoCorr = "psar1" # Panel Specific AR(1) correction
    ,panelCorrMethod = "phet" # Panel-corrected standard errors (PCSE)
  ) 
}



unique(subset(df_analysis, quarter_year >= as.yearqtr("1973 Q4") & quarter_year <= as.yearqtr("2008 Q4")& valid_surveys == 1 & !state %in% c("HI", "ID", "ND", "OK", "SD", "VT"))$state)

# subset(df_analysis, 
#        quarter_year >= as.yearqtr("1973 Q4") & 
#          quarter_year <= as.yearqtr("2008 Q4") & 
#          valid_surveys == 1 & 
#          !state %in% c("HI", "ID", "ND", "OK", "SD", "VT"))

############### Tables

#### Modelos Singer (2023): Honeymoon and Unemployment
# screenreg(models_empl_app
#           ,custom.model.names = c("Relative Approval","Approval", "Disapproval")
#           , digits = 5)

for (model in models_empl_app_psar1) {
  print(summary(model))
}

for (model in models_empl_app_psar1_emplgrowth) {
  print(summary(model))
}

for (model in models_oil_app_psar1) {
  print(summary(model))
}

confint(models_oil_app_psar1$Relative_Not_Smoothed)
coef(models_oil_app_psar1$Relative_Not_Smoothed) + summary(model)$coefficients[, "Std. Error"]



 # 
# screenreg(models_oil_app
#           ,custom.model.names = c("Relative Approval", "Approval", "Disapproval")
#           ,omit.coef = "incumbent|quarter"
#           ,custom.gof.rows = list("Time FE"=c("Yes","Yes","Yes")
#                                   ,"Incumbent FE"=c("Yes","Yes","Yes")
#                                   ,"Nº States"=c("#","#","#"))
#           , digits = 5)


# modelo <- lm(,
#              data)
# 
#   "oil_shock_positive_l1"
# ,"oil_shock_positive_l2"
# ,"oil_shock_positive_l3"
# ,"oil_shock_positive_l4"
# ,"oil_shock_negative_l1"
# ,"oil_shock_negative_l2"
# ,"oil_shock_negative_l3"
# ,"oil_shock_negative_l4"
# ,"significant_negative_effect_from_positive_shock"
# ,"oil_shock_positive_l1:significant_negative_effect_from_positive_shock"
# ,"oil_shock_positive_l2:significant_negative_effect_from_positive_shock"
# ,"oil_shock_positive_l3:significant_negative_effect_from_positive_shock"
# ,"oil_shock_positive_l4:significant_negative_effect_from_positive_shock"
# 
# # Comparação média dos efeitos condicionais
# comparacoes <- avg_comparisons(
#   modelo,
#   variables = "X",
#   by = "Z"
# )
# 
# # Exibir resumo dos resultados
# summary(comparacoes)


mod <- lm(mpg ~ hp * drat * factor(am), data = mtcars)


plot_comparisons(mod, variables = "am", condition = list("hp", "drat" = range))

plot_comparisons(mod, variables = "am", condition = list("hp", "drat" = "threenum"))

colnames(df_analysis)
lm1 <- lm(Approval_Not_Smoothed ~ 
      factor(state)
      + factor ( year)
      +           oil_shock_positive_l2*significant_negative_effect_from_positive_shock
     ,data=subset(df_analysis, quarter_year >= as.yearqtr("1973 Q4") & quarter_year <= as.yearqtr("2008 Q4")& valid_surveys == 1 & !state %in% c("HI", "ID", "ND", "OK", "SD", "VT")) %>%
       filter(!is.na(Relative_Not_Smoothed),
              !is.na(Approval_Not_Smoothed),
              !is.na(Disapproval_Not_Smoothed)))

plot_comparisons(lm1,
                 variables = "oil_shock_positive_l2",
                 condition = list(significant_negative_effect_from_positive_shock = c(0, 1)),
                 data = subset(df_analysis,
                               quarter_year >= as.yearqtr("1973 Q4") & quarter_year <= as.yearqtr("2008 Q4") &
                                 valid_surveys == 1 &
                                 !state %in% c("HI", "ID", "ND", "OK", "SD", "VT")) %>%
                   filter(!is.na(Relative_Not_Smoothed),
                          !is.na(Approval_Not_Smoothed),
                          !is.na(Disapproval_Not_Smoothed)))

avg_comparisons(
  lm1,
  variables = "oil_shock_positive_l2",
  condition = list(significant_negative_effect_from_positive_shock = c(0, 1)),
  data = subset(df_analysis,
                quarter_year >= as.yearqtr("1973 Q4") & quarter_year <= as.yearqtr("2008 Q4") &
                  valid_surveys == 1 &
                  !state %in% c("HI", "ID", "ND", "OK", "SD", "VT")) %>%
    filter(!is.na(Relative_Not_Smoothed),
           !is.na(Approval_Not_Smoothed),
           !is.na(Disapproval_Not_Smoothed))
)


##### Tentativa de estimar marginal effects manualmente:

# Obtem os coeficientes do modelo
coefs <- coef(models_oil_app_psar1$Relative_Not_Smoothed)

# Efeito marginal quando Z = 0 (sem interação)
efeito_z0 <- coefs["oil_shock_positive_l2"]

# Efeito marginal quando Z = 1 (com interação)
efeito_z1 <- coefs["oil_shock_positive_l2"] +
  coefs["oil_shock_positive_l2:significant_negative_effect_from_positive_shock"]

# Matriz de variância-covariância
vcov_mat <- vcov(models_oil_app_psar1$Relative_Not_Smoothed)

# Erro padrão do efeito quando Z = 0
se_z0 <- sqrt(vcov_mat["oil_shock_positive_l2", "oil_shock_positive_l2"])

# Erro padrão do efeito quando Z = 1 (soma da variância + 2*covariância)
se_z1 <- sqrt(
  vcov_mat["oil_shock_positive_l2", "oil_shock_positive_l2"] +
    vcov_mat["oil_shock_positive_l2:significant_negative_effect_from_positive_shock",
             "oil_shock_positive_l2:significant_negative_effect_from_positive_shock"] +
    2 * vcov_mat["oil_shock_positive_l2", 
                 "oil_shock_positive_l2:significant_negative_effect_from_positive_shock"]
)

# Estatísticas t
t_z0 <- efeito_z0 / se_z0
t_z1 <- efeito_z1 / se_z1

# p-valores
p_z0 <- 2 * (1 - pnorm(abs(t_z0)))
p_z1 <- 2 * (1 - pnorm(abs(t_z1)))

data.frame(
  Z = c(0, 1),
  Efeito = c(efeito_z0, efeito_z1),
  SE = c(se_z0, se_z1),
  t = c(t_z0, t_z1),
  p_value = c(p_z0, p_z1)
)

# Dados manuais
efeito <- c(0.0691, -0.0691)
se <- c(0.0347, 0.0222)
grupos <- c("Sem efeito negativo", "Com efeito negativo")

# Intervalo de confiança 95%
lower <- efeito - 1.96 * se
upper <- efeito + 1.96 * se


# Define posição dos pontos no eixo x
x_pos <- 1:2

# Abre o gráfico
plot(
  x = x_pos,
  y = efeito,
  ylim = range(c(lower, upper)),
  xlim = c(0.5, 2.5),
  xaxt = "n",
  pch = 19,
  xlab = "Efeito negativo anterior",
  ylab = "Efeito marginal estimado",
  main = "Efeito marginal de choques positivos sobre aprovação"
)

# Adiciona os nomes dos grupos no eixo x
axis(1, at = x_pos, labels = grupos)

# Adiciona barras de erro (IC 95%)
arrows(x0 = x_pos, y0 = lower, x1 = x_pos, y1 = upper, angle = 90, code = 3, length = 0.1)

# Linha horizontal em zero
abline(h = 0, lty = 2, col = "gray40")


