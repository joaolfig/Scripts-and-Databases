library(plm)
library(lmtest) # Para os clustered standard errors
library(sandwich) # Para os clustered standard errors
library(texreg)
library(panelAR)
# install.packages("C:/Users/Joao arthur/Downloads/panelAR_0.1.tar.gz", 
#                  type= "source", 
#                  repos= NULL)

setwd("C:/Users/Joao arthur/OneDrive - Fundacao Getulio Vargas - FGV/Dissertação/Scripts-and-Databases")
#setwd("C:/Users/b435097/OneDrive - Fundacao Getulio Vargas - FGV/Dissertação/Scripts-and-Databases")


rm(list=ls())

############### Load Dta ################
source('Scripts/R script dissertacao - State Approval.R')
source('Scripts/R script dissertacao - Oil prices.R')
source('Scripts/R script dissertacao - BSF Data.R')
source('Scripts/R script dissertacao - Singer (2023).R')

rm(list=setdiff(ls(), c('df_approval_qtr','oil_qtr',"BSF_dataset",
                        'panel_data','filtered_data')))

data <- merge(df_approval_qtr, oil_qtr, by = c("year", "quarter"), all.x = TRUE)

data <- merge(data, BSF_dataset, by = c("state", "year"), all.x = TRUE)

# Convert dataframe into a Panel Data Frame format

data$quarter_year <- as.yearqtr(paste(data$year, data$quarter), format = "%Y %q")

#Replace all "." in data$quarter_year with "_"
pdata <- pdata.frame(data, index = c( "state","quarter_year"))

#Count values in Approval_Not_Smoothed by state
table(data$Approval_Not_Smoothed, useNA = "always")

# #exclude rows with NA in Approval_Not_Smoothed
pdata <- pdata[!is.na(pdata$Approval_Not_Smoothed), ]

#Plot approval data on qtr


# 
# #Convert data from pdata into wide format
# library(reshape2)
# 
# data_wide <- dcast(pdata, year + quarter ~ state, value.var = "Approval_Not_Smoothed")
# 
# #In data_wide drop columns where all values are NA
# data_wide <- data_wide[, colSums(is.na(data_wide)) < nrow(data_wide)]



#Create a variable that is a concat of state and incumbent
pdata$state_incumbent <- paste(pdata$state, pdata$incumbent, sep = "_")

#pdata$quarter_year <- gsub("\\.", "Q", round(pdata$quarter_year,2))

#merge filtered_data with oil_qtr
singer_w_oil <- merge(filtered_data,oil_qtr,
               by = c("year", "quarter"), all.x = TRUE)



########### Test assumptions
# Test for stationarity:
purtest(pdata[,c('Approval_Smoothed')],test='ips'
        ,exo='intercept',lags='AIC',pmax=5)
# possível rejeitar a hipótese nula de não-estacionariedade. 

# Test for serial autocorrelation:
model_FE <- plm(Approval_Smoothed ~
               + lag(oil_qrt_shock_positive_2)
               ,data = pdata
               ,model = "within" # "within" = fixed effects
               ,index = c("state_incumbent", "quarter_year"))
pwartest(model_FE)

# Tentativas para resolver serial autocorrelation
# coeftest(model_FE, vcov = vcovHC(model_FE, type = "HC1", cluster = "group"))
# 
# model_ab <- pgmm(
#   Approval_Smoothed ~ lag(Approval_Smoothed, 1) |
#     lag(Approval_Smoothed, 2:99), 
#   data = pdata, 
#   effect = "individual", 
#   model = "twosteps", 
#   transformation = "d"
# )
# summary(model_ab)
# 
# model_FE_AR1 <- plm(Approval_Not_Smoothed ~
#                 + lag(Approval_Not_Smoothed)     
#                 + factor(quarter_year)
#                 + state_incumbent
#                 ,data = pdata
#                 ,model = "within" # "within" = fixed effects
#                 ,index = c("id", "quarter_year"))
# pwartest(model_FE_AR1)
# 
# model_FE_PSAR1 <-panelAR(
#                 formula = Approval_Smoothed 
#                 ~ factor(quarter_year) + state_incumbent,
#                 data = pdata,
#                 panelVar = "state",      
#                 timeVar = "qtr",   
#                 autoCorr = "psar1", # autocorrelação Panel Specific AR(1)
#                 panelCorrMethod = "pcse"    
#               )
# pwartest(model_FE_PSAR1)
# 
# ########### Analysis
# 
# screenreg(plm(Approval_Smoothed
#     ~ lag(oil_qrt_shock_positive)
#     + lag(oil_qrt_shock_negative)
#               + lag(Approval_Smoothed)
#               + lag(oil_deflated_fst_diff)
#               + factor(quarter_year)
#               + state_incumbent
#               ,data=pdata
# ),omit.coef=c("state_incumbent|quarter_year"))
# 
# screenreg(plm(Approval_Smoothed
#               ~ lag(oil_qrt_shock_positive)
#               + lag(oil_qrt_shock_negative)
#               + lag(Approval_Smoothed)
#               + lag(oil_deflated_fst_diff)
#               + lag(oil_deflated_qtr) # Não sei se faz sentido ter os dois, ou se isso aqui causa correlação espúria
#               + factor(quarter_year)
#               + state_incumbent
#               ,data=pdata
# ),omit.coef=c("state_incumbent|quarter_year"))
# 
# glimpse(pdata)

##### Efeito do oil em unemployment

screenreg(plm(unemployment_state_l0 ~
      lag(oil_qrt_shock_positive_2) 
    + lag(oil_qrt_shock_negative_2),
    data = singer_w_oil,
    model = "within",
    index = c("staten", "qtr")),omit.coef = "state|qtr")

screenreg(plm(Approval_Not_Smoothed ~
                lag(oil_qrt_shock_positive_2) 
              + lag(oil_qrt_shock_negative_2)
              + lag(Approval_Not_Smoothed),
              data = singer_w_oil,
              model = "within",
              index = c("staten", "qtr")),omit.coef = "state|qtr")

########################################

#Colocar um event study aqui

########################################
# Identificar qual unidade contrbui mais para o coeficiente do oil_qrt_shock_positive_2
results <- list()
for (i in unique(singer_w_oil$state)) {
  temp_data <- subset(singer_w_oil, state != i)
  model <- plm(unemployment_state_l0 ~
                 lag(oil_qrt_shock_positive_2) 
               + lag(oil_qrt_shock_negative_2),
               data = temp_data,
               model = "within",
               index = c("staten", "qtr"))
  results[[i]] <- coef(model)["lag(oil_qrt_shock_positive_2)"]
}
plot(unlist(results))


#plot a line plot of oil prices_deflted with a scatter of shocks
ggplot(
  data = subset(as.data.frame(singer_w_oil), state == "Michigan"),
  aes(x = time, y = unemployment_state_l0)
) +
  geom_line(color = "blue") +
  geom_point(data = subset(as.data.frame(singer_w_oil), state == "Michigan" & oil_qrt_shock_positive_2 == 1),
             aes(x = time, y = unemployment_state_l0), color = "green", size = 1.5) +
  geom_point(data = subset(as.data.frame(singer_w_oil), state == "Michigan" & oil_qrt_shock_negative_2 == 1),
             aes(x = time, y = unemployment_state_l0), color = "red", size = 1.5) +
  labs(title = "Aprovação em Wyoming e Choques de Petróleo",
       x = "Trimestre",
       y = "Aprovação (não suavizada)") +
  theme_minimal()



#################
dependent_vars <- c("Approval_Not_Smoothed", "Disapproval_Not_Smoothed", "Relative_Not_Smoothed")

independent_vars <- c(
  "lag(oil_qrt_shock_positive,1)","lag(oil_qrt_shock_negative,1)"
)

# Loop pra criar e rodar os modelos
models <- list('Relative_Not_Smoothed', 'Approval_Not_Smoothed', 'Disapproval_Not_Smoothed')

for (dep_var in dependent_vars) {
  # Correctly create the formula string by collapsing independent variables with " + "
  formula_text <- paste(dep_var, "~", paste(independent_vars, collapse = " + "))
  model_formula <- as.formula(formula_text)
  
  
  model <- panelAR(
    formula = model_formula,
    data = singer_w_oil,
    panelVar = "staten",
    timeVar = "qtr",
    autoCorr = "psar1",
    panelCorrMethod = "pcse"
  )
  
  models[[dep_var]] <- model
}
glimpse(singer_w_oil)
summary(models$Relative_Not_Smoothed)
summary(models$Approval_Not_Smoothed)
summary(models$Disapproval_Not_Smoothed)
