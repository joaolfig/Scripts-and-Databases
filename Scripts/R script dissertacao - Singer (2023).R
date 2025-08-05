library(haven)
library(dplyr)
library(plm)
library(nlme)
library(texreg)
library(panelAR)

setwd("C:/Users/Joao arthur/OneDrive - Fundacao Getulio Vargas - FGV/Dissertação/Scripts-and-Databases")

sead <- read_dta("Databases/Replication Files for Singer (2023)/SEAD governor quarterly v1.dta")

# Ordenar os dados por estado e trimestre (opcional em R, mas pode ajudar na organização)
sead <- sead %>% arrange(state, qtr)

# Substitua com o caminho do seu arquivo de controles
controls <- read_dta("Databases/Replication Files for Singer (2023)/controls for table 3.dta")

controls <- controls %>% arrange(state, qtr)

# Faz o merge com base em state e qtr
panel_data <- left_join(controls, sead, by = c("state", "qtr"))

panel_data <- pdata.frame(panel_data, index = c("statenumber", "qtr"))

panel_data$unemployment_state_l0 <- panel_data$unemployment_state
panel_data$unemployment_state_l1 <- lag(panel_data$unemployment_state)
panel_data$unemployment_state_l2 <- lag(panel_data$unemployment_state,2)
panel_data$governorresignedthatquarter_l0 <- panel_data$governorresignedthatquarter
panel_data$governorresignedthatquarter_l1 <- lag(panel_data$governorresignedthatquarter)
panel_data$governorresignedthatquarter_l2 <- lag(panel_data$governorresignedthatquarter, 2)
panel_data$governordiedthatquarter_l0 <- panel_data$governordiedthatquarter
panel_data$governordiedthatquarter_l1 <- lag(panel_data$governordiedthatquarter)
panel_data$governordiedthatquarter_l2 <- lag(panel_data$governordiedthatquarter, 2)

panel_data <- panel_data %>%
  rename(year = year.x) %>%
  rename(quarter = quarter.x)

panel_data$qtr <- as.integer(panel_data$qtr)

filtered_data <- panel_data %>%
  filter(
    valid_3qtr == 1,
    year <= 2019,
    !is.na(Relative_Not_Smoothed),
    !is.na(Approval_Not_Smoothed),
    !is.na(Disapproval_Not_Smoothed)
  )

filtered_data$statenumber <- as.character(filtered_data$statenumber)

filtered_data <- as.data.frame(filtered_data)

# Lista de variáveis dependentes
dependent_vars <- c("Approval_Not_Smoothed", "Disapproval_Not_Smoothed", "Relative_Not_Smoothed")

independent_vars <- c(
  "quarter1_first_valid", "quarter2_first_valid", "quarter3_first_valid", "quarter1_repeat_valid", 
  "quarter2_repeat_valid", "quarter3_repeat_valid", "unemployment_state_l0", "unemployment_state_l1", 
  "unemployment_state_l2", "electionquarter", "female", "not_elected", 
  "governorresignedthatquarter_l0", "governorresignedthatquarter_l1", "governorresignedthatquarter_l2", 
  "governordiedthatquarter_l0", "governordiedthatquarter_l1", "governordiedthatquarter_l2",
  "factor(gov_party)"
)

# Loop pra criar e rodar os modelos
models <- list('Relative_Not_Smoothed', 'Approval_Not_Smoothed', 'Disapproval_Not_Smoothed')

for (dep_var in dependent_vars) {
  # Correctly create the formula string by collapsing independent variables with " + "
  formula_text <- paste(dep_var, "~", paste(independent_vars, collapse = " + "))
  model_formula <- as.formula(formula_text)
  

  model <- panelAR(
    formula = model_formula,
    data = filtered_data,
    panelVar = "staten",
    timeVar = "qtr",
    autoCorr = "psar1",
    panelCorrMethod = "pcse"
  )
  
  models[[dep_var]] <- model
}

summary(models$Relative_Not_Smoothed)
summary(models$Approval_Not_Smoothed)
summary(models$Disapproval_Not_Smoothed)
