library(readxl)
library(dplyr)
library(tidyr)

#rm(list=ls())

setwd("C:/Users/Joao arthur/OneDrive - Fundacao Getulio Vargas - FGV/Dissertação/Scripts-and-Databases")

oil_consumption_raw <- readxl::read_excel("Databases/EIA Energy Information Administration/use_pet_capita.xlsx", sheet = 2, skip = 1)

oil_production_raw <- readxl::read_excel("Databases/EIA Energy Information Administration/pet_crd_crpdn_adc_mbbl_a.xls", sheet = 2, skip = 2)

us_2letters_code <- read.csv("Databases/US state 2 letter codes/2_letter_codes.csv", sep = ",", encoding = "UTF-8")

############### Treating oil consumption data
# Transformar os dados para o formato long (um ano por linha)
oil_consumption <- oil_consumption_raw %>%
  pivot_longer(
    cols = -State,
    names_to = "Year",
    values_to = "Consumption (Barrels)"
  )


############### Treating oil production data
colnames(oil_production_raw) <- gsub(" Field Production of Crude Oil \\(Thousand Barrels\\)$", "", colnames(oil_production_raw))

# drop columns with PADD in their name
oil_production_raw <- oil_production_raw %>%
  select(-contains("PADD"))
colnames(oil_production_raw)

# Sum up columns "Alaska", "Alaska South", and "Alaska North Slope Crude Oil Production (Thousand Barrels)"
oil_production_raw$Alaska <- 
  oil_production_raw$`Alaska South` + oil_production_raw$`Alaska North Slope Crude Oil Production (Thousand Barrels)`
  
  
oil_production <- oil_production_raw %>%
  pivot_longer(
    cols = -Date,
    names_to = "State",
    values_to = "Production (Barrels)"
  )

# Order by state, then by date
oil_production <- oil_production %>%
  arrange(State, Date)

#Create a column for year
oil_production$Year <- format(oil_production$Date, "%Y")

# Use us_2letters_code to merge columns and add a new column with the code
# State name in both columns is in column "State", and the code is in column "State_Code"
oil_production <- oil_production %>%
  left_join(us_2letters_code, by = c("State" = "State")) %>%
  select(-State) %>%
  rename(State = State_Code)

############### Merge columns
#Merge columns by Year and State Join such that consumption has all its columns
oil_data <- oil_consumption %>%
  left_join(oil_production, by = c("Year", "State")) %>%
  select(-Date)

#omit rows with NA using NA omit
oil_data <- na.omit(oil_data)

#create a new column with the difference between production and consumption
oil_data <- oil_data %>%
  mutate(Difference = `Production (Barrels)` - `Consumption (Barrels)`)


