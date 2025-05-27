library(plm)
library(texreg)
#install.packages('plm')

setwd("C:/Users/Joao arthur/OneDrive - Fundacao Getulio Vargas - FGV/Dissertação/Scripts-and-Databases")
#setwd("C:/Users/b435097/OneDrive - Fundacao Getulio Vargas - FGV/Dissertação/Scripts-and-Databases")


rm(list=ls())

############### Load Dta ################
source('Scripts/R script dissertacao - State Approval.R')
source('Scripts/R script dissertacao - Oil prices.R')
source('Scripts/R script dissertacao - BSF Data.R')

rm(list=setdiff(ls(), c('df_approval_qtr','oil_qtr',"BSF_dataset")))

data <- merge(df_approval_qtr, oil_qtr, by = c("year", "quarter"), all.x = TRUE)

data <- merge(data, BSF_dataset, by = c("state", "year"), all.x = TRUE)

# Convert dataframe into a Panel Data Frame format
pdata <- pdata.frame(data, index = c( "year","quarter","state"))

#Count values in Approval_Not_Smoothed by state
table(data$Approval_Not_Smoothed, useNA = "always")

#exclude rows with NA in Approval_Not_Smoothed
pdata <- pdata[!is.na(pdata$Approval_Not_Smoothed), ]

#Plot approval data
plot(pdata$Approval_Not_Smoothed)


#Convert data from pdata into wide format
library(reshape2)

data_wide <- dcast(pdata, year + quarter ~ state, value.var = "Approval_Not_Smoothed")

#In data_wide drop columns where all values are NA
data_wide <- data_wide[, colSums(is.na(data_wide)) < nrow(data_wide)]

# In data_wide exclude lines where any value is NA
#data_wide <- data_wide[complete.cases(data_wide), ]

#Create a variable that is a concat of state and incumbent
pdata$state_incumbent <- paste(pdata$state, pdata$incumbent, sep = "_")



########### Analysis

screenreg(plm(Approval_Smoothed
    ~ lag(oil_qrt_shock_positive)
    + lag(oil_qrt_shock_negative)
              + lag(Approval_Smoothed)
              + lag(oil_deflated_fst_diff)
              + factor(quarter_year)
              + state_incumbent
              ,data=pdata
),omit.coef=c("state_incumbent|quarter_year"))

screenreg(plm(Approval_Smoothed
              ~ lag(oil_qrt_shock_positive)
              + lag(oil_qrt_shock_negative)
              + lag(Approval_Smoothed)
              + lag(oil_deflated_fst_diff)
              + lag(oil_deflated_qtr) # Não sei se faz sentido ter os dois, ou se isso aqui causa correlação espúria
              + factor(quarter_year)
              + state_incumbent
              ,data=pdata
),omit.coef=c("state_incumbent|quarter_year"))

glimpse(pdata)

