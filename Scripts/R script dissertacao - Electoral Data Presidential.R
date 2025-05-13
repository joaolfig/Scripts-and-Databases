library(tidyr)
library(dplyr)

#remove all variables
#rm(list=ls())

#set working directory
setwd("C:/Users/Joao arthur/OneDrive - Fundacao Getulio Vargas - FGV/Dissertação/Scripts and Databases")

president_incumbent <- read.csv("Databases/US Election Results Executive/Presidentes_EUA_Wikipedia.csv", header = TRUE, sep = ";")


# Create a dataframe with one row for each year between 1948 and 2020
president_incumbent_ts <- c()
president_incumbent_ts$year <- 1933:2020
president_incumbent_ts <- as.data.frame(president_incumbent_ts)
#merge the datasets, by president_incumbent$Year_mandate_begins and years$year
#president_incumbent <- merge(president_incumbent,years, by.y = "year", by.x = "Year_mandate_begins", all.y = TRUE)
president_incumbent_ts <- merge(president_incumbent_ts,president_incumbent, by.x = "year", by.y = "Year_mandate_begins", all.x = TRUE)

#fill down all columns
president_incumbent_ts <- president_incumbent_ts %>% fill(everything())

#Rename columns
colnames(president_incumbent_ts)[4] <- "president_party"

# Dummies for Presidential Election 
president_incumbent_ts$presidential_election_dm <- ifelse(president_incumbent_ts$year %% 4 == 0, 1, 0)

# Filter data to take only data from 1948 to 2020
president_incumbent_ts <- president_incumbent_ts %>% filter(year >= 1948)

colnames(president_incumbent_ts)
