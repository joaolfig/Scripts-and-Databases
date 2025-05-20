library(dplyr)

#rm(list=ls())

setwd("C:/Users/Joao arthur/OneDrive - Fundacao Getulio Vargas - FGV/Dissertação/Scripts-and-Databases")


# Read excel file .xls on "BLS data/Oil/PPI Crude Petroleum.xls" header is on row 12

oil <- readxl::read_excel("Databases/BLS data/Oil/PPI Crude Petroleum.xlsx", sheet = 1, skip = 11)

deflator  <- read.csv("Databases/BLS data/Oil/GDPCTPI.csv")

# Rename column Year to year
colnames(oil)[1] <- "year"

# Get the mean of each row in oil[,2:13]

oil$mean <- rowMeans(oil[,2:13], na.rm = TRUE)

deflator$observation_date <- as.Date(deflator$observation_date, format = "%Y-%m-%d")
deflator$year <- as.numeric(format(as.Date(deflator$observation_date, format = "%d/%m/%Y"), "%Y"))

# in column deflator$GDPCTPI, average the values of the observations
# by deflator$year

deflator <- deflator %>%
  group_by(year) %>%
  summarise(GDPCTPI = mean(GDPCTPI, na.rm = TRUE))

deflator$deflator_multiplier <- 100 / deflator$GDPCTPI


#drop the last row in oil
oil <- oil[-nrow(oil),]

############### REPRODUÇÃO OIL DEFLATED

oil_deflated <- cbind(oil$year, round(deflator$GDPCTPI,2), round(deflator$deflator_multiplier,2), round(oil$mean,2))

#rename variables
colnames(oil_deflated) <- c("year", "GDPCTPI", "deflator_multiplier", "oil")

oil_deflated <- as.data.frame(oil_deflated)

oil_deflated$oil_deflated <- round(oil_deflated$oil * oil_deflated$deflator_multiplier,2)

oil_deflated$log_oil_deflated <- log(oil_deflated$oil_deflated)

oil_deflated$log_oil_deflated_change <- oil_deflated$log_oil_deflated - lag(oil_deflated$log_oil_deflated, 2)


