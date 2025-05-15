library(mFilter)
library(ggplot2)
library(dplyr)
library(stringr)

#rm(list=ls())

setwd("C:/Users/Joao arthur/OneDrive - Fundacao Getulio Vargas - FGV/Dissertação/Scripts-and-Databases")

employment <- read.csv("Databases/BLS Data/Employment/Annual/Employment_Nonfarm_US_National_Annual.csv", header = TRUE, sep = ",")
unemployment <- read.csv("Databases/BLS Data/Employment/Annual/Unemployment_Rate_US_National_Annual.csv", header = TRUE, sep = ",")

employment$observation_date <- as.Date(employment$observation_date, format = "%Y-%m-%d")
unemployment$observation_date <- as.Date(unemployment$observation_date, format = "%Y-%m-%d")

############### REPRODUÇÃO WOLFERS 2007, APPENDIX B 

employment$PAYEMS <- log(employment$PAYEMS)

employment <- employment %>%
  filter(observation_date >= as.Date("1948-01-01")  & observation_date <= as.Date("2020-12-31"))

employment$employment_trend_lm <- predict(lm(employment$PAYEMS ~ employment$observation_date))
employment$employment_trend_hp <- hpfilter(employment$PAYEMS, freq = 100)$trend 

ggplot(employment, aes(x = observation_date)) +
  geom_line(aes(y = employment[,'PAYEMS'])) +
  geom_line(aes(y = employment[,'employment_trend_hp']), color = "blue") +
  geom_line(aes(y = employment[,'employment_trend_lm']), color = "red") +
  labs(title = "Trend Employment and Non-Farm Payrolls", x = "Year", y = "Log Employment") +
  scale_x_date(limits = as.Date(c("1939-01-01", "1997-12-31"))) +
  scale_y_continuous(limits = c(10.3, 12)) +
  theme_minimal()

employment$employment_gap_lm <- employment$employment_trend_lm - employment$PAYEMS
employment$employment_gap_hp <- employment$employment_trend_hp - employment$PAYEMS

employment <- merge(employment, unemployment, by = "observation_date")

ggplot(employment, aes(x = observation_date)) +
  geom_line(aes(y = (((employment[,'UNRATE'])/100)-mean((employment[,'UNRATE'])/100)))) +
  geom_line(aes(y = employment[,'employment_gap_lm']), color = "blue") +
  geom_line(aes(y = employment[,'employment_gap_hp']), color = "red") + 
  labs(title = "US Employment Gap & Unemployment Rate", x = "Year", y = "Log Employment") +
  scale_x_date(limits = as.Date(c("1940-01-01", "1997-12-31"))) +
  scale_y_continuous(limits = c(-0.10, 0.10)) +
  theme_minimal()

############### REPRODUÇÃO CALCULO EMPLOYMENT GAP DOS ESTADOS

employment_states <- read.csv("Databases/BLS Data/Employment/Monthly/employment.csv", header = TRUE, sep = ";")

employment_states$observation_date <- as.Date(employment_states$observation_date, format = "%d/%m/%Y")

employment_states <- employment_states %>%
  filter(observation_date >= as.Date("1948-01-01") & observation_date <= as.Date("2020-12-31"))

#make a year column, getting the year from the observation_date column
employment_states$year <- as.numeric(format(as.Date(employment_states$observation_date, format = "%d/%m/%Y"), "%Y"))

#average each column by year
employment_states <- employment_states %>%
  group_by(year) %>%
  summarise_all(mean) %>%
  mutate_all(round, 0)

employment_states[,4:54] <- log(employment_states[,4:54])

#drop columns 2 to 4
employment_states <- employment_states[, -c(2:3)]

employment_states <- as.data.frame(employment_states)

employment_states_trends <- employment_states

for (i in 2:51) {
  # Identify complete cases for the current column and the year column
  complete_cases <- complete.cases(employment_states[, i], employment_states$year)
  
  # Fit the linear model only on complete cases
  model <- lm(as.numeric(employment_states[complete_cases, i]) ~ employment_states$year[complete_cases])
  
  # Predict the values for the complete cases and replace the column values
  employment_states_trends[complete_cases, i] <- predict(model, newdata = employment_states[complete_cases, ])
}

# Calculate the gap
employment_states_gap <- employment_states_trends - employment_states
employment_states_gap$year <- employment_states$year

plot(employment_states_gap$year, employment_states_gap$AL, type = "l", col = "blue", xlab = "Year", ylab = "Employment Gap", main = "Employment Gap in Alabama")
employment_states_gap

str(employment_states_gap)
library(dplyr)
library(tidyr)
library(ggplot2)
employment_states_gap %>%
  pivot_longer(cols = -year) %>%
  ggplot(aes(x = year, y = value, group = name, color = name)) +
  geom_line()

# employment_states_gap has columns for states and rows for years, I want to 
# I wanna convert it to a dataframe with three columns: year, state, and gap

employment_states_gap <- employment_states_gap %>% 
  gather(key = 'state', value = 'employment_gap', -year) %>% 
  mutate(state = str_to_title(state))

employment_states_gap$year <- as.character(employment_states_gap$year)
employment_states_gap$state <- toupper(employment_states_gap$state)

employment_states_gap <- employment_states_gap %>%
  group_by(state) %>%
  mutate(employment_gap_change = (employment_gap - lag(employment_gap, 2)))

#round columns 3 and 4
employment_states_gap[,3:4] <- round(employment_states_gap[,3:4], 4)

str(employment_states_gap)
employment_national_gap <- employment_states_gap %>% 
  filter(state == "NATIONAL_NOT_SEASONALLY_ADJUSTED")

colnames(employment_national_gap)[3] <- "employment_gap_national"

colnames(employment_national_gap)[4] <- "employment_gap_change_national"

# Merge and then create the performance variable for competence
employment_states_gap <- merge(employment_states_gap, employment_national_gap[,c('year','employment_gap_national','employment_gap_change_national')], by = c('year'), all.x = TRUE)

employment_states_gap$competence_employment <- employment_states_gap$employment_gap_change - employment_states_gap$employment_gap_change_national
  