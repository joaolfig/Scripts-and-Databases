library(dplyr)
library(tidyverse)
library(zoo)  # para yearqtr

#rm(list=ls())

setwd("C:/Users/Joao arthur/OneDrive - Fundacao Getulio Vargas - FGV/Dissertação/Scripts-and-Databases")
#setwd("C:/Users/b435097/OneDrive - Fundacao Getulio Vargas - FGV/Dissertação/Scripts-and-Databases")


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

deflator_qtr <- deflator 

deflator <- deflator %>%
  group_by(year) %>%
  summarise(GDPCTPI = mean(GDPCTPI, na.rm = TRUE))

deflator$deflator_multiplier <- 100 / deflator$GDPCTPI

deflator_qtr$deflator_multiplier <- 100 / deflator_qtr$GDPCTPI

#drop the last row in oil
oil <- oil[-nrow(oil),]

############### REPRODUÇÃO OIL DEFLATED

oil_deflated <- cbind(oil$year, round(deflator$GDPCTPI,2), round(deflator$deflator_multiplier,2), round(oil$mean,2))

#rename variables
colnames(oil_deflated) <- c("year", "GDPCTPI", "deflator_multiplier", "oil")

oil_deflated <- as.data.frame(oil_deflated)

oil_deflated$oil_deflated <- round(oil_deflated$oil * oil_deflated$deflator_multiplier,2)

oil_deflated$log_oil_deflated <- log(oil_deflated$oil_deflated)

oil_deflated$log_oil_deflated_change <- oil_deflated$log_oil_deflated - dplyr::lag(oil_deflated$log_oil_deflated, 2)


# Aggregate oil variables
oil_monthly <- oil %>%
  dplyr::select(-mean)%>% 
  pivot_longer(
    cols = Jan:Dec,
    names_to = "month",
    values_to = "value"
  )

oil_qtr <- oil_monthly %>%
  mutate(
    quarter = case_when(
      month %in% c("Jan", "Feb", "Mar") ~ "1",
      month %in% c("Apr", "May", "Jun") ~ "2",
      month %in% c("Jul", "Aug", "Sep") ~ "3",
      month %in% c("Oct", "Nov", "Dec") ~ "4"
    )
  ) %>%
  group_by(year, quarter) %>%
  summarise(oil_qtr_avg = round(mean(value, na.rm = TRUE),2), .groups = "drop")



####Deflate qtr oil data
glimpse(deflator_qtr)
#get month of deflator_qtr$observation_date
deflator_qtr$month <- as.numeric(format(deflator_qtr$observation_date, "%m"))
#if deflator_qtr$month is 1, then deflator_qtr$quarter is 01, if its 04, then 2
#if it is 07 then 3, and if 10 then 4
deflator_qtr$quarter <- case_when(
  deflator_qtr$month == 1 ~ 1,
  deflator_qtr$month == 4 ~ 2,
  deflator_qtr$month == 7 ~ 3,
  deflator_qtr$month == 10 ~ 4
)

#left join oil_qtr with deflator_qtr on year and quarter

deflator_qtr$quarter <- as.character(deflator_qtr$quarter)
oil_qtr <- left_join(oil_qtr, deflator_qtr[,c("year","quarter","deflator_multiplier")], 
                     by = c("year", "quarter"))

oil_qtr$oil_deflated_qtr <- round(oil_qtr$oil_qtr_avg * oil_qtr$deflator_multiplier,2)

oil_qtr$quarter_year <- as.yearqtr(paste(oil_qtr$year, oil_qtr$quarter), format = "%Y %q")


#Plot both oil_qtr$oil_deflated_qtr and oil_qtr_avg
library(ggplot2)
ggplot(oil_qtr, aes(x = year, y = oil_deflated_qtr)) +
  geom_line(color = "blue") +
  geom_line(aes(y = oil_qtr_avg), color = "red") +
  labs(title = "Oil Prices Deflated vs. Average Oil Prices",
       x = "Year",
       y = "Oil Price (Deflated and Average)") +
  theme_minimal()


#### Oil fluctuation in percentage
# In percentage
oil_qtr$oil_diff_percent <- (oil_qtr$oil_qtr_avg - dplyr::lag(oil_qtr$oil_qtr_avg))/dplyr::lag(oil_qtr$oil_qtr_avg)

oil_qtr$oil_deflated_fst_diff <- oil_qtr$oil_deflated_qtr - dplyr::lag(oil_qtr$oil_deflated_qtr)


####Measurement of oilshock
m <- mean(oil_qtr$oil_deflated_fst_diff, na.rm = TRUE)
s <- sd(oil_qtr$oil_deflated_fst_diff, na.rm = TRUE)

m_pct <- mean(oil_qtr$oil_diff_percent, na.rm = TRUE)
s_pct <- sd(oil_qtr$oil_diff_percent, na.rm = TRUE)

# Create oil shock variables
oil_qtr <- oil_qtr %>%
            mutate(oil_shock_positive = 
                pmax(0,100 * log(oil_qtr_avg / pmax(
                dplyr::lag(oil_qtr_avg, 2), 
                dplyr::lag(oil_qtr_avg, 3),
                dplyr::lag(oil_qtr_avg, 4),
                dplyr::lag(oil_qtr_avg, 5))), na.rm = TRUE)
                  ,oil_shock_negative = 
                pmin(0,100 * log(oil_qtr_avg / pmin(
                dplyr::lag(oil_qtr_avg, 2), 
                dplyr::lag(oil_qtr_avg, 3),
                dplyr::lag(oil_qtr_avg, 4),
                dplyr::lag(oil_qtr_avg, 5))), na.rm = TRUE)
            )

# Create lag variables for oil shocks
oil_qtr <- oil_qtr %>%
  mutate(oil_shock_positive_l1 = dplyr::lag(oil_shock_positive, 1),
         oil_shock_positive_l2 = dplyr::lag(oil_shock_positive, 2),
         oil_shock_positive_l3 = dplyr::lag(oil_shock_positive, 3),
         oil_shock_positive_l4 = dplyr::lag(oil_shock_positive, 4),
         oil_shock_negative_l1 = dplyr::lag(oil_shock_negative, 1),
         oil_shock_negative_l2 = dplyr::lag(oil_shock_negative, 2),
         oil_shock_negative_l3 = dplyr::lag(oil_shock_negative, 3),
         oil_shock_negative_l4 = dplyr::lag(oil_shock_negative, 4))

oil_qtr <- oil_qtr %>%
  mutate(oil_qtr_avg_l1 = dplyr::lag(oil_qtr_avg, 1),
         oil_qtr_avg_l2 = dplyr::lag(oil_qtr_avg, 2),
         oil_qtr_avg_l3 = dplyr::lag(oil_qtr_avg, 3),
         oil_qtr_avg_l4 = dplyr::lag(oil_qtr_avg, 4))


# oil_qtr$oil_qrt_shock_positive <- ifelse(oil_qtr$oil_deflated_fst_diff >= m + 1.5*s, 1, 0)
# oil_qtr$oil_qrt_shock_negative <- ifelse(oil_qtr$oil_deflated_fst_diff <= m - 1.5*s, 1, 0)
# oil_qtr$oil_qrt_shock_positive_2 <- ifelse(oil_qtr$oil_deflated_fst_diff >= m + 2*s, 1, 0)
# oil_qtr$oil_qrt_shock_negative_2 <- ifelse(oil_qtr$oil_deflated_fst_diff <= m - 2*s, 1, 0)
# oil_qtr$oil_qrt_shock_positive_pct_1.5 <- ifelse(oil_qtr$oil_diff_percent >= m_pct + 1.5*s_pct, 1, 0)
# oil_qtr$oil_qrt_shock_negative_pct_1.5 <- ifelse(oil_qtr$oil_diff_percent <= m_pct - 1.5*s_pct, 1, 0)
# oil_qtr$oil_qrt_shock_positive_pct_2 <- ifelse(oil_qtr$oil_diff_percent >= m_pct + 2*s_pct, 1, 0)
# oil_qtr$oil_qrt_shock_negative_pct_2 <- ifelse(oil_qtr$oil_diff_percent <= m_pct - 2*s_pct, 1, 0)


#plot a line plot of oil prices_deflted with a scatter of shocks
# ggplot(oil_qtr, aes(x = time, y = oil_deflated_fst_diff)) +
#   geom_line(color = "blue") +
#   geom_point(data = subset(oil_qtr, oil_qrt_shock_positive_1.5 == 1), aes(y = oil_deflated_fst_diff), color = "green", size = 1.5) +
#   geom_point(data = subset(oil_qtr, oil_qrt_shock_negative_1.5 == 1), aes(y = oil_deflated_fst_diff), color = "red", size = 1.5) +
#   labs(title = "Oil Price Shocks",
#        x = "Quarter",
#        y = "Oil Price (2017 US dollars)") +
#   theme_minimal()
# 
# ggplot(oil_qtr, aes(x = time, y = oil_deflated_fst_diff)) +
#   geom_line(color = "blue") +
#   geom_point(data = subset(oil_qtr, oil_qrt_shock_positive_2 == 1), aes(y = oil_deflated_fst_diff), color = "green", size = 1.5) +
#   geom_point(data = subset(oil_qtr, oil_qrt_shock_negative_2 == 1), aes(y = oil_deflated_fst_diff), color = "red", size = 1.5) +
#   labs(title = "Oil Price Shocks",
#        x = "Quarter",
#        y = "Oil Price (2017 US dollars)") +
#   theme_minimal()
# 
# ggplot(oil_qtr, aes(x = time, y = oil_diff_percent)) +
#   geom_line(color = "blue") +
#   geom_point(data = subset(oil_qtr, oil_qrt_shock_positive_pct_1.5 == 1), aes(y = oil_diff_percent), color = "green", size = 1.5) +
#   geom_point(data = subset(oil_qtr, oil_qrt_shock_negative_pct_1.5 == 1), aes(y = oil_diff_percent), color = "red", size = 1.5) +
#   labs(title = "Oil Price Shocks",
#        x = "Quarter",
#        y = "Percentage") +
#   theme_minimal()
# ggplot(oil_qtr, aes(x = time, y = oil_diff_percent)) +
#   geom_line(color = "blue") +
#   geom_point(data = subset(oil_qtr, oil_qrt_shock_positive_pct_2 == 1), aes(y = oil_diff_percent), color = "green", size = 1.5) +
#   geom_point(data = subset(oil_qtr, oil_qrt_shock_negative_pct_2 == 1), aes(y = oil_diff_percent), color = "red", size = 1.5) +
#   labs(title = "Oil Price Shocks",
#        x = "Quarter",
#        y = "Percentage") +
#   theme_minimal()