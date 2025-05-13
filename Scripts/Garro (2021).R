library(haven)
library(dplyr)
library(tidyverse)

#rm(list=ls())

setwd('C:/Users/Joao arthur/OneDrive - Fundacao Getulio Vargas - FGV/Dissertação/Scripts and Databases/Databases/Replication Files for Garro (2021)')
getwd()


df_garro <- read_dta("polar_main_dataset.dta")


# # average oilprod by statename
# oilprod <- df_garro %>%
#   group_by(statename) %>%
#   summarise(average_oilprod = mean(oil_gsp, na.rm = TRUE)) %>%
#   arrange(desc(average_oilprod))
# 
# # Make a new version, but now round it to 3
# oilprod <- df_garro %>%
#   group_by(statename) %>%
#   summarise(average_oilprod = round(mean(oil_gsp, na.rm = TRUE), 3)) %>%
#   arrange(desc(average_oilprod))

#.

