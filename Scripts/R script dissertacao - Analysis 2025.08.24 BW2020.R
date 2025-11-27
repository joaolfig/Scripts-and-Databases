library(dplyr)
library(texreg)
library(lfe)
library(estimatr)
library(did)

setwd("C:/Users/Joao arthur/OneDrive - Fundacao Getulio Vargas - FGV/Dissertação/Scripts-and-Databases")

###### IMPORTAÇÃO DATASETS TRATADOS EM OUTROS SCRIPTS
source('Scripts/R script dissertacao - BSF Data.R')
source('Scripts/R script dissertacao - Oil prices.R')

###### TRATAMENTO DADOS PETRÓLEO
# Torna o ano de Novembro de 1 ano até Outubro do próximo, pra pegar período antes da eleição.

oil_yearly <- oil_monthly %>%
  mutate(oil_year = ifelse( (month == "Nov")|(month == "Dec")
                            , year + 1, year)) %>%
  group_by(oil_year) %>%
  summarize(oil_price = round(mean(value, na.rm = TRUE),2)) %>%
  rename(year = oil_year)

#Log change of oil price
oil_yearly <- oil_yearly %>%
  mutate(
    oil_log_change = log(oil_price) - log(dplyr::lag(oil_price))
    ,oil_log_change = round(oil_log_change,2)
  )

###### TRATAMENTO DADOS ELECTORAL RETURNS
# Não tenho os dados pra presidente pro Alaska
pres_elections <- read.csv("Databases/US Election Results Executive/pres_elections_release.csv", header = TRUE, sep = ",")
pres_elections <- pres_elections[,c(1,2,5,8,9,11,13,14)]
gov_elections <- read.csv("Databases/US Election Results Executive/gov_elections_release.csv", header = TRUE, sep = ",")
gov_elections <- gov_elections[,c(2,3,9,10,12,14,15)]

pres_elections <- pres_elections %>%
  mutate(vote_share_dem_pres = democratic_raw_votes / raw_county_vote_totals
         ,vote_share_rep_pres = republican_raw_votes / raw_county_vote_totals) %>%
  group_by(fips) %>%
  arrange(election_year) %>%
  mutate(vote_share_dem_pres_lag = dplyr::lag(vote_share_dem_pres, 1)
         ,vote_share_rep_pres_lag = dplyr::lag(vote_share_rep_pres, 1)
         ,vote_share_dem_pres_delta = vote_share_dem_pres - vote_share_dem_pres_lag
         ,vote_share_rep_pres_delta = vote_share_rep_pres - vote_share_rep_pres_lag) %>%
  ungroup()

gov_elections <- gov_elections %>%
  mutate(vote_share_dem_gov = democratic_raw_votes / raw_county_vote_totals
         ,vote_share_rep_gov = republican_raw_votes / raw_county_vote_totals) %>%
  group_by(fips) %>%
  arrange(election_year) %>%
  mutate(vote_share_dem_gov_lag = dplyr::lag(vote_share_dem_gov, 1)
         ,vote_share_rep_gov_lag = dplyr::lag(vote_share_rep_gov, 1)
         ,vote_share_dem_gov_delta = vote_share_dem_gov - vote_share_dem_gov_lag
         ,vote_share_rep_gov_delta = vote_share_rep_gov - vote_share_rep_gov_lag) %>%
  ungroup()



#### Faz o Merge com os dados de B&W
load("C:/Users/Joao arthur/OneDrive - Fundacao Getulio Vargas - FGV/Dissertação/Scripts-and-Databases/Databases/Replication Files for Benedictis-Kessner & Warshaw (2020)/econ_counties_cities_analysis.Rdata")

#subset for only states that are oil_exporter
data_analysis$oil_exporter <- ifelse(data_analysis$state_abb
                                     %in% c("AK", "CO", "LA", "ND", "NM", "OK", "TX", "WY"), 1, 0)

subset_analysis <- subset(data_analysis, oil_exporter == 1)

subset_analysis <- subset_analysis[,c(1:5,152,153,98,91)]

subset_analysis <- subset_analysis %>%
  left_join(pres_elections, by = c("fips_numeric" = "fips", "year" = "election_year")) %>%
  left_join(gov_elections, by = c("fips_numeric" = "fips", "year" = "election_year")) %>%
  left_join(BSF_dataset[, c(1,2,6:8,13)], by = c("state_abb" = "state", "year" = "year")) %>%
  left_join(oil_yearly, by = c("year" = "year"))

subset_analysis <- subset_analysis %>% 
  mutate(
    vote_share_incumb_pres = ifelse(pres_party == 1,vote_share_rep_pres,vote_share_rep_pres)
    ,vote_share_incumb_pres_delta = ifelse(pres_party == 1,vote_share_rep_pres_delta,vote_share_rep_pres_delta)
    
    # O voto no governador com base no presidente
    ,vote_share_incumb_gov = ifelse(pres_party == 1,vote_share_rep_gov,vote_share_rep_gov)
    ,vote_share_incumb_gov_delta = ifelse(pres_party == 1,vote_share_rep_gov_delta,vote_share_rep_gov_delta)
    
  ) 
colnames(data_analysis)

# Get the list of counties by the column fips that have more than 20000 population in 1972
fips20k <- subset_analysis %>%
  filter(year == 1972 & population >= 10000) %>%
  select(fips) %>%
  distinct()

# Filter the dataset to keep only those counties
subset_analysis <- subset_analysis %>% filter(
  (!is.na(vote_share_incumb_pres_delta) 
  | !is.na(vote_share_incumb_gov_delta))
  & fips %in% fips20k$fips)

# Check if there are counties with missing years
table(table(subset_analysis$fips_numeric))

#drop any fips_numeric that has less observations than 12 observations
subset_analysis <- subset_analysis %>%
  group_by(fips_numeric) %>%
  filter(n() >= 13) %>%
  ungroup()

# Check how many observations by state
table(subset(subset_analysis)$state_abb) / 5

m1pres <- lm_robust(vote_share_incumb_pres_delta ~ 
                      oil_price * BSF_implemented 
                    + factor(state_abb) + factor(year)
                    ,clusters = factor(state_abb)
                    , data = subset_analysis)

m2pres <- lm_robust(vote_share_incumb_pres_delta ~ 
                      oil_price * BSF_implemented 
                    + pres_party
                    + factor(state_abb) + factor(state_year) + factor(fips)
                    ,clusters = factor(fips)
                    , data = subset_analysis)

screenreg(list(m1pres,m2pres),omit.coef = "factor", single.row = TRUE,digits=3)


m1gov <- lm_robust(vote_share_rep_gov_delta ~ 
                     oil_price * BSF_implemented * pres_party
                   + factor(state_abb) + factor(year)
                   ,clusters = factor(state_abb)
                   , data = subset_analysis)

m2gov <- lm_robust(vote_share_rep_gov_delta ~ 
                     oil_price * BSF_implemented * pres_party
                   + factor(state_abb) + factor(state_year) + factor(year)
                   ,clusters = factor(state_abb)
                   , data = subset_analysis)

screenreg(list(m1gov,m2gov),omit.coef = "factor", single.row = TRUE,digits=3)


# ###### Event Study
# subset_analysis_es <- subset_analysis %>% 
#   group_by(fips) %>%
#   mutate(first_treated_year = ifelse(any(BSF_implemented == 1),
#                                           min(year[BSF_implemented == 1]), + 0L)
#          ,period = (year - first_treated_year)/4) %>% 
#   filter(period >= -3 & period <= 6) %>%
#   ungroup()
# 
# table(subset(subset_analysis_es)$period)
# 
# subset_analysis_es <- subset_analysis_es %>%
#   mutate(period = relevel(factor(period), ref = "0"))
# 
# m_bsf_pres_es <- lm_robust(
#   vote_share_incumb_pres_delta ~ 
#     oil_price+
#     BSF_implemented+
#     oil_price:period
#   + factor(state_year) + factor(state_abb) + factor(fips),
#   clusters = factor(fips),
#   data = subset_analysis_es
# )
# 
# 
# screenreg(list(m_bsf_pres_es),omit.coef = "state|fips", single.row = TRUE,digits=3)
# 
# 
# ###### Callway Sant'Anna 
#Não está certo, o tratamento aqui está sendo Estado X Ano
subset_analysis_did <- subset_analysis %>%
  group_by(fips) %>%
  mutate(first_treat = ifelse(any(BSF_implemented == 1),
                              min(year[BSF_implemented == 1]), + 0L)) %>%
  ungroup()

table(subset(subset_analysis_did)$first_treat) / 26
table(subset(subset_analysis_did)$year)
# 
# att <- att_gt(yname = "vote_share_incumb_pres_delta",
#                         tname = "year",
#                         idname = "fips_numeric",
#                         gname = "first_year_BSF",
#                         xformla = ~NULL,
#                         data = subset_analysis_did,
#                         control_group = "notyettreated"
#                         
# )
# 
# summary(att)
# 
# es <- aggte(att, type = "dynamic", min_e = -6, max_e = 6)
# 
# ggdid(es)
# 
# summary(es)


# For state_abb = CO, I want the average dem_share, rep_share, incum_share de gov e pres
subset_analysis %>%
  filter(state_abb == "CO") %>%
  group_by(year) %>%
  summarize(
    mean(vote_share_dem_pres, na.rm = TRUE)
    ,mean(vote_share_rep_pres, na.rm = TRUE)
    ,mean(vote_share_incumb_pres, na.rm = TRUE)
    
    ,mean(vote_share_dem_gov, na.rm = TRUE)
    ,mean(vote_share_rep_gov, na.rm = TRUE)
    ,mean(vote_share_incumb_gov, na.rm = TRUE)
  ) %>%
  arrange(year)



#######
# devtools::install_github("synth-inference/synthdid")
# 
# library(synthdid)
# 
# # Estimate the effect of California Proposition 99 on cigarette consumption
# data('california_prop99')
# setup = panel.matrices(california_prop99)
# tau.hat = synthdid_estimate(setup$Y, setup$N0, setup$T0)
# se = sqrt(vcov(tau.hat, method='placebo'))
# sprintf('point estimate: %1.2f', tau.hat)
# sprintf('95%% CI (%1.2f, %1.2f)', tau.hat - 1.96 * se, tau.hat + 1.96 * se)
# plot(tau.hat)

