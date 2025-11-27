library(dplyr)
library(texreg)
library(lfe)
library(estimatr)
library(MatchIt)
library(marginaleffects)

setwd("C:/Users/Joao arthur/OneDrive - Fundacao Getulio Vargas - FGV/Dissertação/Scripts-and-Databases")

###### IMPORTAÇÃO DATASETS TRATADOS EM OUTROS SCRIPTS
source('Scripts/R script dissertacao - BSF Data.R')
source('Scripts/R script dissertacao - Oil prices.R')
source('Scripts/R script dissertacao - Electoral returns.R')

###### TRATAMENTO DADOS PETRÓLEO
#Torna o ano de Novembro de 1 ano até Outubro do próximo, pra pegar período antes da eleição.

oil_yearly <- oil_monthly %>%
  mutate(oil_year = ifelse( (month == "Nov")|(month == "Dec")
                            , year + 1, year)) %>%
  group_by(oil_year) %>%
  summarize(oil_price = round(mean(value, na.rm = TRUE),2)) %>%
  rename(year = oil_year)

# Standardize oil price
oil_yearly <- oil_yearly %>%
  mutate(
    oil_price_std = (oil_price - mean(oil_price, na.rm = TRUE)) / sd(oil_price, na.rm = TRUE)
    ,oil_price_std = round(oil_price_std,2)
  )

#Log change of oil price
oil_yearly <- oil_yearly %>%
  mutate(
    oil_log_change = log(oil_price) - log(dplyr::lag(oil_price))
    ,oil_log_change = round(oil_log_change,2)
  )

oil_deflated <- oil_deflated %>%
  mutate(
    oil_deflated_std = (oil_deflated - mean(oil_deflated, na.rm = TRUE)) / sd(oil_deflated, na.rm = TRUE)
    ,oil_deflated_std = round(oil_deflated_std,2)
  )

#### Faz o Merge com os dados de B&W
load("C:/Users/Joao arthur/OneDrive - Fundacao Getulio Vargas - FGV/Dissertação/Scripts-and-Databases/Databases/Replication Files for Benedictis-Kessner & Warshaw (2020)/econ_counties_cities_analysis.Rdata")

#subset for only states that are oil_exporter
data_analysis$oil_exporter <- ifelse(data_analysis$state_abb
                                     %in% c("LA","MT","ND","NM","OK","TX","WY"), 1, 0)

# Voting cohorts
cohort1 <- c("NM","OK","TX","WY")
cohort2 <- c("MT","ND")

subset_analysis <- subset(data_analysis, oil_exporter == 1)

subset_analysis <- subset_analysis[,c(1:5,152,153,91)]

subset_analysis <- subset_analysis %>%
  left_join(pres_elections, by = c("fips_numeric" = "fips", "year" = "election_year")) %>%
  left_join(gov_elections, by = c("fips_numeric" = "fips", "year" = "election_year")) %>%
  left_join(BSF_dataset[, c(1,2,6:8,13)], by = c("state_abb" = "state", "year" = "year")) %>%
  left_join(oil_deflated, by = c("year" = "year")) %>%
  left_join(oil_yearly, by = c("year" = "year"))

subset_analysis <- subset_analysis %>% 
  mutate(
    vote_share_incumb_pres = ifelse(party_pres == "Democrat",vote_share_dem_pres,vote_share_rep_pres)
    ,vote_share_incumb_pres_delta = ifelse(party_pres == "Democrat",vote_share_dem_pres_delta,vote_share_rep_pres_delta)
    # MINHA VARIAVEL DEPENDENTE ESTÁ TENSA DE RUIM
    # # O voto no governador com base no presidente
    ,vote_share_incumb_gov = ifelse(party_gov == 1,vote_share_dem_gov,vote_share_rep_gov)
    ,vote_share_incumb_gov_delta = ifelse(party_gov == 1,vote_share_dem_gov_delta,vote_share_rep_gov_delta)) 

subset_analysis <- subset_analysis %>%
  filter(year < 2007)

subset_analysis$treat <- ifelse(subset_analysis$state_abb == "NM", 1, 0)

subset_analysis$treat_cohort2 <- ifelse(subset_analysis$state_abb == "ND", 1, 0)

subset_analysis <- subset_analysis %>%
  mutate(party_pres = ifelse(party_pres == "Elevated", "Republican", party_pres)
         ,party_pres = ifelse(subset_analysis$party_pres %in% c("Democratic","Republican"), subset_analysis$party_pres, NA)
         ,party_gov = ifelse(subset_analysis$party_gov %in% c("Democratic","Republican"), subset_analysis$party_gov, NA)
         # Eu preciso comparar mudar essa parte pra conseguir usar essa var em ano que é midterm
         #,same_party = ifelse(party_pres == party_gov, 1, ifelse(!is.na(party_pres) & !is.na(party_gov) & party_pres != party_gov, 0, NA))
  )


subset_analysis$party_pres <- ifelse(subset_analysis$party_pres %in% c("Democratic","Republican")
                                     , subset_analysis$party_pres, NA)
#subset_analysis$party_pres <- zoo::na.locf(subset_analysis$party_pres, na.rm = FALSE,fromLast = TRUE)

subset_analysis$party_gov <- ifelse(subset_analysis$party_gov %in% c("Democratic","Republican")
                                    , subset_analysis$party_gov, NA)

subset_analysis <- subset_analysis %>%
  mutate(
    same_party = ifelse(party_pres == party_gov, 1,
                        ifelse(!is.na(party_pres) & !is.na(party_gov) & party_pres != party_gov, 0, NA))
  )

# MATCHING
# subset_matching_pres <- subset_analysis %>%
#   filter(
#     !is.na(vote_share_incumb_pres_delta) &
#       year < 1978)


subset_matching_gov <- subset_analysis %>%
  filter(
    !is.na(vote_share_incumb_gov_delta) & year < 1978)
#       year != 1968 &
#       year != 1972)

# # subset_analysis[,c(2,6,57)] drop NA
# df <- subset_analysis[!is.na(subset_analysis$vote_share_incumb_gov_delta),][,c(2,6,57)]
# 
# summary(df)
# 
# # Show all the unique values of state_abb for each year
# table <- df %>%
#   group_by(year) %>%
#   summarize(states = paste(unique(state_abb), collapse = ", ")) %>%
#   arrange(year)
# 
# table
# 
# #Calculate the cor between oil_deflated and vote_share_incumb for each state and show me

# subset_matching_gov %>%
#   group_by(state_abb) %>%
#   summarize(correlation = cor(oil_deflated, vote_share_incumb_gov_delta, use = "complete.obs")) %>%
#   arrange(desc(correlation))
# 
# # Get the mean vote share for each county before treatment
# subset_matching_pres <- subset_matching_pres %>%
#   group_by(fips_numeric) %>%
#   mutate(
#     oil_corr_pres = cor(oil_deflated, vote_share_incumb_pres, use = "complete.obs"),
#     mean_vote_pres_pretreat = round(mean(raw_county_vote_totals.x, na.rm = TRUE),0),
#     mean_vote_pres_pretreat_dem = round(mean(vote_share_dem_pres, na.rm = TRUE),2),
#     mean_vote_pres_pretreat_rep = round(mean(vote_share_rep_pres, na.rm = TRUE),2),
#     mean_vote_gov_pretreat = round(mean(raw_county_vote_totals.y, na.rm = TRUE),0),
#     mean_vote_gov_pretreat_dem = round(mean(vote_share_dem_gov, na.rm = TRUE),2),
#     mean_vote_gov_pretreat_rep = round(mean(vote_share_rep_gov, na.rm = TRUE),2),
#   ) %>%
#   ungroup()

# subset_matching_gov %>%
#   group_by(fips_numeric) %>%
#   summarize(
#     n_obs = n(),
#     var_oil = var(oil_deflated, na.rm = TRUE),
#     var_vote = var(vote_share_incumb_gov, na.rm = TRUE),
#     cor_check = cor(oil_deflated, vote_share_incumb_gov, use = "complete.obs")
#   ) %>%
#   filter(is.na(cor_check) | var_oil == 0 | var_vote == 0)

subset_matching_gov <- subset_matching_gov %>%
  group_by(fips_numeric) %>%
  mutate(
    #oil_corr_gov = cor(oil_deflated, vote_share_incumb_gov, use = "complete.obs"),
    mean_vote_pres_pretreat = round(mean(raw_county_vote_totals.x, na.rm = TRUE),0),
    mean_vote_pres_pretreat_dem = round(mean(vote_share_dem_pres, na.rm = TRUE),2),
    mean_vote_pres_pretreat_rep = round(mean(vote_share_rep_pres, na.rm = TRUE),2),
    mean_vote_gov_pretreat = round(mean(raw_county_vote_totals.y, na.rm = TRUE),0),
    mean_vote_gov_pretreat_dem = round(mean(vote_share_dem_gov, na.rm = TRUE),2),
    mean_vote_gov_pretreat_rep = round(mean(vote_share_rep_gov, na.rm = TRUE),2)) %>%
  ungroup()

# Dataframe com fips_numeric, oil_corr e as colunas de interesse
# subset_matching_pres <- subset_matching_pres %>%
#   dplyr::select(fips_numeric,state_abb, oil_corr_pres
#                 ,mean_vote_pres_pretreat ,mean_vote_pres_pretreat_dem ,mean_vote_pres_pretreat_rep
#                 ,treat) %>%
#   distinct()

subset_matching_gov <- subset_matching_gov %>%
  dplyr::select(fips_numeric,state_abb
                #, oil_corr_gov
                ,mean_vote_gov_pretreat ,mean_vote_gov_pretreat_dem ,mean_vote_gov_pretreat_rep
                ,treat,treat_cohort2) %>%
  distinct()

# #Use matchit to match on these variables
# m.out_pres <- matchit(
#   formula  = treat ~ oil_corr_pres +
#     mean_vote_pres_pretreat +
#     mean_vote_pres_pretreat_dem +
#     mean_vote_pres_pretreat_rep,
#   data     = subset_matching_pres,
#   method   = "nearest",
#   estimand = "ATT",
#   distance = "glm",  # logistic PS
#   link     = "logit",
#   ratio    = 3,
#   replace  = TRUE,
#   caliper  = 0.15,    # 0.2 SD on logit(PS)
#   std.caliper = TRUE,
#   discard  = "both", # drop units outside common support
#   m.order  = "closest",
#   verbose  = TRUE
# )
m.out_gov1 <- matchit(
  formula  = treat ~ #oil_corr_gov +
    mean_vote_gov_pretreat +
    mean_vote_gov_pretreat_dem +
    mean_vote_gov_pretreat_rep,
  data     = subset(subset_matching_gov, state_abb %in% cohort1),
  method   = "nearest",
  estimand = "ATT",
  distance = "glm",      # pode trocar para "mahalanobis" se preferir sem PS
  link     = "logit",
  ratio    = 3,          # menos controles por tratado pode dar melhor balance
  replace  = TRUE,       # permite reuso de controles, aumenta precisão
  caliper  = 0.30,       # caliper mais restritivo ajuda no balance
  std.caliper = TRUE,
  discard  = "both",  # em vez de ambos, mantém tratados
  m.order  = "closest",  # "closest" costuma dar pareamentos mais estáveis
  verbose  = TRUE
)

m.out_gov2 <- matchit(
  formula  = treat_cohort2 ~ #oil_corr_gov +
    mean_vote_gov_pretreat +
    mean_vote_gov_pretreat_dem +
    mean_vote_gov_pretreat_rep,
  data     = subset(subset_matching_gov, state_abb %in% cohort2),
  method   = "nearest",
  estimand = "ATT",
  distance = "glm",      # pode trocar para "mahalanobis" se preferir sem PS
  link     = "logit",
  ratio    = 1,          # menos controles por tratado pode dar melhor balance
  replace  = FALSE,       # permite reuso de controles, aumenta precisão
  caliper  = 0.20,       # caliper mais restritivo ajuda no balance
  std.caliper = TRUE,
  discard  = "both",  # em vez de ambos, mantém tratados
  m.order  = "closest",  # "closest" costuma dar pareamentos mais estáveis
  verbose  = TRUE
)


# summary(m.out_pres)
# #plot(m.out_pres,type="density")
# plot(summary(m.out_pres))

summary(m.out_gov1)
#plot(m.out_gov1,type="density")
plot(summary(m.out_gov1))

summary(m.out_gov2)
#plot(m.out_gov2,type="density")
plot(summary(m.out_gov2))

# matched_fips_pres <- match.data(m.out_pres)$fips_numeric

matched_fips_gov1 <- match.data(m.out_gov1)$fips_numeric

matched_fips_gov2 <- match.data(m.out_gov2)$fips_numeric

#Now filter the original dataset to keep only the matched fips
# subset_analysis_matched_pres <- subset_analysis %>%
#   filter(fips_numeric %in% matched_fips_pres)

subset_analysis_matched_gov1 <- subset_analysis %>%
  filter(fips_numeric %in% matched_fips_gov1)

subset_analysis_matched_gov2 <- subset_analysis %>%
  filter(fips_numeric %in% matched_fips_gov2)

colnames(subset_analysis_matched_gov2)

# subset_analysis[,c(2,6,57)] drop NA
df <- subset_analysis_matched_gov2[!is.na(subset_analysis_matched_gov2$vote_share_incumb_gov_delta),][,c(2,6,38,47,57)]

summary(df)
# Show all the unique values of state_abb for each year
table <- df %>%
  group_by(year) %>%
  summarize(states = paste(unique(state_abb), collapse = ", ")) %>%
  arrange(year)

table

subset_analysis_matched_gov1 <- subset_analysis_matched_gov1 %>%
  filter(
    year != 1968 &
      year != 1972)


# # # REGRESSÕES COM DADOS DO PRESIDENTE
# m6a <- lm(vote_share_incumb_pres_delta ~ BSF_implemented * oil_deflated_std
#                     + party_pres
#                     + factor(year) + factor(fips_numeric) + factor(state_abb)
#           , data = subset_analysis)
# m6b <- lm_robust(vote_share_incumb_pres_delta ~ BSF_implemented * oil_deflated_std
#                  + party_pres
#                  + factor(year) + factor(state_abb)
#                  , data = subset_analysis
#                  , fixed_effects = ~(fips_numeric)
#                  , clusters = factor(state_abb))
# m6c <- lm(vote_share_incumb_pres_delta ~ BSF_implemented * oil_deflated_std
#           + party_pres + factor(state_abb)
#           + factor(year) + factor(fips_numeric)
#           , data = subset_analysis_matched_pres)
# m6d <- lm_robust(vote_share_incumb_pres_delta ~ BSF_implemented * oil_deflated_std
#                  + party_pres + factor(state_abb)
#                  + factor(year)
#                  , data = subset_analysis_matched_pres
#                  , fixed_effects = ~factor(fips_numeric)
#                  , clusters = factor(state_abb) ,  se_type = "CR2")
# 
# screenreg(list(m6a,m6b,m6c,m6d)
#           ,ci.force = TRUE
#           ,omit.coef = "(Intercept)|factor"
#           # ,custom.coef.names = c("BSF Implementation Dummy"
#           #                        ,"Oil Price (std.)"
#           #                        ,"BSF Implementation Dummy x Oil Price (std.)")
#           ,custom.header = list("No Matching" = 1:2,"Mathing on NM" = 3:4)
#           ,custom.model.names = c("m6.a", "m6.b","m6.c","m6.d")
#           ,custom.gof.rows = list("State, county and Year FE" = c("Yes", "Yes","Yes","Yes")
#                                   ,"SE" = c("OLS", "CR2","OLS", "CR2")))
#
# # Marginal effects
# plot_slopes(m6c, variables = "oil_deflated_std", by = "BSF_implemented")
# 
# plot_predictions(m6c, condition = c("oil_deflated_std", "BSF_implemented"))

# # Regressões com dados do governador
m1a <- lm(vote_share_incumb_gov_delta ~ BSF_implemented * oil_deflated_std
          + party_gov
          + factor(state_abb)
          + factor(year) + factor(fips_numeric)
          , data = subset_analysis)

m1b <- lm_robust(vote_share_incumb_gov_delta ~ BSF_implemented * oil_deflated_std
                 + party_gov
                 + factor(state_abb)
                 + factor(year)
                 , data = subset_analysis
                 , fixed_effects = ~factor(fips_numeric)
                 , clusters = factor(state_abb) ,  se_type = "CR2")

m2a <- lm(vote_share_incumb_gov_delta ~ BSF_implemented * oil_deflated_std
          + party_gov
          + factor(state_abb)
          + factor(year) + factor(fips_numeric)
          , data = subset_analysis_matched_gov1)

m2b <- lm_robust(vote_share_incumb_gov_delta ~ BSF_implemented * oil_deflated_std
                 + party_gov
                 + factor(state_abb)
                 + factor(year) + factor(state_abb)
                 , data = subset_analysis_matched_gov1
                 , fixed_effects = ~factor(fips_numeric)
                 , clusters = factor(state_abb),  se_type = "CR2")

m3a <- lm(vote_share_incumb_gov_delta ~ BSF_implemented * oil_deflated_std
          + party_gov
          + factor(state_abb)
          + factor(year) + factor(fips_numeric)
          , data = subset_analysis_matched_gov2)

m3b <- lm_robust(vote_share_incumb_gov_delta ~ BSF_implemented * oil_deflated_std
                 + party_gov
                 + factor(state_abb)
                 + factor(year) + factor(state_abb)
                 , data = subset_analysis_matched_gov2
                 , fixed_effects = ~factor(fips_numeric)
                 , clusters = factor(state_abb),  se_type = "CR2")

screenreg(list(m1a,m1b,m2a,m2b,m3a,m3b)
          ,ci.force = TRUE
          ,omit.coef = "(Intercept)|factor"
          ,custom.coef.names = c("BSF Implementation Dummy"
                        ,"Oil Price (std.)"
                        ,"Republican incumbent"
                        ,"BSF Implementation Dummy x Oil Price (std.)")
          ,custom.header = list("No Matching" = 1:2,"Mathing cohort 1" = 3:4,"Mathing cohort 1" = 5:6)
          ,custom.model.names = c("m1.a", "m1.b","m2.a","m2.b","m3.a","m3.b")
          ,custom.gof.rows = list("State, county and year FE" = c("Yes", "Yes","Yes","Yes","Yes","Yes")
                        ,"SE" = c("OLS", "CR2","OLS", "CR2","OLS", "CR2")))

#make m3b into latex

library(equatiomatic)
equation(m3a)

#Marginal effects
plot_slopes(m1a, variables = "oil_deflated_std", by = "BSF_implemented")

plot_predictions(m1a, condition = c("oil_deflated_std", "BSF_implemented"))

