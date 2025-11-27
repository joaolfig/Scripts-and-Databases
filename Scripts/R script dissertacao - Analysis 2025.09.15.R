library(dplyr)
library(texreg)
library(lfe)
library(estimatr)
library(MatchIt)
library(marginaleffects)
library(did)

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



#### Faz o Merge com os dados de B&W
load("C:/Users/Joao arthur/OneDrive - Fundacao Getulio Vargas - FGV/Dissertação/Scripts-and-Databases/Databases/Replication Files for Benedictis-Kessner & Warshaw (2020)/econ_counties_cities_analysis.Rdata")

#subset for only states that are oil_exporter
data_analysis$oil_exporter <- ifelse(data_analysis$state_abb
                                     #%in% c("AK","LA","MT","ND","NM","OK","SD","TX","WY"), 1, 0)
                                     %in% c("LA","MT","ND","NM","OK","TX","WY"), 1, 0)
  
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


# Matching on pre-treatment outcomes
subset_analysis_pres <- subset_analysis %>%
  filter(
    !is.na(vote_share_incumb_pres_delta) &
      year < 2005)

subset_analysis_gov <- subset_analysis %>%
  filter(
    !is.na(vote_share_incumb_gov_delta) &
      year < 2005)

#Group counties and than calculate the correlation between oil prices and vote_share_incumb_pres

#Subset to match only years before 1978
subset_analysis_pres <- subset_analysis_pres %>%
  filter(year < 1978)

unique(subset_analysis_pres$year)

table(subset_analysis_pres$state_abb)/3

# Drop any column without 

subset_analysis_pres <- subset_analysis_pres %>%
  group_by(fips_numeric) %>%
  mutate(
    oil_corr = cor(oil_price, vote_share_incumb_pres, use = "complete.obs"),
    rep_pres_1968 = if (any(year == 1968)) vote_share_rep_pres[year == 1968] else NA_real_,
    rep_pres_1972 = if (any(year == 1972)) vote_share_rep_pres[year == 1972] else NA_real_,
    rep_pres_1976 = if (any(year == 1976)) vote_share_rep_pres[year == 1976] else NA_real_,
    dem_pres_1968 = if (any(year == 1968)) vote_share_dem_pres[year == 1968] else NA_real_,
    dem_pres_1972 = if (any(year == 1972)) vote_share_dem_pres[year == 1972] else NA_real_,
    dem_pres_1976 = if (any(year == 1976)) vote_share_dem_pres[year == 1976] else NA_real_
  ) %>%
ungroup()


subset_analysis_pres <- subset_analysis_pres %>%
  group_by(fips_numeric) %>%
  mutate(mean_vote_pres_pretreat = round(mean(raw_county_vote_totals.x, na.rm = TRUE),0)
         #,round(mean_vote_gov_pre_treat = mean(raw_county_vote_totals.y, na.rm = TRUE),0)) 
         )%>%
  ungroup()



# Dataframe com fips_numeric, oil_corr e as colunas de interesse
matching_df <- subset_analysis_pres %>%
  select(fips_numeric,state_abb, oil_corr
         ,rep_pres_1968, rep_pres_1972, rep_pres_1976
         ,dem_pres_1968, dem_pres_1972, dem_pres_1976
         ,mean_vote_pres_pretreat) %>%
  distinct()

#remove any row with NA in any column
matching_df <- na.omit(matching_df)

matching_df <- matching_df %>%
  mutate(avg_dem = (rep_pres_1968 + rep_pres_1972 + rep_pres_1976) / 3
         ,avg_rep = (dem_pres_1968 + dem_pres_1972 + dem_pres_1976) / 3)


matching_df$treat <- ifelse(matching_df$state_abb == "NM", 1, 0)

#Use matchit to mathc on these variables
m.out <- matchit(treat ~ oil_corr + avg_dem + avg_rep + mean_vote_pres_pretreat,
                 data = matching_df,
                 method = "nearest",
                 distance = "logit",
                 ratio = 5)

summary(m.out)

matched_data <- match.data(m.out)
matched_fips <- matched_data$fips_numeric

#Now filter the original dataset to keep only the matched fips
subset_analysis_pres_matched <- subset_analysis %>%
  filter(fips_numeric %in% matched_fips,
         !is.na(vote_share_incumb_pres_delta)
         , year <= 1984)

unique(subset_analysis_pres_matched$year)

m1pres_matched <- lm_robust(vote_share_incumb_pres_delta ~
                      oil_price_std * BSF_implemented
                    + pres_party
                    + factor(state_abb)
                    + factor(fips_numeric)
                    ,clusters = factor(fips_numeric)
                    , data = subset_analysis_pres_matched)

screenreg(list(m1pres_matched),omit.coef = "factor", single.row = TRUE,digits=3)

length(unique(subset_analysis$year))

round(table(subset_analysis$state_abb)/51,0)

table(subset_analysis_pres_matched$state_abb)/5

#Plot marginal effects
plot_comparisons(m1pres_matched, variables = "oil_price_std", condition = "BSF_implemented")

plot_predictions(m1pres_matched,condition = c("oil_price_std","BSF_implemented"))

#drop columns 1,4,6,7,9:11,13,14,18,19
