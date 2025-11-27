setwd("C:/Users/Joao arthur/OneDrive - Fundacao Getulio Vargas - FGV/Dissertação/Scripts-and-Databases")


###### TRATAMENTO DADOS ELECTORAL RETURNS
# Não tenho os dados pra presidente pro Alaska
pres_elections <- read.csv("Databases/US Election Results Executive/pres_elections_release.csv", header = TRUE, sep = ",")
pres_elections <- pres_elections[,c(1,2,5,8,9,11,13,14)]
gov_elections <- read.csv("Databases/US Election Results Executive/gov_elections_release.csv", header = TRUE, sep = ",")
gov_elections <- gov_elections[,c(2,3,5,9,10,12,14,15)]

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

# Analysis:

# gov_elections <- gov_elections %>%
#   filter(election_year >= 1947 & election_year <= 1990)
# 
# gov_elections <- gov_elections %>%
#   filter(state %in% c("LA", "NM", "TX", "WY", "OK", "MT", "ND", "SD"))


#Check unique "state" by "election_year" value
table <- unique(gov_elections[,c("state","election_year")])%>%
  arrange(election_year,state)

### Break seat_status variable into two
gov_elections <- gov_elections %>%
  mutate(seat_status_open = ifelse(seat_status == "Open Seat", 1, 0)
         ,seat_status_incumbent = ifelse(seat_status == "Incumbent", 1, 0)
         ,seat_status_challenger = ifelse(seat_status == "Challenger", 1, 0))

#break seat_status into two by " ". Make the second one the all text but word one
pres_elections <- pres_elections %>%
  mutate(party_pres = word(seat_status, 1)
         ,seat_status_pres = word(seat_status, 2, -1))

gov_elections <- gov_elections %>%
  mutate(party_gov = word(seat_status, 1)
         ,seat_status_gov = word(seat_status, 2, -1))


