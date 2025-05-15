library(dplyr)

#remove all variables
#rm(list=ls())

#set working directory
setwd("C:/Users/Joao arthur/OneDrive - Fundacao Getulio Vargas - FGV/Dissertação/Scripts-and-Databases")

gov_elections <- read.csv("Databases/US Election Results Executive/gov_elections_release.csv", header = TRUE, sep = ",")


############### Table of candidates ######
candidates <- unique(gov_elections[, c("election_id",'seat_status', "dem_nominee", "rep_nominee")])

############### Table of Alaska ##########
# I will still need to go after this data

############### TREAT DATA ###############
#convert county_first_date to dates
gov_elections$county_first_date <- as.Date(gov_elections$county_first_date, format = "%Y-%m-%d")
gov_elections$county_end_date <- as.Date(gov_elections$county_end_date, format = "%Y-%m-%d")


# Aggregate data by summing votes to dem, rep, for the 2 parties together, and total of votes by election_id
vote_dem <- aggregate(gov_elections$democratic_raw_votes, by = list(gov_elections$election_id), FUN = sum)
vote_rep <-  aggregate(gov_elections$republican_raw_votes, by = list(gov_elections$election_id), FUN = sum)[2]
vote_2pty <- aggregate(gov_elections$gov_raw_county_vote_totals_two_party, by = list(gov_elections$election_id), FUN = sum)[2]
vote_tot <- aggregate(gov_elections$raw_county_vote_totals, by = list(gov_elections$election_id), FUN = sum)[2]

#merge the data
vote_state <- cbind(vote_dem, vote_rep, vote_2pty, vote_tot)

#rename columns
colnames(vote_state) <- c("election_id", "democratic_raw_votes", "republican_raw_votes", "gov_raw_county_vote_totals_two_party", "raw_county_vote_totals")

#add the candidates
vote_state <- merge(vote_state, candidates, by = "election_id")


#Create State and Year columns from election_id
vote_state$year <- substr(vote_state$election_id, 1, 4)

vote_state$state <- substr(vote_state$election_id, 6, 7)

# Create vote share columns
dem_pct_votes_2pty <- round(vote_state$democratic_raw_votes/vote_state$gov_raw_county_vote_totals_two_party,4)
dem_pct_votes_tot <- round(vote_state$democratic_raw_votes/vote_state$raw_county_vote_totals,4)
rep_pct_votes_2pty <- round(vote_state$republican_raw_votes/vote_state$gov_raw_county_vote_totals_two_party,4)
rep_pct_votes_tot <- round(vote_state$republican_raw_votes/vote_state$raw_county_vote_totals,4)

vote_state <- cbind(vote_state, dem_pct_votes_2pty, dem_pct_votes_tot, rep_pct_votes_2pty, rep_pct_votes_tot)

# Incumbent and Winner of the election (party)
vote_state$winner_party <- ifelse(vote_state$democratic_raw_votes > vote_state$republican_raw_votes, "Democrat", "Republican")
##### (Still gotta consider the independents from a table of independents to replace 3rd party  cases)

# Incumbent Party
vote_state$incumbent_party <- sub(" .*", "", vote_state$seat_status)

# vote_state <- vote_state %>%
#   group_by(state) %>%
#   mutate(incumbent_party = lag(winner_party)) 

# Re-election of the incumbent (party)
vote_state$reelection_party <- ifelse(vote_state$winner_party == vote_state$incumbent_party, 1, 0)


# Incumbent and Winner of the election (candidate)

vote_state$winner_candidate <- ifelse(vote_state$democratic_raw_votes > vote_state$republican_raw_votes, vote_state$dem_nominee , vote_state$rep_nominee)
vote_state <- vote_state %>%
  arrange(state, year) %>%
  group_by(state) %>%
  mutate(incumbent = lag(winner_candidate)) 

# Re-election of the incumbent (candidate)
vote_state$reelection_candidate <- ifelse(vote_state$winner_candidate == vote_state$incumbent, 1, 0)

# Dummies indicating whether incumbent and challenger are trying re-election
vote_state$incumbent_running <- ifelse( vote_state$incumbent != 'None' &
                                        (vote_state$incumbent == vote_state$dem_nominee
                                       |vote_state$incumbent == vote_state$rep_nominee), 1, 0)

vote_state <- vote_state %>%
  group_by(state) %>%
  mutate(challenger_running =
    (dem_nominee != 'None' & rep_nominee != 'None') &
    ((dem_nominee != incumbent) & (dem_nominee == lag(dem_nominee))
    | (rep_nominee != incumbent) & (rep_nominee == lag(rep_nominee))))

vote_state$challenger_running <- as.integer(vote_state$challenger_running)

#Variable of % two-party vote on the incumbent party
vote_state$incumbent_pct_2pty <- ifelse(vote_state$incumbent_party == 'Democratic'
                                         ,vote_state$dem_pct_votes_2pty
                                         ,vote_state$rep_pct_votes_2pty)

#Variable of % two-party vote on the incumbent party
vote_state$incumbent_pct_tot <- ifelse(vote_state$incumbent_party == 'Democratic'
                                        ,vote_state$dem_pct_votes_tot
                                        ,vote_state$rep_pct_votes_tot)

# Percentage (pct) change in votes in the party from one election to another
vote_state <- vote_state %>%
  group_by(state) %>%
  mutate(dem_pct_2pty_change = dem_pct_votes_2pty - lag(dem_pct_votes_2pty))

vote_state <- vote_state %>%
  group_by(state) %>%
  mutate(rep_pct_2pty_change = (rep_pct_votes_2pty - lag(rep_pct_votes_2pty)))

vote_state$incumbent_pct_2pty_change <- ifelse(vote_state$incumbent_party == 'Democratic'
                                        ,vote_state$rep_pct_2pty_change
                                        ,vote_state$rep_pct_2pty_change)


############### FILTERING AS IN WOLFERS (2007)
vote_state <- vote_state %>%
  filter(year >= 1948) %>%
# filter(year <= 1997) %>%
  filter(dem_pct_votes_tot + rep_pct_votes_tot > .8) %>%
  filter(dem_pct_votes_tot >!.8 & rep_pct_votes_tot >!.8) %>%
  filter(dem_nominee != 'None' & rep_nominee != 'None') %>%
  filter(!is.na(dem_nominee) & !is.na(rep_nominee)) %>%
  filter(incumbent_party == 'Democratic' | incumbent_party == 'Republican')  
# I still gotta filter special elections, but I'm not sure what they are


#filter unqiue values in gov_elections$seat_status
#unique(gov_elections$seat_status)
#This can be insightful later


#List of special elections: https://en.wikipedia.org/wiki/Category:United_States_gubernatorial_special_elections

