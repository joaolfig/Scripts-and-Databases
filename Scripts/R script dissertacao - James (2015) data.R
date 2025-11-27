suppressPackageStartupMessages({
  library(tidyr);library(dplyr);library(texreg);library(estimatr);library(marginaleffects);
  library(AER);library(lmtest);library(sandwich);library(flextable);library(mediation);
  library(equatiomatic)
})


setwd("C:/Users/Joao arthur/OneDrive - Fundacao Getulio Vargas - FGV/Dissertação/Scripts-and-Databases")

source('Scripts/R script dissertacao - Oil prices.R')
source('Scripts/R script dissertacao - BSF Data.R')

#remove all datasets but "oil_deflated"
rm(list = setdiff(ls(), c("oil_deflated","oil_monthly","BSF_dataset","BSF_rules")))


setwd("C:/Users/Joao arthur/OneDrive - Fundacao Getulio Vargas - FGV/Dissertação/Scripts-and-Databases")
df_james <- read_csv("Databases/Replication Files for James (2015)/57.to.2008.raw.data.Aug4.2014.csv")
df_james$`price oil (pb)` <- as.numeric(df_james$`price oil (pb)`)

#rename columns
colnames(df_james)[5] <- "State"
colnames(df_james)[20] <- "resrev"


df_james <- df_james %>%
  filter(year >= 1957 & year <= 2007)


df_james <- df_james %>%
  mutate(res_share = resrev / (resrev + `non res rev`))

table1 <- aggregate(res_share ~ State, data = df_james, FUN = mean)  %>%
  arrange(desc(res_share))

# I want a table with mean, min e max of res_share by State
table1 <- df_james %>%
  group_by(State) %>%
  summarize(mean_res_share = mean(res_share, na.rm = TRUE)
            ,max_res_share = max(res_share, na.rm = TRUE)
            ,min_res_share = min(res_share, na.rm = TRUE)) %>%
  arrange(desc(mean_res_share))

flextable(table1)

#Select only the states with mean_res_share > 0.1
states_selected <- table1 %>%
  filter(mean_res_share > 0.1) %>%
  pull(State)

df_james_subset <- df_james %>%
  filter(State %in% states_selected)

# Plot the res_share over time for each state
ggplot(df_james_subset, aes(x = year, y = res_share, color = `State`)) +
  geom_line() +
  geom_point() +
  labs(title = "Share of Natural Resources Revenue in Total Revenue by State",
       x = "Year",
       y = "Share") +
  theme_minimal()

# remove alaska from df_james_subset
df_james_subset <- df_james_subset %>%
  filter(State != "AK")


# Oil variables
oil_monthly <- oil_monthly %>%
  filter(month=="Dec")

colnames(oil_monthly)[3] <- "oil_dec"

oil_monthly$oil_dec_lag <- dplyr::lag(oil_monthly$oil_dec)

df_james_subset <- df_james_subset %>%
  left_join(oil_deflated, by = c("year")) %>%
  left_join(oil_monthly, by = c("year")) 

df_james_subset$oil_deflated <- as.numeric(df_james_subset$`price oil (pb)`)

#Group df_james by State and arrange by year
#And create a resrev_lag variable
df_james_subset <- df_james_subset %>%
  group_by(State) %>%
  arrange(year) %>%
  mutate(resrev_lag = dplyr::lag(resrev)
         ,d_resrev = resrev_lag - resrev
         ,nonresrev_lag = dplyr::lag(`non res rev`)
         ,savings_lag = dplyr::lag(savings)
         ,totexp_lag = dplyr::lag(`tot exp`)
         ,toteduexp_lag = dplyr::lag(`tot edu exp`)) %>%
  ungroup()

#drop columns 1,4,6,7,9:11,13,14,18,19
df_james_subset <- df_james_subset %>%
  dplyr::select(-c(1,4,6,7,9:11,13,14,18,19,36:38,46))

#deflate resrev, non res rev, savings, tot exp and tot edu exp by deflator_multiplier
df_james_subset <- df_james_subset %>%
  mutate(resrev_i = resrev * deflator_multiplier
         ,nonresrev_i = `non res rev` * deflator_multiplier
         ,savings_i = savings * deflator_multiplier
         ,totexp_i = `tot exp` * deflator_multiplier
         ,toteduexp_i = `tot edu exp` * deflator_multiplier)

# Now the lag values, grouped by State and sorted by year for these _i
df_james_subset <- df_james_subset %>%
  group_by(State) %>%
  arrange(year) %>%
  mutate(resrev_i_lag = dplyr::lag(resrev_i)
         ,nonresrev_i_lag = dplyr::lag(nonresrev_i)
         ,savings_i_lag = dplyr::lag(savings_i)
         ,totexp_i_lag = dplyr::lag(totexp_i)
         ,toteduexp_i_lag = dplyr::lag(toteduexp_i)
         ,oil_deflated_lag = dplyr::lag(oil_deflated)) %>%
  ungroup()

df_james_subset <- df_james_subset %>%
  left_join(BSF_dataset, by = c("State" = "state", "year" = "year"))

#filter states in BSF_rules that are in selected_states

BSF_rules_subset <- BSF_rules %>%
  filter(state %in% states_selected)

#remove AK
BSF_rules_subset <- BSF_rules_subset %>%
  filter(state != "AK")

BSF_rules_subset <- BSF_rules_subset %>%
  filter(state != "MT")

# flextable(BSF_rules_subset)

#________________________________________________________________________________

#Oil on revenue and expenses
m1a <- lm_robust(resrev_i ~ oil_deflated
                 + resrev_i_lag
                 + factor(year)
                 ,fixed_effects = ~ factor(State)
                 ,clusters = State
                 ,data = subset(df_james_subset)
                 ,se_type = "CR2")


m1b <- lm_robust(resrev_i ~ oil_deflated_lag
                + resrev_i_lag
                + factor(year)
                ,fixed_effects = ~ factor(State)
                ,clusters = State
                ,data = subset(df_james_subset)
                ,se_type = "CR2")

m1c <- lm_robust(resrev_i ~ oil_dec
                 + resrev_i_lag
                 + factor(year)
                 ,fixed_effects = ~ factor(State)
                 ,clusters = State
                 ,data = subset(df_james_subset)
                 ,se_type = "CR2")

m1d <- lm_robust(resrev_i ~ oil_dec_lag
                + resrev_i_lag
                + factor(year)
                ,fixed_effects = ~ factor(State)
                ,clusters = State
                ,data = subset(df_james_subset)
                ,se_type = "CR2")

screenreg(list(m1a,m1b,m1c,m1d)
          ,omit.coef = "factor"
          ,custom.coef.names = c("Mean Oil price in T (Real values)"
                                 ,"Natural Resources Revenue in T-1 (Real values)"
                                 ,"Mean Oil price in T-1 (Real values)"
                                 ,"Oil prices in Dec of T (Nominal values)"
                                 ,"Oil prices in Dec of T-1 (Nominal values)")
          ,custom.model.names = c("m1.a", "m1.b", "m1.c","m1.d")
          ,custom.gof.rows = list("State and Year FE" = c("Yes", "Yes", "Yes","Yes")
                                  ,"SE" = c("CR2", "CR2", "CR2","CR2")))
colnames(df_james_subset)
m2a <- lm_robust(savings_i ~ oil_deflated*BSF_deposit_5level
                  + oil_deflated*`Deposit Rule`
                  + savings_i_lag
                  + factor(year)
                  ,fixed_effects = ~ factor(State)
                  ,clusters = State
                  ,data = df_james_subset)
m2b <- lm_robust(savings_i ~ oil_deflated*factor(`Deposit Rule`)
                 + savings_i_lag
                 + factor(year)
                 ,fixed_effects = ~ factor(State)
                 ,clusters = State
                 ,data = subset(df_james_subset, year <= 1977)
                 ,se_type = "CR2")
m2c <- lm_robust(savings_i ~ oil_deflated*BSF_deposit_5level
                  + savings_i_lag
                  + factor(year)
                  ,fixed_effects = ~ factor(State)
                  ,clusters = State
                  ,data = subset(df_james_subset, year >= 1991)
                  ,se_type = "CR2")


screenreg(list(m2a,m2b,m2c)
          ,omit.coef = "State|year"
          ,custom.model.names = c("m2.a", "m2.b", "m2.c")
          ,custom.gof.rows = list("State and Year FE" = c("Yes", "Yes", "Yes")
                                  ,"Period" = c("Full Sample", "1957-1977", "1991-2007")
                                  ,"SE" = c("CR2", "CR2", "CR2")))


m3.a <- lm_robust(totexp_i ~ oil_deflated
                  + totexp_i_lag
                  + factor(year)
                  ,fixed_effects = ~ factor(State)
                  ,clusters = State
                  ,data = df_james_subset
                  ,se_type = "CR2")

screenreg(m3.a)

m3.a <- lm_robust(totexp_i ~ oil_deflated
                  + totexp_i_lag
                  + factor(year)
                  ,fixed_effects = ~ factor(State)
                  ,clusters = State
                  ,data = subset(df_james_subset, year <= 1977)
                  ,se_type = "CR2")
m3.b <- lm_robust(totexp_i ~ oil_deflated
                  + totexp_i_lag
                  + factor(year)
                  ,fixed_effects = ~ factor(State)
                  ,clusters = State
                  ,data = subset(df_james_subset, year >= 1991)
                  ,se_type = "CR2")

m4.a <- lm_robust(toteduexp_i ~ oil_deflated
                  + toteduexp_i_lag
                  + factor(year)
                  ,fixed_effects = ~ factor(State)
                  ,clusters = State
                  ,data = subset(df_james_subset, year <= 1977)
                  ,se_type = "CR2")
m4.b <- lm_robust(toteduexp_i ~ oil_deflated
                  + toteduexp_i_lag
                  + factor(year)
                  ,fixed_effects = ~ factor(State)
                  ,clusters = State
                  ,data = subset(df_james_subset, year >= 1991)
                  ,se_type = "CR2")

screenreg(list(m3.a,m3.b,m4.a,m4.b)
          ,custom.model.names = c("m3.a", "m3.b", "m4.a","m4.b")
          ,omit.coef = "factor"
          ,custom.gof.rows = list("State and Year FE" = c("Yes", "Yes", "Yes","Yes")
                                  ,"Period" = c("1957-1977", "1991-2007","1957-1977","1991-2007")
                                  ,"SE" = c("CR2", "CR2", "CR2","CR2")))


m5a <- lm_robust(savings_i ~ oil_deflated*BSF_implemented
                + savings_i_lag
                + factor(year)
                ,fixed_effects = ~ factor(State)
                ,clusters = State
                ,data = subset(df_james_subset)
                ,se_type = "CR2")
m5b <- lm_robust(savings_i ~ resrev_i*BSF_implemented
                + savings_i_lag
                + factor(year)
                ,fixed_effects = ~ factor(State)
                ,clusters = State
                ,data = subset(df_james_subset)
                ,se_type = "CR2")


m6a <- lm_robust(totexp_i ~ oil_deflated*BSF_implemented
                + totexp_i_lag
                + factor(year)
                ,fixed_effects = ~ factor(State)
                ,clusters = State
                ,data = subset(df_james_subset)
                ,se_type = "CR2")
m6b <- lm_robust(totexp_i ~ resrev_i*BSF_implemented
                + totexp_i_lag
                + factor(year)
                ,fixed_effects = ~ factor(State)
                ,clusters = State
                ,data = subset(df_james_subset)
                ,se_type = "CR2")


screenreg(list(m5a,m5b,m6a,m6b)
          ,custom.model.names = c("m5.a", "m5.b", "m6.a","m6.b")
          ,omit.coef = "factor"
          ,custom.gof.rows = list("State and Year FE" = c("Yes", "Yes", "Yes","Yes"))
          ,"SE" = c("CR2", "CR2", "CR2","CR2"))


m5 <- lm(savings_i ~ resrev_i*BSF_implemented
                 + savings_i_lag
                 + factor(year)
                 + factor(State)
                 ,data = df_james_subset)

screenreg(m5,omit.coef="factor")


# med.mod <- lm(resrev_i ~ oil_deflated + resrev_i_lag + factor(year) +factor(State), data = df_james_subset)
# out.mod1 <- lm(savings_i ~ oil_deflated + resrev_i + factor(year) +factor(State), data = df_james_subset)
# out.mod2 <- lm(totexp_i ~ oil_deflated + resrev_i + factor(year) +factor(State), data = df_james_subset)
# 
# med.out1 <- mediate(
#   model.m = med.mod,
#   model.y = out.mod1,
#   treat   = "oil_deflated",
#   mediator= "resrev_i",
#   sims    = 1000
# )
#______________________________________________________
gov_elections <- read.csv("Databases/US Election Results Executive/gov_elections_release.csv", header = TRUE, sep = ",")
gov_elections <- gov_elections[,c(2,3,5,9,10,12,14,15)]

#Filter years between 1957 and 1990
gov_elections <- gov_elections %>%
  filter(election_year >= 1957 & election_year <= 2007)
#Filter states in states_selected in column state
gov_elections <- gov_elections %>%
  filter(state %in% states_selected)

# Agregate votes by state and election_year
gov_elections <- gov_elections %>%
  group_by(state, election_year) %>%
  summarize(seat_status = first(seat_status)
            ,democratic_raw_votes = sum(democratic_raw_votes)
            ,republican_raw_votes = sum(republican_raw_votes)
            ,raw_county_vote_totals = sum(raw_county_vote_totals)) %>%
  ungroup()

#Drop LA 1968 and LA 1975
gov_elections <- gov_elections %>%
  filter(!(state == "LA" & election_year == 1968)) %>%
  filter(!(state == "LA" & election_year == 1975))  %>%
  filter(!(state == "LA" & election_year == 1987))

# Make the first word of seat_status into a column
gov_elections <- gov_elections %>%
  mutate(incumbent_party = ifelse(grepl("Democratic", seat_status), "Democratic", 
                                  ifelse(grepl("Republican", seat_status), "Republican", "Other")))

# Make a republican vote share and a democrat vote share column
gov_elections <- gov_elections %>%
  mutate(dem_vote_share_2parties = democratic_raw_votes / (democratic_raw_votes + republican_raw_votes)
         ,rep_vote_share_2parties = republican_raw_votes / (democratic_raw_votes + republican_raw_votes))

# Make lag variables for dem_vote_share_2parties and rep_vote_share_2parties by state 
gov_elections <- gov_elections %>%
  group_by(state) %>%
  arrange(election_year) %>%
  mutate(dem_vote_share_2parties_lag = dplyr::lag(dem_vote_share_2parties)
         ,rep_vote_share_2parties_lag = dplyr::lag(rep_vote_share_2parties)) %>%
  ungroup()

# Delta vote share of the incumbent party
gov_elections <- gov_elections %>%
  mutate(delta_vote_share_incumbent = ifelse(incumbent_party == "Democratic", 
                                             dem_vote_share_2parties - dem_vote_share_2parties_lag,
                                             ifelse(incumbent_party == "Republican", 
                                                    rep_vote_share_2parties - rep_vote_share_2parties_lag, NA)))

# Join with df_james_subset
test_elections <- df_james_subset %>%
  left_join(gov_elections[,c(1,2,7,12)], by = c("State" = "state", "year" = "election_year"))

# Drop rows with NA in delta_vote_share_incumbent
# test_elections <- test_elections %>%
#   filter(!is.na(delta_vote_share_incumbent))
#Place a dummy of 1 if delta_vote_share_incumbent is not NA and 0 if it is NA
test_elections <- test_elections %>%
  mutate(state_election_year_dummy = ifelse(!is.na(delta_vote_share_incumbent), 1, 0))

colnames(test_elections)

m5 <- lm_robust( savings_i ~ 
                 oil_deflated*state_election_year_dummy
                 + savings_i_lag
                 ,fixed_effects = ~ factor(State) + factor(year)
                 ,data = subset(test_elections,BSF_implemented == 1))

screenreg(m5,omit.coef = "factor")


