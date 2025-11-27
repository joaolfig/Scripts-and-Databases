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
         ,d_resrev_i = resrev_i - resrev_i_lag
         ,nonresrev_i_lag = dplyr::lag(nonresrev_i)
         ,savings_i_lag = dplyr::lag(savings_i)
         ,totexp_i_lag = dplyr::lag(totexp_i)
         ,toteduexp_i_lag = dplyr::lag(toteduexp_i)
         ,oil_deflated_lag = dplyr::lag(oil_deflated)
         ,savings_rate = savings /  (resrev + `non res rev`)
         ,savings_rate_lag = dplyr::lag(savings_rate)
         ,res_share_lag = dplyr::lag(res_share)) %>%
  ungroup()

df_james_subset <- df_james_subset %>%
  left_join(BSF_dataset, by = c("State" = "state", "year" = "year"))

#filter states in BSF_rules that are in selected_states

BSF_rules_subset <- BSF_rules %>%
  filter(state %in% states_selected)

#remove AK
BSF_rules_subset <- BSF_rules_subset %>%
  filter(state != "AK")


flextable(BSF_rules_subset)


# DUMMY FOR ELECTION YEARS
gov_elections <- read.csv("Databases/US Election Results Executive/gov_elections_release.csv", header = TRUE, sep = ",")
gov_elections <- gov_elections[,c(2,3,5,9,10,12,14,15)]
gov_elections

states_election_years <- gov_elections[,c(1,3)] %>%
  distinct(state, election_year, .keep_all = TRUE)

states_election_years$election_year_dm <- 1

df_james_subset <- df_james_subset %>%
  left_join(states_election_years, by = c("State" = "state", "year" = "election_year"))

df_james_subset <- df_james_subset %>%
  mutate(election_year_dm = ifelse(is.na(election_year_dm), 0, election_year_dm))

#Check the correlatio between oil revenues and oil prices by state:
correlation_results <- df_james_subset %>%
  group_by(State) %>%
  summarize(correlation = cor(resrev_i, oil_deflated, use = "complete.obs")) %>%
  ungroup()

# Print the correlation results sort it by alphabetic order
print(correlation_results)


# Reduce the scale of numerical variables that I use
df_james_subset <- df_james_subset %>%
  mutate(resrev_i = resrev_i / 1000000
         ,resrev_i_lag = resrev_i_lag / 1000000
         ,savings_i = savings_i / 1000000
         ,savings_i_lag = savings_i_lag / 1000000
         ,totexp_i = totexp_i / 1000000
         ,totexp_i_lag = totexp_i_lag / 1000000)


summary(df_james_subset[,c("resrev_i","resrev_i_lag","savings_i","savings_i_lag","totexp_i","totexp_i_lag")])
colnames(df_james_subset)
#create the share of resrev_i in total revenue
df_james_subset <- df_james_subset %>%
  mutate(total_revenue_i = resrev_i + nonresrev_i
         ,resrev_share_i = resrev_i / total_revenue_i)

#________________________________________________________________________________

#Oil on revenue and expenses
m1a <- lm(resrev_i ~ oil_deflated
          + factor(State)
          ,data = df_james_subset)
m1b <- lm_robust(resrev_i ~ oil_deflated_lag
                 ,fixed_effects = ~ factor(State)
                 ,clusters = State
                 ,data = df_james_subset
                 ,se_type = "CR2")

m1c <- lm_robust(resrev_i ~ oil_dec
                 + factor(State)
                 ,data = df_james_subset)
m1d <- lm_robust(resrev_i ~ oil_dec_lag
                 ,fixed_effects = ~ factor(State)
                 ,clusters = State
                 ,data = df_james_subset
                 ,se_type = "CR2")

screenreg(list(m1a,m1b,m1c,m1d)
          ,ci.force = TRUE
          ,omit.coef = "factor|i_lag|(Intercept)"
          ,custom.coef.names = c("Mean Oil price in t"
                                 ,"Mean Oil price in t-1"
                                 ,"Oil prices in Dec of t"
                                 ,"Oil prices in Dec of t-1")
          ,custom.model.names = c("m1.a", "m1.b", "m1.c","m1.d")
          ,custom.gof.rows = list("State FE" = c("Yes", "Yes", "Yes","Yes")
                                  ,"Standard Errors" = c("OLS", "CR2", "OLS","CR2")))

m2a <- lm(savings_i ~ resrev_i*election_year_dm
          + savings_i_lag
          + factor(year)
          + factor(State)
          ,data = df_james_subset)
m2b <- lm_robust(savings_i ~ resrev_i*election_year_dm
                 + savings_i_lag
                 + factor(year)
                 ,fixed_effects = ~ factor(State)
                 ,clusters = State
                 ,se_type = "CR2"
                 ,data = df_james_subset)

m3a <- lm(totexp_i ~ resrev_i*election_year_dm
          + totexp_i_lag
          + factor(year)
          + factor(State)
          ,data = df_james_subset)
m3b <- lm_robust(totexp_i ~ resrev_i*election_year_dm
                 + totexp_i_lag
                 + factor(year)
                 ,fixed_effects = ~ factor(State)
                 ,clusters = State
                 ,data = df_james_subset
                 ,se_type = "CR2")

screenreg(list(m2a,m2b,m3a,m3b)
          ,ci.force = TRUE
          ,omit.coef = "(Intercept)|State|factor|^election_year_dm$|i_lag"
          ,custom.coef.names = c("Natural Resources Revenue t"
                                 ,"Natural Resources Revenue t x Election Year Dummy")
          ,custom.header = list("Savings" = 1:2,"Total Expenditure" = 3:4)
          ,custom.model.names = c("m2.a", "m2.b", "m3.a","m3.b")
          ,custom.gof.rows = list(" " = c("AR(1)", "AR(1)","AR(1)","AR(1)")
                                  ,"State and Year FE" = c("Yes", "Yes","Yes","Yes")
                                  ,"Standard Errors" = c("OLS", "CR2","OLS","CR2")))



m4a <- lm(savings_i ~ resrev_i*election_year_dm*BSF_implemented
          + savings_i_lag
          + factor(year)
          + factor(State)
          ,data = df_james_subset)
m4b <- lm_robust(savings_i ~ resrev_i*election_year_dm*BSF_implemented 
                 + savings_i_lag
                 + factor(year)
                 ,fixed_effects = ~ factor(State)
                 ,clusters = State
                 ,data = df_james_subset
                 ,se_type = "CR2")

m5a <- lm(totexp_i ~ resrev_i*election_year_dm*BSF_implemented
          + totexp_i_lag
          + factor(year)
          + factor(State)
          ,data = df_james_subset)
m5b <- lm_robust(totexp_i ~ resrev_i*election_year_dm*BSF_implemented 
                 + totexp_i_lag
                 + factor(year)
                 ,fixed_effects = ~ factor(State)
                 ,clusters = State
                 ,data = df_james_subset
                 ,se_type = "CR2")

screenreg(list(m4a,m4b,m5a,m5b)
          ,ci.force = TRUE
          ,omit.coef = "(Intercept)|State|factor|^election_year_dm$|i_lag"
          ,custom.coef.names = c("Natural Resources Revenue t"
                                 ,"BSF Implementation Dummy"
                                 ,"Natural Resources Revenue t x Election Year Dummy"
                                 ,"Natural Resources Revenue t x BSF Implementation Dummy"
                                 ,"Election Year Dummy x BSF Implementation Dummy"
                                 ,"Natural Resources Revenue t x Election Year Dummy x BSF Implementation Dummy")
          ,custom.header = list("Savings" = 1:2,"Total Expenditure" = 3:4)
          ,custom.model.names = c("m4.a", "m4.b","m5.a","m5.b")
          ,custom.gof.rows = list(" " = c("AR(1)", "AR(1)","AR(1)","AR(1)")
                                  ,"State and Year FE" = c("Yes", "Yes","Yes","Yes")
                                  ,"SE" = c("OLS", "CR2","OLS", "CR2")))


#Latex for models:
equation(m1a)
equation(m2a)
equation(m3a)

# 1) Marginal effects (slopes) of resrev_i in each election group
plot_slopes(m2a, variables = "resrev_i", by = "election_year_dm") +
  labs(
    title = "Marginal effect of resrev_i on savings_i",
    subtitle = "Separate slopes by election year dummy (0 = non-election, 1 = election)",
    x = "Resource revenues (resrev_i)",
    y = "Marginal effect on savings_i (dY/dX)",
    caption = "95% CIs shown. From slopes(): non-election dy/dx = 0.69 [0.34; 1.03], p<0.001;
election dy/dx = 0.37 [−0.05; 0.79], p=0.081."
  ) +
  theme_minimal()

# 2) Predicted values across resrev_i, colored by election group
plot_predictions(m2a, condition = c("resrev_i", "election_year_dm")) +
  labs(
    title = "Predicted savings_i across resrev_i",
    subtitle = "Lines and 95% CI ribbons by election year dummy (0 vs 1)",
    x = "Resource revenues (resrev_i)",
    y = "Predicted savings_i",
    caption = "From predictions(): mean predicted savings_i — non-election = 2.05 [1.65; 2.45],
election = 1.43 [0.88; 1.99]. Other covariates held at typical values."
  ) +
  theme_minimal()



# Slopes by election year and BSF status
plot_slopes(m4a, 
            variables = "resrev_i", 
            by = c("election_year_dm", "BSF_implemented"))

# Predictions across resrev_i, faceted by election year and BSF status
plot_predictions(m4a, 
                 condition = c("resrev_i", "BSF_implemented","election_year_dm"))

# 1) Marginal effects (slopes) of resrev_i by election year and BSF
plot_slopes(m4a, variables = "resrev_i", by = c("election_year_dm", "BSF_implemented")) +
  labs(
    title = "Marginal effect of resrev_i on savings_i",
    subtitle = "Separate slopes by election year (0/1) and BSF implementation (0/1)",
    x = "Resource revenues (resrev_i)",
    y = "Marginal effect on savings_i (dY/dX)",
    caption = "95% CIs shown. From slopes(): values differ by election timing and BSF status."
  ) +
  theme_minimal()

# 2) Predicted values over resrev_i, by election year and BSF implementation
plot_predictions(
  m4a,
  condition = c("resrev_i", "BSF_implemented", "election_year_dm")
) +
  labs(
    title = "Predicted savings_i over resource revenues (resrev_i)",
    subtitle = "Grouped by BSF implementation (0 = no, 1 = yes) and election year (0 = non-election, 1 = election)",
    x = "Resource revenues (resrev_i)",
    y = "Predicted savings_i",
    caption = "95% confidence bands. Estimated via plot_predictions(m4a) conditioning on resrev_i × BSF_implemented × election_year_dm; other covariates held at typical values (numeric = means; factors = modes)."
  ) +
  theme_minimal()



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


m5 <- lm_robust( savings_i ~ 
                   oil_deflated*state_election_year_dummy
                 + savings_i_lag
                 ,fixed_effects = ~ factor(State) + factor(year)
                 ,data = subset(test_elections,BSF_implemented == 1))

screenreg(m5,omit.coef = "factor")



