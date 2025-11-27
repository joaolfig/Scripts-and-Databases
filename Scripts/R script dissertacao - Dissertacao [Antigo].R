suppressPackageStartupMessages({
  library(stringr)
  library(dplyr)
  library(estimatr)
  library(texreg)
  library(stargazer)
  library(marginaleffects)
  library(flextable)
  library(readxl)
  library(regclass)
  library(gdata)
})

setwd("C:/Users/Joao arthur/OneDrive - Fundacao Getulio Vargas - FGV/Dissertação/Scripts-and-Databases")

rm(list=ls())

############### Load Data #######################################
source('Scripts/R script dissertacao - Electoral Data Gubernatorial.R')
source('Scripts/R script dissertacao - Electoral Data Presidential.R')
source('Scripts/R script dissertacao - Oil prices.R')
source('Scripts/R script dissertacao - BSF Data.R')

fiscal_data <- read_csv("Databases/Replication Files for James (2015)/57.to.2008.raw.data.Aug4.2014.csv")

rm(list=setdiff(ls(), c("vote_state",'president_incumbent_ts'
                        ,"fiscal_data"
                        ,"oil_deflated"
                        ,"BSF_rules",'BSF_dataset')))


############### Treatment Oil Prices ###########################

oil_deflated <- oil_deflated %>%
  mutate(
    oil_deflated_std = (oil_deflated - mean(oil_deflated, na.rm = TRUE)) / sd(oil_deflated, na.rm = TRUE)
    ,oil_deflated_std = round(oil_deflated_std,2)
  )

############### Treatment Fiscal Data ###########################
colnames(fiscal_data)[5] <- "State"
colnames(fiscal_data)[20] <- "resrev"

fiscal_data <- fiscal_data[,c(2,5,16,20:23,25:26)]

fiscal_data <- fiscal_data %>%
  mutate(res_share = resrev / (resrev + `non res rev`)
         ,savings_rate = savings /  (resrev + `non res rev`)) %>%
  left_join(oil_deflated[,c('year','deflator_multiplier')], by = 'year') %>%
  mutate(resrev_i = resrev * deflator_multiplier
         ,nonresrev_i = `non res rev` * deflator_multiplier
         ,savings_i = savings * deflator_multiplier
         ,totexp_i = `tot exp` * deflator_multiplier
         ,toteduexp_i = `tot edu exp` * deflator_multiplier) %>%
  mutate(resrev_i_lag = dplyr::lag(resrev_i)
         ,d_resrev_i = resrev_i - resrev_i_lag
         ,nonresrev_i_lag = dplyr::lag(nonresrev_i)
         ,savings_i_lag = dplyr::lag(savings_i)
         ,totexp_i_lag = dplyr::lag(totexp_i)
         ,toteduexp_i_lag = dplyr::lag(toteduexp_i)
         ,res_share_lag = dplyr::lag(res_share)
         ,savings_rate_lag = dplyr::lag(savings_rate))


# Reduce the scale of numerical variables that I use
fiscal_data <- fiscal_data %>%
  mutate(resrev_i = resrev_i / 1000000
         ,resrev_i_lag = resrev_i_lag / 1000000
         ,savings_i = savings_i / 1000000
         ,savings_i_lag = savings_i_lag / 1000000
         ,totexp_i = totexp_i / 1000000
         ,totexp_i_lag = totexp_i_lag / 1000000)


############### Treatment Vote State Data #######################

vote_state <- vote_state[,c('state','year','incumbent_party','winner_party'
                            #,'dem_pct_votes_2pty','rep_pct_votes_2pty'
                            #,'dem_pct_2pty_change','rep_pct_2pty_change'
                            #,'incumbent_pct_2pty','incumbent_pct_2pty_change'
                            ,'reelection_party','reelection_candidate'
                            ,'incumbent_running','challenger_running')]

vote_state$gubernatorial_election_year_dm <- 1

vote_state$year <- as.numeric(vote_state$year)

vote_state <- vote_state %>%
  filter(year >= 1948 & year <= 2017) 

############### Treatment President Incumbent TS #######################

president_incumbent_ts$presidential_election_year_dm <- 
  president_incumbent_ts$presidential_election_dm

############### Oil info and Subject selection ##################

# Table 1 - Oil dependence
table1 <- fiscal_data %>%
  group_by(State) %>%
  summarize(mean_res_share = mean(res_share, na.rm = TRUE)
            ,max_res_share = max(res_share, na.rm = TRUE)
            ,min_res_share = min(res_share, na.rm = TRUE)) %>%
  arrange(desc(mean_res_share))

#Select only the states with mean_res_share > 0.1
selected_states <- table1 %>%
  filter(mean_res_share > 0.1) %>%
  pull(State)

# Figure 1 - Oil dependence over time
ggplot(subset(fiscal_data, State %in% selected_states), aes(x = year, y = res_share, color = `State`)) +
  geom_line() +
  geom_point() +
  labs(title = "Share of Natural Resources Revenue in Total Revenue by State",
       x = "Year",
       y = "Share") +
  theme_minimal()

#Drop AK from the list
selected_states <- selected_states[selected_states != "AK"]

fiscal_data <- fiscal_data %>%
  filter(year >= 1957 & year <= 2007)



############### Unified government ##################

state_legis_elections <- read_excel("Databases/US Election Results State Legislative/State Partisan Balance Data 1937 - 2011/Partisan_Balance_For_Use2011_06_09b.xlsx")

state_legis_elections <- state_legis_elections[,c(1,3,42)]

us_2letters_code <- read.csv("Databases/US state 2 letter codes/2_letter_codes.csv", sep = ",", encoding = "UTF-8")

state_legis_elections <- state_legis_elections %>%
  left_join(us_2letters_code, by = c("state" = "State")) %>%
  dplyr::select(-state) %>%
  rename(state = State_Code)

state_legis_elections$unified_gov <- ifelse(state_legis_elections$divided_gov == 0, 1, 0)

################ BSF Rules table ################################

#Make BSF_implemented for MT be 2017 in BSF_rules and BSF_dataset
flextable(BSF_rules %>%
            filter(state %in% selected_states))

############### Merge Column and create variables ###############
df_analysis <- fiscal_data %>%
  full_join(vote_state, by = c("State" = "state", "year" = "year")) %>%
  filter(State %in% selected_states) %>%
  left_join(president_incumbent_ts[,c('year','president_party','presidential_election_year_dm')], by = 'year') %>%
  left_join(oil_deflated[,c('year','oil_deflated','oil_deflated_std')], by = 'year') %>%
  left_join(BSF_dataset, by = c("State" = "state", "year" = "year")) %>%
  left_join(state_legis_elections, by = c("State" = "state", "year" = "year"))

df_analysis$midterm_punishment <- 
  ifelse(df_analysis$presidential_election_year_dm == 0 &
           df_analysis$incumbent_party == df_analysis$incumbent_party, 1, 0)

df_analysis$gov_presi_same_party <- 
  ifelse(df_analysis$incumbent_party == df_analysis$president_party, 1, -1)

# Set gubernatorial_election_year_dm to 0 where NA
df_analysis$gubernatorial_election_year_dm[is.na(df_analysis$gubernatorial_election_year_dm)] <- 0

df_analysis$period <- df_analysis$year - 1958

# Fill the incumbent_party upward
df_analysis <- df_analysis %>%
  arrange(State, year) %>%
  group_by(State) %>%
  tidyr::fill(incumbent_party, .direction = "updown") %>%
  ungroup()

# df_analysis <- df_analysis %>%
#   filter(year >= 1958 & year <= 2007)

############### Check state year for Electoral ##################

# Show me each state i have data for in each year
table2 <- subset(df_analysis,gubernatorial_election_year_dm==1) %>%
  dplyr::select(State, year) %>%
  distinct() %>%
  arrange(State, year) %>%
  group_by(year) %>%
  summarize(states = paste(State, collapse = ", ")) %>%
  arrange(year)

flextable(table2,cwidth = 2)

table3 <- subset(df_analysis,!is.na(resrev)) %>%
  dplyr::select(State, year) %>%
  distinct() %>%
  arrange(State, year) %>%
  group_by(year) %>%
  summarize(states = paste(State, collapse = ", ")) %>%
  arrange(year)

flextable(table3,cwidth = 2)


############### Plot of real oil prices and fiscal variables ###################

############### Main Analysis - Gubernatorial Elections ########################
m1a <- lm(reelection_party ~ oil_deflated_std*BSF_implemented
           + unified_gov
           + incumbent_party
           + incumbent_running
           + midterm_punishment
           + State 
          ,data = df_analysis)
m1b <- lm_robust(reelection_party ~ oil_deflated_std*BSF_implemented
                 + unified_gov
                 + incumbent_party
                 + incumbent_running
                 + midterm_punishment
                 , fixed_effects = State
                 #,clusters = State
                 ,data = df_analysis ,se_type="HC2")

m2a <- lm(incumbent_running ~ oil_deflated_std*BSF_implemented
          + unified_gov
          + incumbent_party
          + midterm_punishment
          + State 
          ,data = df_analysis)
m2b <- lm_robust(incumbent_running ~ oil_deflated_std*BSF_implemented
                 + unified_gov
                 + incumbent_party
                 + midterm_punishment
                 , fixed_effects = ~State
                 #, clusters = State
                 ,data = df_analysis ,se_type="HC2")

m3a <- lm(reelection_candidate ~ oil_deflated_std*BSF_implemented
          + unified_gov
          + incumbent_party
          + midterm_punishment
          + State 
          ,data = subset(df_analysis, incumbent_running == 1))
m3b <- lm_robust(reelection_candidate ~ oil_deflated_std*BSF_implemented
                 + unified_gov
                 + incumbent_party
                 + midterm_punishment
                 , fixed_effects = ~State
                 #, clusters = State
                 ,data = subset(df_analysis, incumbent_running == 1)
                 ,se_type="HC2")

screenreg(
  list(m1a, m1b, m2a, m2b, m3a, m3b),
  omit.coef = "(Intercept)|State",
  ci.force = TRUE,
  custom.header = list(
    "Party re-election" = 1:2,
    "Incumbent re-run" = 3:4,
    "Candidate re-election" = 5:6
  ),
  custom.model.names = c(
    "m1a","m1b","m2a","m2b","m3a","m3b"
  ),
  custom.coef.names = c(
    "Real oil prices (std.)"
    , "BSF"
    , "Unified government"
    , "Republican"
    , "Incumbent re-run"
    , "Midterm punishment"
    , "Oil prices (std.) x BSF"
  ),
  custom.gof.rows = list(
    "State FE" = c("Yes", "Yes", "Yes", "Yes", "Yes", "Yes"),
    #"State Linear Trends" = c("Year","Yes","Year","Yes","Year","Yes"),
    "SE" = c("OLS", "HC2", "OLS", "HC2", "OLS", "HC2")
  )
  
)  

# Check multicollinearity and number of observations/parameters 
length(m1a$coefficients) # número de parâmetros estimados
nobs(m1a)
VIF(m1a)
length(m2a$coefficients) # número de parâmetros estimados
nobs(m2a)
VIF(m2a)
length(m3a$coefficients) # número de parâmetros estimados
nobs(m3a)
VIF(m3a)

#maginal effect pairs

plot_slopes(m1a,
            variables = "oil_deflated_std",
            by = c("BSF_implemented"))
plot_predictions(m1a,
                 condition = c("oil_deflated_std"
                               ,"BSF_implemented"))
plot_slopes(m2a,
            variables = "oil_deflated_std",
            by = c("BSF_implemented"))
plot_predictions(m2a,
                 condition = c("oil_deflated_std"
                               ,"BSF_implemented"))

plot_slopes(m3a,
            variables = "oil_deflated_std",
            by = c("BSF_implemented"))
plot_predictions(m3a,
                 condition = c("oil_deflated_std"
                               ,"BSF_implemented"))


############### Secondary Analysis - Fiscal data ########################

#Oil on revenue and expenses
m4a <- lm(savings_i ~ oil_deflated_std*BSF_implemented
          + gubernatorial_election_year_dm
          + incumbent_party
          + unified_gov
          + savings_i_lag
          + factor(State)
          + factor(State):year
          ,data = df_analysis)
m4b <- lm_robust(savings_i ~ oil_deflated_std*BSF_implemented
                 + gubernatorial_election_year_dm
                 + incumbent_party
                 + unified_gov
                 + savings_i_lag 
                 + factor(State):year
                 ,fixed_effects = factor(State)
                 ,data = df_analysis
                 ,se_type = "HC2")

m5a <- lm(totexp_i ~ oil_deflated_std*BSF_implemented
          + gubernatorial_election_year_dm
          + incumbent_party
          + unified_gov
          + totexp_i_lag
          + factor(State)
          + factor(State):year
          ,data = df_analysis)
m5b <- lm_robust(totexp_i ~ oil_deflated_std*BSF_implemented
                 + gubernatorial_election_year_dm
                 + incumbent_party
                 + unified_gov
                 + savings_i_lag 
                 + factor(State):year
                 ,fixed_effects = factor(State)
                 ,data = df_analysis
                 ,se_type = "HC2")

screenreg(list(m4a,m4b,m5a,m5b),
          omit.coef = "(Intercept)|State|_lag",
          ci.force = TRUE,
          custom.header = list(
            "Real savings" = 1:2,
            "Real expenditures" = 3:4
          ),
          custom.model.names = c(
            "m4a","m4b","m5a","m5b"
          ),
          custom.coef.names = c(
            "Real oil prices (std.)"
            , "BSF"
            , "Gubernatorial election year"
            , "Republican"
            , "Unified government"
            , "Oil prices (std.) x BSF"),
          custom.gof.rows = list(
            "State FE" = c("Yes", "Yes", "Yes", "Yes"),
            "State Linear Trends" = c("Year","Yes","Year","Yes"),
            "SE" = c("OLS", "HC2", "OLS", "HC2")
          )
          
)  

# Check multicollinearity and number of observations/parameters 
length(m1a$coefficients) # número de parâmetros estimados
nobs(m4a)
VIF(m4a)
length(m2a$coefficients) # número de parâmetros estimados
nobs(m5a)
VIF(m5a)

#Plot chart together


# Slopes by election year and BSF status
plot_slopes(m4a,
            variables = "oil_deflated_std",
            by = c("BSF_implemented"))
plot_predictions(m4a,
                 condition = c("oil_deflated_std"
                               ,"BSF_implemented"))
plot_slopes(m5a,
            variables = "oil_deflated_std",
            by = c("BSF_implemented"))
plot_predictions(m5a,
                 condition = c("oil_deflated_std"
                               ,"BSF_implemented"))




