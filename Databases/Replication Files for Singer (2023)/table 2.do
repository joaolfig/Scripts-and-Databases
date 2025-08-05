** The analysis presented in Table 2

** BE SURE TO REPLCE [directory] WITH YOUR WORKING DIRECTORY where you saved the replication files

clear

** open the raw version of the SEAD dataset and generate a count variable for the number of survey recordings conducted in each quarter (requires generating a measure of the quarter)

use "[directory]\Approval data for SEAD v1.dta"

gen surveyn=1

gen quarter=1
replace quarter=2 if month>=4
replace quarter=3 if month>=7
replace quarter=4 if month>=10

collapse (sum) surveyn, by( state year quarter)

drop if year<=1979

sort state year quarter

**save count of surveys as a separate file and then merge it with the data on the election margin, election timing, population, and time periods from the replication materials. 

save "[directory]\count surveys from sead.dta", replace

clear

use "[directory]\descriptives count data 1980 to 2020.dta"

sort state year quarter

merge state year quarter using "[directory]\count surveys from sead.dta"

replace surveyn=0 if surveyn==.

drop _merge

** save data from combined files in a single file

save "[directory]\data for count models.dta", replace

tsset staten ticker

** Run the negative binomial models for Table 2

xtnbreg surveyn C.margin_last_gov_election##C.electionquarter lead_gov_election_quarter##C.margin_last_gov_election lead2_gov_election_quarter##C.margin_last_gov_election lead3_gov_election_quarter##C.margin_last_gov_election special_election lead_special lead2_special_election C.margin_last_pres_election##pres_election_quarter C.margin_last_pres_election##lead_pres_election_quarter C.margin_last_pres_election##lead2_pres_election_quarter C.margin_last_pres_election##lead3_pres_election_quarter  ln_population yrs1986_1990- yrs2016_2020 if year<=2020

xtnbreg surveyn C.margin_next_gov_election##C.electionquarter lead_gov_election_quarter##C.margin_next_gov_election lead2_gov_election_quarter##C.margin_next_gov_election lead3_gov_election_quarter##C.margin_next_gov_election special_election lead_special lead2_special_election  C.margin_next_pres_election##pres_election_quarter C.margin_next_pres_election##lead_pres_election_quarter C.margin_next_pres_election##lead2_pres_election_quarter C.margin_next_pres_election##lead3_pres_election_quarter ln_population yrs1986_1990- yrs2016_2020 if year<=2020

