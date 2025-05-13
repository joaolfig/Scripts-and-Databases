clear
set more off


cd " "

************
**** T1 ****
************

use "polar_main_dataset", clear

xtset state_ch year
sort state_ch year

local party_controls lagcompet laggovdem lagsplitleg lagdemcontrol 
local all_controls lagcompet laggovdem lagsplitleg lagdemcontrol laglogpop

drop if year < 1996

reghdfe pol_polar lagloggsppc, absorb(state_ch year) cluster(stateno)

reghdfe pol_polar lagloggsppc `party_controls', absorb(state_ch year) cluster(stateno)

reghdfe pol_polar lagloggsppc `all_controls', absorb(state_ch year) cluster(stateno)

************
**** T2 ****
************

use "polar_main_dataset", clear

xtset state_ch year
sort state_ch year

local party_controls lagcompet laggovdem lagsplitleg lagdemcontrol 
local all_controls lagcompet laggovdem lagsplitleg lagdemcontrol laglogpop

drop if year < 1996

reghdfe pol_polar lag2logoil_iv_two, absorb(state_ch year) cluster(stateno)

reghdfe pol_polar lag2logoil_iv_two `party_controls', absorb(state_ch year) cluster(stateno)

reghdfe pol_polar lag2logoil_iv_two `all_controls', absorb(state_ch year) cluster(stateno)

************
**** T3 ****  
************

use "polar_main_dataset", clear

xtset state_ch year
sort state_ch year	

local party_controls lagcompet laggovdem lagsplitleg lagdemcontrol 
local all_controls lagcompet laggovdem lagsplitleg lagdemcontrol laglogpop

drop if year < 1996
	
reghdfe loggsppc laglogoil_iv_two, absorb(stateno year) cluster(stateno)

reghdfe loggsppc laglogoil_iv_two `party_controls', absorb(stateno year) cluster(stateno)

reghdfe loggsppc laglogoil_iv_two `all_controls', absorb(stateno year) cluster(stateno)

************
**** T4 ****  
************

use "polar_main_dataset", clear

xtset state_ch year
sort state_ch year

tab year, gen(time)
tab state_ch, gen(stcham)

local party_controls lagcompet laggovdem lagsplitleg lagdemcontrol 
local all_controls lagcompet laggovdem lagsplitleg lagdemcontrol laglogpop

drop if year < 1996

ivreg2 pol_polar time* stcham* (lagloggsppc = lag2logoil_iv_two), ///
first cluster(stateno) partial(time* stcham*)

ivreg2 pol_polar `party_controls' time* stcham* (lagloggsppc = lag2logoil_iv_two), ///
first cluster(stateno) partial(time* stcham*)

ivreg2 pol_polar `all_controls' time* stcham* (lagloggsppc = lag2logoil_iv_two), ///
first cluster(stateno) partial(time* stcham*)

************
**** T5 ****  	
************

use "polar_main_dataset", clear
xtset state_ch year
sort state_ch year

tab year, gen(time)
tab state_ch, gen(stcham)

local party_controls lagcompet laggovdem lagsplitleg lagdemcontrol 
local all_controls lagcompet laggovdem lagsplitleg lagdemcontrol laglogpop

drop if year < 1996

ivreg2 dem_polar time* stcham* (lagloggsppc = lag2logoil_iv_two), ///
first cluster(stateno) partial(time* stcham*)

ivreg2 dem_polar time* stcham* `party_controls' (lagloggsppc = lag2logoil_iv_two), ///
first cluster(stateno) partial(time* stcham*)

ivreg2 dem_polar time* stcham* `all_controls' (lagloggsppc = lag2logoil_iv_two), ///
first cluster(stateno) partial(time* stcham*)

ivreg2 rep_polar time* stcham* (lagloggsppc = lag2logoil_iv_two), ///
first cluster(stateno) partial(time* stcham*)

ivreg2 rep_polar time* stcham* `party_controls' (lagloggsppc = lag2logoil_iv_two), ///
first cluster(stateno) partial(time* stcham*)

ivreg2 rep_polar time* stcham* `all_controls' (lagloggsppc = lag2logoil_iv_two), ///
first cluster(stateno) partial(time* stcham*)

*********************
**** T6: Panel A ****  	
*********************

use "polar_main_dataset", clear

xtset state_ch year
sort state_ch year

tab year, gen(time)
tab state_ch, gen(stcham)

merge 1:1 st chamber year using "92_14_contributions_bychamber"
tab _merge
drop if _merge == 2
drop _merge

local party_controls lagcompet laggovdem lagsplitleg lagdemcontrol 
local all_controls lagcompet laggovdem lagsplitleg lagdemcontrol laglogpop

drop if year < 1996	

gen logtotal_donations = log(1 + donations_cha_st)

ivreg2 logtotal_donations `all_controls' time* stcham* (lagloggsppc = lag2logoil_iv_two), ///
first cluster(stateno) partial(time* stcham*)

gen logrep_donations = log(1 + donations_cha_st_rep)

ivreg2 logrep_donations `all_controls' time* stcham* (lagloggsppc = lag2logoil_iv_two), ///
first cluster(stateno) partial(time* stcham*)

gen logdem_donations = log(1 + donations_cha_st_dem)

ivreg2 logdem_donations `all_controls' time* stcham* (lagloggsppc = lag2logoil_iv_two), ///
first cluster(stateno) partial(time* stcham*)

gen frac_rep_donations = donations_cha_st_rep/(donations_cha_st_rep + donations_cha_st_dem)

ivreg2 frac_rep_donations `all_controls' time* stcham* (lagloggsppc = lag2logoil_iv_two), ///
first cluster(stateno) partial(time* stcham*)

gen logind_donations = log(1 + donations_cha_st_ind)

ivreg2 logind_donations `all_controls' time* stcham* (lagloggsppc = lag2logoil_iv_two), ///
first cluster(stateno) partial(time* stcham*)

gen logorg_donations = log(1 + donations_cha_st_org)

ivreg2 logorg_donations `all_controls' time* stcham* (lagloggsppc = lag2logoil_iv_two), ///
first cluster(stateno) partial(time* stcham*)

*********************
**** T6: Panel B ****  
*********************	

use "polar_main_dataset", clear

xtset state_ch year
sort state_ch year

tab year, gen(time)
tab state_ch, gen(stcham)

merge 1:1 st chamber year using "92_14_contributions_bychamber"
tab _merge
drop if _merge == 2
drop _merge

local party_controls lagcompet laggovdem lagsplitleg lagdemcontrol 
local all_controls lagcompet laggovdem lagsplitleg lagdemcontrol laglogpop

drop if year < 1996	

gen logright_donations = log(1 + right_donations_cha_st)

ivreg2 logright_donations `all_controls' time* stcham* (lagloggsppc = lag2logoil_iv_two), ///
first cluster(stateno) partial(time* stcham*)

gen logmoderate_donations = log(1 + moderate_donations_cha_st)

ivreg2 logmoderate_donations `all_controls' time* stcham* (lagloggsppc = lag2logoil_iv_two), ///
first cluster(stateno) partial(time* stcham*)

gen logleft_donations = log(1 + left_donations_cha_st)

ivreg2 logleft_donations `all_controls' time* stcham* (lagloggsppc = lag2logoil_iv_two), ///
first cluster(stateno) partial(time* stcham*)

gen frac_right_donations = right_donations_cha_st/(right_donations_cha_st + left_donations_cha_st + moderate_donations_cha_st)

ivreg2 frac_right_donations `all_controls' time* stcham* (lagloggsppc = lag2logoil_iv_two), ///
first cluster(stateno) partial(time* stcham*)

gen logright_ind_donations = log(1 + right_ind_donations_cha_st)

ivreg2 logright_ind_donations `all_controls' time* stcham* (lagloggsppc = lag2logoil_iv_two), ///
first cluster(stateno) partial(time* stcham*)

gen logright_org_donations = log(1 + right_org_donations_cha_st)

ivreg2 logright_org_donations `all_controls' time* stcham* (lagloggsppc = lag2logoil_iv_two), ///
first cluster(stateno) partial(time* stcham*)

