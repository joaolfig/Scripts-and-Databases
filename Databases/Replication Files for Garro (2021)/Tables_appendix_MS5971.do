clear
set more off

cd "C:\Users\Joao arthur\OneDrive - Fundacao Getulio Vargas - FGV\Dissertação\Scripts\Replication Files for Garro (2021)"

***************
**** T A.3 ****
***************

use "polar_main_dataset", clear

xtset state_ch year
sort state_ch year

gen elecs_this_yr = 0

* House
replace elecs_this_yr = 1 if chamber == 9 & hs_elections_this_year == 1

* Senate
replace elecs_this_yr = 1 if chamber == 8 & sen_elections_this_year == 1

keep if elecs_this_yr == 1

* Controls
local party_controls lagcompet laggovdem lagsplitleg lagdemcontrol 
local all_controls lagcompet laggovdem lagsplitleg lagdemcontrol laglogpop

* Start in 1996
drop if year < 1996

reghdfe pol_polar lagloggsppc, absorb(state_ch year) cluster(stateno)

reghdfe pol_polar lagloggsppc `party_controls', absorb(state_ch year) cluster(stateno)

reghdfe pol_polar lagloggsppc `all_controls', absorb(state_ch year) cluster(stateno)


***************
**** T A.4 ****
***************

use "polar_main_dataset", clear

xtset state_ch year
sort state_ch year

gen constant = 1

* Regions

* Northeast: CT, ME, MA, NH, RI, VT, NJ, NY, PA - 9 states
gen northeast = 0
replace northeast = 1 if stateno == 7 | stateno == 19 | stateno == 21 | stateno == 29 | stateno == 39 ///
| stateno == 45 | stateno == 30 | stateno == 32 | stateno == 38 

* Midwest: IL, IN, MI, OH, WI, IA, KS, MN, MO, ND, SD, NE (omitted) 11 states + Nebraska
gen midwest = 0
replace midwest = 1 if stateno == 13 | stateno == 14 | stateno == 22 | stateno == 35 | stateno == 49 | ///
stateno == 15 | stateno == 16 | stateno == 23 | stateno == 25 | stateno == 34 | stateno == 41 

* South: DE, FL, GA, MD, NC, SC, VA, WV, AL, KY, MS, TN, AR, LA, OK, TX - 16 states
gen south = 0
replace south = 1 if stateno == 8 | stateno == 9 | stateno == 10 | stateno == 20 | stateno == 33 | ///
stateno == 40 | stateno == 46 | stateno == 48 | stateno == 1 | stateno == 17 | stateno == 24 | ///
stateno == 42 | stateno == 4 | stateno == 18 | stateno == 36 | stateno == 43  

* West: AZ, CO, ID, MT, NV, NM, UT, WY, AK, CA, HI, OR, WA - 13 states
gen west = 0
replace west = 1 if stateno == 3 | stateno == 6 | stateno == 12 | stateno == 26 | stateno == 28 | ///
stateno == 31 | stateno == 44 | stateno == 50 | stateno == 2 | stateno == 5 | stateno == 11 | ///
stateno == 37 | stateno == 47 

gen regions = 0
replace regions = 1 if northeast == 1
replace regions = 2 if midwest == 1
replace regions = 3 if south == 1
replace regions = 4 if west == 1

gen elecs_this_yr = 0
* House
replace elecs_this_yr = 1 if chamber == 9 & hs_elections_this_year == 1
* Senate
replace elecs_this_yr = 1 if chamber == 8 & sen_elections_this_year == 1

keep if elecs_this_yr == 1

sort state_ch year
bysort state_ch: gen yy = _n

xtset state_ch yy

gen dpolar = d.pol_polar
gen dlagloggsppc = d.lagloggsppc
gen dlagcompet = d.lagcompet
gen dlaggovdem = d.laggovdem
gen dlagsplitleg = d.lagsplitleg
gen dlagdemcontrol = d.lagdemcontrol
gen dlaglogpop = d.laglogpop

local d_all_controls dlagcompet dlaggovdem dlagsplitleg dlagdemcontrol dlaglogpop

drop if year < 1996

reghdfe dpolar dlagloggsppc `d_all_controls', absorb(constant) cluster(stateno)

reghdfe dpolar dlagloggsppc `d_all_controls', absorb(state_ch) cluster(stateno)

reghdfe dpolar dlagloggsppc `d_all_controls', absorb(regions) cluster(stateno)

reghdfe dpolar dlagloggsppc c.yy#i.stateno `d_all_controls', absorb(constant) cluster(stateno)

***************
**** T A.5 ****
***************

use "polar_main_dataset", clear

xtset state_ch year
sort state_ch year

* Alternative oil instruments
gen logoilp_bre           = log(brent)
gen logoil_iv_two_bre     = logoilp_bre*oil_gsp_twopart
gen laglogoil_iv_two_bre  = l.logoil_iv_two_bre
gen lag2logoil_iv_two_bre = l.laglogoil_iv_two_bre

gen logoilp_dub           = log(dubai)
gen logoil_iv_two_dub     = logoilp_dub*oil_gsp_twopart
gen laglogoil_iv_two_dub  = l.logoil_iv_two_dub
gen lag2logoil_iv_two_dub = l.laglogoil_iv_two_dub

gen logoilp_tex           = log(texas)
gen logoil_iv_two_tex     = logoilp_tex*oil_gsp_twopart
gen laglogoil_iv_two_tex  = l.logoil_iv_two_tex
gen lag2logoil_iv_two_tex = l.laglogoil_iv_two_tex


tab year, gen(time)
tab state_ch, gen(stcham)

local all_controls lagcompet laggovdem lagsplitleg lagdemcontrol laglogpop

drop if year < 1996

ivreg2 pol_polar time* stcham* (lagloggsppc = lag2logoil_iv_two_bre), ///
first cluster(stateno) partial(time* stcham*)

ivreg2 pol_polar `all_controls' time* stcham* (lagloggsppc = lag2logoil_iv_two_bre), ///
first cluster(stateno) partial(time* stcham*)

ivreg2 pol_polar time* stcham* (lagloggsppc = lag2logoil_iv_two_dub), ///
first cluster(stateno) partial(time* stcham*)

ivreg2 pol_polar `all_controls' time* stcham* (lagloggsppc = lag2logoil_iv_two_dub), ///
first cluster(stateno) partial(time* stcham*)

ivreg2 pol_polar time* stcham* (lagloggsppc = lag2logoil_iv_two_tex), ///
first cluster(stateno) partial(time* stcham*)

ivreg2 pol_polar `all_controls' time* stcham* (lagloggsppc = lag2logoil_iv_two_tex), ///
first cluster(stateno) partial(time* stcham*)

***************
**** T A.6 ****
***************

use "polar_main_dataset", clear

xtset state_ch year
sort state_ch year

tab year, gen(time)
tab state_ch, gen(stcham)

* Col 1
gen barrel_pc         = (oilprod*1000)/pop
by stateno first_half, sort: egen barrel_twopart = mean(barrel_pc) if year >= 1994
gen barrel_inst       = barrel_twopart*oilprice

xtset state_ch year
sort state_ch year

gen lag_barrel_inst   = l.barrel_inst
gen lag2_barrel_inst  = l.lag_barrel_inst

* Col 2
gen oil_gsp_past = oil_gspt + l.oil_gspt + l2.oil_gspt + l3.oil_gspt + l4.oil_gspt 
replace oil_gsp_past = oil_gspt + l.oil_gspt + l2.oil_gspt + l3.oil_gspt if year == 1996
replace oil_gsp_past = oil_gspt + l.oil_gspt + l2.oil_gspt if year == 1995
replace oil_gsp_past = oil_gspt + l.oil_gspt if year == 1994

gen logoil_iv_past     = logoilp*oil_gsp_past
gen laglogoil_iv_past  = l.logoil_iv_past
gen lag2logoil_iv_past = l.laglogoil_iv_past


* Col 3

gen oil_gsp_rolling = oil_gsp_twopart

sort state_ch year

replace oil_gsp_rolling = (f5.oil_gspt + f4.oil_gspt + f3.oil_gspt + f2.oil_gspt +f.oil_gspt + oil_gspt + ///
l.oil_gspt + l2.oil_gspt + l3.oil_gspt + l4.oil_gspt + l5.oil_gspt + l6.oil_gspt)/12 ///
if year == 2001 | year == 2002 | year == 2003 | year == 2004 | year == 2005
replace oil_gsp_rolling = (f5.oil_gspt + f4.oil_gspt + f3.oil_gspt + f2.oil_gspt +f.oil_gspt + oil_gspt + ///
l.oil_gspt + l2.oil_gspt + l3.oil_gspt + l4.oil_gspt + l5.oil_gspt)/11 if ///
year == 2006 | year == 2007 | year == 2008 | year == 2009 | year == 2010

gen logoil_iv_rolling = logoilp*oil_gsp_rolling
gen laglogoil_iv_rolling = l.logoil_iv_rolling
gen lag2logoil_iv_rolling = l.laglogoil_iv_rolling


* Col 4

sort state_ch year

gen first_third      = 0
replace first_third  = 1 if year >= 1994 & year <= 2001 
gen second_third     = 0
replace second_third = 1 if year >= 2002 & year <= 2008
gen third_third      = 0
replace third_third  = 1 if year >= 2009 & year <= 2016
gen thirds           = 0
replace thirds       = 1 if first_third  == 1
replace thirds       = 2 if second_third == 1
replace thirds       = 3 if third_third  == 1

sort state_ch year
by stateno thirds, sort: egen oil_gsp_threepart = mean(oil_gspt) if year >= 1994 
sort state_ch year
gen logoil_iv_three     = logoilp*oil_gsp_threepart
gen laglogoil_iv_three     = l.logoil_iv_three
gen lag2logoil_iv_three     = l.laglogoil_iv_three

* Col 5

gen reserves_pc = (reserves*1000)/pop 


by stateno first_half, sort: egen reserves_twopart = mean(reserves_pc) if year >= 1994
gen reserves_inst       = reserves_twopart*oilprice

xtset state_ch year
sort state_ch year

gen lag_reserves_inst   = l.reserves_inst
gen lag2_reserves_inst  = l.lag_reserves_inst

* Col 6

gen reserves_pc_96 = .
replace reserves_pc_96 = (reserves*1000)/pop if year == 1996

bysort state_ch: egen init_reserves_pc = sum(reserves_pc_96)

gen init_reserve_inst       = init_reserves_pc*oilprice

xtset state_ch year
sort state_ch year

gen lag_init_reserve_inst   = l.init_reserve_inst
gen lag2_init_reserve_inst  = l.lag_init_reserve_inst


local all_controls lagcompet laggovdem lagsplitleg lagdemcontrol laglogpop

drop if year < 1996

ivreg2 pol_polar `all_controls' time* stcham* (lagloggsppc = lag2_barrel_inst), first cluster(stateno) partial(time* stcham*)

ivreg2 pol_polar `all_controls' time* stcham* (lagloggsppc = lag2logoil_iv_past), first cluster(stateno) partial(time* stcham*)

ivreg2 pol_polar `all_controls' time* stcham* (lagloggsppc = lag2logoil_iv_rolling), first cluster(stateno) partial(time* stcham*)

ivreg2 pol_polar `all_controls' time* stcham* (lagloggsppc = lag2logoil_iv_three), first cluster(stateno) partial(time* stcham*)

ivreg2 pol_polar `all_controls' time* stcham* (lagloggsppc = lag2_reserves_inst), first cluster(stateno) partial(time* stcham*)

ivreg2 pol_polar `all_controls' time* stcham* (lagloggsppc = lag2_init_reserve_inst), first cluster(stateno) partial(time* stcham*)

***************
**** T A.7 ****
***************

use "polar_main_dataset", clear

xtset state_ch year
sort state_ch year

tab year, gen(time)
tab state_ch, gen(stcham)

local party_controls lagcompet laggovdem lagsplitleg lagdemcontrol 
local all_controls lagcompet laggovdem lagsplitleg lagdemcontrol laglogpop

drop if year < 1996

ivreg2 pol_polar time* stcham* (lagloggsppc = lag2logoil_iv_two) if chamber==9, first cluster(stateno) partial(time* stcham*)

ivreg2 pol_polar `party_controls' time* stcham* (lagloggsppc = lag2logoil_iv_two) if chamber==9, first cluster(stateno) partial(time* stcham*)

ivreg2 pol_polar `all_controls' time* stcham* (lagloggsppc = lag2logoil_iv_two) if chamber==9, first cluster(stateno) partial(time* stcham*)

ivreg2 pol_polar time* stcham* (lagloggsppc = lag2logoil_iv_two) if chamber==8, first cluster(stateno) partial(time* stcham*)

ivreg2 pol_polar `party_controls' time* stcham* (lagloggsppc = lag2logoil_iv_two) if chamber==8, first cluster(stateno) partial(time* stcham*)

ivreg2 pol_polar `all_controls' time* stcham* (lagloggsppc = lag2logoil_iv_two) if chamber==8, first cluster(stateno) partial(time* stcham*)

***************
**** T A.8 ****
***************

use "polar_main_dataset", clear

sort state_ch year
xtset state_ch year

tab year, gen(time)
tab state_ch, gen(stcham)

gen l_abs_compet    = l.abs_compet
gen l_abs_compet_sq = l_abs_compet^2

gen ddem_control    = d.dem_control

gen change_power     = 0

replace change_power = 1 if ddem_control == 1 | ddem_control == -1 
replace change_power = . if ddem_control == . 

drop if year < 1996

ivreg2 change_power l_abs_compet time* stcham* (lagloggsppc = lag2logoil_iv_two), ///
first cluster(stateno) partial(time* stcham*)

ivreg2 change_power l_abs_compet l_abs_compet_sq time* stcham* ///
(lagloggsppc = lag2logoil_iv_two), first cluster(stateno) partial(time* stcham*)

ivreg2 cha_score time* stcham* (lagloggsppc = lag2logoil_iv_two), ///
first cluster(stateno) partial(time* stcham*)


***************
**** T A.9 ****
***************

use "house_senate_entries_exits_96_16", clear


egen state_chamber_fe = group(stateno senator)
gen constant = 1

egen ent_ex_rep_dem_ch_y = group(state_chamber_fe exiters dem year)
sort ent_ex_rep_dem_ch_y

egen av_score = mean(np_score), by(ent_ex_rep_dem_ch_y)

egen numb_leg = count(constant), by(ent_ex_rep_dem_ch_y)

sort ent_ex_rep_dem_ch_y
drop if ent_ex_rep_dem_ch_y == ent_ex_rep_dem_ch_y[_n - 1]

drop id_name name st_id np_score abs_np

tab state_chamber_fe, gen(stcham)
	
* Panel A, Col 1

preserve

keep if dem == 1 & exiters == 0
xtset state_chamber_fe year
sort state_chamber_fe year

tab year, gen(yyy)

ivreg2 av_score yyy* stcham* (lagloggsppc = lag2logoil_iv_two) [aweight=numb_leg], first cluster(stateno) partial(stcham* yyy*)

restore


* Panel A, Col 2

preserve

keep if dem == 0 & exiters == 0
xtset state_chamber_fe year
sort state_chamber_fe year

tab year, gen(yyy)

ivreg2 av_score yyy* stcham* (lagloggsppc = lag2logoil_iv_two) [aweight=numb_leg], first cluster(stateno) partial(stcham* yyy*)

restore

* Panel B, Col 1
	
preserve
keep if dem == 1 & exiters == 1
xtset state_chamber_fe year
sort state_chamber_fe year

tab year, gen(yyy)

ivreg2 av_score yyy* stcham* (loggsppc = laglogoil_iv_two) [aweight=numb_leg], first cluster(stateno) partial(yyy* stcham*)

restore

* Panel B, Col 2

preserve

keep if dem == 0 & exiters == 1
xtset state_chamber_fe year
sort state_chamber_fe year

tab year, gen(yyy)

ivreg2 av_score yyy* stcham* (loggsppc = laglogoil_iv_two) [aweight=numb_leg], first cluster(stateno) partial(yyy* stcham*)

restore


* Cols 3 and 4

use "house_senate_entries_exits_96_16", clear


egen state_chamber_fe = group(stateno senator)
gen constant = 1


*preserve

egen ent_ex_ch_y = group(state_chamber_fe exiters year)
sort ent_ex_ch_y

egen av_score = mean(np_score), by(ent_ex_ch_y)

egen numb_lawmaker = count(constant), by(ent_ex_ch_y)

egen numb_rep = sum(rep), by(ent_ex_ch_y)
egen numb_dem = sum(dem), by(ent_ex_ch_y)
gen pct_rep = (numb_rep/numb_lawmaker)*100

sort ent_ex_ch_y
drop if ent_ex_ch_y == ent_ex_ch_y[_n - 1]

drop id_name name st_id np_score abs_np dem rep

preserve

keep if exiters == 0
xtset state_chamber_fe year
sort state_chamber_fe year

tab state_chamber_fe, gen(stcham)

tab year, gen(yyy)

* Panel A, Col 3
ivreg2 av_score yyy* stcham* (lagloggsppc = lag2logoil_iv_two) [aweight=numb_lawmaker], first cluster(stateno) partial(stcham* yyy*)

* Panel A, Col 4
ivreg2 pct_rep yyy* stcham* (lagloggsppc = lag2logoil_iv_two) [aweight=numb_lawmaker], first cluster(stateno) partial(stcham* yyy*)

restore


preserve

keep if exiters == 1
xtset state_chamber_fe year
sort state_chamber_fe year

tab state_chamber_fe, gen(stcham)

tab year, gen(yyy)

* Panel B, Col 3
ivreg2 av_score yyy* stcham* (loggsppc = laglogoil_iv_two) [aweight=numb_lawmaker], first cluster(stateno) partial(stcham* yyy*)

* Panel B, Col 4
ivreg2 pct_rep yyy* stcham* (loggsppc = laglogoil_iv_two) [aweight=numb_lawmaker], first cluster(stateno) partial(stcham* yyy*)

restore


***************
**** T A.10 ***
***************

use "polar_main_dataset", clear

sort stateno chamber year
merge 1:1 stateno chamber year using "st_legis_turnout_96_12"
tab _merge
drop if _merge == 2
drop _merge

xtset state_ch year
sort state_ch year

tab year, gen(tt)
tab state_ch, gen(stcham)

ivreg2 st_legis_turnout tt* stcham* (loggsppc = laglogoil_iv_two), ///
first cluster(stateno) partial(tt* stcham*)

ivreg2 st_legis_turnout logpop tt* stcham* (loggsppc = laglogoil_iv_two), ///
first cluster(stateno) partial(tt* stcham*)


***************
**** T A.11 ***
***************

use "polar_main_dataset", clear

xtset state_ch year
sort state_ch year

local party_controls lagcompet laggovdem lagsplitleg lagdemcontrol 
local all_controls lagcompet laggovdem lagsplitleg lagdemcontrol laglogpop

drop if year < 1996

gen decade     = 0
replace decade = 1 if year >= 2006

gen econ_dec = lagloggsppc*decade

reghdfe pol_polar lagloggsppc econ_dec, absorb(state_ch year) cluster(stateno)

reghdfe pol_polar lagloggsppc econ_dec `party_controls', absorb(state_ch year) cluster(stateno)

reghdfe pol_polar lagloggsppc econ_dec `all_controls', absorb(state_ch year) cluster(stateno)





