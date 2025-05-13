clear
set more off

cd " "

******************
**** Figure 1 ****
******************

use "polar_main_dataset", clear

xtset state_ch year
sort state_ch year

gen sam_final = year >= 1997 
gen dpolar = d.pol_polar
egen ch_polar = sum(dpolar), by(state_ch sam_final) 

gen ini_polar = pol_polar if year==1996
bys state_ch (year): replace ini_polar = sum(ini_polar)

* ME, MT, NC, ND, OR, WV
gen ini_polar_97 = pol_polar if year == 1997
bys state_ch (year): replace ini_polar = sum(ini_polar_97) if year>=1996 & ini_polar==0

gen sam_final2 = year>=1996 & year<=2015
gen dgsppc = d.gsppc
egen ch_income = sum(dgsppc), by(state_ch sam_final2)

gen ini_gsppc = gsppc if year == 1995
bys state_ch (year): replace ini_gsppc = sum(ini_gsppc)
gen gsppc_chg = ch_income/ini_gsppc

replace gsppc_chg = gsppc_chg*100


keep if year == 2000

gen clock = 12
replace clock = 9 if statename == "North Dakota"

reg ch_polar gsppc_chg

twoway (lfitci ch_polar gsppc_chg) (scatter ch_polar gsppc_chg, mlabel(statename) mlabvposition(clock))


********************
**** Figure A.7 ****
********************

drop if statename == "North Dakota"

reg ch_polar gsppc_chg

twoway (lfitci ch_polar gsppc_chg) (scatter ch_polar gsppc_chg, mlabel(statename) mlabvposition(clock))


********************
**** Figure A.1 ****
********************

use "polar_main_dataset", clear

bysort year: egen Polarization = median(pol_polar) 
twoway line Polarization year if year>=1996, xlabel(1996(2)2016) xtitle(Year)
		
		
*****************************
**** Figures A.2 and A.3 ****
*****************************


use "shor_mccarty_93_16_individual", clear

gen ninesix      = 0
replace ninesix  = 1 if senate1996 == 1 | house1996 == 1

gen sixteen     = 0
replace sixteen = 1 if senate2016 == 1 | house2016 == 1


twoway (histogram np_score if party == "D" & ninesix == 1, color(red)) ///
       (histogram np_score if party == "R" & ninesix == 1, ///
	    fcolor(none) lcolor(black)), xlabel(-3(1)3) ylabel(0(0.4)1.2) xtitle (Ideology scores) legend(order( 1 "Democrats" 2 "Republicans"))

	
twoway (histogram np_score if party=="D" & sixteen==1, color(red)) ///
       (histogram np_score if party=="R" & sixteen==1, ///
	    fcolor(none) lcolor(black)), xlabel(-3(1)3) ylabel(0(0.4)1.2) xtitle (Ideology scores) legend(order( 1 "Democrats" 2 "Republicans"))

*****************************
**** Figures A.4 and A.5 ****
*****************************

use "shor_mccarty_1993_2016", clear

gen cham_dem = .
replace cham_dem = hou_dem if chamber == 9
replace cham_dem = sen_dem if chamber == 8

gen cham_rep = .
replace cham_rep = hou_rep if chamber == 9
replace cham_rep = sen_rep if chamber == 8

bysort year: egen med_rep = median(cham_rep) 
bysort year: egen med_dem = median(cham_dem) 

twoway line med_rep year if year >= 1996, xlabel(1996(2)2016) xtitle(Year)

twoway line med_dem year if year >= 1996, xlabel(1996(2)2016) xtitle(Year)


********************
**** Figure A.6 ****
********************

use "polar_main_dataset", clear

hist pol_polar if year >= 1996, xtitle (Polarization)

********************
**** Figure A.8 ****
********************

use "polar_main_dataset", clear

keep if stateno == 1 & chamber == 8

keep if year > 1993
keep if year < 2016

twoway line oilprice year, connect(L)

*********************
**** Figure A.10 ****
*********************

use "polar_main_dataset", clear

drop if year < 1995

keep statename stateno gsppc year chamber

keep if chamber == 8

replace gsppc = gsppc/1000

twoway line gsppc year, by(statename) connect(L)


