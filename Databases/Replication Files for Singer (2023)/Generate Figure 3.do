* Generating the example data of the latent series from four different states with different levels of polling frequency and volatility for figure 3

** BE SURE TO REPLCE [directory] WITH YOUR WORKING DIRECTORY where you saved the replication files

clear
use "[directory]\SEAD governor quarterly v1.dta" 

keep if staten==33 | staten==9 | staten==18 | staten==7

gen where=25
gen pipe = "|"
egen tag_valid = tag(valid_surveys)

label define staten 33 "North Carolina" 18 "Louisiana" 9 "Florida" 7 "Connecticut"
label values staten staten


preserve
drop if Relative_Not_Smoothed==.

xtline Relative_Not_Smoothed if year>=2000, xlabel(, angle(vertical)) xtitle("Time") ytitle("Relative Approval") ylabel(, nogrid) addplot((scatter where qtr if valid_surveys~=0 & year>=2000, plotr(m(b 4)) ms(none) mlabcolor(gs5) mlabel(pipe) mlabpos(6)) ) byopts(legend(off), , note("") graphregion(fcolor(white) ifcolor(white) lcolor(white) ilcolor(white)) ) 

restore

graph save "Graph" "[directory]\Figure 3.gph", replace

