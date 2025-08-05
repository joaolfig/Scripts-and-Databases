** Generating the example data of the raw and latent series from New York as an example of the process that is presented in Figure 2

** BE SURE TO REPLCE [directory] WITH YOUR WORKING DIRECTORY where you saved the replication files

clear

* generate the quarterly data for New York create a date variable for the middle of the quarter

use "[directory]\SEAD governor quarterly v1.dta" 

keep if state=="New York"

gen series=36

gen temporary_month=2
replace temporary_month=5 if quarter==2
replace temporary_month=8 if quarter==3
replace temporary_month=11 if quarter==4
gen edate1=mdy(temporary_month, 15, year)
format edate1 %tdnn/dd/YY


rename Approval_Not_Smoothed positive

save "[directory]\New York quarterly data.dta", replace

** generate the raw data series for new york, label them for Figure 2, merge in the latent quarterly series

clear

use "[directory]\Approval data for SEAD v1.dta" 

keep if state=="New York"

* generate a series variable to tsset the data and to make selecting series for the graphs easy and also a date variable

gen edate1=mdy(month, day, year)
format edate1 %tdnn/dd/YY

gen series=.   
replace series= 1 if variable=="blum_ny"
replace series= 3 if variable=="ipsos"
replace series= 4 if variable=="liberty"
replace series= 5 if variable=="manhattan"
replace series= 6 if variable=="marist_approve"
replace series= 7 if variable=="marist_excellent"
replace series= 8 if variable=="marist_favorable"
replace series= 9 if variable=="masondixon_approval"
replace series= 10 if variable=="masondixon_excellent1"
replace series= 11 if variable=="masondixon_excellent2"
replace series= 12 if variable=="masondixon_excellent3"
replace series= 13 if variable=="morning_consult"
replace series= 15 if variable=="nypost"
replace series= 14 if variable=="nytimes"
replace series= 16 if variable=="publicpolicypolling"
replace series= 17 if variable=="quinnipiac_approval"
replace series= 18 if variable=="quinnipiac_favorable"
replace series= 19 if variable=="rasmussen_approve"
replace series= 20 if variable=="rasmussen_excellent"
replace series= 21 if variable=="rasmussen_favorable"
replace series= 22 if variable=="siena_approve"
replace series= 23 if variable=="siena_excellent1"
replace series= 24 if variable=="siena_excellent2"
replace series= 25 if variable=="siena_favorable"
replace series= 26 if variable=="strategicvision"
replace series= 27 if variable=="surveyusa"
replace series= 28 if variable=="wabc_app"
replace series= 29 if variable=="wabc_excellent"
replace series= 30 if variable=="wirthlin_ny"
replace series= 31 if variable=="zogby_approve"
replace series= 32 if variable=="zogby_excellent1"
replace series= 33 if variable=="zogby_favorable"
replace series= 34 if variable=="zogby_favorable2"
replace series= 35 if variable=="zogby_positive"

label define series 1 "Blum & Weprin Poll" 2 "Emerson College Polling" 3 "IPSOS" 4 "Liberty Opinion Research" 5 "Manhattan College Poll" 6 `""Marist Institute Poll-""Approval""' 7 `""Marist Institute Poll-""Excellence""' 8 `""Marist Institute Poll-""Favorability""' 9 `""Mason-Dixon/Political" "Media Research-""Approval""' 10 `""Mason-Dixon/Political" "Media Research-""Excellence v1""' 11 `""Mason-Dixon/Political" "Media Research-""Excellence v2""' 12 `""Mason-Dixon/Political" "Media Research-""Excellence v3""' 13 `""Morning Consult ""(Quarterly rolling sample)""' 14 `""New York Times/""Kaiser Family Foundation""' 15 "NY Post Poll" 16 `""Public Policy Polling-""Approval""' 17 `""Quinnipiac University Poll-""Approval""' 18 `""Quinnipiac University Poll-""Favorability""' 19 `""Rasmussen Reports-""Approval""' 20 `""Rasmussen Reports-""Excellence""' 21 "Rasmussen Reports-Favorability" 22 `""Siena Research Institute" "Poll-Approval""' 23 `""Siena Research Institute ""Poll-Excellence v1""' 24 `""Siena Research Institute" "Poll-Excellence v2""' 25 `""Siena Research Institute" "Poll-Favorability""' 26 "Strategic Vision" 27 "Survey USA" 28 `""WABC/NY Daily News Poll/" "Newsday Poll Approval""' 29 `""WABC/NY Daily News Poll/" "Newsday Poll Excellence""' 30 "Wirthlin Group Poll" 31 `""Zogby International Poll/""Zogby Group-Approval""' 32 `""Zogby International Poll/""Zogby Group-Excellence""' 33 `""Zogby International Poll/""Zogby Group-Favorable""' 34 `""Zogby International Poll/""Zogby Group-Favorable v2""' 35 `""Zogby International Poll/""Zogby Group-Positive""' 36 "LATENT SERIES", replace

label values series series

** Figure 2a: graphs of all series

set scheme s2mono

tsset series edate1

xtline positive if series<=35, recast(connected) ytitle(Approval Rating) ttitle(Date) byopts(legend(off) note("") graphregion(fcolor(white) ifcolor(white) lcolor(none) ilcolor(none))) tlabel(7306 "1980" 10959 "1990" 14611 "2000" 18264 "2010" 21916 "2020", valuelabel) ylabel(, nogrid) msize(vsmall)

graph save "Graph" "[directory]\Figure 2 Panel a.gph", replace

** Figure 2b: graph of three key series

twoway (line positive edate1 if series==7, lcolor(black) lpattern(solid)) (line positive edate1 if series==17, lcolor(black) lpattern(dash)) (line positive edate1 if series==25, lcolor(black) lpattern(vshortdash)), legend(on) legend(order(1 "Marist Institute Poll-Excellence" 2 "Quinnipiac Poll-Approval" 3 "Siena Research Institute Poll-Favorability") size(small)) ytitle(Approval Rating) xtitle(Date) graphregion(fcolor(white) ifcolor(white) lcolor(white) ilcolor(white)) xlabel(7306 "1980" 10959 "1990" 14611 "2000" 18264 "2010" 21916 "2020", valuelabel) ylabel(, nogrid)

graph save "Graph" "[directory]\Figure 2 Panel b.gph", replace

** Figure 2c merge in latent variable

append using "[directory]\New York quarterly data.dta"

twoway (scatter positive edate if series<=35, msize(vsmall) mcolor(gs12)) (line positive edate if series==36, lcolor(black) lpattern(solid)), ytitle(Approval Rating) xtitle(Date) graphregion(fcolor(white) ifcolor(white) lcolor(white) ilcolor(white)) xlabel(7306 "1980" 10959 "1990" 14611 "2000" 18264 "2010" 21916 "2020", valuelabel) ylabel(, nogrid) legend(order(1 "Observed Approval Rating" 2 "Latent Series") size(small))

graph save "Graph" "[directory]\Figure 2 Panel c.gph", replace

clear

