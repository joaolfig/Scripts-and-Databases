** Generate the models of governor approval in Table 3

** BE SURE TO REPLCE [directory] WITH YOUR WORKING DIRECTORY where you saved the replication files

** merge the control variables with the SEAD latent measures

clear

use "C:\Users\Joao arthur\OneDrive - Fundacao Getulio Vargas - FGV\Dissertação\Scripts-and-Databases\Databases\Replication Files for Singer (2023)\SEAD governor quarterly v1.dta"

sort state qtr

saveold "C:\Users\Joao arthur\OneDrive - Fundacao Getulio Vargas - FGV\Dissertação\Scripts-and-Databases\Databases\Replication Files for Singer (2023)\SEAD governor quarterly v1.dta", replace version(12)

clear

use "C:\Users\Joao arthur\OneDrive - Fundacao Getulio Vargas - FGV\Dissertação\Scripts-and-Databases\Databases\Replication Files for Singer (2023)\controls for table 3.dta"

sort state qtr

merge state qtr using "C:\Users\Joao arthur\OneDrive - Fundacao Getulio Vargas - FGV\Dissertação\Scripts-and-Databases\Databases\Replication Files for Singer (2023)\SEAD governor quarterly v1.dta"

tsset staten qtr

** run the models for the results in columns 1-3 of Table 3

xtgls Relative_Not_Smoothed  quarter1_first_valid quarter2_first_valid quarter3_first_valid quarter1_repeat_valid quarter2_repeat_valid quarter3_repeat_valid C.l(0/2).unemployment_state electionquarter  female not_elected C.l(0/2).governorresignedthatquarter C.l(0/2).governordiedthatquarter i.gov_party if valid_3qtr==1 & year<=2019 & Relative_Not_Smoothed~=. & Approval_Not_Smoothed~=. & Disapproval_Not_Smoothed~=., cor(psar1) force panels(het)

xtgls Approval_Not_Smoothed quarter1_first_valid quarter2_first_valid quarter3_first_valid quarter1_repeat_valid quarter2_repeat_valid quarter3_repeat_valid C.l(0/2).unemployment_state electionquarter  female not_elected C.l(0/2).governorresignedthatquarter C.l(0/2).governordiedthatquarter i.gov_party if valid_3qtr==1 & year<=2019 & Relative_Not_Smoothed~=. & Approval_Not_Smoothed~=. & Disapproval_Not_Smoothed~=., cor(psar1) force panels(het)

xtgls Disapproval_Not_Smoothed quarter1_first_valid quarter2_first_valid quarter3_first_valid quarter1_repeat_valid quarter2_repeat_valid quarter3_repeat_valid C.l(0/2).unemployment_state electionquarter  female not_elected C.l(0/2).governorresignedthatquarter C.l(0/2).governordiedthatquarter i.gov_party if valid_3qtr==1 & year<=2019 & Relative_Not_Smoothed~=. & Approval_Not_Smoothed~=. & Disapproval_Not_Smoothed~=., cor(psar1) force panels(het)

** reun the models for the results for appendix

* let the three dependent variables be modeled on all the cases that exist for it, regadless if they are missing for other measures of approval (Table A1)

xtgls Relative_Not_Smoothed  quarter1_first_valid quarter2_first_valid quarter3_first_valid quarter1_repeat_valid quarter2_repeat_valid quarter3_repeat_valid C.l(0/2).unemployment_state electionquarter  female not_elected C.l(0/2).governorresignedthatquarter C.l(0/2).governordiedthatquarter i.gov_party if valid_3qtr==1 & year<=2019 , cor(psar1) force panels(het)

xtgls Approval_Not_Smoothed quarter1_first_valid quarter2_first_valid quarter3_first_valid quarter1_repeat_valid quarter2_repeat_valid quarter3_repeat_valid C.l(0/2).unemployment_state electionquarter  female not_elected C.l(0/2).governorresignedthatquarter C.l(0/2).governordiedthatquarter i.gov_party if valid_3qtr==1 & year<=2019 , cor(psar1) force panels(het)

xtgls Disapproval_Not_Smoothed quarter1_first_valid quarter2_first_valid quarter3_first_valid quarter1_repeat_valid quarter2_repeat_valid quarter3_repeat_valid C.l(0/2).unemployment_state electionquarter  female not_elected C.l(0/2).governorresignedthatquarter C.l(0/2).governordiedthatquarter i.gov_party if valid_3qtr==1 & year<=2019 , cor(psar1) force panels(het)

* Run the models with no imputed approval data (Table A2)

xtgls Relative_Not_Smoothed  quarter1_first_valid quarter2_first_valid quarter3_first_valid quarter1_repeat_valid quarter2_repeat_valid quarter3_repeat_valid C.l(0/2).unemployment_state electionquarter  female not_elected C.l(0/2).governorresignedthatquarter C.l(0/2).governordiedthatquarter i.gov_party if valid_3qtr==1 & year<=2019 & valid_surveys>=1 & Relative_Not_Smoothed~=. & Approval_Not_Smoothed~=. & Disapproval_Not_Smoothed~=., cor(psar1) force panels(het)

xtgls Approval_Not_Smoothed quarter1_first_valid quarter2_first_valid quarter3_first_valid quarter1_repeat_valid quarter2_repeat_valid quarter3_repeat_valid C.l(0/2).unemployment_state electionquarter  female not_elected C.l(0/2).governorresignedthatquarter C.l(0/2).governordiedthatquarter i.gov_part if valid_3qtr==1 & year<=2019 & valid_surveys>=1 & Relative_Not_Smoothed~=. & Approval_Not_Smoothed~=. & Disapproval_Not_Smoothed~=., cor(psar1) force panels(het)

xtgls Disapproval_Not_Smoothed quarter1_first_valid quarter2_first_valid quarter3_first_valid quarter1_repeat_valid quarter2_repeat_valid quarter3_repeat_valid C.l(0/2).unemployment_state electionquarter  female not_elected C.l(0/2).governorresignedthatquarter C.l(0/2).governordiedthatquarter i.gov_party if valid_3qtr==1 & year<=2019 & valid_surveys>=1 & Relative_Not_Smoothed~=. & Approval_Not_Smoothed~=. & Disapproval_Not_Smoothed~=., cor(psar1) force panels(het)

