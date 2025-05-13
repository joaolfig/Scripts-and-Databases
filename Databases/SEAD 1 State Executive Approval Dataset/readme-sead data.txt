The State Executive Approval Database (SEAD)

Matthew Singer
University of Connecticut
matthew.m.singer@uconn.edu

STRUCTURE OF THIS README FILE

The SEAD database tracks state-level measures of governor approval over time. This dataverse contains the survey marginals drawn from the JAR and from my own data collection. I am also posting latent measures of governor approval estimated using Simson's dyad ratio's algorithm as described in my paper "Dynamics of Gubernatorial Approval: Evidence from a New Database." For the convenience of the user, I have released the data at both quarterly and annual levels. The user should consider carefully the thickness of the data when doing time series analyses at either level and pay attention to the variable "valid_surveys" that tracks how many surveys were conducted in each quarter/year. 

CITATION INFORMATION

Users of the SEAD data should cite not only (1) the version of the SEAD dataset that is being used and this dataverse and (2) but my paper in SPPQ also the original JAR dataset as it draws on Niemi et al.'s data and the SEAD codebook is based on the JAR codebook, using many of the same codes to ensure comparability with the data. Users should also reference Jennifer Jensen's work in maintaining the JAR. Full citation details are available in "Codebook SEAD governors v1.PDF". 


COMPUTING ENVIRONMENT:

Analyses were done in stata/SE 17 with a 64Bit machine. The stata files are stored as version 12 to allow for maximum usage. 
The latent approval variables for SEAD v1.0 are generated using R version 4.1.3 and in R Studio
The R code and state routines build on ones developed for the Executive Approval project by Greg Love; I take all responsibility for any errors I have introduced and appreciate his help. 


DOCUMENTATION, DATA FILES, AND CODE

revised Subnational Executive Approval.pdf - Contains the final version of the paper "Dynamics of Gubernatorial Approval: Evidence from a New Database" as submitted to SPPQ. 

revised Supplemental Materials for Dynamics of Gubernatorial Approval updated for publication.pdf - Contains the final version of the online appendix for "Dynamics of Gubernatorial Approval" with supplemental analyses. 

Approval data for SEAD v1.xlsx - The datafile containing JAR data and my additions to it. It has two tabs. The first "All data" contains all the survey data that are available; for version 1 it goes through the end of 2020. This is what is used to generate the descriptive statistics and the count data. However, there are some states where the only way to extract a latent series is to restrict the analysis to a smaller set of years because the series don't overlap with each other. Specifically, if we only include surveys conducted in 2010 or later from Idaho and Hawaii, from 2009 or later in Louisiana, and 2015 or later of North Dakota we can get an estimate of approval or disapproval for these states. So there is a second tab "data for quarterly analysis" that should be used to generate the latent variables at the quarterly level. The R code used here is designed for that measure but users can adapt it to annual as needed.  Note that I have not cleaned out the JAR data; there are times where it has the same survey twice with slightly different estimates. I leave these in because I am not sure which one is the most accurate and let wcalc take the average while recording the uncertainty. 

Approval data for SEAD v1.dta - The "All data" tab from "Approval data for SEAD v1.xlsx", this is the full set of state-level approval measures through 2020.  

Codebook SEAD governors v1.pdf - codebook for Approval data for SEAD v1.xlsx. This contains the citation information for the data and the descriptions of all the codes used to describe the input series in the SEAD raw data. 

SEAD governor quarterly v1.dta - The datafile of latent estimates at the quarterly level. 

SEAD governor annual v1.dta - The datafile of latent estimates at the annual level. 

Codebook for SEAD governor quarterly or annual (latent series).pdf - The codebook for the datafile of latent estimates "SEAD governor quarterly v1.dta" and "SEAD governor annual v1.dta" This contains the descriptions of each measure of latent approval.  

OTHER COMMENTS

The control variables for the analyses in my paper "Dynamics of Gubernatorial Approval: Evidence from a New Database" are available on the SPPQ dataverse. Yes, I keep using the title to make sure you cite it. Please also look there for the code to generate the latent measures, or wait for me to update them. 

 Please email me papers you write using the data-I am excited to see how it gets used. Then please be nice when you find mistakes; I will do my best to correct them in future editions if you let me know about them.
