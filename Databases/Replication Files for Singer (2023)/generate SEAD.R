# Updated 10-22-2020
# This code was developed with assistance from Greg Love and is modeled on his code. Because I am not particularly proficient in R, I have used his nomenclature, labeling states as countries

# here and throughout [directory]/refers to the directory where the data and folders for the replication analysis are downloaded to

# packages and functions 
install.packages("devtools")
devtools::install_github("patrick-eng/bootstrap.dyads")

library(bootstrap.dyads)
library(readxl)
library(haven)

# load functions for analysis
call.dr.code("[directory]/files and code for SEAD dataset v1/load_functions.R")

#-----------------------------------------------------------#
# load working data

setwd("[directory]/files and code for SEAD dataset v1/") 
dat <- read_excel("Approval data for SEAD v1.xlsx", sheet = "data for quarterly analysis")
# reformat dat like data used by Carlin et al. in EAD to allow me to reuse our code from that project
str(dat$Date)
dat$DATE <- as.Date(dat$Date)
str(dat$DATE)
dat$POSITIVE <- dat$Positive
dat$NEGATIVE <- dat$Neg
dat$NET <- dat$Net
dat$N <- dat$sample
dat$Country <- dat$state
#-----------------------------------------------------------#


####-----------------------------------------------------------#
# set 'country' names and create x variable for indexing
country_names <- dat$Country # here country == state
x<-country_names
#-----------------------------------------------------------#

####approval
setwd("[directory]/files and code for SEAD dataset v1/Quarters/logs") 

for (i in (x)){
  tryCatch({
    setwd("[directory]/files and code for SEAD dataset v1/Quarters/logs")
    d<-as.data.frame(subset(dat, Country==i)) # subset data by state names
    output<-extract(data=d, varname="VARIABLE", date="DATE", index='POSITIVE', 
                    ncases='N', unit="Q",smoothing=TRUE, log=TRUE,
                    filename=paste(i, "_q_app_smooth.txt", sep = "")) # run extract function on each state
    
    
    setwd("[directory]/files and code for SEAD dataset v1/Quarters/app_series") # FIXME
    
    display(output, paste(i, "_data_q_app_smooth.txt", sep = ""))
    
    assign(i,output)
  }, error=function(e){cat("ERROR :",conditionMessage(e), "\n")})
}

#approval not smoothing


for (i in (x)){
  tryCatch({
    setwd("[directory]/files and code for SEAD dataset v1/Quarters/logs") 
    
    d<-as.data.frame(subset(dat, Country==i)) # subset data by state names
    output<-extract(data=d, varname='VARIABLE', date='DATE', index='POSITIVE', 
                    ncases='N', unit='Q',smoothing=FALSE, log=FALSE)
    
    setwd("[directory]/files and code for SEAD dataset v1/Quarters/app_series_not_smoothed")
    
    display(output, paste(i, "_data_q_app_not_smooth.txt", sep = ""))
    
    assign(i,output)
  }, error=function(e){cat("ERROR :",conditionMessage(e), "\n")})
}

####Neg
setwd("[directory]/files and code for SEAD dataset v1/Quarters/logs") 

for (i in (x)){
  tryCatch({
    setwd("[directory]/files and code for SEAD dataset v1/Quarters/logs")
    d<-as.data.frame(subset(dat, Country==i)) # subset data by state names
    output<-extract(data=d, varname="VARIABLE", date="DATE", index='NEGATIVE', 
                    ncases='N', unit="Q",smoothing=TRUE, log=TRUE,
                    filename=paste(i, "_q_neg_smooth.txt", sep = "")) # run extract function on each state
    
    
    setwd("[directory]/files and code for SEAD dataset v1/Quarters/neg_series") # FIXME
    
    display(output, paste(i, "_data_q_neg_smooth.txt", sep = ""))
    
    assign(i,output)
  }, error=function(e){cat("ERROR :",conditionMessage(e), "\n")})
}


####Negative not smoothing


for (i in (x)){
  tryCatch({
    setwd("[directory]/files and code for SEAD dataset v1/Quarters/logs") 
    
    d<-as.data.frame(subset(dat, Country==i)) # subset data by state names
    output<-extract(data=d, varname='VARIABLE', date='DATE', index='NEGATIVE', 
                    ncases='N', unit='Q',smoothing=FALSE, log=FALSE)
    
    setwd("[directory]/files and code for SEAD dataset v1/Quarters/neg_series_not_smoothed")
    
    display(output, paste(i, "_data_q_neg_not_smooth.txt", sep = ""))
    
    assign(i,output)
  }, error=function(e){cat("ERROR :",conditionMessage(e), "\n")})
}


####App_AppDis

for (i in (x)){
  tryCatch({
    
    setwd("[directory]/files and code for SEAD dataset v1/Quarters/logs")  # FIXME
    
    d<-as.data.frame(subset(dat, Country==i)) # subset data by state names
    
    output<-extract(data=d, varname='VARIABLE', date='DATE', index='App_AppDis', 
                    ncases='N', unit='Q',smoothing=TRUE, log=TRUE, 
                    filename=paste(i, "_q_relative_smooth.txt", sep = ""))
    
    setwd("[directory]/files and code for SEAD dataset v1/Quarters/app_appdis")  # FIXME
    
    display(output, paste(i, "_data_q_App_AppDis_smooth.txt", sep = ""))
    
    assign(i,output)
  }, error=function(e){cat("ERROR :",conditionMessage(e), "\n")})
}


###App_AppDis no smoothing


for (i in (x)){
  tryCatch({
    setwd("[directory]/files and code for SEAD dataset v1/Quarters/logs")  # FIXME
    
    d<-as.data.frame(subset(dat, Country==i)) # subset data by state names
    
     output<-extract(data=d, varname='VARIABLE', date='DATE', index='App_AppDis',
                    ncases='N', unit='Q',smoothing=FALSE, log=FALSE)
    
    setwd("[directory]/files and code for SEAD dataset v1/Quarters/app_appdis_not_smoothed")  # FIXME
    
    display(output, paste(i, "_data_q_App_AppDis_not_smooth.txt", sep = ""))
    
    assign(i,output)
  }, error=function(e){cat("ERROR :",conditionMessage(e), "\n")})
}


