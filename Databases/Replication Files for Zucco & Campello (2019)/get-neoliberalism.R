### This compares GET with Orthodoxy and Statism Indices ####
### Generates stats that are mentioned in the text 
### As well as Tables A.5 and A.6 in the Supplemental Information
 
 
library(reshape)
 
# Read yearly rendering of GET (original from 2016 paper)
setwd("~/Dropbox/LatexFiles/Paper-JOP-Rejoinder/Replic")#set working directory to local folder
load('data_panel.RData')
print(comment(gdpla))
gdpla <- subset(gdpla,country!="lat"&year>=1980)


## Read HC's data 
load('~/Dropbox/LatexFiles/Paper-JOP-Rejoinder/Data/HC-figure_2.RData')
hc <- table
rm(table)
hc$country <- tolower(hc$country)

## First check correlations across countries is neoliberalism
hc2 <- subset(hc,select=c(country,year,dfa4,dfa2))
hc2$neolib <- hc2$dfa4-hc2$dfa2 
hc2 <- subset(hc2,select=c(country,year,neolib))
tmp <- melt(hc2, id.vars=c("country","year"), measure.vars="neolib")
hcc <- cast(tmp, formula = ... ~ country + variable)
	#Examine these results on the screen
	round(cor(hcc[,-1]),2) #correlation matrix...
	tmp.cor <- as.numeric(lower.tri(cor(hcc[,-1]))*cor(hcc[,-1])) #take 1 instance of each pair of items
	quantile(tmp.cor[-which(tmp.cor==0)],na.rm=T) #distribution of correlations between items

## Merge the two datasets #####
gdpla <- merge(gdpla,hc,by=c("country","year"),all.x=T)
gdpla$neolib <- gdpla$dfa4-gdpla$dfa2 #generate the neoliberalism index variable


## Analysis - Correlations between indices and GET, some of which are mentioned in paper ###
cor.test(gdpla$geti,gdpla$dfa4)
cor.test(gdpla$geti,gdpla$dfa2)
cor.test(gdpla$geti,gdpla$neolib)
cor.test(gdpla$geti[gdpla$lsce==T],gdpla$dfa4[gdpla$lsce==T])


## Analysis - Panel, in Supplemental Information ###
library(plm)
pd <-pdata.frame(gdpla,index=c("country","year"))

## Variances 
var(pd$dfa4,na.rm=T) #neoliberalism
var(pd$dfa2,na.rm=T) #orthodoxy
summary(pd$dfa2) #orthodoxy is mostly cross sectional
summary(pd$neolib) #neolib is mostly over time

## Effects of GET on neoliberalism

stack.all <- function(x,lab=NULL){#simple function to stack results from the regressions
	tmp<-data.frame(summary(x)$coef)[,-3]#eliminate t-values
	tmp <- stack(tmp)
	tmp$values <- round(tmp$values,4)
	tmp$ind <- gsub("Pr.*","P-value",tmp$ind)
	tmp$var <- rep(rownames(summary(x)$coef),3)
	if(is.null(lab)==F){names(tmp)[1] <- lab}
	return(tmp)
}

#The panel regressions
reg1p <- lm(neolib~geti,data=gdpla)  #pooling 
reg1 <- lm(neolib~geti*lsce,data=gdpla)  #pooling 

rfe1p <- plm(neolib~geti,model="within",data=pd) #within (country FE)
rfe1l <- plm(neolib~geti,model="within",data=subset(pd,lsce==T))
rfe1 <- plm(neolib~geti*lsce,model="within",data=pd)

rre1p <- plm(neolib~geti,model="random",data=pd) #country random effects
rre1l <- plm(neolib~geti,model="random",data=subset(pd,lsce==T))
rre1 <- plm(neolib~geti*lsce,model="random",data=pd)

#Table A.5 in the appendix
tab <- merge(merge(
merge(stack.all(reg1p,"pooled"),stack.all(rfe1p,"fe"),by=c('ind','var'),all=T),
merge(stack.all(rre1p,"re"),stack.all(reg1,"pooled"),by=c('ind','var'),all=T)
	,by=c('ind','var'),all=T),
merge(stack.all(rfe1,"fe"),stack.all(rre1,"re"),by=c('ind','var')),by=c('ind','var'),all=T)
			
tab <- tab[order(tab$var),]
rownames(tab)<-NULL
tab <- tab[,-1]
library(xtable)
xtab <- xtable(tab,digits=c(0,0,rep(3,6)))
caption(xtab)<-"Effects of GET on Neoliberalism Index"
label(xtab)<-"tab:getneolib"
print(xtab,include.rownames=F,caption.placement="top")

# For fixed effect "variable coefficients" model, use pvcm (not plm)
# Exactly the same as estimating one regression per country 
rcbc <-pvcm(neolib~geti
			,data = pd
			, model = "within"
			,effect="individual") 
						
# Look at coefficients
fevc <- rcbc$coef


# Look at standard errors
se <- rcbc$std.error 

# Assemble table
fevc$est <- "Estimate"
se$est <- "S.E."
fevctab <- rbind(fevc,se) 
fevctab <- fevctab[order(rownames(fevctab)),-3]
xfevctab <- xtable(fevctab,digits=c(0,3,3))
caption(xfevctab)<-"Country-by-Country Effects of GET on Neoliberalism Index"
label(xfevctab)<-"tab:getneolibbycountry"
print(xfevctab,caption.placement="top")

