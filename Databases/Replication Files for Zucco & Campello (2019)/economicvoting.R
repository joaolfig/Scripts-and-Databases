## This code performes the analysis of economic voting in Section B of the Supplemental Information
## Generates Tables A3 and A4, as well as Figure A2

rm(list=ls(all=TRUE))
library(countrycode)
library(Hmisc)
library(foreign)
library(plm)
library(plyr)
library(xtable)


setwd('~/Dropbox/LatexFiles/Paper-JOP-Rejoinder/Replic') #


### Load the data, as described in the supplemental information, section B
load('data-economicvoting.RData')

## Read HC's data and merge
load('HC-figure_2.RData')
hc <- table
hc$ifs <- car::recode(hc$country,"'DR'='DOM';'GUA'='GTM';'HON'='HND';'URU'='URY'")
hc$neolib <- hc$dfa4-hc$dfa2
rm(table)
d <- merge(d,hc,by=c('year','ifs'),all.x=T,suffixes=c('','CH'))

#function to stack regression results
stack.plm <- function(x){
	if(is.element("plm",class(x)==F)){cat("Not a plm object");break}
	x<-summary(x)	
	core<-stack(data.frame(round(rbind(x$coef[,"Estimate"],
 								x$coef[,"Std. Error"],
 								x$coef[,"Pr(>|t|)"]),3)))
 	if(nrow(x$coef)==1){core$ind <- rownames(x$coef)}
 	ind<-c(as.character(core$ind),"N","Countries","Years")
	 values<-c(as.numeric(core$values),
 				length(x$residuals),
 				length(unique(attr(x$model,"index")$ifs)),
 				length(unique(attr(x$model,"index")$year))
 				)
 type <- c(rep(1:3,nrow(core)/3),4,4,4)
 core <- data.frame(ind,Mod.=values,type)
  return(core)}

### Set up the panel analys format
### We only kept one year per term, so this is, in fact a country/term panel
cat("Final data is"
	,nrow(d),"elections in"
	,length(unique(d$country)),"countries in"
	,length(unique(d$region)),"regions.\n")

cor.test(d$tag,d$tagpenn) #compare term average growth from PWT and WDI


#### THe panel data analysis ###
library(plm)
pd  <- pdata.frame(d,index=c("ifs","year"))
pdim(pd)


reg1dev   <- plm(reelect~tag
		,data = subset(pd,region=="Dev. Democracies")
		, model = "within",effect="individual")#wdi
reg1devstd   <- plm(reelect~tagsd
		,data = subset(pd,region=="Dev. Democracies")
		, model = "within",effect="individual")#wdi
reg1   <- plm(reelect~tag
		,data = subset(pd,region=="Latin America & Carribean")
		, model = "within",effect="individual")#wdi
reg1std   <- plm(reelect~tagsd
		,data = subset(pd,region=="Latin America & Carribean")
		, model = "within",effect="individual")#wdi	
reg1p   <- plm(reelect~tagpenn
		,data = subset(pd,region=="Latin America & Carribean")
		, model = "within",effect="individual")#wdi
reg1devy   <- plm(reelect~tlyg
		,data = subset(pd,region=="Dev. Democracies")
		, model = "within",effect="individual")#wdi
reg1y  <- plm(reelect~tlyg
		,data = subset(pd,region=="Latin America & Carribean")
		, model = "within",effect="individual")# last year
reg1r  <- plm(reelect~trnetag
			,data = subset(pd,region=="Latin America & Carribean")
			 ,model = "within",effect="individual")#regional net
reg2 <- plm(reelect~tag*neolib
			,data = subset(pd,region=="Latin America & Carribean")
				, model = "within",effect="individual")
reg2std <- plm(reelect~tagsd*neolib
			,data = subset(pd,region=="Latin America & Carribean")
				, model = "within",effect="individual")

reg2y <- plm(reelect~tlyg*neolib
			,data = subset(pd,region=="Latin America & Carribean")
				, model = "within",effect="individual")

### Table A3 in Supplemental Informax ###
library(xtable)
tab02 <- merge(merge(merge(stack.plm(reg1dev),stack.plm(reg1devy)
				,by=c("ind","type"),all=T,suffixes=c("Dev.Dem","Dev.Dem.Yr")),
			   merge(stack.plm(reg1),stack.plm(reg1y)
				,by=c("ind","type"),all=T,suffixes=c('Lac','LacYr'))
				,by=c("ind","type"),all=T),
			   merge(stack.plm(reg2),stack.plm(reg2y)
				,by=c("ind","type"),all=T,suffixes=c('NeoLib','NeolibYr')),by=c("ind","type"),all=T)
				
				
tab02o <- tab02[c(3:5,7:18,1,6,2),-2]
tab02x <- xtable(tab02o,label="tab:ecovoting",digits=3)
caption(tab02x) <- "Domestic Economic Growth and Elections"
print(tab02x,caption.placement="top",include.rownames =F)				
	

# Figure A2 in Supplemental Information 
# Graph of quantile regressions (alternative to linear interaction model)
pdna <- subset(pd,region=="Latin America & Carribean"&is.na(neolib)==F)
the.range <- quantile(pdna$neolib,na.rm=T,prob=c(0.05,0.95))
sims <- seq(the.range[1],
				  the.range[2],0.1)
pdna$neolib.q <- factor(paste0("neolib",cut(pdna$neolib
			, breaks=quantile(pdna$neolib, probs=seq(0,1, by=0.2),na.rm=T)
      		, include.lowest=TRUE,labels=F)))
# Estiamte model 2 by quantile (quintiles)
	regsq <- by(pdna, pdna[,"neolib.q"],
   					function(x) plm(reelect~tag, data = x, model = "within",effect="individual"))
	coefs <- sapply(regsq,coef)
	ses <- sapply(regsq,function(x){summary(x)$coef[2]})

 #Estiamte model with standardized growth by quantile	
	 regsqstd <- by(pdna, pdna[,"neolib.q"],
   				  function(x) plm(reelect~tagsd, data = x, model = "within",effect="individual"))
	  coefsstd <- sapply(regsqstd,coef)
	  sesstd <- sapply(regsqstd,function(x){summary(x)$coef[2]})


pdf(file="fig-neolibq.pdf"
	,width=6,height=6)
par(mar=c(4,4,1,1),mfrow=c(1,1),new=F)
plot(c(0.5,length(coefs)+0.5),range(coefs)
	,xlab="Neoliberalism (quintiles)"
	,ylab="Marginal Effects of (Term Average) Growth"
	,type="n",bty="n",ylim=range(c(coefs-1.64*ses,coefs+1.64*ses))
	,xaxt="n")		
axis(side=1
	,at=1:length(coefs)
	,labels=paste0("Q",1:length(coefs)))
segments(x0=1:length(coefs),x1=1:length(coefs)
 			,y0=coefs-1.64*ses
 			,y1=coefs+1.64*ses
 			,col=1)
points(1:length(coefs),coefs,pch=19)
polygon(x=c(1:length(coefs),length(coefs):1),
		y=c(coefs-1.64*ses,coefs[5:1]+1.64*ses[5:1]),
		border=NA,angle=45,density=35,col=gray(0.7))		
abline(h=0,col=gray(0.4),lty=3)
dev.off()


pdf(file="~/Dropbox/LatexFiles/Paper-JOP-Rejoinder/Figures/fig-neolibqstd.pdf"
	,width=6,height=6)
par(mar=c(4,4,1,1),mfrow=c(1,1),new=F)
#par(mar=c(4,4,1,1),fig=c(0,1,0,0.85),new=F) #to add the histogram or boxplot
plot(c(0.5,length(coefsstd)+0.5),range(coefsstd)
	,xlab="Neoliberalism (quintiles)"
	,ylab="Marginal Effects of Standardized (Average Term) Growth"
	,type="n",bty="n",ylim=range(c(coefsstd-1.64*sesstd,coefsstd+1.64*sesstd))
	,xaxt="n")		
axis(side=1
	,at=1:length(coefs)
	,labels=paste0("Q",1:length(coefsstd)))
segments(x0=1:length(coefsstd),x1=1:length(coefsstd)
 			,y0=coefsstd-1.64*sesstd
 			,y1=coefsstd+1.64*sesstd
 			,col=1)
points(1:length(coefsstd),coefsstd,pch=19)
polygon(x=c(1:length(coefsstd),length(coefsstd):1),
		y=c(coefsstd-1.64*sesstd,coefsstd[5:1]+1.64*sesstd[5:1]),
		border=NA,angle=45,density=35,col=gray(0.7))	
#lines(simcompsig,coefev,lty=1)		
abline(h=0,col=gray(0.4),lty=3)
dev.off()




### CLOGIT MODELS TABLE A4 IN SUPPLEMENTAL INFORMATION
stack.mfx <- function(x){
	if(class(x)!="logitmfx"){cat("Incorrect class\n");break}
	the.coefs <- x$mfxest[,-3]#drop zscors
	the.ind <- colnames(the.coefs)
	to.drop <- grep("country",rownames(the.coefs))
	varnames <- rownames(the.coefs)[-to.drop]
	out <- the.coefs[-to.drop,]
	out2 <- data.frame(stack(data.frame(out)))
	out2$var <- rep(varnames,3)
	if(nrow(out2)>3){out2 <- out2[order(out2$var),]}
	if(nrow(out2)==3){out2$ind <- the.ind}
	N <- data.frame(values=nrow(x$fit$data),ind="N",var="N")
	out2 <- rbind(out2,N)
	out2$ind <- as.character(out2$ind)
	out2$ind[grep("^dF",out2$ind)] <- "Mar.Eff"
	out2$ind[grep("^Std",out2$ind)] <- "S.E."
	out2$ind[grep("^P",out2$ind)] <- "pval"
	return(out2)}
	
dev <- subset(d,region=="Dev. Democracies")
dlac <- subset(d,region=="Latin America & Carribean")

library(mfx)
	clog1dev <-  mfx::logitmfx(reelect~tag + factor(country)
			, data=ddev, atmean=F)
    clog1devy <- mfx::logitmfx(reelect~tlyg + factor(country)
    		, data= ddev, atmean=F)
    clog1 <- mfx::logitmfx(reelect~tag + factor(country)
    		, data= dlac, atmean=F)
    clog1y <- mfx::logitmfx(reelect~tlyg + factor(country)
    		, data= dlac, atmean=F)
    clog2 <- mfx::logitmfx(reelect~tag*neolib + factor(country)
    		, data= dlac, atmean=F)
    clog2y <- mfx::logitmfx(reelect~tlyg*neolib + factor(country)
    		, data= dlac, atmean=F)

library(xtable)
tab02c <- merge(merge(merge(stack.mfx(clog1dev),stack.mfx(clog1devy)
				,by=c("var","ind"),all=T,suffixes=c("Dev.Dem","Dev.Dem.Yr")),
			   merge(stack.mfx(clog1),stack.mfx(clog1y)
				,by=c("var","ind"),all=T,suffixes=c('Lac','LacYr'))
				,by=c("var","ind"),all=T),
			   merge(stack.mfx(clog2),stack.mfx(clog2y)
				,by=c("var","ind"),all=T,suffixes=c('NeoLib','NeolibYr'))
				,by=c("var","ind"),all=T)
				
				
tab02co <- tab02c[c(5:7,11:13,2:4,8:10,14:16,1),-2]
tab02cx <- xtable(tab02co,label="tab:clogecovoting",digits=3)
caption(tab02cx) <- "Domestic Economic Growth and Elections (Conditional Logit Marginal Effects)"
print(tab02cx,caption.placement="top",include.rownames =F)				
	









