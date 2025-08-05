#### This code generates Figure 1 in the paper, which is a small tweak on 
#### a Figure in the original Campello and Zucco 2016 piece

#### Load the same file from Campello and Zucco 2016
load("data_fundamentals.RData")

### Take averages of the values on each dimension over the democratic years #####
s1 <- data.frame(as.table(by(d$ServiceExports[d$Year>=d$startyear]
							,d$country[d$Year>=d$startyear] ,mean,na.rm=T)))

s2 <- data.frame(as.table(by(d$CommDep[d$Year>=d$startyear]
							,d$country[d$Year>=d$startyear] ,mean,na.rm=T)))

names(s1) <- names(s2) <- c("Country","v")

### Impute Uruguay, as discussed in Campello and Zucco 2016
s1$v[s1$Country=="uru"] <- mean(s1$v[s1$Country=="arg"],s1$v[s1$Country=="bra"])

### COmbine and plot to generate the original figure in Campello and Zucco 2016
ss <- merge(s1,s2,by="Country",suffixes=c("IRexposure","CommDep"))
ss$Country <- toupper(ss$Country)

pdf(file="fig-fundamentals.pdf", width = 6, height = 6)
par(mfrow=c(1,1),mar=c(4.5,4.5,1,1))
plot(ss$vIRexposure,ss$vCommDep,type="n",
	xlab="",ylab="",bty="n"
	#xlim=c(0,.5),ylim=c(.1,.9)
	)
mtext(side=1,line=2,text="Debt Service/Exports")
mtext(side=1,line=3,text="(Exposure to International Interest Rates)",cex=.8)
mtext(side=2,line=3.3,text="Commodities/Total Exports")
mtext(side=2,line=2.3,text="(Exposure to Commodity Prices)",cex=.8)
polygon(x=c(.13,.6,.6,.2),
		y=c(.9,.1,.9,.9),
		col=gray(.85),border=NA)
text(ss$vIRexposure,ss$vCommDep,labels=ss$Country,cex=1)
#abline(.9,-1.6)
dev.off()


### Mow compute the 25th and 75th percentiles of the variables
s1m <- data.frame(as.table(by(d$ServiceExports[d$Year>=d$startyear]
			,d$country[d$Year>=d$startyear] ,mean,na.rm=T)))

s1.25 <- data.frame(as.table(by(d$ServiceExports[d$Year>=d$startyear]
			,d$country[d$Year>=d$startyear] ,quantile,prob=c(0.25),na.rm=T)))

s1.75 <- data.frame(as.table(by(d$ServiceExports[d$Year>=d$startyear]
			,d$country[d$Year>=d$startyear] ,quantile,prob=c(0.75),na.rm=T)))

s2m <- data.frame(as.table(by(d$CommDep[d$Year>=d$startyear]
			,d$country[d$Year>=d$startyear] ,mean,na.rm=T)))

s2.25 <- data.frame(as.table(by(d$CommDep[d$Year>=d$startyear]
			,d$country[d$Year>=d$startyear] ,quantile,prob=c(0.25),na.rm=T)))

s2.75 <- data.frame(as.table(by(d$CommDep[d$Year>=d$startyear]
			,d$country[d$Year>=d$startyear] ,quantile,prob=c(0.75),na.rm=T)))

names(s1m) <- names(s2m) <- c("Country","v")
ss <- merge(s1m,s2m,by="Country",suffixes=c("IRexposure","CommDep"))
ss <- merge(ss,merge(merge(s1.25,s1.75,by=1),merge(s2.25,s2.75,by=1),by=1),by=1)
ss$Country <- toupper(ss$Country)
names(ss)[4:7]<- c("IRexposure.25","IRexposure.75","CommDep.25","CommDep.75")


#Impute Uruguay's point estimate, as discussed in Campello and Zucco 2016
ss$vIRexposure[ss$Country=="URU"] <- mean(ss$vIRexposure[ss$Country=="ARG"]
								,ss$vIRexposure[ss$Country=="BRA"])#IMPUTE URY

#Account for data reporting issues with EPZs, as discussed in the paper
#Linearly impute the years for which EPZ's were not included in Manufacture
#Honduras: WTO prior to 2000 exclude EPZ;  impute from 1990
#El Salvador: WTO prior to 1990 exclude EPZ;  don't impute 
#Guatemala prior to 2002 exclude EPZ; impute from 1990
d$CommDep2 <- d$CommDep
to.impute <- d$CommDep2[which(d$country=="hon"&d$Year>=1990&d$Year<=2000)]
d$CommDep2[which(d$country=="hon"&d$Year>=1990&d$Year<=2000)] <- approx(c(1990,2000),c(to.impute[1],to.impute[length(to.impute)]),n=length(to.impute))$y


to.impute <- d$CommDep2[which(d$country=="gua"&d$Year>=1990&d$Year<=2002)]
d$CommDep2[which(d$country=="gua"&d$Year>=1990&d$Year<=2002)] <- approx(c(1990,2000),c(to.impute[1],to.impute[length(to.impute)]),n=length(to.impute))$y


s2alt <- data.frame(as.table(by(d$CommDep2[d$Year>=d$startyear]
							,d$country[d$Year>=d$startyear] ,mean,na.rm=T)))
names(s2alt)<-c("Country","CommDepImputed")
s2alt$Country <- toupper(s2alt$Country)
ss <- merge(ss,s2alt,by="Country",suffixes=c("","imputed"))

#New plot: Figure 1 in the paper
ss$lsce <- is.element(ss$Country,c("ARG","BRA","COL","BOL","CHI","PER","NIC","ECU","VEN","URU"))

pdf(file="fig-fundamentalsrejoinder.pdf", width = 6, height = 6)
par(mfrow=c(1,1),mar=c(4.5,4.5,1,1))
plot(ss$vIRexposure,ss$vCommDep,type="n",
	xlab="",ylab="",bty="n"
	,xlim=c(0,.5),ylim=c(.1,.9)
	)
mtext(side=1,line=2,text="Debt Service/Exports")
mtext(side=1,line=3,text="(Exposure to International Interest Rates)",cex=.8)
mtext(side=2,line=3.3,text="Commodities/Total Exports")
mtext(side=2,line=2.3,text="(Exposure to Commodity Prices)",cex=.8)
polygon(x=c(.13,.6,.6,.2),
		y=c(.9,.1,.9,.9),
		col=gray(.85),border=NA)
segments(x0=ss$IRexposure.25,x1=ss$IRexposure.75,
		y0=ss$vCommDep,y1=ss$vCommDep,col=gray(0.4),lty=ifelse(ss$lsce,1,2))
segments(x0=ss$vIRexposure,x1=ss$vIRexposure,
		y0=ss$CommDep.25,y1=ss$CommDep.75 ,col=gray(0.4),lty=ifelse(ss$lsce,1,2))
		points(ss$vIRexposure,ss$vCommDep,cex=1,pch=21,bg=ifelse(ss$lsce,1,"white"))
text(ss$vIRexposure,ss$vCommDep,labels=ss$Country,cex=0.6,adj=c(-.25,-.5))
points(ss$vIRexposure[ss$Country=="GUA"|ss$Country=="HON"]
	,ss$CommDepImputed[ss$Country=="GUA"|ss$Country=="HON"]
	,xlab="",ylab="",bty="n",pch=25,bg="white"
	,xlim=c(0,.5),ylim=c(.1,.9)
	)
legend(x="bottomright",legend=c("LSCE","Non-LSCE","Corrected")
		,pch=c(21,21,25)
		,pt.bg=c(1,"white","white"),bty="n")
dev.off()

