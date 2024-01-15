load("complexity.Rdata")
tab<-do.call("rbind",tab)
tab<-data.frame(tab)
for (i in 2:ncol(tab)) tab[,i]<-as.numeric(tab[,i])
#i<-grep("roar_",tab[,1])
#z<-tab[-i,]
z<-tab
names(z)[1]<-'fn'
##
load("mirt.Rdata")
tab<-data.frame(x)
for (i in 2:ncol(tab)) tab[,i]<-as.numeric(tab[,i])
names(tab)[1:2]<-c('fn','mirt')
tab<-merge(z,tab,all.x=TRUE)
#save(tab,file="irw_results.Rdata")
##saved in results

#load("irw_results.Rdata")

##
tab0<-tab[,-1]
names(tab0)[1:3]<-c("people","items","prevalence")
tab0$prevalence<-abs(.5-tab0$prevalence)
tab00<-tab0[,c("people","items","prevalence","rasch","twopl","threepl","mirt")]
coors<-cor(tab00,use='p')
c0<-cor(tab00[-3,],use='p')
coors[2,]<-c0[2,]
coors[lower.tri(coors)]<-NA
library(xtable)
xtable(coors[,4:7])


tab00<-tab0[,c("rasch","twopl","threepl")] #all imv values
ran<-range(tab00)

#plot(log10(tab0$people),tab0$items,xlab="Log10(N respondents)",ylab="N items",pch=19,col='blue')

pdf("~/Dropbox/Apps/Overleaf/IMV_IRT/empiricalresults.pdf",width=7,height=3)
par(mfrow=c(1,3),mgp=c(2,1,0),mar=c(3,3,1,1))
##
plot(tab0$prevalence,tab0$rasch,xlab=bquote("|.5-"~bar(x)~"|"),ylab="IMV(CTT,1PL)",pch=19,col='blue',ylim=ran); abline(h=0)
legend("topright",bty='n',title="Avg",legend=round(mean(tab0$rasch),3))
plot(tab0$prevalence,tab0$twopl,xlab=bquote("|.5-"~bar(x)~"|"),ylab="IMV(1PL,2PL)",pch=19,col='blue',ylim=ran); abline(h=0)
legend("topright",bty='n',title="Avg",legend=round(mean(tab0$twopl),3))
plot(tab0$prevalence,tab0$threepl,xlab=bquote("|.5-"~bar(x)~"|"),ylab="IMV(2PL,3PL)",pch=19,col='blue',ylim=ran); abline(h=0)
legend("topright",bty='n',title="Avg",legend=signif(mean(tab0$threepl),1))
dev.off()

pdf("~/Dropbox/Apps/Overleaf/IMV_IRT/mirt_hist.pdf",width=3,height=3)
par(mgp=c(2,1,0),mar=c(3,3,1,1))
hist(tab0$mirt,col='blue',main='',sub='',xlab='IMV(2PL,2PL-2f)',ylab='')
dev.off()



## ##
## tab0<-tab[,-1]
## layout(matrix(c(1,1,1,2,2,2,3,3,4,4,5,5),ncol=2,nrow=6,byrow=FALSE))
## par(mgp=c(2,1,0),mar=c(3,3,1,1))
## plot(log10(tab0[,1]),tab0[,2],xlab="Log10(N respondents)",ylab="N items",pch=19,col='blue')
## ##
## plot(tab0[,3],tab0[,4],xlab="Average response",ylab="IMV(CTT,1PL)",pch=19,col='blue')
## ##
## d<-density(tab0[,4])
## ran<-range(tab0[,4])
## plot(d,sub='',main='',ylab='',yaxt='n',xlab="IMV(CTT,1PL)",xlim=ran)
## polygon(c(d$x,rev(d$x)),c(d$y,rep(0,length(d$x))),col='blue')
## abline(v=.1,lwd=3)
## ##
## d<-density(tab0[,6],na.rm=TRUE)
## plot(d,sub='',main='',ylab='',yaxt='n',xlab="IMV(1PL,2PL)",xlim=ran)
## polygon(c(d$x,rev(d$x)),c(d$y,rep(0,length(d$x))),col='red')
## abline(v=.01,lwd=3)
## ##
## d<-density(tab0[,7],na.rm=TRUE)
## plot(d,sub='',main='',ylab='',yaxt='n',xlab="IMV(2PL,3PL)",xlim=ran)
## polygon(c(d$x,rev(d$x)),c(d$y,rep(0,length(d$x))),col='green')
## abline(v=5e-4,lwd=3)

