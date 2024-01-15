#just trying a much smaller number of respondents

##get parfun from misfit.R

library(irtimv)
set.seed(867.5309^2)
np<-c(250)#,5000)
sl.sd<-c(0,.25,.5,.75,1)
gm<-0 #gm<-c(0,.3)
z<-expand.grid(1:50,np,gm,sl.sd)
argvals<-list()
for (i in 1:nrow(z)) argvals[[i]]<-list(z[i,2],z[i,3],z[i,4])

library(parallel)
tab1<-mclapply(argvals,parfun,mc.cores=3,numitems=25)


f2<-function(tab,nitem) {
    z<-do.call("rbind",tab)
    z<-data.frame(z)
    ##
    zz<-split(z,paste(z$n,z$sd,z$gm))
    zz<-lapply(zz,colMeans)
    z<-data.frame(do.call("rbind",zz))
    ##
    L<-split(z,z$n)
    z<-L[[1]] #just for configuring the plot
    plot(NULL,xlim=c(1,nrow(z)+2.3),ylim=c(0,.04),xlab="",xaxt="n",ylab="IMV")
    for (i in seq(-.025,.025,by=.025)) abline(h=i,lwd=1,col='gray')
    axis(side=1,at=1:nrow(z),labels=rep("",nrow(z)))
    mtext(side=1,at=1:nrow(z),z$sd,line=.75,cex=.5)
    mtext(side=1,line=.75,at=nrow(z)+1.3,expression(sigma),cex=.5)
    mtext(side=1,at=1:nrow(z),z$gm,line=1.75,cex=.5)
    mtext(side=1,line=1.75,at=nrow(z)+1.3,"C",cex=.5)
    ##
    for (i in 1:length(L)) {
        z<-L[[i]]
        #points(1:nrow(z),z$or.r,col='blue',pch=19,cex=2)
        points(1:nrow(z),z$or.2,col='red',pch=19,cex=2)
        #points(1:nrow(z),z$or.3,col='black',pch=19,cex=1)
        #text(nrow(z),z$or.r[nrow(z)],pos=4,paste("Oracle 1"),col='blue',cex=.75)
        text(nrow(z),z$or.2[nrow(z)],pos=4,paste("Oracle 2"),col='red',cex=.75)
        #text(nrow(z),z$or.3[nrow(z)],pos=1,paste("Oracle 3"),col='black',cex=.75)
    }
    mtext(side=3,line=0,paste(nitem," items"))
    ##
}
numitems<-c(25)
par(mfrow=c(1,1),mgp=c(2,1,0),mar=c(3,3,1.3,.1),oma=rep(0,4),bty='n')
f2(tab1,nitem=numitems)
