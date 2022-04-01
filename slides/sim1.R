
###################################################
f1<-function(tab,nitem,plot=TRUE,three=TRUE) {
    z<-do.call("rbind",tab)
    z<-data.frame(z)
    ##
    zz<-split(z,paste(z$n,z$sd,z$gm))
    zz<-lapply(zz,colMeans)
    z<-data.frame(do.call("rbind",zz))
    ##
    L<-split(z,z$n)
    z<-L[[1]] #just for configuring the plot
    plot(NULL,xlim=c(1,nrow(z)+2.3),ylim=c(-.002,.017),xlab="",xaxt="n",ylab="IMV")
    #for (i in seq(-.01,.025,by=.005)) abline(h=i,lwd=1,col='gray')
    abline(h=0,col='gray')
    axis(side=1,at=1:nrow(z),labels=rep("",nrow(z)))
    mtext(side=1,at=1:nrow(z),z$sd,line=.75,cex=1)
    mtext(side=1,line=.75,at=nrow(z)+1.3,expression(sigma),cex=1)
    mtext(side=1,at=1:nrow(z),z$gm,line=1.75,cex=1)
    mtext(side=1,line=1.75,at=nrow(z)+1.3,"C",cex=1)
    ##
    if (plot) {
        for (i in 1:length(L)) {
            z<-L[[i]]
            lines(1:nrow(z),z$om2,type='b',col='blue',lwd=2,pch=19,lty=i)
            if (three) lines(1:nrow(z),z$om3,type='b',col='red',lwd=2,pch=19,lty=i)
                                        #text(nrow(z),z$om2[nrow(z)],pos=4,paste("IMV(1,2),N=",substr(names(L)[i],1,1),"K",sep=""),col='blue',cex=1)
                                        #text(nrow(z),z$om3[nrow(z)],pos=4,paste("IMV(2,3),N=",substr(names(L)[i],1,1),"K",sep=""),col='red',cex=1)
            text(nrow(z),z$om2[nrow(z)],pos=4,paste("IMV(1,2)",sep=""),col='blue',cex=1)
            if (three) text(nrow(z),z$om3[nrow(z)],pos=4,paste("IMV(2,3)",sep=""),col='red',cex=1)
            z2<-z[z$gm==.3,]
            print(z2)
            print(summary(z2$om2/z2$om3))
        }
    }
    mtext(side=3,line=0,paste(nitem," items"))
    ##
}
##
f2<-function(tab,nitem) {
    z<-do.call("rbind",tab)
    z<-data.frame(z)
    z<-z[z$n==1000,]
    ##
    zz<-split(z,paste(z$n,z$sd,z$gm))
    zz<-lapply(zz,colMeans)
    z<-data.frame(do.call("rbind",zz))
    ##
    L<-split(z,z$n)
    z<-L[[1]] #just for configuring the plot
    plot(NULL,xlim=c(1,nrow(z)+2.3),ylim=c(-.05,.05),xlab="",xaxt="n",ylab="IMV")
    for (i in seq(-.025,.025,by=.025)) abline(h=i,lwd=1,col='gray')
    axis(side=1,at=1:nrow(z),labels=rep("",nrow(z)))
    mtext(side=1,at=1:nrow(z),z$sd,line=.75,cex=1)
    mtext(side=1,line=.75,at=nrow(z)+1.3,expression(sigma),cex=1)
    mtext(side=1,at=1:nrow(z),z$gm,line=1.75,cex=1)
    mtext(side=1,line=1.75,at=nrow(z)+1.3,"C",cex=1)
    ##
    for (i in 1:length(L)) {
        z<-L[[i]]
        lines(1:nrow(z),z$or.r,type='b',col='blue',lwd=2,pch=19,lty=1)
        lines(1:nrow(z),z$or.2,type='b',col='red',lwd=2,pch=19,lty=1)
        lines(1:nrow(z),z$or.3,type='b',col='black',lwd=2,pch=19,lty=1,cex=1)
        text(nrow(z),z$or.r[nrow(z)],pos=4,paste("Oracle 1"),col='blue',cex=1)
        text(nrow(z),z$or.2[nrow(z)],pos=4,paste("Oracle 2"),col='red',cex=1)
        text(nrow(z),z$or.3[nrow(z)],pos=4,paste("Oracle 3"),col='black',cex=1)
        ##
        lines(1:nrow(z),z$of.r,type='b',col='blue',lwd=2,pch=19,lty=3)
        lines(1:nrow(z),z$of.2,type='b',col='red',lwd=2,pch=19,lty=3)
        lines(1:nrow(z),z$of.3,type='b',col='black',lwd=2,pch=19,lty=3,cex=1)
        text(nrow(z),z$of.r[nrow(z)],pos=4,paste("Overfit 1"),col='blue',cex=1)
        text(nrow(z),z$of.2[nrow(z)],pos=4,paste("Overfit 2"),col='red',cex=1)
        text(nrow(z),z$of.3[nrow(z)],pos=4,paste("Overfit 3"),col='black',cex=1)
    }
    mtext(side=3,line=0,paste(nitem," items"))
    ##
}



load("misfit.Rdata")
numitems<-c(25,50,100)
#pdf("/home/bd/Dropbox/Apps/Overleaf/IMV_IRT/misfit.pdf",width=7.5,height=5)
i<-1

par(mfrow=c(2,1),mgp=c(2,1,0),mar=c(3,3,1.3,.1),oma=rep(0.3,4),bty='n')
f1(tab[[i]],nitem=numitems[i],plot=FALSE,three=FALSE)

par(mfrow=c(2,1),mgp=c(2,1,0),mar=c(3,3,1.3,.1),oma=rep(0.3,4),bty='n')
f1(tab[[i]],nitem=numitems[i],plot=TRUE,three=FALSE)

par(mfrow=c(2,1),mgp=c(2,1,0),mar=c(3,3,1.3,.1),oma=rep(0.3,4),bty='n')
f1(tab[[i]],nitem=numitems[i],plot=TRUE,three=TRUE)


par(mfrow=c(2,1),mgp=c(2,1,0),mar=c(3,3,1.3,.1),oma=rep(0.3,4),bty='n')
f1(tab[[i]],nitem=numitems[i],plot=TRUE,three=TRUE)
f2(tab[[i]],nitem=numitems[i])
