
###################################################
f1<-function(tab,nitem) {
    z<-do.call("rbind",tab)
    z<-data.frame(z)
    z<-z[z$sd==0,]
    ##
    zz<-split(z,paste(z$mu,z$sd,z$gm))
    zz<-lapply(zz,colMeans)
    z<-data.frame(do.call("rbind",zz))
    ##
    plot(NULL,xlim=c(1,nrow(z)+2.3),ylim=c(-.002,.015),xlab="",xaxt="n",ylab="IMV")
    #for (i in seq(-.01,.025,by=.005)) abline(h=i,lwd=1,col='gray')
    abline(h=0,col='gray')
    axis(side=1,at=1:nrow(z),labels=rep("",nrow(z)))
    mtext(side=1,at=1:nrow(z),z$sd,line=.75,cex=1)
    mtext(side=1,line=.75,at=nrow(z)+1.3,expression(sigma),cex=1)
    mtext(side=1,at=1:nrow(z),z$gm,line=1.75,cex=1)
    mtext(side=1,line=1.75,at=nrow(z)+1.3,"C",cex=1)
    mtext(side=1,at=1:nrow(z),z$mu,line=2.75,cex=1)
    mtext(side=1,line=2.75,at=nrow(z)+1.3,"mu",cex=1)
    mtext(side=1,at=1:nrow(z),round(z$mad,3),line=3.75,cex=1)
    mtext(side=1,line=3.75,at=nrow(z)+1.3,"MAD",cex=1)
    ##
    lines(1:nrow(z),z$om2,type='b',col='blue',lwd=2,pch=19)
    lines(1:nrow(z),z$om3,type='b',col='red',lwd=2,pch=19)
                                        #text(nrow(z),z$om2[nrow(z)],pos=4,paste("IMV(1,2),N=",substr(names(L)[i],1,1),"K",sep=""),col='blue',cex=.75)
                                        #text(nrow(z),z$om3[nrow(z)],pos=4,paste("IMV(2,3),N=",substr(names(L)[i],1,1),"K",sep=""),col='red',cex=.75)
    text(nrow(z),z$om2[nrow(z)],pos=4,paste("IMV(1,2)",sep=""),col='blue',cex=.75)
    text(nrow(z),z$om3[nrow(z)],pos=4,paste("IMV(2,3)",sep=""),col='red',cex=.75)
    #z2<-z[z$gm==.3,]
    #print(z2)
    #print(summary(z2$om2/z2$om3))
    mtext(side=3,line=0,paste(nitem," items"))
    ##
}



load("misfit2.Rdata")
numitems<-c(50)
par(mfrow=c(2,1),mgp=c(2,1,0),mar=c(4,3,1.3,.1),oma=rep(0.5,4),bty='n')
f1(tab,nitem=numitems)
