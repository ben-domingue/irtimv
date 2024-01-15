parfun<-function(arg,numitems=50,b.mean=0) {
    np<-arg[[1]]
    gm<-arg[[2]]
    sl.sd<-arg[[3]]
    ##
    simfun<-function(np,a,b,c) {
        th<-rnorm(np)
        p<-outer(th,b,"-")
        for (i in 1:ncol(p)) p[,i]<-a[i]*p[,i]
        for (i in 1:ncol(p)) p[,i]<-c[i]+(1-c[i])*1/(1+exp(-p[,i]))
        resp<-p
        for (i in 1:ncol(resp)) resp[,i]<-rbinom(nrow(resp),1,p[,i])
        L<-list()
        for (i in 1:ncol(resp)) L[[i]]<-data.frame(item=paste0("item_",i),id=1:nrow(resp),resp=as.numeric(resp[,i]),truep=as.numeric(p[,i]))
        x<-data.frame(do.call("rbind",L))
        x
    }
    irt<-function(x,mod) { ###re beta prior on guessing param https://groups.google.com/g/mirt-package/c/8Usx53BoXyw
        ##
        resp<-makeresponse(x)
        library(mirt)
        index<-grep("id",names(resp))
        ni<-ncol(resp)-1
        if (mod=="Rasch") {
            m<-mirt(resp[,-index],1,"Rasch")
        }
        if (mod=="2PL") {
            s<-paste("F=1-",ni,"\nPRIOR = (1-",ni,", a1, lnorm, 0.0, 1.0)",
                     sep="") 
            model<-mirt.model(s)
            test<-try(m<-mirt(resp[,-index],model,itemtype=rep("2PL",ni),method="EM",technical=list(NCYCLES=10000)))
        }
        if (mod=="3PL") {
            s<-paste("F=1-",ni,"\nPRIOR = (1-",ni,", a1, lnorm, 0.0, 1.0),(1-",ni,", g, expbeta, 2, 17)",
                     sep="") 
            model<-mirt.model(s)
            test<-try(m<-mirt(resp[,-index],model,itemtype=rep(mod,ni),method="EM",technical=list(NCYCLES=10000)))
        }
        list(resp,m)
    }
    b<-rnorm(numitems,mean=b.mean)
    omega2<-omega3<-numeric()
    c<-runif(numitems,0,gm)
    a<-exp(rnorm(numitems,sd=sl.sd))
    ##
    x<-simfun(np,a=a,b=b,c=c)
    #index<-sample(1:nrow(x),nrow(x)/2,replace=FALSE)
    #x$gr<-ifelse(1:nrow(x) %in% index,1,2)
    #x$oos<-ifelse(x$gr==1,1,0)
    ##
    z<-list()
    for (me in c("Rasch","2PL","3PL")) {
        m<-irt(x,mod=me)
        if (me=="Rasch") {
            resp<-m[[1]]
            index<-grep("id",colnames(resp))
            id<-resp[,index]
            resp<-resp[,-index]
        }
        m<-m[[2]]
        co<-coef(m)
        nms<-names(co)
        co<-do.call("rbind",co[-length(co)])
        item<-data.frame(item=nms[-length(nms)],easy=co[,2],load=co[,1],gues=co[,3])
        th.est<-fscores(m,response.pattern=resp)
        index<-grep("^F",colnames(th.est))
        stud<-data.frame(id=id,th=th.est[,index])
        oos<-x
        y<-merge(oos,stud)
        y<-merge(y,item)
        ##
        kk<-y$load*y$th+y$easy
        kk<-exp(kk)
        y$p<-y$gues+(1-y$gues)*kk/(1+kk)
        hold<-y$resp
        truep<-y$truep
        y<-y[,c("item","id","p")]
        if (me=="Rasch") {
            y$resp<-hold
            y$truep<-truep
        }
        names(y)[3]<-me
        z[[me]]<-y                
    }
    x<-merge(z[[1]],z[[2]])
    x<-merge(x,z[[3]])
    ##
    overfit.rasch<-imv(x,p1="Rasch",p2="truep")
    overfit.2<-imv(x,p1="2PL",p2="truep")
    overfit.3<-imv(x,p1="3PL",p2="truep")
    x$p0<-mean(x$resp)
    tmp<-by(x$resp,x$item,mean)
    tmp<-data.frame(item=names(tmp),pctt=as.numeric(tmp))
    x<-merge(x,tmp)
    ##
    x$resp<-rbinom(nrow(x),1,x$truep)
    oracle.rasch<-imv(x,p1="Rasch",p2="truep")
    oracle.2<-imv(x,p1="2PL",p2="truep")
    oracle.3<-imv(x,p1="3PL",p2="truep")
    omega2<-imv(x,p1="Rasch",p2="2PL")
    omega3<-imv(x,p1="2PL",p2="3PL")
    ##
    om0<-imv(x,p1='p0',p2='pctt')
    omctt<-imv(x,p1='pctt',p2='Rasch')
    ##
    c(n=np,sd=sl.sd,gm=gm,om2=mean(omega2),om3=mean(omega3),
      or.r=oracle.rasch,or.2=oracle.2,or.3=oracle.3,
      of.r=overfit.rasch,of.2=overfit.2,of.3=overfit.3,
      om0=om0,omctt=omctt)
}


library(irtimv)
set.seed(867.5309^2)
np<-c(1000)#,5000)
sl.sd<-c(0,.25,.5)
gm<-c(0,.3)
z<-expand.grid(1:100,np,gm,sl.sd)
argvals<-list()
for (i in 1:nrow(z)) argvals[[i]]<-list(z[i,2],z[i,3],z[i,4])

library(parallel)
tab0<-mclapply(argvals,parfun,mc.cores=10,numitems=10)
tab1<-mclapply(argvals,parfun,mc.cores=10,numitems=25)
tab2<-mclapply(argvals,parfun,mc.cores=10,numitems=50)
tab3<-mclapply(argvals,parfun,mc.cores=10,numitems=200)
tab<-list(tab0,tab1,tab2,tab3)

save(tab,file="misfit.Rdata")


###################################################
## f1<-function(tab,nitem) { #continuous lines
##     z<-do.call("rbind",tab)
##     z<-data.frame(z)
##     ##
##     zz<-split(z,paste(z$n,z$sd,z$gm))
##     zz<-lapply(zz,colMeans)
##     z<-data.frame(do.call("rbind",zz))
##     ##
##     L<-split(z,z$n)
##     z<-L[[1]] #just for configuring the plot
##     plot(NULL,xlim=c(1,nrow(z)+2.3),ylim=c(-.002,.017),xlab="",xaxt="n",ylab="IMV")
##     #for (i in seq(-.01,.025,by=.005)) abline(h=i,lwd=1,col='gray')
##     abline(h=0,col='gray')
##     axis(side=1,at=1:nrow(z),labels=rep("",nrow(z)))
##     mtext(side=1,at=1:nrow(z),z$sd,line=.75,cex=.5)
##     mtext(side=1,line=.75,at=nrow(z)+1.3,expression(sigma),cex=.5)
##     mtext(side=1,at=1:nrow(z),z$gm,line=1.75,cex=.5)
##     mtext(side=1,line=1.75,at=nrow(z)+1.3,"C",cex=.5)
##     ##
##     for (i in 1:length(L)) {
##         z<-L[[i]]
##         lines(1:nrow(z),z$om2,type='b',col='blue',lwd=2,pch=19,lty=i)
##         lines(1:nrow(z),z$om3,type='b',col='red',lwd=2,pch=19,lty=i)
##         #text(nrow(z),z$om2[nrow(z)],pos=4,paste("IMV(1,2),N=",substr(names(L)[i],1,1),"K",sep=""),col='blue',cex=.75)
##         #text(nrow(z),z$om3[nrow(z)],pos=4,paste("IMV(2,3),N=",substr(names(L)[i],1,1),"K",sep=""),col='red',cex=.75)
##         text(nrow(z),z$om2[nrow(z)],pos=4,paste("IMV(1,2)",sep=""),col='blue',cex=.75)
##         text(nrow(z),z$om3[nrow(z)],pos=4,paste("IMV(2,3)",sep=""),col='red',cex=.75)
##         z2<-z[z$gm==.3,]
##         print(z2)
##         print(summary(z2$om2/z2$om3))
##     }
##     mtext(side=3,line=0,paste(nitem," items"))
##     ##
## }
## ##
## f2<-function(tab,nitem) {
##     z<-do.call("rbind",tab)
##     z<-data.frame(z)
##     z<-z[z$n==1000,]
##     ##
##     zz<-split(z,paste(z$n,z$sd,z$gm))
##     zz<-lapply(zz,colMeans)
##     z<-data.frame(do.call("rbind",zz))
##     ##
##     L<-split(z,z$n)
##     z<-L[[1]] #just for configuring the plot
##     plot(NULL,xlim=c(1,nrow(z)+2.3),ylim=c(-.05,.05),xlab="",xaxt="n",ylab="IMV")
##     for (i in seq(-.025,.025,by=.025)) abline(h=i,lwd=1,col='gray')
##     axis(side=1,at=1:nrow(z),labels=rep("",nrow(z)))
##     mtext(side=1,at=1:nrow(z),z$sd,line=.75,cex=.5)
##     mtext(side=1,line=.75,at=nrow(z)+1.3,expression(sigma),cex=.5)
##     mtext(side=1,at=1:nrow(z),z$gm,line=1.75,cex=.5)
##     mtext(side=1,line=1.75,at=nrow(z)+1.3,"C",cex=.5)
##     ##
##     for (i in 1:length(L)) {
##         z<-L[[i]]
##         lines(1:nrow(z),z$or.r,type='b',col='blue',lwd=2,pch=19,lty=1)
##         lines(1:nrow(z),z$or.2,type='b',col='red',lwd=2,pch=19,lty=1)
##         lines(1:nrow(z),z$or.3,type='b',col='black',lwd=2,pch=19,lty=1,cex=.5)
##         text(nrow(z),z$or.r[nrow(z)],pos=4,paste("Oracle 1"),col='blue',cex=.75)
##         text(nrow(z),z$or.2[nrow(z)],pos=4,paste("Oracle 2"),col='red',cex=.75)
##         text(nrow(z),z$or.3[nrow(z)],pos=4,paste("Oracle 3"),col='black',cex=.75)
##         ##
##         lines(1:nrow(z),z$of.r,type='b',col='blue',lwd=2,pch=19,lty=3)
##         lines(1:nrow(z),z$of.2,type='b',col='red',lwd=2,pch=19,lty=3)
##         lines(1:nrow(z),z$of.3,type='b',col='black',lwd=2,pch=19,lty=3,cex=.5)
##         text(nrow(z),z$of.r[nrow(z)],pos=4,paste("Overfit 1"),col='blue',cex=.75)
##         text(nrow(z),z$of.2[nrow(z)],pos=4,paste("Overfit 2"),col='red',cex=.75)
##         text(nrow(z),z$of.3[nrow(z)],pos=4,paste("Overfit 3"),col='black',cex=.75)
##     }
##     mtext(side=3,line=0,paste(nitem," items"))
##     ##
## }

f1<-function(tab,nitem) { #error bars
    z<-do.call("rbind",tab)
    z<-data.frame(z)
    ##
    zz<-split(z,paste(z$n,z$sd,z$gm))
    zz.ci<-lapply(zz,function(x) apply(x,2,quantile,c(.025,.975)))
    zz<-lapply(zz,colMeans)
    z<-data.frame(do.call("rbind",zz))
    ##
    L<-split(z,z$n)
    z<-L[[1]] #just for configuring the plot
    plot(NULL,xlim=c(1,nrow(z)+2.3),ylim=c(-.002,.017),xlab="",xaxt="n",ylab="IMV")
    #for (i in seq(-.01,.025,by=.005)) abline(h=i,lwd=1,col='gray')
    abline(h=0,col='gray')
    axis(side=1,at=1:nrow(z),labels=rep("",nrow(z)))
    mtext(side=1,at=1:nrow(z),z$sd,line=.75,cex=.5)
    mtext(side=1,line=.75,at=nrow(z)+1.3,expression(sigma),cex=.5)
    mtext(side=1,at=1:nrow(z),z$gm,line=1.75,cex=.5)
    mtext(side=1,line=1.75,at=nrow(z)+1.3,"C",cex=.5)
    ##
    for (i in 1:length(L)) {
        z<-L[[i]]
        points(1:nrow(z),z$om2,col='blue',pch=19)
        points(1:nrow(z),z$om3,col='red',pch=19)
        ##
        for (j in 1:length(zz.ci)) {
            yy<-as.data.frame(zz.ci[[j]])
            segments(j,yy$om2[1],j,yy$om2[2],col='blue')
            segments(j,yy$om3[1],j,yy$om3[2],col='red')
        }
        #text(nrow(z),z$om2[nrow(z)],pos=4,paste("IMV(1,2),N=",substr(names(L)[i],1,1),"K",sep=""),col='blue',cex=.75)
        #text(nrow(z),z$om3[nrow(z)],pos=4,paste("IMV(2,3),N=",substr(names(L)[i],1,1),"K",sep=""),col='red',cex=.75)
        text(nrow(z),z$om2[nrow(z)],pos=4,paste("IMV(1,2)",sep=""),col='blue',cex=.75)
        text(nrow(z),z$om3[nrow(z)],pos=4,paste("IMV(2,3)",sep=""),col='red',cex=.75)
        z2<-z[z$gm==.3,]
        print(z2)
        print(summary(z2$om2/z2$om3))
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
    plot(NULL,xlim=c(1,nrow(z)+2.3),ylim=c(-.05,.06),xlab="",xaxt="n",ylab="IMV")
    for (i in seq(-.025,.025,by=.025)) abline(h=i,lwd=1,col='gray')
    axis(side=1,at=1:nrow(z),labels=rep("",nrow(z)))
    mtext(side=1,at=1:nrow(z),z$sd,line=.75,cex=.5)
    mtext(side=1,line=.75,at=nrow(z)+1.3,expression(sigma),cex=.5)
    mtext(side=1,at=1:nrow(z),z$gm,line=1.75,cex=.5)
    mtext(side=1,line=1.75,at=nrow(z)+1.3,"C",cex=.5)
    ##
    for (i in 1:length(L)) {
        z<-L[[i]]
        points(1:nrow(z),z$or.r,col='blue',pch=19,cex=2)
        points(1:nrow(z),z$or.2,col='red',pch=19,cex=2)
        points(1:nrow(z),z$or.3,col='black',pch=19,cex=1)
        text(nrow(z),z$or.r[nrow(z)],pos=4,paste("Oracle 1"),col='blue',cex=.75)
        text(nrow(z),z$or.2[nrow(z)],pos=4,paste("Oracle 2"),col='red',cex=.75)
        text(nrow(z),z$or.3[nrow(z)],pos=1,paste("Oracle 3"),col='black',cex=.75)
        ##
        points(1:nrow(z),z$of.r,col='blue',pch=1,cex=2)
        points(1:nrow(z),z$of.2,col='red',pch=1,cex=2)
        points(1:nrow(z),z$of.3,col='black',pch=1,cex=1)
        text(nrow(z),z$of.r[nrow(z)],pos=4,paste("Overfit 1"),col='blue',cex=.75)
        text(nrow(z),z$of.2[nrow(z)],pos=4,paste("Overfit 2"),col='red',cex=.75)
        text(nrow(z),z$of.3[nrow(z)],pos=1,paste("Overfit 3"),col='black',cex=.75)
    }
    mtext(side=3,line=0,paste(nitem," items"))
    ##
}



pdf("~/Dropbox/Apps/Overleaf/IMV_IRT/misfitA2.pdf",width=7.5,height=2.5)
load("misfit.Rdata")
numitems<-c(10,25,50,200)
par(mfrow=c(1,4),mgp=c(2,1,0),mar=c(3,3,1.3,.1),oma=rep(0,4),bty='n')
for (i in 1:length(tab)) f1(tab[[i]],nitem=numitems[i])
dev.off()

pdf("~/Dropbox/Apps/Overleaf/IMV_IRT/misfitB.pdf",width=7.5,height=2.5)
load("misfit.Rdata")
numitems<-c(10,25,50,200)
par(mfrow=c(1,4),mgp=c(2,1,0),mar=c(3,3,1.3,.1),oma=rep(0,4),bty='n')
for (i in 1:length(tab)) f2(tab[[i]],nitem=numitems[i])
dev.off()

load("misfit.Rdata")
f<-function(tab) {
    z<-do.call("rbind",tab)
    z<-data.frame(z)
    c(mean(z$om0),mean(z$omctt))
}
z<-sapply(tab,f)
rowMeans(z)

    
