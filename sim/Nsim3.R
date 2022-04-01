parfun<-function(arg,numitems=50,method="EAP") {
                                        #source("/home/bd/Dropbox/projects/irt_meta/src/00_funs.R")
    mod<-arg[[1]]
    np<-arg[[2]]
    ##
    simfun<-function(np,a,b,c,theta=FALSE) {
        th<-rnorm(np)
        p<-outer(th,b,"-")
        for (i in 1:ncol(p)) p[,i]<-a[i]*p[,i]
        for (i in 1:ncol(p)) p[,i]<-c[i]+(1-c[i])*1/(1+exp(-p[,i]))
        resp<-p
        for (i in 1:ncol(resp)) resp[,i]<-rbinom(nrow(resp),1,p[,i])
        L<-list()
        for (i in 1:ncol(resp)) L[[i]]<-data.frame(item=paste0("item_",i+10),id=1:nrow(resp),resp=as.numeric(resp[,i]),truep=as.numeric(p[,i]))
        x<-data.frame(do.call("rbind",L))
        if (theta) list(x,th) else x
    }
    rmse<-function(x,y) sqrt(mean((x-y)^2))
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
            s<-paste("F=1-",ni,"\nPRIOR = (1-",ni,", a1, lnorm, 0.2, 0.2)",
                     sep="") 
            model<-mirt.model(s)
            test<-try(m<-mirt(resp[,-index],model,itemtype=rep("2PL",ni),method="EM",technical=list(NCYCLES=10000)))
        }
        if (mod=="3PL") {
            s<-paste("F=1-",ni,"\nPRIOR = (1-",ni,", a1, lnorm, 0.2, 0.2),(1-",ni,", g, expbeta, 2, 17)",
                     sep="") 
            model<-mirt.model(s)
            test<-try(m<-mirt(resp[,-index],model,itemtype=rep(mod,ni),method="EM",technical=list(NCYCLES=10000)))
        }
        list(resp,m)
    }
    ##
    b<-rnorm(numitems)
    a<-rep(1,numitems)
    c<-rep(0,numitems)
    if (mod=="3PL") c<-runif(numitems,0,.3)
    if (mod %in% c("2PL","3PL")) a<-exp(rnorm(numitems,sd=.5))
    #if (mod=="3PL") c<-rbeta(numitems,2,17)
    #if (mod %in% c("2PL","3PL")) a<-rlnorm(numitems,.2,.2) #exp(rnorm(numitems,sd=.3))
    ##
    x<-simfun(np,a=a,b=b,c=c)
    ##
    m<-irt(x,mod=mod)
    id<-m[[1]]$id
    m<-m[[2]]
    co<-coef(m)
    nms<-names(co)
    co<-do.call("rbind",co[-length(co)])
    item<-data.frame(item=nms[-length(nms)],easy=co[,2],load=co[,1],gues=co[,3])
    stud<-data.frame(id=id,th=fscores(m)[,1])
    x<-merge(x,stud)
    x<-merge(x,item)
    kk<-x$load*x$th+x$easy
    kk<-exp(kk)
    x$p<-x$gues+(1-x$gues)*kk/(1+kk)
    ##
    omega.overfit<-imv(x,p1="p",p2="truep")
    z<-aggregate(x$resp,list(x$item),mean)
    names(z)<-c("item","p0")
    x<-merge(x,z)    
    x$resp<-rbinom(nrow(x),1,x$truep) ##oos response
    omega0<-imv(x,p1="p0",p2="p")
    omega.oracle<-imv(x,p1="p",p2="truep")
    ##
    r1<-rmse(x$p,x$truep)
    c(mod,np,omega0=omega0,oracle=omega.oracle,overfit=omega.overfit,method=method,r1=r1)
}


library(irtimv)
set.seed(867.5309^2)
mods<-rev(c("Rasch","2PL","3PL"))
f<-function(nn) {
    n<-runif(nn,min=log10(100),max=log10(5000))
    z<-expand.grid(mods,round(10^n))
    argvals<-list()
    for (i in 1:nrow(z)) argvals[[i]]<-list(as.character(z[i,1]),z[i,2])
    argvals
}

library(parallel)
tab<-list()
argvals<-f(250)
tab[[as.character(25)]]<-mclapply(argvals,parfun,mc.cores=25,numitems=25)
argvals<-f(250)
tab[[as.character(50)]]<-mclapply(argvals,parfun,mc.cores=25,numitems=50)
argvals<-f(250)
tab[[as.character(200)]]<-mclapply(argvals,parfun,mc.cores=25,numitems=200)

save(tab,file="Nsim3.Rdata")




load("Nsim3.Rdata")    
tabL<-tab#[as.character(c(25,50,400))]

pdf("/home/bd/Dropbox/Apps/Overleaf/IMV_IRT/nsim3.pdf",width=7,height=5)
par(mfrow=c(2,3),
    mgp=c(2,1,0),bty='n',cex.axis=.7,mar=c(3,3,1.3,.1),oma=rep(.3,4))
for (ii in 1:length(tabL)) {
    vals<-seq(0,.04,by=.025)
    tab<-tabL[[ii]]
    z<-data.frame(do.call("rbind",tab))
    names(z)[1:2]<-c("m","n")
    ##new
    z$n<-as.numeric(z$n)
    z$oracle<-as.numeric(z$oracle)
    z$omega0<-as.numeric(z$omega0)
    z$ln<-log10(z$n)
    L<-split(z,z$m)
    plot(NULL,xlim=log10(c(100,10000)),
         ylim=c(0,.033),xaxt='n',
         yaxt='n',ylab="Oracle IMV",xlab="N respondents")
    for (i in 1:length(vals)) abline(h=vals[i],lwd=.5,col='gray')
    xv<-c(100,2500,5000)
    axis(side=1,at=log10(xv),xv)
    axis(side=2,line=0,at=vals,vals)
    cols<-c("black","red","blue")
    pf<-function(x,y,col,txt) {
        m<-loess(y~x)
        tmp<-cbind(m$x,m$fitted)
        tmp<-tmp[order(tmp[,1]),]
        lines(tmp,col=cols[i],lwd=2)
        text(tmp[nrow(tmp),1],tmp[nrow(tmp),2],txt,cex=.7,col=col,pos=4)
    }
    for (i in 1:length(L)) {
        y<-L[[i]]
        ##
        pf(y$ln,y$oracle,col=cols[i],paste(names(L)[i]))
        #pf(y$ln,y$overfit,col=cols[i],paste(names(L)[i],"overfit"))
        #pf(y$ln,y$oracle,col=cols[i],paste(names(L)[i],"oracle"))
    }
    #if (ii==1) text(log10(2500),.0225,'Dashed lines represent IMV values\nfor true response probabilities\nrelative to estimates',cex=.7)
    mtext(side=3,line=0,paste(names(tabL)[ii],"items"))
}
for (ii in 1:length(tabL)) {
    tab<-tabL[[ii]]
    z<-data.frame(do.call("rbind",tab))
    names(z)[1:2]<-c("m","n")
    z$method<-NULL
    ##new
    z$n<-as.numeric(z$n)
    z$r1<-as.numeric(z$r1)
    z$ln<-log10(z$n)
    L<-split(z,z$m)
    vals<-seq(0,.1,length.out=5)
    plot(NULL,xlim=log10(c(100,10000)),
         ylim=range(vals),xaxt='n',
         yaxt='n',ylab="RMSE",xlab="N respondents")
    for (i in 1:length(vals)) abline(h=vals[i],lwd=.5,col='gray')
    xv<-c(100,2500,5000)
    axis(side=1,at=log10(xv),xv)
    axis(side=2,line=0,at=vals,vals)
    cols<-c("black","red","blue")
    for (i in 1:length(L)) {
        y<-L[[i]]
        ##
        m<-loess(r1~ln,y)
        tmp<-cbind(m$x,m$fitted)
        tmp<-tmp[order(tmp[,1]),]
        lines(tmp,col=cols[i],lwd=2)
        text(tmp[nrow(tmp),1],tmp[nrow(tmp),2],names(L)[i],cex=.7,col=cols[i],pos=4)
    }
    mtext(side=3,line=0,paste(names(tabL)[ii],"items"))
    ##
}
dev.off()


#old oracle/overfit
## for (ii in 1:length(tabL)) {
##     tab<-tabL[[ii]]
##     z<-data.frame(do.call("rbind",tab))
##     names(z)[1:2]<-c("m","n")
##     ##new
##     z$n<-as.numeric(z$n)
##     z$omega<-as.numeric(z$omega)
##     z$omega0<-as.numeric(z$omega0)
##     z$ln<-log10(z$n)
##     L<-split(z,z$m)
##     plot(NULL,xlim=log10(c(100,10000)),
##          ylim=range(vals),xaxt='n',
##          yaxt='n',ylab="IMV",xlab="N respondents")
##     for (i in 1:length(vals)) abline(h=vals[i],lwd=.5,col='gray')
##     xv<-c(100,2500,5000)
##     axis(side=1,at=log10(xv),xv)
##     axis(side=2,line=0,at=vals,vals)
##     cols<-c("black","red","blue")
##     pf<-function(x,y,col,txt,lty=1) {
##         m<-loess(y~x)
##         tmp<-cbind(m$x,m$fitted)
##         tmp<-tmp[order(tmp[,1]),]
##         lines(tmp,col=cols[i],lty=lty,lwd=2)
##         text(tmp[nrow(tmp),1],tmp[nrow(tmp),2],txt,cex=.7,col=col,pos=4)
##     }
##     for (i in 1:length(L)) {
##         y<-L[[i]]
##         ##
##         #pf(y$ln,y$omega0,col=cols[i],paste(names(L)[i],"omega0"))
##         pf(y$ln,y$overfit,col=cols[i],paste(names(L)[i]),lty=2)
##         pf(y$ln,y$oracle,col=cols[i],paste(names(L)[i]))
##     }
##     #if (ii==1) text(log10(2500),.0225,'Dashed lines represent IMV values\nfor true response probabilities\nrelative to estimates',cex=.7)
##     #mtext(side=3,line=0,paste(names(tabL)[ii],"items"))
## }
## legend("topright",bty='n',lty=c(1,2),c("Oracle","Overfit"))
#dev.off()
