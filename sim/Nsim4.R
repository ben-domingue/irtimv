parfun<-function(mod,np,numitems=50,method="EAP") {
                                        #source("/home/bd/Dropbox/projects/irt_meta/src/00_funs.R")
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
    ##
    b<-rnorm(numitems)
    a<-rep(1,numitems)
    c<-rep(0,numitems)
    if (mod=="3PL") c<-runif(numitems,0,.3)
    if (mod %in% c("2PL","3PL")) a<-exp(rnorm(numitems,sd=.5))
    ##
    x<-simfun(np,a=a,b=b,c=c)
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
    ##
    x$resp<-rbinom(nrow(x),1,x$truep)
    oracle.rasch<-imv(x,p1="Rasch",p2="truep")
    oracle.2<-imv(x,p1="2PL",p2="truep")
    oracle.3<-imv(x,p1="3PL",p2="truep")
    omega2<-imv(x,p1="Rasch",p2="2PL")
    omega3<-imv(x,p1="2PL",p2="3PL")
    ##
    c(mod,np,omega2,omega3,oracle.rasch,oracle.2,oracle.3,overfit.rasch,overfit.2,overfit.3)
}


library(irtimv)
set.seed(867.5309^2)
ff<-function(dumval,ni,parfun=parfun) {
    n<-runif(1,min=log10(100),max=log10(10000))
    n<-round(10^n)
    mods<-c("Rasch","2PL","3PL")
    out<-list()
    for (mod in mods) out[[mod]]<-parfun(numitems=ni,np=n,mod=mod) #argvals<-list()
    tmp<-data.frame(do.call("rbind",out))
    names(tmp)<-c("mod","n","om2","om3","or.r","or.2","or.3","of.r","of.2","of.3")
    tmp
}

library(parallel)

tab<-mclapply(1:250,ff,mc.cores=2,ni=50,parfun=parfun)

save(tab,file="Nsim4.Rdata")








load("Nsim4.Rdata")
z<-data.frame(do.call("rbind",tab))
for (i in 2:ncol(z)) z[,i]<-as.numeric(z[,i])

pdf("~/Dropbox/Apps/Overleaf/IMV_IRT/nsim4.pdf",width=8,height=6)
par(mfrow=c(2,3),
    mgp=c(2,1,0),bty='n',cex.axis=.7,mar=c(3,3,1.3,.71),oma=rep(.3,4))
##
plotfun<-function(x) {
    pf<-function(x,y,col,txt,pos) {
        m<-loess(y~x)
        tmp<-cbind(m$x,m$fitted)
        tmp<-tmp[order(tmp[,1]),]
        lines(tmp,col=col,lwd=2)
        text(tmp[nrow(tmp),1],tmp[nrow(tmp),2],txt,cex=.7,col=col,pos=pos)
    }
    vals<-seq(-.025,.025,by=.025)
    plot(NULL,xlim=log10(c(100,12500)),
         ylim=c(-.005,.015),xaxt='n',
         ylab="IMV",xlab="N respondents")
    for (i in 1:length(vals)) abline(h=vals[i],lwd=.5,col='gray')
    xv<-c(100,2500,5000,10000)
    axis(side=1,at=log10(xv),xv)
    x$ln<-log10(x$n)
    pf(x$ln,x$om2,col="red",txt='IMV(1,2)',pos=1)
    pf(x$ln,x$om3,col="blue",txt='IMV(2,3)',pos=3)
}
for (mod in c("Rasch","2PL","3PL")) {
    plotfun(z[z$mod==mod,])
    if (mod=="Rasch") '1PL'->txt else mod->txt
    legend("topleft",bty='n',paste("DGM=",txt,sep=''))
}
##
plotfun<-function(x) {
    pf<-function(x,y,col,txt,pos,lty=1) {
        m<-loess(y~x)
        tmp<-cbind(m$x,m$fitted)
        tmp<-tmp[order(tmp[,1]),]
        lines(tmp,col=col,lwd=2,lty=lty)
        text(tmp[nrow(tmp),1],tmp[nrow(tmp),2],txt,cex=.7,col=col,pos=pos)
    }
    vals<-seq(-.025*2,2*.025,by=.025)
    plot(NULL,xlim=log10(c(100,12500)),
         ylim=c(-.035,.035),xaxt='n',
         ylab="IMV",xlab="N respondents")
    for (i in 1:length(vals)) abline(h=vals[i],lwd=.5,col='gray')
    xv<-c(100,2500,5000,10000)
    axis(side=1,at=log10(xv),xv)
    x$ln<-log10(x$n)
    pf(x$ln,x$or.r,col="red",txt='1',pos=4)
    pf(x$ln,x$or.2,col="blue",txt='2',pos=4)
    pf(x$ln,x$or.3,col="black",txt='3',pos=4)
    ##
    pf(x$ln,x$of.r,col="red",txt='1',pos=4,lty=3)
    pf(x$ln,x$of.2,col="blue",txt='2',pos=4,lty=3)
    pf(x$ln,x$of.3,col="black",txt='3',pos=4,lty=3)
}
for (mod in c("Rasch","2PL","3PL")) {
    plotfun(z[z$mod==mod,])
    if (mod=="Rasch") '1PL'->txt else mod->txt
    legend("topleft",bty='n',paste("DGM=",txt,sep=''))
    legend("bottomright",bty='n',lty=c(1,3),c("Oracle","Overfit"))
}
dev.off()


