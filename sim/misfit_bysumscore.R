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
    ##sum score
    ss<-by(x$resp,x$id,sum)
    ss<-data.frame(id=names(ss),ss=as.numeric(ss))
    x<-merge(x,ss)
    ##
    fun<-function(x) {
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
        c(ss=unique(x$ss),
          n=np,sd=sl.sd,gm=gm,om2=mean(omega2),om3=mean(omega3),
          or.r=oracle.rasch,or.2=oracle.2,or.3=oracle.3,
          of.r=overfit.rasch,of.2=overfit.2,of.3=overfit.3,
          om0=om0,omctt=omctt,
          nss=length(unique(x$id))
          )
    }
    L<-split(x,x$ss)
    data.frame(do.call("rbind",lapply(L,fun)))
}


library(irtimv)
set.seed(867.5309^2)
np<-c(5000)#,5000)
sl.sd<-c(.5)
gm<-c(0,.3,.6)
z<-expand.grid(1:1000,np,gm,sl.sd)
argvals<-list()
for (i in 1:nrow(z)) argvals[[i]]<-list(z[i,2],z[i,3],z[i,4])

library(parallel)
tab1<-mclapply(argvals,parfun,mc.cores=25,numitems=25)
tab2<-mclapply(argvals,parfun,mc.cores=25,numitems=50)
tab3<-mclapply(argvals,parfun,mc.cores=25,numitems=200)
tab<-list(tab1,tab2,tab3)

save(tab,file="/home/users/bdomingu/irt_meta/misfit_bysumscore.Rdata")


pdf("/home/bd/Dropbox/Apps/Overleaf/IMV_IRT/guess_sumscore.pdf",width=6,height=7)
load("misfit_bysumscore.Rdata")
par(mfcol=c(3,3),mgp=c(2,1,0),mar=c(3,3,1.3,.1),oma=c(0.2,0.2,1.2,0.2),bty='n')
pf<-function(tab,ni) {
    tab<-tab[tab$nss>10,]
    ##
    plot(NULL,xlim=c(0,ni),ylim=c(-.025,.2),xlab="Sum Score",ylab="IMV(2,3)")
    abline(h=0)
    zz<-by(tab$om3,tab$ss,quantile,c(.025,.5,.975))
    zz<-do.call("rbind",zz)
    ss<-as.numeric(rownames(zz))
    lines(ss,zz[,2],lwd=2,col='red')
    cc<-col2rgb("red")
    cc<-rgb(cc[1],cc[2],cc[3],max=255,alpha=55)
    polygon(c(ss,rev(ss)),c(zz[,1],rev(zz[,3])),col=cc,border=NA)
    gm<-unique(tab$gm)
    legend("topright",bty='n', legend=bquote(C~"="~.(gm)))
}
ni<-c(25,50,200)
for (i in 1:length(tab)) {
    z<-tab[[i]]
    z<-data.frame(do.call("rbind",z))
    ll<-split(z,z$gm)
    for (j in 1:length(ll)) {
        pf(ll[[j]],ni[i])
        if (j==1) mtext(side=3,line=0,paste(ni[i],"items"))
    }
}
dev.off()
    
