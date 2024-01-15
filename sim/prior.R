
##expanding to have even more bad behavior
parfun<-function(arg,numitems=50,b.mean=0,prior.base=17) {
    np<-arg[[1]]
    gm<-arg[[2]]
    sl.sd<-arg[[3]]
    mu<-arg[[4]]
    pp<-arg[[5]]
    if (pp==prior.base) stop()
    ##
    simfun<-function(np,a,b,c,mu,d=.9995) {
        th<-rnorm(np,mean=mu)
        p<-outer(th,b,"-")
        for (i in 1:ncol(p)) p[,i]<-a[i]*p[,i]
        for (i in 1:ncol(p)) p[,i]<-c[i]+(d-c[i])*1/(1+exp(-p[,i]))
        resp<-p
        for (i in 1:ncol(resp)) resp[,i]<-rbinom(nrow(resp),1,p[,i])
        L<-list()
        for (i in 1:ncol(resp)) L[[i]]<-data.frame(item=paste0("item_",i),id=1:nrow(resp),resp=as.numeric(resp[,i]),truep=as.numeric(p[,i]))
        x<-data.frame(do.call("rbind",L))
        x
    }
    irt<-function(x,mod,pp) { ###re beta prior on guessing param https://groups.google.com/g/mirt-package/c/8Usx53BoXyw
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
            s<-paste("F=1-",ni,"\nPRIOR = (1-",ni,", a1, lnorm, 0.0, 1.0),(1-",ni,", g, expbeta, 2, ",pp,")",
                     sep="") 
            model<-mirt.model(s)
            test<-try(m<-mirt(resp[,-index],model,itemtype=rep("3PL",ni),method="EM",technical=list(NCYCLES=10000)))
        }
        list(resp,m)
    }
    b<-rnorm(numitems,mean=b.mean)
    omega2<-omega3<-numeric()
    c<-runif(numitems,0,gm)
    a<-exp(rnorm(numitems,sd=sl.sd))
    ##
    x<-simfun(np,a=a,b=b,c=c,mu=mu)
                                        #index<-sample(1:nrow(x),nrow(x)/2,replace=FALSE)
                                        #x$gr<-ifelse(1:nrow(x) %in% index,1,2)
                                        #x$oos<-ifelse(x$gr==1,1,0)
    ##
    z<-list()
    me<-"3PL"
    for (prior in c(prior.base,pp)) {
        m<-irt(x,mod=me,pp=prior)
        if (prior==prior.base) {
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
        if (prior==prior.base) {
            y$resp<-hold
            y$truep<-truep
        }
        names(y)[3]<-paste("prior",ifelse(prior==prior.base,'a','b'),sep='')
        z[[as.character(prior)]]<-y                
    }
    x<-merge(z[[1]],z[[2]])
    ##
    x$resp0<-x$resp
    om<-imv(x,p1='priora',p2='priorb')
    ##
    c(n=np,sd=sl.sd,gm=gm,mu=mu,pp=pp,om=om)
}

library(irtimv)
set.seed(867.5309^2)
np<-c(5000)
sl.sd<-0 #c(0,1)
gm<-c(0,.3,.6,.9)
mu<-c(0)
pp<-c(1,5,10,15,20)
pp<-z<-expand.grid(1:25,np,gm,sl.sd,mu,pp)
argvals<-list()
for (i in 1:nrow(z)) argvals[[i]]<-list(z[i,2],z[i,3],z[i,4],z[i,5],z[i,6])


library(parallel)
tab<-mclapply(argvals,parfun,mc.cores=3,numitems=50)

save(tab,file="prior.Rdata")



###################################################
f1<-function(tab,nitem) {
    z<-do.call("rbind",tab)
    z<-data.frame(z)
    ##
    zz<-split(z,paste(z$gm,z$pp))
    zz<-lapply(zz,colMeans)
    z<-data.frame(do.call("rbind",zz))
    z<-z[order(z$gm,z$pp),]
    ##
    bvals<-sort(unique(z$pp))
    cols<-colorRampPalette(c("blue", "red"))(length(bvals))
    L<-split(z,z$pp)
    z<-L[[1]]
    plot(NULL,xlim=c(1,nrow(z)+1),ylim=c(-7e-4,4e-4),xlab="",xaxt="n",ylab="IMV")
    #for (i in seq(-.01,.025,by=.005)) abline(h=i,lwd=1,col='gray')
    abline(h=0,col='gray')
    axis(side=1,at=1:nrow(z),labels=rep("",nrow(z)))
    mtext(side=1,at=1:nrow(z),z$gm,line=.75,cex=.5)
    mtext(side=1,line=.75,at=nrow(z)+.5,"C",cex=.5)
    #mtext(side=1,at=1:nrow(z),z$pp,line=1.75,cex=.5)
    #mtext(side=1,line=1.75,at=nrow(z)+1.3,expression(beta),cex=.5)
    ##
    for (i in 1:length(L)) {
        z<-L[[i]]
        lines(1:nrow(z),z$om,type='b',col=cols[i],lwd=2,pch=19)
    }
    mtext(side=3,line=0,paste(nitem," items"))
    legend("topright",bty='n',fill=cols,as.character(bvals),title=expression(beta))
    ##
}


pdf("~/Dropbox/Apps/Overleaf/IMV_IRT/prior.pdf",width=7.5,height=4)
layout(matrix(c(1,2,2),nrow=1))
par(mgp=c(2,1,0),mar=c(3,3,2,1))
plot(NULL,xlim=0:1,ylim=c(0,8),yaxt='n',xlab="Beta distribution",ylab="")
bvals<-c(1,5,10,15,20)
cols<-colorRampPalette(c("blue", "red"))(length(bvals))
for (i in 1:length(bvals)) {
    z<-rbeta(5000,2,bvals[i])
    lines(density(z),col=cols[i],lwd=2)
}
legend("topright",bty='n',fill=cols,as.character(bvals),title=expression(beta))

load("prior.Rdata")
numitems<-c(50)
f1(tab,nitem=numitems)
dev.off()
