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
    for (me in c("Rasch")) {#,"2PL","3PL")) {
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
    x<-z[[1]] #x<-merge(z[[1]],z[[2]])
    #x<-merge(x,z[[3]])
    ##
    overfit.rasch<-imv(x,p1="Rasch",p2="truep")
    #overfit.2<-imv(x,p1="2PL",p2="truep")
    #overfit.3<-imv(x,p1="3PL",p2="truep")
    x$p0<-mean(x$resp)
    tmp<-by(x$resp,x$item,mean)
    tmp<-data.frame(item=names(tmp),pctt=as.numeric(tmp))
    x<-merge(x,tmp)
    ##
    x$resp<-rbinom(nrow(x),1,x$truep)
    oracle.rasch<-imv(x,p1="Rasch",p2="truep")
    #oracle.2<-imv(x,p1="2PL",p2="truep")
    #oracle.3<-imv(x,p1="3PL",p2="truep")
    #omega2<-imv(x,p1="Rasch",p2="2PL")
    #omega3<-imv(x,p1="2PL",p2="3PL")
    ##
    om0<-imv(x,p1='p0',p2='pctt')
    om00<-imv(x,p1='p0',p2='Rasch')
    omctt<-imv(x,p1='pctt',p2='Rasch')
    ##
    c(n=np,sd=sl.sd,gm=gm,b.mean=b.mean,#om2=mean(omega2),om3=mean(omega3),
      #or.r=oracle.rasch,or.2=oracle.2,or.3=oracle.3,
      #of.r=overfit.rasch,of.2=overfit.2,of.3=overfit.3,
      om00=om00,om0=om0,omctt=omctt)
}


library(irtimv)
set.seed(867.5309^2)
bm<-runif(500,min=0,max=1.5)
out<-list()
for (i in 1:length(bm)) {
    out[[i]]<-parfun(list(1000,0,0),b.mean=bm[i])
}
x<-data.frame(do.call("rbind",out))
save(x,file="~/Dropbox/projects/irt_meta/src/output/pvalue.Rdata")

pdf("~/Dropbox/Apps/Overleaf/IMV_IRT/pvalue.pdf",width=4,height=3)
load("~/Dropbox/projects/irt_meta/src/output/pvalue.Rdata")
bm<-x$b.mean
par(mgp=c(2,1,0),mar=c(3,3,1,1))
plot(NULL,xlim=c(0,1.5),ylim=c(0,.55),xlab="E(b)",ylab="IMV")
abline(h=0,col='gray')
##
m<-loess(x$om0~bm)
tmp<-cbind(m$x,predict(m))
tmp<-tmp[order(tmp[,1]),]
lines(tmp,col='red',lwd=2.5)
##
m<-loess(x$omctt~bm)
tmp<-cbind(m$x,predict(m))
tmp<-tmp[order(tmp[,1]),]
lines(tmp,col='black',lwd=2.5)
##
m<-loess(x$om00~bm)
tmp<-cbind(m$x,predict(m))
tmp<-tmp[order(tmp[,1]),]
lines(tmp,col='blue',lwd=2.5)
##
legend("topright",bty='n',fill=c("blue","red","black"),
       legend=c(as.expression(bquote("IMV("~bar(x)~",1PL)")),
                as.expression(bquote("IMV("~bar(x)~","~bar(x[j])~")")),
                as.expression(bquote("IMV("~bar(x[j])~",1PL)"))
                )
       )
dev.off()
