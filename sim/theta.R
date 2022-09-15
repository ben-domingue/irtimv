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
        for (meth in c("ML","EAP")) {
            th.est<-fscores(m,response.pattern=resp,method=meth)
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
            z[[paste(me,meth)]]<-y                
        }
    }
    om<-list()
    zz<-z[["Rasch ML"]]
    zz<-zz[,c("item","id","resp","truep")]
    for (nm in c("Rasch","2PL","3PL")) {
        x1<-z[[paste(nm,"ML")]]
        x2<-z[[paste(nm,"EAP")]]
        x1$ml<-x1[,3]
        x2$eap<-x2[,3]
        xx<-merge(x1[,c("item","id","ml")],x2[,c("item","id","eap")])
        xx<-merge(xx,zz)
        xx$resp<-rbinom(nrow(xx),1,xx$truep)
        xx<-xx[is.finite(xx$ml),]
        xx<-xx[xx$ml>0 & xx$ml<1,]
        om[[nm]]<-imv(xx,p1='ml',p2='eap')
    }
    c(n=np,sd=sl.sd,gm=gm,unlist(om))
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
tab2<-mclapply(argvals,parfun,mc.cores=25,numitems=50)

save(tab2,file="theta.Rdata")

#######################################

pdf("/home/bd/Dropbox/Apps/Overleaf/IMV_IRT/theta.pdf",width=3.5,height=3)
par(mgp=c(2,1,0),mar=c(3,3,1,1))
load("theta.Rdata")
x<-do.call("rbind",tab2)
z<-data.frame(x)
##
zz<-split(z,paste(z$n,z$sd,z$gm))
zz<-lapply(zz,colMeans)
z<-data.frame(do.call("rbind",zz))
##
L<-split(z,z$n)
z<-L[[1]] #just for configuring the plot
plot(NULL,xlim=c(1,nrow(z)+.7),ylim=c(0,.003),xlab="",xaxt="n",ylab="IMV(ML,EAP)")
                                        #for (i in seq(-.01,.025,by=.005)) abline(h=i,lwd=1,col='gray')
abline(h=0,col='gray')
axis(side=1,at=1:nrow(z),labels=rep("",nrow(z)))
mtext(side=1,at=1:nrow(z),z$sd,line=.75,cex=.5)
mtext(side=1,line=.75,at=nrow(z)+.3,expression(sigma),cex=.5)
mtext(side=1,at=1:nrow(z),z$gm,line=1.75,cex=.5)
mtext(side=1,line=1.75,at=nrow(z)+.3,"C",cex=.5)
##
for (i in 1:length(L)) {
    z<-L[[i]]
    points(1:nrow(z),z$Rasch,col='black',pch=19)
    text(nrow(z),z$Rasch[nrow(z)],pos=4,'1PL',col='black',cex=.75)
    points(1:nrow(z),z$X2PL,col='red',pch=19)
    text(nrow(z),z$X2PL[nrow(z)],pos=1,'2PL',col='red',cex=.75)
    points(1:nrow(z),z$X3PL,col='blue',pch=19)
    text(nrow(z),z$X3PL[nrow(z)],pos=4,'3PL',col='blue',cex=.75)
}
dev.off()
