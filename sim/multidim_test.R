twopl.2f.test<-function(x,corr=FALSE) {
    ##
    resp<-makeresponse(x[x$oos==0,])
    library(mirt)
    index<-grep("id",names(resp))
    ni<-ncol(resp)-1
    s <- paste("F1=1-", ni, ",\nF2=1-", ni,
               ifelse(corr,"\nCOV=F1*F2",""),
               "\nPRIOR = (1-", ni, ", a1, lnorm, 0.0, 1.0),(1-", ni, ", a2, lnorm, 0.0, 1.0)", 
               sep = "")
    model<-mirt.model(s)
    test<-tryCatch(m<-mirt(resp[,-index],model,itemtype=rep("2PL",ni),method="EM",technical=list(NCYCLES=10000)))
    print(coef(m))
    if (class(test)!="try-error") {
        co<-coef(m)
        co<-do.call("rbind",co[-length(co)])
        item<-data.frame(item=names(resp)[-index],easy=co[,3],load1=co[,1],load2=co[,2])
        ##
        th<-fscores(m)
        stud<-data.frame(id=resp$id,th1=th[,1],th2=th[,2])
        ##
        x<-merge(x[x$oos==1,],stud)
        x<-merge(x,item)
        ##
        kk<-x$load1*x$th1+x$load2*x$th2+x$easy
        kk<-exp(kk)
        x$p<-kk/(1+kk)
        x
    } else NULL
}


simdat<-function(r,
                 tau,
                 np=5000,ni=50,
                 check.itemestimates=FALSE
                 ) {
    library(MASS)
    th<-mvrnorm(np,c(0,0),matrix(c(1,r,r,1),2,2))
    b<-rnorm(ni)
    a<-rnorm(ni*2,sd=.3)
    a<-exp(a)
    a<-matrix(a,ncol=2,nrow=ni,byrow=FALSE)
    ##tau
    if (tau>0) {
        index<-sample(1:ni,ni*tau)
        for (i in index) {
            colum<-1+rbinom(1,1,.5)
            a[i,colum]<-0
        }
    }
                                        #mm<-quantile(a[,1],tau)
                                        #a[,2]<-ifelse(a[,1]>mm,0,a[,2])
    ##
    p<-th %*% t(a)
    for (i in 1:length(b)) p[,i]<-p[,i]+b[i]
    p<-1/(1+exp(-p))
    resp<-p
    for (i in 1:ncol(p)) resp[,i]<-rbinom(np,1,p[,i])
    ##
    df<-list()
    for (i in 1:ncol(p)) df[[i]]<-data.frame(id=1:np,item=paste('item_',i,sep=''),resp=resp[,i],p.true=p[,i],a1=a[i,1],a2=a[i,2],b=b[i])
    df<-data.frame(do.call("rbind",df))
    df
}

proc<-function(x) {
    library(irtimv)
    ##
    ii<-match(c("a1","a2","b"),names(x))
    items<-x[x$id==1,ii]
    x<-x[,-ii]
    ##
    x$oos<-0
    z<-rbinom(nrow(x),1,x$p)
    z<-data.frame(id=x$id,item=x$item,resp=z,p.true=x$p,oos=1)
    z<-z[sample(1:nrow(z),10000),]
    x<-data.frame(rbind(x,z))
    x2t<-twopl.2f.test(x,corr=TRUE)
    x2f<-twopl.2f.test(x,corr=FALSE)
    ##
    madt<-mean(abs(x2t$th1-x2t$th2))
    madf<-mean(abs(x2f$th1-x2f$th2))
    ##
    L<-split(x2,x2$item)
    tmp<-sapply(L,function(x) colMeans(x[,c("load1","load2","easy")]))
    true<-data.frame(item=paste("item_",1:50,sep=''),easy.true=items[,3],load1.true=items[,1],load2.true=items[,2])
    est<-data.frame(item=colnames(tmp),easy=tmp[3,],load1=tmp[1,],load2=tmp[2,])
    z<-merge(true,est)
    b.cor<-cor(z$easy,z$easy.true)
    a1.cor<-cor(z$load1,z$load1.true)
    a2.cor<-cor(z$load2,z$load2.true)
    ##
    x1<-x1[,c("item","id","p","resp")]
    names(x1)[3]<-"p1"
    x2<-x2[,c("item","id","p")]
    names(x2)[3]<-"p2"
    x<-merge(x1,x2,by=c("id","item"))
    ##
    om<-imv(x,p1="p1",p2="p2")
    c(om=om,b=b.cor,a1=a1.cor,a2=a2.cor,twopl.err=twopl.err,twopl.2f.err=twopl.2f.err,mad=mad)
}

f<-function(r,tau,np=10000) {
    x<-simdat(r,tau,np=np)
    z<-proc(x)
    c(r=r,tau=tau,z)
}
