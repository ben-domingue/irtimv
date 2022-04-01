imv<-function(pr,p1,p2,eps=1e-6) {
    pr$pv1<-pr[[p1]]
    pr$pv2<-pr[[p2]]
    pr$pv1<-ifelse(pr$pv1 < eps,eps,pr$pv1)
    pr$pv2<-ifelse(pr$pv2 < eps,eps,pr$pv2)
    pr$pv1<-ifelse(pr$pv1 > 1-eps,1-eps,pr$pv1)
    pr$pv2<-ifelse(pr$pv2 > 1-eps,1-eps,pr$pv2)
    ##
    ll<-function(x,p='pv') {
        z<-log(x[[p]])*x$resp+log(1-x[[p]])*(1-x$resp)
        z<-sum(z)/nrow(x)
        exp(z)
    }    
    loglik1<-ll(pr,'pv1')
    loglik2<-ll(pr,'pv2')
    getcoins<-function(a) {
        f<-function(p,a) abs(p*log(p)+(1-p)*log(1-p)-log(a))
        nlminb(.5,f,lower=0.001,upper=.999,a=a)$par
    }
    c1<-getcoins(loglik1)
    c2<-getcoins(loglik2)
    ew<-function(p1,p0) (p1-p0)/p0
    imv<-ew(c2,c1)
    imv
}

makeresponse<-function(x) {
    #make IR matrix
    nms<-unique(x$item)
    if (all(nms %in% 1:length(nms))) x$item<-paste("item_",x$item,sep='')
    ##make response matrix
    id<-unique(x$id)
    L<-split(x,x$item)
    out<-list()
    for (i in 1:length(L)) {
        z<-L[[i]]
        index<-match(z$id,id)
        resp<-rep(NA,length(id))
        resp[index]<-z$resp
        out[[i]]<-resp
    }
    resp<-do.call("cbind",out)
    resp<-data.frame(resp)
    names(resp)<-names(L)
    resp$id<-id
    nr<-apply(resp,2,function(x) length(table(x)))
    resp<-resp[,nr>1]
    resp<-resp[rowSums(!is.na(resp))>1,]
    resp
}


rasch<-function(x,ability="EAP") {
    ##
    resp<-makeresponse(x[x$oos==0,])
    library(mirt)
    index<-grep("id",names(resp))
    ni<-ncol(resp)-1
    m<-mirt(resp[,-index],1,"Rasch")
    co<-coef(m)
    co<-do.call("rbind",co[-length(co)])
    item<-data.frame(item=names(resp)[-index],easy=co[,2],load=co[,1])
    ##
    th<-fscores(m,method=ability)
    stud<-data.frame(id=resp$id,th=th[,1])
    ##
    x<-merge(x[x$oos==1,],stud)
    x<-merge(x,item)
    ##
    kk<-x$load*x$th+x$easy
    kk<-exp(kk)
    x$p<-kk/(1+kk)
    x
}


twopl<-function(x,ability="EAP") { ###re beta prior on guessing param https://groups.google.com/g/mirt-package/c/8Usx53BoXyw
    ##
    resp<-makeresponse(x[x$oos==0,])
    library(mirt)
    index<-grep("id",names(resp))
    ni<-ncol(resp)-1
    s<-paste("F=1-",ni,"
             PRIOR = (1-",ni,", a1, lnorm, 0.2, 0.2)",sep="") #-1.5
    model<-mirt.model(s)
    test<-try(m<-mirt(resp[,-index],model,itemtype=rep("2PL",ni),method="EM",technical=list(NCYCLES=10000)))
    if (class(test)!="try-error") {
        co<-coef(m)
        co<-do.call("rbind",co[-length(co)])
        item<-data.frame(item=names(resp)[-index],easy=co[,2],load=co[,1])
        ##
        th<-fscores(m,method=ability)
        stud<-data.frame(id=resp$id,th=th[,1])
        ##
        x<-merge(x[x$oos==1,],stud)
        x<-merge(x,item)
        ##
        kk<-x$load*x$th+x$easy
        kk<-exp(kk)
        x$p<-kk/(1+kk)
        x
    } else NULL
}

threepl<-function(x,ability="EAP") { ###re beta prior on guessing param https://groups.google.com/g/mirt-package/c/8Usx53BoXyw
    ##
    resp<-makeresponse(x[x$oos==0,])
    library(mirt)
    index<-grep("id",names(resp))
    ni<-ncol(resp)-1
    s<-paste("F=1-",ni,"
     PRIOR = (1-",ni,", a1, lnorm, 0.2, 0.2),(1-",ni,", g, expbeta, 2, 17)",sep="") 
    model<-mirt.model(s)
    test<-try(m<-mirt(resp[,-index],model,itemtype=rep("3PL",ni),method="EM",technical=list(NCYCLES=10000)))
    if (class(test)!="try-error") {
        co<-coef(m)
        co<-do.call("rbind",co[-length(co)])
        item<-data.frame(item=names(resp)[-index],easy=co[,2],load=co[,1],guess=co[,3])
        ##
        th<-fscores(m,method=ability)
        stud<-data.frame(id=resp$id,th=th[,1])
        ##
        x<-merge(x[x$oos==1,],stud)
        x<-merge(x,item)
        ##
        kk<-x$load*x$th+x$easy
        kk<-x$guess+(1-x$guess)/(1+exp(-1*kk))
        x$p<-kk #/(1+kk)
        x
    } else NULL
}


twopl.2f<-function(x) { ###re beta prior on guessing param https://groups.google.com/g/mirt-package/c/8Usx53BoXyw
    ##
    resp<-makeresponse(x[x$oos==0,])
    library(mirt)
    index<-grep("id",names(resp))
    ni<-ncol(resp)-1
    s<-paste("F1=1-",ni,",
F2=1-",ni,"
            PRIOR = (1-",ni,", a1, lnorm, 0.2, 0.2),(1-",ni,", a2, lnorm, 0.2, 0.2)",sep="") #-1.5
    model<-mirt.model(s)
    test<-try(m<-mirt(resp[,-index],model,itemtype=rep("2PL",ni),method="EM",technical=list(NCYCLES=10000)))
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


imv_c<-function(y,pctt.tab,p1,p2) {
    nn<-length(unique(y$resp))
    om<-numeric()
    iis<-0:(nn-1)
    for (ii in iis) {
        ns<-om.tmp<-numeric()
        jjs<-iis[-match(ii,iis)]
        for (jj in jjs) {
            y2<-y[y$resp %in% c(ii,jj),]
            resp<-ifelse(y2$resp==ii,1,0)
            ##irt p-values for being 
            p1.ii<-y2[[paste(p1,ii,sep='')]]
            p1.jj<-y2[[paste(p1,jj,sep='')]]
            p2.ii<-y2[[paste(p2,ii,sep='')]]
            p2.jj<-y2[[paste(p2,jj,sep='')]]
            ##
            z<-data.frame(resp=resp,
                          p1=p1.ii/(p1.ii+p1.jj),
                          p2=p2.ii/(p2.ii+p2.jj)
                          )
            j0<-as.character(jj)
            om.tmp[j0]<-imv(z,p1="p1",p2="p2")
            ns[as.character(jj)]<-nrow(z)
        }
        om[ii+1]<-sum(om.tmp*ns)/sum(ns)
    }
    sum(om*pctt.tab)/sum(pctt.tab)
}

imv_t<-function(y,pctt.tab,p1,p2) {
    nn<-length(unique(y$resp))
    om<-numeric()
    for (ii in 0:(nn-2)) {
        resp<-ifelse(y$resp<=ii,1,0)
        ##irt p-values for being below ii
        pr1<-rowSums(y[,paste(p1,0:ii,sep=''),drop=FALSE])
        pr2<-rowSums(y[,paste(p2,0:ii,sep=''),drop=FALSE])
        z<-data.frame(resp=resp,p1=pr1,p2=pr2)
        om[ii+1]<-imv(z,p1="p1",p2="p2")
    }
    ##
    pctt.tab<-pctt.tab[1:(nn-1)]/(1-pctt.tab[nn])
    ##
    sum(om*pctt.tab)/sum(pctt.tab)
}
