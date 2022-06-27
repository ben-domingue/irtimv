parfun<-function(arg,numitems=50) {
    np<-arg[[1]]
    gm<-arg[[2]]
    sl.sd<-arg[[3]]
    diff<-arg[[4]]
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
    b<-rnorm(numitems,mean=diff)
    omega2<-omega3<-numeric()
    c<-runif(numitems,0,gm)
    a<-exp(rnorm(numitems,sd=sl.sd))
    ##
    x<-simfun(np,a=a,b=b,c=c)
    ##
    mod<-fit<-z<-list()
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
        f1<-M2(m)
        f2<-m@Fit[c("AIC",  "BIC", "SABIC", "DIC", "HQ","logLik")] ##hi! aic is first here, ok below
        fit[[me]]<-c(f2,f1)
        z[[me]]<-y
        mod[[me]]<-m
    }
    x<-merge(z[[1]],z[[2]])
    x<-merge(x,z[[3]])
    ##
    x$p0<-mean(x$resp)
    x$resp<-rbinom(nrow(x),1,x$truep)
    om1<-imv(x,p1="p0",p2="Rasch")
    om2<-imv(x,p1="p0",p2="2PL")
    om3<-imv(x,p1="p0",p2="3PL")
    om21<-imv(x,p1="Rasch",p2="2PL")
    om32<-imv(x,p1="2PL",p2="3PL")
    ##
    llrt<-function(m1,m2) {
        cs<-m1@Fit$logLik-m2@Fit$logLik
        an<-anova(m1,m2)
        ii<-match("Bayes_Factor",colnames(an))
        bf<-an[2,ii]
        ii<-match("df",colnames(an))
        pc<-pchisq(-2*cs,df=an[2,ii],lower.tail=FALSE)
        c(bf,pc)
    }
    p2<-llrt(mod[[1]],mod[[2]])
    p3<-llrt(mod[[2]],mod[[3]])
    ##
    fit<-lapply(fit,unlist)
    if (names(f2)[1]!="AIC") stop()
    tab<-rbind(do.call("cbind",fit),
               `Bayes Factor`=c(NA,p2[1],p3[1]), #bf
               `Likelihood Ratio Test`=c(NA,p2[2],p3[2]), #lrt
               `IMV`=c(om1,om2,om3), #raw imv
               `IMV change`=c(NA,om21,om32) #delta
               )
    tab<-rbind(tab,
          `AIC delta`=c(NA,tab[1,2]-tab[1,1],tab[1,3]-tab[1,2])#tab[1,]-min(tab[1,]) #see f2 line above
          )
    tab<-as.matrix(tab)
    tmp<-list()
    for (i in 1:ncol(tab)) {
        foo<-data.frame(value=as.numeric(tab[,i]),metric=rownames(tab))
        foo$model<-colnames(tab)[i]
        foo$n<-np
        foo$sd<-sl.sd
        foo$gm<-gm
        foo$diff<-diff
        tmp[[i]]<-foo
    }
    data.frame(do.call("rbind",tmp))
}


#################################################################################
##slopes
library(irtimv)
set.seed(867.5309^2)
np<-1000 #np<-c(1000,5000)
sl.sd<-runif(100,min=0,max=.5)
gm<-0 #gm<-c(0,.3)
library(parallel)
tab<-list()
diff<-c(-1.5,0,1.5)
z<-expand.grid(1,np,gm,sl.sd,diff)
argvals<-list()
for (i in 1:nrow(z)) argvals[[i]]<-list(z[i,2],z[i,3],z[i,4],z[i,5])
tmp<-mclapply(argvals,parfun,mc.cores=10,numitems=50)
tab.slope<-data.frame(do.call("rbind",tmp))

save(tab.slope,file="misfit_slope.Rdata")

#################################################################################
##guessing
library(irtimv)
set.seed(867.5309^2)
np<-1000 #np<-c(1000,5000)
sl.sd<-0
gm<-runif(100,min=0,max=.3)
library(parallel)
tab<-list()
diff<-c(-1.5,0,1.5)
z<-expand.grid(1,np,gm,sl.sd,diff)
argvals<-list()
for (i in 1:nrow(z)) argvals[[i]]<-list(z[i,2],z[i,3],z[i,4],z[i,5])
tmp<-mclapply(argvals,parfun,mc.cores=10,numitems=50)
tab.guess<-data.frame(do.call("rbind",tmp))

save(tab.guess,file="misfit_guess.Rdata")


#################################################################################
pfouter<-function(z,xx.name,...) {
    pf<-function(xx,yy,col) {
        if (!all(is.na(yy))) {
            mm<-loess(yy~xx)
            mm2<-predict(mm,se=TRUE)
            tmp<-cbind(mm$x,mm2$fit,mm2$se.fit)
            tmp<-tmp[order(tmp[,1]),]
            lines(tmp,col=col)
            cc<-col2rgb(col)
            cc<-rgb(cc[1],cc[2],cc[3],max=255,alpha=55)
            polygon(c(tmp[,1],rev(tmp[,1])),c(tmp[,2]-1.96*tmp[,3],rev(tmp[,2]+1.96*tmp[,3])),col=cc,border=NA)
        }
    }
    cols<-c("black","blue","red")
    ##
    nm<-unique(as.character(z$metric))
    plot(NULL,ylab="",xaxt='n',yaxt='n',...)
    if (nm=="IMV change") abline(h=0,col='gray')
    if (nm=="AIC delta") abline(h=0,col='gray')
    if (nm=="Likelihood Ratio Test") abline(h=-log10(.05),col='gray')
    ##
    zz<-list(Rasch=z[z$model=="Rasch",],
             two=z[z$model=="2PL",],
             three=z[z$model=="3PL",]
             )
    for (i in 1:length(zz)) pf(zz[[i]][[xx.name]],zz[[i]]$value,col=cols[i])
    cols
}
##
names<-list("RMSEA"="RMSEA", "AIC delta"="AIC difference", "Likelihood Ratio Test"="-log10(p, LRT)", "IMV change"="IMV change")
metrics<-c("RMSEA","AIC delta","Likelihood Ratio Test","IMV change")

pdf("/home/bd/Dropbox/Apps/Overleaf/IMV_IRT/misfit_slope.pdf",width=6,height=7)
load("misfit_slope.Rdata")
x<-tab.slope
x$value<-ifelse(x$metric=="Likelihood Ratio Test",-log10(x$value),x$value)
par(mfcol=c(length(metrics),3),mar=c(1,1,1,1),mgp=c(2,1,0),oma=c(6,6,1,1))
diff<-unique(x$diff)
for (d in diff) {
    for (metric in metrics) {
        z<-x[x$metric==metric & x$diff==d,]
        cols<-pfouter(z,xx.name="sd",xl=c(0,.5),xlab=expression(sigma),ylim=range(x$value[x$metric==metric],na.rm=TRUE))
        if (metric==metrics[1]) mtext(side=3,line=0,bquote(mu[b]~"="~.(d))) #paste("Mean difficulty",d))
        ##legend
        if (d==diff[1]) {
            pos<-"topleft"
            if (metric=="AIC delta") pos<-'left'
            if (metric=="RMSEA") {
                legend(pos,legend=c("1PL","2PL","3PL"),fill=cols,bty='n')
            } else {
                legend(pos,legend=c("2PL v 1PL","3PL v 2PL"),fill=cols[-1],bty='n')
            }
        }
        ##y-axis
        if (d==diff[1]) {
            axis(side=2)
            mtext(side=2,names[[metric]],line=2)
        }
    }
    axis(side=1)
    mtext(side=1,expression(sigma),line=2)
}
dev.off()

pdf("/home/bd/Dropbox/Apps/Overleaf/IMV_IRT/misfit_guess.pdf",width=6,height=7)
load("misfit_guess.Rdata")
x<-tab.guess
x$value<-ifelse(x$metric=="Likelihood Ratio Test",-log10(x$value),x$value)
par(mfcol=c(length(metrics),3),mar=c(1,1,1,1),mgp=c(2,1,0),oma=c(6,6,1,1))
diff<-unique(x$diff)
for (d in diff) {
    for (metric in metrics) {
        z<-x[x$metric==metric & x$diff==d,]
        cols<-pfouter(z,xx.name="gm",xl=c(0,.3),xlab="C",ylim=range(x$value[x$metric==metric],na.rm=TRUE))
        if (metric==metrics[1]) mtext(side=3,line=0,bquote(mu[b]~"="~.(d))) #paste("Mean difficulty",d))
        ##legend
        if (d==diff[1]) {
            pos<-"topleft"
            if (metric=="AIC delta") pos<-'left'
            if (metric=="RMSEA") {
                legend(pos,legend=c("1PL","2PL","3PL"),fill=cols,bty='n')
            } else {
                legend(pos,legend=c("2PL v 1PL","3PL v 2PL"),fill=cols[-1],bty='n')
            }
        }
        ##y-axis
        if (d==diff[1]) {
            axis(side=2)
            mtext(side=2,names[[metric]],line=2)
        }
    }
    axis(side=1)
    mtext(side=1,"C",line=2)
}
dev.off()

