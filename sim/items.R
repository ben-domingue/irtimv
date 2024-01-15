parfun<-function(arg,numitems=50) {
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
            s<-paste("F=1-",ni,"\nPRIOR = (1-",ni,", a1, lnorm, 0.0, 0.0)",
                     sep="") 
            model<-mirt.model(s)
            test<-try(m<-mirt(resp[,-index],model,itemtype=rep("2PL",ni),method="EM",technical=list(NCYCLES=10000)))
        }
        if (mod=="3PL") {
            s<-paste("F=1-",ni,"\nPRIOR = (1-",ni,", a1, lnorm, 0.0, 0.0),(1-",ni,", g, expbeta, 2, 17)",
                     sep="") 
            model<-mirt.model(s)
            test<-try(m<-mirt(resp[,-index],model,itemtype=rep(mod,ni),method="EM",technical=list(NCYCLES=10000)))
        }
        list(resp,m)
    }
    b<-rnorm(numitems)
    omega2<-omega3<-numeric()
    c<-runif(numitems,0,gm)
    a<-exp(rnorm(numitems,sd=sl.sd))
    ##
    x<-simfun(np,a=a,b=b,c=c)
    #index<-sample(1:nrow(x),nrow(x)/2,replace=FALSE)
    #x$gr<-ifelse(1:nrow(x) %in% index,1,2)
    #x$oos<-ifelse(x$gr==1,1,0)
    ##
    fit<-z<-list()
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
        fit[[me]]<-itemfit(m,fit_stats=c("S_X2","Zh","X2","G2","PV_Q1","infit"))

##         z[[me]]<-y                
##     }
##     x<-merge(z[[1]],z[[2]])
##     x<-merge(x,z[[3]])
##     ##
##     x$p0<-mean(x$resp)
##     x$resp<-rbinom(nrow(x),1,x$truep)
##     om1<-imv(x,p1="p0",p2="Rasch")
##     om2<-imv(x,p1="p0",p2="2PL")
##     om3<-imv(x,p1="p0",p2="3PL")
##     fit<-lapply(fit,unlist)
##     tab<-rbind(do.call("cbind",fit),c(om1,om2,om3))
##     tab<-as.matrix(tab)
##     ##
##     list(n=np,sd=sl.sd,gm=gm,tab)
## }


## set.seed(867.5309^2)
## np<-c(1000,5000)
## sl.sd<-c(0,.5)
## gm<-c(0,.3)
## z<-expand.grid(1,np,gm,sl.sd)
## argvals<-list()
## for (i in 1:nrow(z)) argvals[[i]]<-list(z[i,2],z[i,3],z[i,4])
