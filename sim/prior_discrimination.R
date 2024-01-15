
parfun<-function(arg,numitems=50) {
    np<-arg[[1]]
    sl.sd<-arg[[2]]
    mu<-arg[[3]]
    p1<-arg[[4]]
    p2<-arg[[5]]
    ##
    library(irtimv)
    simfun<-function(np,a,b,mu) {
        th<-rnorm(np,mean=0)
        p<-outer(th,b,"-")
        for (i in 1:ncol(p)) p[,i]<-a[i]*p[,i]
        for (i in 1:ncol(p)) p[,i]<-1/(1+exp(-p[,i]))
        resp<-p
        for (i in 1:ncol(resp)) resp[,i]<-rbinom(nrow(resp),1,p[,i])
        L<-list()
        for (i in 1:ncol(resp)) L[[i]]<-data.frame(item=paste0("item_",i),id=1:nrow(resp),resp=as.numeric(resp[,i]),truep=as.numeric(p[,i]))
        x<-data.frame(do.call("rbind",L))
        x
    }
    irt<-function(x,mod) { 
        ##
        resp<-makeresponse(x)
        library(mirt)
        index<-grep("id",names(resp))
        ni<-ncol(resp)-1
        if (mod=="2PLno") {
            m<-mirt(resp[,-index],1,"2PL")
        }
        if (mod=="2PLyes") {
            s<-paste("F=1-",ni,"\nPRIOR = (1-",ni,", a1, lnorm, ",p1,", ",p2,")",
                     sep="") 
            model<-mirt.model(s)
            test<-try(m<-mirt(resp[,-index],model,itemtype=rep("2PL",ni),method="EM",technical=list(NCYCLES=10000)))
        }
        list(resp,m)
    }
    b<-rnorm(numitems,mean=mu)
    a<-exp(rnorm(numitems,sd=sl.sd))
    omega2<-numeric()
    ##
    x<-simfun(np,a=a,b=b,mu=mu)
                                        #index<-sample(1:nrow(x),nrow(x)/2,replace=FALSE)
                                        #x$gr<-ifelse(1:nrow(x) %in% index,1,2)
                                        #x$oos<-ifelse(x$gr==1,1,0)
    ##
    z<-list()
    for (me in c("2PLno","2PLyes")) {
        m<-irt(x,mod=me)
        if (me=="2PLno") {
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
        if (me=="2PLno") {
            y$resp<-hold
            y$truep<-truep
        }
        names(y)[3]<-paste("mod",me,sep='')
        z[[me]]<-y                
    }
    x<-merge(z[[1]],z[[2]])
    ##
    x$resp<-rbinom(nrow(x),1,x$truep)
    om<-imv(x,p1='mod2PLno',p2='mod2PLyes')
    ##
    c(n=np,sd=sl.sd,p1=p1,p2=p2,om=om)
}

np<-c(1000)
sl.sd<-c(0,.75,1.5) #c(0,1)
mu<-c(0)
p1<-c(0,.2)
p2<-runif(250,min=0.05,max=1.5)
z<-expand.grid(1:1,np,sl.sd,mu,p1,p2)
argvals<-list()
for (i in 1:nrow(z)) argvals[[i]]<-list(z[i,2],z[i,3],z[i,4],z[i,5],z[i,6])

library(parallel)
tab<-mclapply(argvals,parfun,mc.cores=20)
tab<-do.call("rbind",tab)
save(tab,file='prior_disc.Rdata')

pdf("~/Dropbox/Apps/Overleaf/IMV_IRT/prior_disc.pdf",width=7,height=3)
load("prior_disc.Rdata")
tab<-data.frame(tab)
z<-split(tab,tab$sd)
par(mfrow=c(1,length(z)),mgp=c(2,1,0),mar=c(3,3,1,1),oma=rep(.5,4))
cols<-c("black","blue")
for (i in 1:length(z)) {
    plot(NULL,xlim=c(0,1.5),ylim=c(-.01,.002),xlab='s',ylab="IMV(no prior,prior)")
    abline(h=0,col='gray',lty=2)
    l<-z[[i]]
    ll<-split(l,l$p1)
    txt<-unique(l$sd)
    legend("bottomright",bty='n',title='m',fill=cols,legend=names(ll))
    mtext(side=3,line=.2,as.expression(bquote(sigma==.(txt))))
    for (ii in 1:length(ll)) {
        y<-ll[[ii]]
        mm<-loess(om~p2,y)
        mm<-cbind(mm$x,fitted(mm))
        mm<-mm[order(mm[,1]),]
        lines(mm,col=cols[ii])
    }
}
dev.off()
