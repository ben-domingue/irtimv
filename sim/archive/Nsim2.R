##instead of creating an out-of-sample dataset, going to just pull responses out

parfun<-function(arg,numitems=50,method="EAP") {
                                        #source("/home/bd/Dropbox/projects/irt_meta/src/00_funs.R")
    mod<-arg[[1]]
    np<-arg[[2]]
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
    ##
    b<-rnorm(numitems)
    a<-rep(1,numitems)
    c<-rep(0,numitems)
    if (mod=="3PL") c<-rbeta(numitems,2,17)
    if (mod %in% c("2PL","3PL")) a<-rlnorm(numitems,.2,.2) #exp(rnorm(numitems,sd=.3))
    ##
    x2<-simfun(10*np,a=a,b=b,c=c)
    x2$id.item<-paste(x2$id,x2$item)
    ##get x1 subsample
    ids<-sample(unique(x2$id),np)
    x1<-x2[x2$id %in% ids,]
    ##make oos
    ## ids<-sample(x1$id.item,round(np/10))
    ## i1<-x1$id.item %in% ids
    ## oos<-x1[i1,]
    ## x1<-x1[!i1,]
    ## i2<-x2$id.item %in% ids
    ## x2<-x2[!i2,]
    ##
    xx<-list(x1,x2)
    stud<-item<-list()
    for (i in 1:length(xx)) {
        x<-xx[[i]]
        m<-irt(x,mod=mod)
        id<-m[[1]]$id
        m<-m[[2]]
        co<-coef(m)
        nms<-names(co)
        co<-do.call("rbind",co[-length(co)])
        item[[i]]<-data.frame(item=nms[-length(nms)],easy=co[,2],load=co[,1],gues=co[,3])
        #
        stud[[i]]<-data.frame(id=id,th=fscores(m)[,1])
    }
    z<-list()
    oos<-x1
    oos$resp<-rbinom(nrow(oos),1,oos$truep)
    for (i in 1:length(stud)) {
        x<-merge(oos,stud[[i]])
        x<-merge(x,item[[i]])
        ##
        kk<-x$load*x$th+x$easy
        kk<-exp(kk)
        x$p<-x$gues+(1-x$gues)*kk/(1+kk)
        z[[i]]<-x[,c("item","id","p","resp","truep")]
    }
    x1<-z[[1]]
    names(x1)[3]<-"p1"
    x2<-z[[2]]
    x2$resp<-NULL
    x2$truep<-NULL
    names(x2)[3]<-"p2"
    x<-merge(x1,x2)
    ##
    r0<-rmse(x$p1,x$p2)
    r1<-rmse(x$p1,x$truep)
    r2<-rmse(x$p2,x$truep)
    omega<-imv(x,p1="p1",p2="p2")
    omega0<-imv(x,p1="p1",p2="truep")
    ##
    c(mod,np,omega,omega0,r0,r1,r2,method=method)
}


library(irtimv)
set.seed(867.5309^2)
mods<-rev(c("Rasch","2PL","3PL"))
f<-function(nn) {
    n<-runif(nn,min=log10(100),max=log10(5000))
    z<-expand.grid(mods,round(10^n))
    argvals<-list()
    for (i in 1:nrow(z)) argvals[[i]]<-list(as.character(z[i,1]),z[i,2])
    argvals
}

library(parallel)
tab<-list()
argvals<-f(50)
tab[[as.character(400)]]<-mclapply(argvals,parfun,mc.cores=28,numitems=400)
argvals<-f(500)
tab[[as.character(25)]]<-mclapply(argvals,parfun,mc.cores=28,numitems=25)
argvals<-f(250)
tab[[as.character(50)]]<-mclapply(argvals,parfun,mc.cores=28,numitems=50)

save(tab,file="Nsim2.Rdata")




##no log
load("Nsim2.Rdata")    
tabL<-tab[as.character(c(25,50,400))]
pdf("/home/bd/Dropbox/Apps/Overleaf/IMV_IRT/nsim.pdf",width=7,height=3)
par(mfrow=c(1,3),
    mgp=c(2,1,0),bty='n',cex.axis=.7,mar=c(3,3,1.3,.1),oma=rep(.3,4))
for (ii in 1:length(tabL)) {
    tab<-tabL[[ii]]
    n<-sapply(tab,length)
    tab<-tab[n==8]
    z<-data.frame(do.call("rbind",rev(tab)))
    names(z)<-c("m","n","omega","omega0","r0","r1","r2","method")
    z$method<-NULL
    ##new
    z$n<-as.numeric(z$n)
    z$omega<-as.numeric(z$omega)
    z$omega0<-as.numeric(z$omega0)
    print(table(z$omega<0))
    z$ln<-log10(z$n)
    L<-split(z,z$m)
    vals<-seq(0,.025,by=.005)
    plot(NULL,xlim=log10(c(100,10000)),
         ylim=range(vals),xaxt='n',
         yaxt='n',ylab="IMV(N,10N)",xlab="N respondents")
    for (i in 1:length(vals)) abline(h=vals[i],lwd=.5,col='gray')
    xv<-c(100,2500,5000)
    axis(side=1,at=log10(xv),xv)
    axis(side=2,line=0,at=vals,vals)
    cols<-c("black","red","blue")
    for (i in 1:length(L)) {
        y<-L[[i]]
        ##
        m<-loess(omega0~ln,y)
        tmp<-cbind(m$x,m$fitted)
        tmp<-tmp[order(tmp[,1]),]
        lines(tmp,col=cols[i],lty=2)
        text(tmp[nrow(tmp),1],tmp[nrow(tmp),2],names(L)[i],cex=.7,col=cols[i],pos=4)
        ##
        m<-loess(omega~ln,y)
        xx<-sort(y$ln)
        yy<-predict(m,data.frame(ln=xx),se=TRUE)
        tmp<-data.frame(xx,yy$fit,yy$se.fit)
        lines(tmp,col=cols[i])
        #cc<-col2rgb(cols[i])
        #col<-rgb(cc[1],cc[2],cc[3],max=255,alpha=55)
        #polygon(c(tmp[,1],rev(tmp[,1])),c(tmp[,2]-1.96*tmp[,3],rev(tmp[,2]+1.96*tmp[,3])),col=col)
        text(tmp[nrow(tmp),1],tmp[nrow(tmp),2],names(L)[i],cex=.7,col=cols[i],pos=4)
    }
    if (ii==1) text(log10(2500),.0225,'Dashed lines represent IMV values\nfor true response probabilities\nrelative to estimates',cex=.7)
    mtext(side=3,line=0,paste(names(tabL)[ii],"items"))
}
dev.off()
