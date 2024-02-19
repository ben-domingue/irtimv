sim<-function(mu=0,N=1000,ni=50,sl.sd=0.3,gg=0,uu=1,xi.sd=0) {
    irf<-function(th,
                  a=1,
                  b,
                  xi=1, #eqn 2 here. https://link.springer.com/article/10.1007/s11336-017-9586-5
                  g=0,
                  u=1
                  ) {
        p<-a*(th-b)
        p<-1/(1+exp(-p))
        p<-p^xi
        p<-g+(u-g)*p
        p
    }
    th<-rnorm(N)
    b<-rnorm(ni,mean=mu)
    a<-exp(rnorm(ni,sd=sl.sd))
    g<-runif(ni,min=0,gg)
    u<-runif(ni,min=uu,max=1)
    xi<-exp(rnorm(ni,sd=xi.sd))
    ##
    p<-list()
    for (i in 1:ni) p[[i]]<-irf(th=th,b=b[i],a=a[i],xi=xi[i],g=g[i],u=u[i])
    p<-do.call("cbind",p)
    resp<-p
    for (i in 1:ncol(p)) resp[,i]<-rbinom(nrow(p),1,p[,i])
    x<-list()
    for (i in 1:ncol(resp)) x[[i]]<-data.frame(id=1:length(th),item=paste("item_",i,sep=''),resp=resp[,i],p.true=p[,i])
    x<-data.frame(do.call("rbind",x))
    ##
    x$oos<-0
    x2<-x[sample(1:nrow(x),25000),]
    x2$resp<-rbinom(nrow(x2),1,x2$p)
    x2$oos<-1
    x<-data.frame(rbind(x,x2))
    ##
    library(irtimv)
    x0<-x[x$oos==0,]
    ##
    xr<-rasch(x,return.model=TRUE)
    mr<-xr[[2]]
    x2<-twopl(x,return.model=TRUE)
    m2<-x2[[2]]
    x3<-threepl(x,return.model=TRUE)
    m3<-x3[[2]]
    ##
    z<-xr[[1]][,c("item","id","resp","p.true","p")]
    names(z)[5]<-'pr'
    z2<-x2[[1]][,c("item","id","p")]
    names(z2)[3]<-'p2'
    z<-merge(z,z2)
    z3<-x3[[1]][,c("item","id","p")]
    names(z3)[3]<-'p3'
    z<-merge(z,z3)
    ##
    om.r2<-imv(z,p1="pr",p2="p2")
    om.23<-imv(z,p1="p2",p2="p3")
    rmse<-function(x,y) sqrt(mean((x-y)^2))
    rmse.r<-rmse(z$p.true,z$pr)
    rmse.2<-rmse(z$p.true,z$p2)
    rmse.3<-rmse(z$p.true,z$p3)
    ##
    fit<-function(m) {
        f1<-M2(m)
        f2<-m@Fit[c("AIC",  "BIC", "SABIC", "HQ","logLik")] ##hi! aic is first here, ok below
        c(f2,rmsea=f1$RMSEA)
    }
    out<-list(c(om.r2=om.r2,om.23=om.23,rmse.r=rmse.r,rmse.2=rmse.2,rmse.3=rmse.3),rasch=fit(mr),two=fit(m2),three=fit(m3))
    c(mu=mu,prev=mean(x$resp),unlist(out))
}

mu<-runif(250,min=-3,max=3)

library(parallel)
L1<-mclapply(mu,sim,mc.cores=10,N=1000)
#L2<-mclapply(mu,sim,mc.cores=10,N=5000,gg=.2,uu=.8,xi.sd=.3)

L<-list(L1) #,L2)
save(L,file='altmetric.Rdata')

pdf("~/Dropbox/Apps/Overleaf/IMV_IRT/altmetric.pdf",width=7.5,height=2.5)
load("altmetric.Rdata")
pf<-function(x,y,...) {
    m<-loess(y~x)
    lines(m$x,fitted(m),lwd=2,...)
}
par(mfrow=c(1,4),mgp=c(2,1,0),mar=c(3,3,2,1),oma=rep(.5,4))
pf2<-function(L,...) {
    z<-sapply(L,function(x) x[1])
    z<-as.numeric(z)
    L<-L[!is.na(z)]
    ##
    x<-data.frame(do.call("rbind",L))
    x[order(x$mu),]->x
    xl<-range(x$mu)
    plot(NULL,xlim=xl,ylim=c(-.005,.08),ylab='Change in RMSE',xlab=expression(mu)); abline(h=0,lty=2,col='gray')
    #legend("topright",bty='n',col=c("black","blue","red"),c("1PL","2PL","3PL"),lty=2)
    legend("left",bty='n',fill=c("blue","red"),c("1PL-2PL","2PL-3PL"))
    pf(x$mu,x$rmse.r,lty=2)
    pf(x$mu,x$rmse.2,col='blue',lty=2)
    pf(x$mu,x$rmse.3,col='red',lty=2)
    pf(x$mu,x$rmse.r-x$rmse.2,col='blue')
    pf(x$mu,x$rmse.2-x$rmse.3,col='red')
    ##
    plot(NULL,xlim=xl,ylim=c(-.005,.01),ylab='IMV',xlab=expression(mu)); abline(h=0,lty=2,col='gray')
    pf(x$mu,x$om.r2,col='blue')
    pf(x$mu,x$om.23,col='red')
    legend("topright",bty='n',fill=c("blue","red"),c("(1PL,2PL)","(2PL,3PL)"))
    ##
    plot(NULL,xlim=xl,ylim=c(0,.02),ylab='Change in RMSEA',xlab=expression(mu)); abline(h=0,lty=2,col='gray')
    pf(x$mu,x$rasch.rmsea-x$two.rmsea,col='blue')
    pf(x$mu,x$two.rmsea-x$three.rmsea,col='red')
    legend("topright",bty='n',fill=c("blue","red"),c("1PL-2PL","2PL-3PL"))
    ##
    y<-c(x$rasch.AIC-x$two.AIC,x$two.AIC-x$three.AIC)
    ran<-range(y)
    plot(NULL,xlim=xl,ylim=ran,ylab='Change in AIC',xlab=expression(mu)); abline(h=0,lty=2,col='gray')
    y<-x$rasch.AIC-x$two.AIC
    pf(x$mu,y,col='blue')
    y<-x$two.AIC-x$three.AIC
    pf(x$mu,y,col='red')
    legend("topright",bty='n',fill=c("blue","red"),c("1PL-2PL","2PL-3PL"))
}
pf2(L[[1]],'1PL')
#pf2(L[[2]],'Asymmetric 3PL')
dev.off()



