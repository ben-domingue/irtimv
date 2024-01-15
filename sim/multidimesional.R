
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
    x1<-twopl(x)
    x2<-twopl.2f(x)
    ##
    rmse<-function(x,y) sqrt(mean((x-y)^2))
    twopl.err<-rmse(x1$p.true,x1$p)
    twopl.2f.err<-rmse(x2$p.true,x2$p)
    mad<-mean(abs(x2$th1-x2$th2))
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

r<-sort(runif(200))
library(parallel)
L<-list()
taus<-seq(0,1,by=.5)
for (tau in taus) {
    tmp<-mclapply(r,f,tau=tau,mc.cores=25)
    L[[as.character(tau)]]<-do.call("rbind",tmp)
}

save(L,file="multidimensional.Rdata")

###############

pdf("~/Dropbox/Apps/Overleaf/IMV_IRT/md.pdf",width=7,height=3)

load("multidimensional.Rdata")
cm<-sapply(L,colMeans)
layout(matrix(c(1,1,2),nrow=1,byrow=TRUE))
par(mgp=c(2,1,0),mar=c(3,3,1,1))
cols<-colorRampPalette(c("blue", "red"))( length(L) ) ## (n)
##
plot(NULL,xlim=c(-.07,1),xaxt='n',ylim=c(-.005,.05),xlab=expression(rho),ylab="IMV(2PL,2F-2PL)")
axis(side=1,at=seq(0,1,by=.25))
abline(h=0)
for (i in 1:length(L)) {
    z<-L[[i]]
    z<-cbind(z[,1],z[,3])
    m<-loess(z[,2]~z[,1])
    lines(m$x,fitted(m),col=cols[i],lwd=2)
    text(m$x[1],fitted(m)[1],pos=2,cm[2,i],col=cols[i])
}
legend("topright",bty='n',title=expression(tau),fill=cols,legend=cm[2,])
##
plot(NULL,xlim=c(0,1),xaxt='n',xlab=expression(rho),ylab='RMSE',ylim=c(-.005,.15))
axis(side=1,at=seq(0,1,by=.25))
abline(h=0)
for (ii in 7:8) {
    for (i in 1:length(L)) {
        z<-L[[i]]
        z<-cbind(z[,1],z[,ii])
        m<-loess(z[,2]~z[,1])
        lines(m$x,fitted(m),col=cols[i],lwd=2,lty=ii-6)
        #text(m$x[1],fitted(m)[1],pos=2,cm[2,i],col=cols[i])
    }
}
legend('topright',bty='n',title='RMSE',legend=c("2PL","2F-2PL"),lty=c(1,2),lwd=2)

dev.off()


