#add noise to p-values

library(irtimv)
set.seed(867.5309^2)


nitems<-c(25,50,200)

fuzzy<-function(np,ni) {
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
    tab<-list()
    rmse<-function(x,y) sqrt(mean((x-y)^2))
    dels<-sort(runif(25,min=0,max=.2))
    for (i in 1:length(dels)) {
        eps<-.001
        del<-dels[i]
        b<-rnorm(ni)
        c<-runif(ni,0,.3)
        a<-exp(rnorm(ni,sd=.5))
        omega<-numeric()
        ##
        x<-simfun(np,a=a,b=b,c=c)
        x$p<-x$truep+runif(nrow(x),min=-1*del,max=del)
        x$p<-ifelse(x$p>1,1-eps,x$p)
        x$p<-ifelse(x$p<0,eps,x$p)
        omega<-imv(x,p1="p",p2="truep")
        tab[[paste(del)]]<-c(del,rmse(x$p,x$truep),mean(omega))
    }
    z<-do.call("rbind",tab)
    z
}
fl<-list()
for (ni in nitems) fl[[as.character(ni)]]<-fuzzy(np=2000,ni=ni)

pdf("/home/bd/Dropbox/Apps/Overleaf/IMV_IRT/nsim_si.pdf",width=8,height=3)
par(mfrow=c(1,3),mgp=c(2,1,0),bty='n',cex.axis=.7,mar=c(3,3,1.3,.1),oma=rep(.3,4))
##
for (nm in names(fl)) {
    z<-fl[[nm]]
    plot(z[,2],z[,3],xlab="RMSE",ylab="IMV(noisy,true)",ylim=c(0,.053))
    mtext(side=3,line=0,paste(nm,"items"))
    m<-loess(z[,3]~z[,2])
    lines(m$x,predict(m),col="red")
}
dev.off()
