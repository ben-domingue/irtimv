## parfun<-function(arg,numitems=50,method="EAP") {
##                                         #source("/home/bd/Dropbox/projects/irt_meta/src/00_funs.R")
##     source("00_funs.R")
##     mod<-arg[[1]]
##     np<-arg[[2]]
##     simfun<-function(np,a,b,c,theta=FALSE) {
##         th<-rnorm(np)
##         p<-outer(th,b,"-")
##         for (i in 1:ncol(p)) p[,i]<-a[i]*p[,i]
##         for (i in 1:ncol(p)) p[,i]<-c[i]+(1-c[i])*1/(1+exp(-p[,i]))
##         resp<-p
##         for (i in 1:ncol(resp)) resp[,i]<-rbinom(nrow(resp),1,p[,i])
##         L<-list()
##         for (i in 1:ncol(resp)) L[[i]]<-data.frame(item=paste0("item_",i+10),id=1:nrow(resp),resp=as.numeric(resp[,i]),truep=as.numeric(p[,i]))
##         x<-data.frame(do.call("rbind",L))
##         if (theta) list(x,th) else x
##     }
##     rmse<-function(x,y) sqrt(mean((x-y)^2))
##     irt<-function(x,mod) { ###re beta prior on guessing param https://groups.google.com/g/mirt-package/c/8Usx53BoXyw
##         ##
##         resp<-makeresponse(x)
##         library(mirt)
##         index<-grep("id",names(resp))
##         ni<-ncol(resp)-1
##         if (mod=="Rasch") {
##             m<-mirt(resp[,-index],1,"Rasch")
##         }
##         if (mod=="2PL") {
##             s<-paste("F=1-",ni,"\nPRIOR = (1-",ni,", a1, lnorm, 0.2, 0.2)",
##                      sep="") 
##             model<-mirt.model(s)
##             test<-try(m<-mirt(resp[,-index],model,itemtype=rep("2PL",ni),method="EM",technical=list(NCYCLES=10000)))
##         }
##         if (mod=="3PL") {
##             s<-paste("F=1-",ni,"\nPRIOR = (1-",ni,", a1, lnorm, 0.2, 0.2),(1-",ni,", g, expbeta, 2, 17)",
##                      sep="") 
##             model<-mirt.model(s)
##             test<-try(m<-mirt(resp[,-index],model,itemtype=rep(mod,ni),method="EM",technical=list(NCYCLES=10000)))
##         }
##         list(resp,m)
##     }
##     ##
##     b<-rnorm(numitems)
##     a<-rep(1,numitems)
##     c<-rep(0,numitems)
##     if (mod=="3PL") c<-rbeta(numitems,2,17)
##     if (mod %in% c("2PL","3PL")) a<-rlnorm(numitems,.2,.2) #exp(rnorm(numitems,sd=.3))
##     ##
##     x1<-simfun(np,a=a,b=b,c=c)
##     x2<-simfun(10*np,a=a,b=b,c=c)
##     x0<-simfun(1000,a=a,b=b,c=c,theta=TRUE)
##     resp0<-makeresponse(x0[[1]])
##     #
##     xx<-list(x1,x2)
##     stud<-item<-list()
##     for (i in 1:length(xx)) {
##         x<-xx[[i]]
##         m<-irt(x,mod=mod)
##         resp.nms<-colnames(m[[1]])
##         m<-m[[2]]
##         co<-coef(m)
##         nms<-names(co)
##         co<-do.call("rbind",co[-length(co)])
##         item[[i]]<-data.frame(item=nms[-length(nms)],easy=co[,2],load=co[,1],gues=co[,3])
##         #
##         resp<-resp0[,resp.nms]
##         index<-grep("id",colnames(resp))
##         id<-resp[,index]
##         rr<-resp[,-index]
##         rs<-rowSums(rr)
##         rr<-rr[rs>0 & rs<ncol(rr),]
##         th.est<-fscores(m,response.pattern=rr,method=method)
##         id<-id[rs>0 & rs<ncol(rr)]
##         #
##         index<-grep("^F",colnames(th.est))
##         stud[[i]]<-data.frame(id=id,th=th.est[,index])
##     }
##     z<-list()
##     for (i in 1:length(stud)) {
##         x<-merge(x0[[1]],stud[[i]])
##         x<-merge(x,item[[i]])
##         ##
##         kk<-x$load*x$th+x$easy
##         kk<-exp(kk)
##         x$p<-x$gues+(1-x$gues)*kk/(1+kk)
##         z[[i]]<-x[,c("item","id","p","resp","truep")]
##     }
##     x1<-z[[1]]
##     names(x1)[3]<-"p1"
##     x2<-z[[2]]
##     x2$resp<-NULL
##     x2$truep<-NULL
##     names(x2)[3]<-"p2"
##     x<-merge(x1,x2)
##     r0<-rmse(x$p1,x$p2)
##     r1<-rmse(x$p1,x$truep)
##     r2<-rmse(x$p2,x$truep)
##     omega<-imv(x,p1="p1",p2="p2")
##     omega0<-imv(x,p1="truep",p2="p1")
##     c(mod,np,omega,omega0,r0,r1,r2,method=method)
## }


## ## niter<-2
## ## mods<-rev(c("Rasch","2PL","3PL"))
## ## nvals<-c(100,500,1000,5000)# ,10000,50000)
## ## z<-expand.grid(1:niter,mods,nvals)
## ## argvals<-list()
## ## for (i in 1:nrow(z)) argvals[[i]]<-list(as.character(z[i,2]),z[i,3])

## set.seed(867.5309^2)
## mods<-rev(c("Rasch","2PL","3PL"))
## n<-runif(100,min=log10(100),max=log10(5000))
## ##hist(10^(n))
## z<-expand.grid(mods,round(10^n))
## argvals<-list()
## for (i in 1:nrow(z)) argvals[[i]]<-list(as.character(z[i,1]),z[i,2])

## library(parallel)
## tab<-list()
## tab[[as.character(200)]]<-mclapply(argvals,parfun,mc.cores=28,numitems=200)

## tab[[as.character(25)]]<-mclapply(argvals,parfun,mc.cores=28,numitems=25)
## tab[[as.character(50)]]<-mclapply(argvals,parfun,mc.cores=28,numitems=50)

## save(tab,file="Nsim.Rdata")





## ##no log
## load("Nsim.Rdata")    
## tabL<-tab[as.character(c(25,50,200))]
## pdf("/home/bd/Dropbox/Apps/Overleaf/IMV_IRT/nsim.pdf",width=8,height=4)
## par(mfrow=c(1,3),
##     mgp=c(2,1,0),bty='n',cex.axis=.7,mar=c(3,3,.1,.1),oma=rep(.3,4))
## for (ii in 1:length(tabL)) {
##     tab<-tabL[[ii]]
##     n<-sapply(tab,length)
##     tab<-tab[n==8]
##     z<-data.frame(do.call("rbind",rev(tab)))
##     names(z)<-c("m","n","omega","omega0","r0","r1","r2","method")
##     z$method<-NULL
##     ##new
##     z$n<-as.numeric(z$n)
##     z$omega<-as.numeric(z$omega)
##     z$omega0<-as.numeric(z$omega0)
##     print(table(z$omega<0))
##     z$ln<-log10(z$n)
##     L<-split(z,z$m)
##     vals<-seq(-.01,.02,by=.005)
##     plot(NULL,xlim=log10(c(100,10000)),
##          ylim=range(vals),xaxt='n',
##          yaxt='n',ylab="IMV(N,10N)",xlab="N respondents")
##     for (i in 1:length(vals)) abline(h=vals[i],lwd=.5,col='gray')
##     xv<-c(100,2500,5000)
##     axis(side=1,at=log10(xv),xv)
##     axis(side=2,line=0,at=vals,vals)
##     cols<-c("black","red","blue")
##     for (i in 1:length(L)) {
##         y<-L[[i]]
##         ##
##         m<-loess(omega0~ln,y)
##         tmp<-cbind(m$x,m$fitted)
##         tmp<-tmp[order(tmp[,1]),]
##         lines(tmp,col=cols[i],lty=2)
##         text(tmp[nrow(tmp),1],tmp[nrow(tmp),2],names(L)[i],cex=.7,col=cols[i],pos=4)
##         ##
##         m<-loess(omega~ln,y)
##         tmp<-cbind(m$x,m$fitted)
##         tmp<-tmp[order(tmp[,1]),]
##         lines(tmp,col=cols[i])
##         text(tmp[nrow(tmp),1],tmp[nrow(tmp),2],names(L)[i],cex=.7,col=cols[i],pos=4)
##     }
##     legend("topleft",bty='n',paste(names(tabL)[ii],"items"))
##     if (ii==1) text(log10(2500),.0125,'Dashed lines represent IMV values\nfor true response probabilities\nrelative to estimates',cex=.7)
## }
## dev.off()

## ## load("Nsim.Rdata")    
## ## tabL<-tab
## ## par(mfrow=c(1,3),
## ##     mgp=c(2,1,0),bty='n',cex.axis=.7,mar=c(3,3,.1,.1),oma=rep(.3,4))
## ## for (ii in 1:length(tabL)) {
## ##     tab<-tabL[[ii]]
## ##     n<-sapply(tab,length)
## ##     tab<-tab[n==8]
## ##     z<-data.frame(do.call("rbind",rev(tab)))
## ##     names(z)<-c("m","n","omega","omega0","r0","r1","r2","method")
## ##     z$method<-NULL
## ##     ##new
## ##     z$n<-as.numeric(z$n)
## ##     z$omega<-as.numeric(z$omega)
## ##     z$omega0<-as.numeric(z$omega0)
## ##     print(table(z$omega<0))
## ##     z$l<-log10(z$omega)
## ##     z$l0<-log10(z$omega0)
## ##     z$ln<-log10(z$n)
## ##     L<-split(z,z$m)
## ##     vals<-c(.1,.01,.001,.0001,.00001)
## ##     plot(NULL,xlim=log10(c(100,10000)),
## ##          ylim=range(log10(vals)),xaxt='n',
## ##          yaxt='n',ylab="IMV(N,10N)",xlab="N respondents")
## ##     for (i in 1:length(vals)) abline(h=log10(vals[i]),lwd=.5,col='gray')
## ##     xv<-c(100,seq(1000,5000,by=1000))
## ##     axis(side=1,at=log10(xv),xv)
## ##     axis(side=2,line=0,at=log10(vals),vals)
## ##     cols<-c("black","red","blue")
## ##     for (i in 1:length(L)) {
## ##         y<-L[[i]]
## ##         ##
## ##         m<-loess(l0~ln,y)
## ##         tmp<-cbind(m$x,m$fitted)
## ##         tmp<-tmp[order(tmp[,1]),]
## ##         lines(tmp,col=cols[i],lty=2)
## ##         text(tmp[nrow(tmp),1],tmp[nrow(tmp),2],names(L)[i],cex=.7,col=cols[i],pos=4)
## ##         ##
## ##         m<-loess(l~ln,y)
## ##         tmp<-cbind(m$x,m$fitted)
## ##         tmp<-tmp[order(tmp[,1]),]
## ##         lines(tmp,col=cols[i])
## ##         text(tmp[nrow(tmp),1],tmp[nrow(tmp),2],names(L)[i],cex=.7,col=cols[i],pos=4)
## ##     }
## ## }


## ## ##old
## ## L<-split(z,list(z$m,z$n))
## ## f<-function(x) colMeans(apply(x[,-1],2,as.numeric))
## ## L<-lapply(L,f)
## ## z<-data.frame(do.call("rbind",L))
## ## z$m<-sapply(strsplit(rownames(z),".",fixed=TRUE),"[",1)
## ## z$n<-as.numeric(z$n)
## ## z$omega<-as.numeric(z$omega)
## ## z$omega0<-as.numeric(z$omega0)
## ## z$l<-log10(z$omega)
## ## z$l0<-log10(z$omega0)
## ## z$ln<-log10(z$n)
## ## z<-z[order(z$n),]
## ## L<-split(z,z$m)
## ## #pdf("/home/bd/Dropbox/Apps/Overleaf/IMV_IRT/nsim.pdf",width=8,height=4)
## ## par(mfrow=c(1,2),mgp=c(2,1,0),bty='n',cex.axis=.7,mar=c(3,3,.1,.1),oma=rep(.3,4))
## ## vals<-c(.1,.01,.001,.0001,.00001)
## ## plot(NULL,xlim=log10(c(min(z$n),2*max(z$n))),ylim=range(log10(vals)),xaxt='n',
## ##      yaxt='n',ylab="IMV(N,2N)",xlab="N respondents")
## ## for (i in 1:length(vals)) abline(h=log10(vals[i]),lwd=.5,col='gray')
## ## axis(side=1,at=log10(unique(z$n)),unique(z$n))
## ## axis(side=2,line=0,at=log10(vals),vals)
## ## cols<-c("black","red","blue")
## ## for (i in 1:length(L)) {
## ##     y<-L[[i]]
## ##     lines(y$ln,y$l,type="b",col=cols[i],pch=19)
## ##     text(y$ln[nrow(y)],y$l[nrow(y)],unique(y$m),cex=.7,col=cols[i],pos=4)
## ## }
## ## ##
## ## plot(NULL,xlim=log10(c(min(z$n),2*max(z$n))),ylim=range(log10(vals)),xaxt='n',
## ##      yaxt='n',ylab="IMV(estimates,true)",xlab="N respondents")
## ## for (i in 1:length(vals)) abline(h=log10(vals[i]),lwd=.5,col='gray')
## ## axis(side=1,at=log10(unique(z$n)),unique(z$n))
## ## axis(side=2,line=0,at=log10(vals),vals)
## ## cols<-c("black","red","blue")
## ## for (i in 1:length(L)) {
## ##     y<-L[[i]]
## ##     lines(y$ln,y$l0,type="b",col=cols[i],pch=19)
## ##     text(y$ln[nrow(y)],y$l0[nrow(y)],unique(y$m),cex=.7,col=cols[i],pos=4)
## ## }
## ## #dev.off()

## tmp<-z[z$m=="3PL" & z$n==10000,,drop=FALSE]
## tmp$r1
## tmp$omega0


## plot(z$n,z$r1,ylim=c(0,.1))
