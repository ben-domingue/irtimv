## ##the imv is now whether you are above or below a given score

## parfun<-function(args,simfun) {
##     library(irtimv)
##     simfun_graded<-function(np,a=1,J=15,K=5) {
##         th<-rnorm(np)
##         ##graded
##         L<-list()
##         for (j in 1:J) { #over items
##             md<-0
##             while (md<0.1) {
##                 b<-sort(rnorm(K-1))
##                 md<-min(diff(b))
##             }
##             p<-list()
##             for (i in 1:length(b)) {
##                 k<-a*th-b[i]
##                 p[[i]]<-1/(1+exp(-k))
##             }
##             p<-do.call("cbind",p)
##             p0<-1-p[,1]
##             for (i in 1:(ncol(p)-1)) p[,i]<-p[,i]-p[,i+1]
##             p<-cbind(p0,p)
##             colnames(p)<-paste("truep",0:(ncol(p)-1),sep='')
##             resp<-numeric()
##             for (i in 1:np) resp[i]<-which(rmultinom(1,1,p[i,])[,1]>0)-1
##             L[[j]]<-data.frame(item=paste0("item_",10+j),id=1:np,resp=resp,p)
##         }
##         data.frame(do.call("rbind",L))
##     }
##     ##
##     simfun_pcm<-function(np,a=NULL,J=15,K=5) {
##         th<-rnorm(np)
##         if (is.null(a)) a<-rep(1,J)
##         L<-list()
##         for (j in 1:J) { #over items
##             md<-0
##             while (md<0.1) {
##                 b<-sort(rnorm(K-1))
##                 md<-min(diff(b))
##             }
##             psi<-list()
##             psi[[1]]<-rep(1,length(th))
##             for (k in 1:(K-1)) {
##                 kern<-k*th-sum(b[1:k])
##                 psi[[k+1]]<-exp(a[j]*kern)
##             }
##             psi<-do.call("cbind",psi)
##             den<-rowSums(psi)
##             p<-psi/den
##             colnames(p)<-paste("truep",0:(ncol(p)-1),sep='')
##             resp<-numeric()
##             for (i in 1:np) resp[i]<-which(rmultinom(1,1,p[i,])[,1]>0)-1
##             L[[j]]<-data.frame(item=paste0("item_",10+j),id=1:np,resp=resp,p)
##         }
##         data.frame(do.call("rbind",L))
##     }
##     if (simfun=="graded") sim<-simfun_graded
##     if (simfun=="pcm") sim<-simfun_pcm
##     ##
##     J<-args[[3]]
##     np<-args[[2]]
##     K<-args[[1]]
##     ##
##     library(mirt)
##     irt<-function(x,mod) { ###re beta prior on guessing param https://groups.google.com/g/mirt-package/c/8Usx53BoXyw
##         ##
##         resp<-makeresponse(x)
##         index<-grep("id",names(resp))
##         ni<-ncol(resp)-1
##         m<-mirt(resp[,-index],1,mod)
##         list(resp,m)
##     }
##     ##
##     x<-sim(np,K=K,J=J)
##     ##
##     z<-list()
##     for (me in c("graded","gpcm")) {
##         m<-irt(x,mod=me)
##         if (me=="graded") {
##             resp<-m[[1]]
##             index<-grep("id",colnames(resp))
##             id<-resp[,index]
##             resp<-resp[,-index]
##         }
##         th.est<-fscores(m[[2]],response.pattern=resp)
##         index<-grep("^F",colnames(th.est))
##         th<-data.frame(id=1:nrow(th.est),th=th.est[,index])
##         ##
##         tmp<-list()
##         for (j in unique(x$item)) {
##             item<-extract.item(m[[2]],j)
##             pr<-probtrace(item,th$th)
##             pr<-data.frame(pr)
##             names(pr)<-paste(me,0:(K-1),sep='')
##             tmp[[j]]<-data.frame(th,pr,item=j)
##         }
##         tmp<-data.frame(do.call("rbind",tmp))
##         tmp$th<-NULL
##         z[[me]]<-tmp
##     }
##     x<-merge(x,z[[1]])
##     x<-merge(x,z[[2]])
##     ##
##     L<-split(x,x$item)
##     pctt.tab<-lapply(L,function(x) table(x$resp)/nrow(x))
##     ##
##     x$resp0<-x$resp
##     pr<-x[,paste("truep",0:(K-1),sep='')]
##     resp<-numeric()
##     for (i in 1:nrow(pr)) resp[i]<-which(rmultinom(1,1,pr[i,])[,1]>0)-1
##     x$resp<-resp
##     ########################
##     ## imv category
##     L<-split(x,x$item)
##     f<-function(y,pctt.tab) {
##         nn<-length(unique(y$resp))
##         oracle<-overfit<-om<-om0<-numeric()
##         iis<-0:(nn-1)
##         for (ii in iis) {
##             ns<-oracle.tmp<-overfit.tmp<-om.tmp<-om0.tmp<-numeric()
##             jjs<-iis[-match(ii,iis)]
##             for (jj in jjs) {
##                 y2<-y[y$resp %in% c(ii,jj),]
##                 resp<-ifelse(y2$resp==ii,1,0)
##                 resp0<-ifelse(y2$resp0==ii,1,0)
##                 ##irt p-values for being 
##                 p2.ii<-y2[[paste("graded",ii,sep='')]]
##                 p2.jj<-y2[[paste("graded",jj,sep='')]]
##                 p3.ii<-y2[[paste("gpcm",ii,sep='')]]
##                 p3.jj<-y2[[paste("gpcm",jj,sep='')]]
##                 true.ii<-y2[[paste("truep",ii,sep='')]]
##                 true.jj<-y2[[paste("truep",jj,sep='')]]
##                 p0<-mean(resp0) #BADBADBAD
##                 ##
##                 z<-data.frame(resp=resp,resp0=resp0,
##                               p2=p2.ii/(p2.ii+p2.jj),p3=p3.ii/(p3.ii+p3.jj),true=true.ii/(true.ii+true.jj),
##                               p0=p0)
##                 j0<-as.character(jj)
##                 if (simfun=="pcm") {
##                     om0.tmp[j0]<-imv(z,p1='p0',p2='p3')
##                     om.tmp[j0]<-imv(z,p1="p2",p2="p3")
##                     oracle.tmp[j0]<-imv(z,"p3","true")
##                     z$resp<-z$resp0
##                     overfit.tmp[j0]<-imv(z,"p3","true")
##                 }
##                 if (simfun=="graded") {
##                     om0.tmp[j0]<-imv(z,p1='p0',p2='p2')
##                     om.tmp[j0]<-imv(z,p1="p3",p2="p2")
##                     oracle.tmp[j0]<-imv(z,"p2","true")
##                     z$resp<-z$resp0
##                     overfit.tmp[j0]<-imv(z,"p2","true")
##                 }
##                 ns[as.character(jj)]<-nrow(z)
##             }
##             om0[ii+1]<-sum(om0.tmp*ns)/sum(ns)
##             om[ii+1]<-sum(om.tmp*ns)/sum(ns)
##             oracle[ii+1]<-sum(oracle.tmp*ns)/sum(ns)
##             overfit[ii+1]<-sum(overfit.tmp*ns)/sum(ns)
##         }
##         c(sum(om*pctt.tab),sum(oracle*pctt.tab),sum(overfit*pctt.tab),sum(om0*pctt.tab))/sum(pctt.tab)
##     }
##     om<-list()
##     for (i in 1:length(L)) om[[i]]<-f(L[[i]],pctt.tab[[i]])
##     om<-colMeans(do.call("rbind",om))
##     om1<-c(unlist(args),om)
##     ############################
##     ##cumulative imv
##     L<-split(x,x$item)
##     f<-function(y,pctt.tab) {
##         nn<-length(unique(y$resp))
##         oracle<-overfit<-om<-om0<-numeric()
##         for (ii in 0:(nn-2)) {
##             resp<-ifelse(y$resp<=ii,1,0)
##             resp0<-ifelse(y$resp0<=ii,1,0)
##             ##irt p-values for being below ii
##             p2<-rowSums(y[paste("graded",0:ii,sep='')])
##             p3<-rowSums(y[paste("gpcm",0:ii,sep='')])
##             true<-rowSums(y[paste("truep",0:ii,sep='')])
##             p0<-mean(resp0)
##             ##
##             z<-data.frame(resp=resp,p2=p2,p3=p3,true=true,resp0=resp0,p0=p0)
##             if (simfun=="pcm") {
##                 om0[ii+1]<-imv(z,p1='p0',p2='p3')
##                 om[ii+1]<-imv(z,p1="p2",p2="p3")
##                 oracle[ii+1]<-imv(z,"p3","true")
##                 z$resp<-z$resp0
##                 overfit[ii+1]<-imv(z,"p3","true")
##             }
##             if (simfun=="graded") {
##                 om0[ii+1]<-imv(z,p1='p0',p2='p2')
##                 om[ii+1]<-imv(z,p1="p3",p2="p2")
##                 oracle[ii+1]<-imv(z,"p2","true")
##                 z$resp<-z$resp0
##                 overfit[ii+1]<-imv(z,"p2","true")
##             }
##         }
##         ##
##         pctt.tab<-pctt.tab[1:(nn-1)]/(1-pctt.tab[nn])
##         ##
##         c(sum(om*pctt.tab),sum(oracle*pctt.tab),sum(overfit*pctt.tab),sum(om0*pctt.tab))/sum(pctt.tab)
##     }
##     om<-list()
##     for (i in 1:length(L)) om[[i]]<-f(L[[i]],pctt.tab[[i]])
##     om<-colMeans(do.call("rbind",om))
##     om2<-c(unlist(args),om)
##     ##
##     list(om.cat=om1,om.cum=om2)
## }


## K<-c(3,5,7)
## J<-c(10,20,40)
## np<-c(1000)
## z<-expand.grid(1:100,K,np,J)
## argvals<-list()
## for (i in 1:nrow(z)) argvals[[i]]<-list(z[i,2],z[i,3],z[i,4])

## set.seed(867.5309^2)
## library(parallel)
## tab1<-mclapply(argvals,parfun,mc.cores=3,simfun="graded")
## tab2<-mclapply(argvals,parfun,mc.cores=3,simfun="pcm")


## f<-function(tab,index) {
##     z<-lapply(tab,"[[",index)
##     z<-data.frame(do.call("rbind",z))
##     names(z)<-c("K","n","J","om1","oracle","overfit","om0")
##     z
## }
## t1a<-f(tab1,1)
## t1b<-f(tab1,2)
## t2a<-f(tab2,1)
## t2b<-f(tab2,2)
## tab<-tab<-list(t1a=t1a,t2a=t2a,t1b=t1b,t2b=t2b)

## save(tab,file="misfit_poly_cumul.Rdata")


## #########################################################

## load("misfit_poly_cumul.Rdata")


## f<-function(z) {
##     zz<-split(z,paste(z$n,z$K,z$J))
##     zz<-lapply(zz,colMeans)
##     z<-data.frame(do.call("rbind",zz))
##     z<-z[order(z$K,z$n,z$J),]
## }
## t1a<-f(tab$t1a)
## t1b<-f(tab$t1b)
## t2a<-f(tab$t2a)
## t2b<-f(tab$t2b)


## pdf("/home/bd/Dropbox/Apps/Overleaf/IMV_IRT/poly_ctt.pdf",width=8,height=3)
## par(mfrow=c(1,2),mgp=c(2,1,0),mar=c(4,3,.1,.1),bty='n',oma=rep(.3,4))
## ##
## pf<-function(t1a,t1b) {
##     z<-t1a
##     plot(NULL,xlim=c(1-3,nrow(z)),ylim=c(0,.6),xlab="",xaxt="n",ylab="IMV(CTT,IRT)")
##     #for (i in seq(-.025,.025,by=.025)) abline(h=i,lwd=1,col='gray')
##     axis(side=1,at=1:nrow(z),labels=rep("",nrow(z)))
##     mtext(side=1,at=1:nrow(z),z$J,line=.75,cex=.8)
##     mtext(side=1,line=.75,at=1-1.3,"J",cex=.8)
##     #mtext(side=1,at=1:nrow(z),z$n,line=1.75,cex=.8)
##     #mtext(side=1,line=1.75,at=1-1.3,"N",cex=.8)
##     mtext(side=1,at=1:nrow(z),z$K,line=1.75,cex=.8)
##     mtext(side=1,line=1.75,at=1-1.3,"K",cex=.8)
##     ##
##     lines(1:nrow(t1a),t1a$om0,type='b',col='blue',lwd=2,pch=19)
##     text(0,t1a$om0[1],expression(omega[c]),col='blue')
##     lines(1:nrow(t1b),t1b$om0,type='b',col='red',lwd=2,pch=19)
##     text(0,t1b$om0[1],expression(omega[t]),col='red')
## }
## pf(t1a,t1b)
## legend("topleft",bty='n','DGM=GRM')
## pf(t2a,t2b)
## legend("topleft",bty='n','DGM=PCM')
## dev.off()

## pdf("/home/bd/Dropbox/Apps/Overleaf/IMV_IRT/misfit_poly.pdf",width=8,height=6)
## par(mfrow=c(2,2),mgp=c(2,1,0),mar=c(4,3,.1,.1),bty='n',oma=rep(.5,4))
## ###############################
## ##
## pf<-function(t1a,t1b) {
##     z<-t1a
##     plot(NULL,xlim=c(1-3,nrow(z)),ylim=c(0,.006),xlab="",xaxt="n",ylab="")
##     for (i in seq(-.025,.025,by=.025)) abline(h=i,lwd=1,col='gray')
##     axis(side=1,at=1:nrow(z),labels=rep("",nrow(z)))
##     mtext(side=1,at=1:nrow(z),z$J,line=.75,cex=.8)
##     mtext(side=1,line=.75,at=1-1.3,"J",cex=.8)
##     mtext(side=1,at=1:nrow(z),z$K,line=1.75,cex=.8)
##     mtext(side=1,line=1.75,at=1-1.3,"K",cex=.8)
##     ##
##     lines(1:nrow(t1a),t1a$om1,type='b',col='blue',lwd=2,pch=19)
##     lines(1:nrow(t1b),t1b$om1,type='b',col='red',lwd=2,pch=19)
##     text(0,t1a$om1[1],expression(omega[c]),col='blue')
##     text(0,t1b$om1[1],expression(omega[t]),col='red')
## }
## pf(t1a,t1b)
## legend("topleft",bty='n','DGM=GRM')
## mtext(side=2,line=2,"IMV(PCM,GRM)")
## pf(t2a,t2b)
## legend("topleft",bty='n','DGM=PCM')
## mtext(side=2,line=2,"IMV(GRM,PCM)")
## ##
## pf<-function(t1a,t1b) {
##     z<-t1a
##     plot(NULL,xlim=c(1-3,nrow(z)),ylim=c(-.05,.05),xlab="",xaxt="n",ylab="")
##     for (i in seq(-.05,.05,by=.025)) abline(h=i,lwd=1,col='gray')
##     axis(side=1,at=1:nrow(z),labels=rep("",nrow(z)))
##     mtext(side=1,at=1:nrow(z),z$J,line=.75,cex=.8)
##     mtext(side=1,line=.75,at=1-1.3,"J",cex=.8)
##     mtext(side=1,at=1:nrow(z),z$K,line=1.75,cex=.8)
##     mtext(side=1,line=1.75,at=1-1.3,"K",cex=.8)
##     ##
##     lines(1:nrow(t1a),t1a$oracle,type='b',col='blue',lwd=2,pch=19)
##     lines(1:nrow(t1b),t1b$oracle,type='b',col='red',lwd=2,pch=19)
##     text(0,t1a$oracle[1],expression(omega[c]),col='blue')
##     text(0,t1b$oracle[1],expression(omega[t]),col='red')
##     lines(1:nrow(t1a),t1a$overfit,type='b',col='blue',lwd=2,pch=19,lty=2)
##     lines(1:nrow(t1b),t1b$overfit,type='b',col='red',lwd=2,pch=19,lty=2)
##     text(0,t1a$overfit[1],expression(omega[c]),col='blue')
##     text(0,t1b$overfit[1],expression(omega[t]),col='red')
## }
## pf(t1a,t1b)
## legend("topleft",bty='n','DGM=GRM')
## legend("bottomright",bty='n',lty=1:2,c("Oracle","Overfit"))
## mtext(side=2,line=2,"IMV")
## pf(t2a,t2b)
## legend("topleft",bty='n','DGM=PCM')
## mtext(side=2,line=2,"IMV")
## dev.off()
