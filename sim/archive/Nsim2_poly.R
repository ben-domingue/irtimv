parfun<-function(arg,sim) {
    ##
    for (i in 1:length(arg)) assign(names(arg)[i],arg[[i]])
    ##
    irt<-function(x,mod) { ###re beta prior on guessing param https://groups.google.com/g/mirt-package/c/8Usx53BoXyw
        ##
        resp<-makeresponse(x)
        library(mirt)
        index<-grep("id",names(resp))
        ni<-ncol(resp)-1
        m<-mirt(resp[,-index],1,mod)
        list(resp,m)
    }
    x2<-sim(10*np,K=K,J=J)
    x2$id.item<-paste(x2$id,x2$item)
    ##get x1 subsample
    ids<-sample(unique(x2$id),np)
    x1<-x2[x2$id %in% ids,]
    ##
    xx<-list(x1,x2)
    z<-list()
    for (i in 1:length(xx)) {
        x<-xx[[i]]
        m<-irt(x,mod=mod)
        resp<-m[[1]]
        index<-grep("id",colnames(resp))
        id<-resp[,index]
        resp<-resp[,-index]
        th.est<-fscores(m[[2]],response.pattern=resp)
        index<-grep("^F",colnames(th.est))
        th<-data.frame(id=id,th=th.est[,index])
        ##
        tmp<-list()
        for (j in unique(x$item)) {
            item<-extract.item(m[[2]],j)
            pr<-probtrace(item,th$th)
            pr<-data.frame(pr)
            names(pr)<-paste('g',i,"_",0:(K-1),sep='')
            tmp[[j]]<-data.frame(th,pr,item=j)
        }
        tmp<-data.frame(do.call("rbind",tmp))
        tmp$th<-NULL
        z[[i]]<-tmp
    }
    x<-merge(xx[[1]],z[[1]])
    x<-merge(x,z[[2]])
    ##
    L<-split(x,x$item)
    pctt.tab<-sapply(L,function(x) table(x$resp)/nrow(x))
    ##
    pr<-x[,paste("truep",0:(K-1),sep='')]
    resp<-numeric()
    for (i in 1:nrow(pr)) resp[i]<-which(rmultinom(1,1,pr[i,])[,1]>0)-1
    x$resp<-resp
    ##experimenting
    L<-split(x,x$item)
    f<-function(y,pctt.tab) {
        nn<-length(unique(y$resp))
        om<-om0<-numeric()
        for (ii in 0:(nn-1)) {
            resp<-ifelse(y$resp==ii,1,0)
            ##irt p-values for being below ii
            p2<-y[[paste("g1_",ii,sep='')]]
            p3<-y[[paste("g2_",ii,sep='')]]
            p0<-y[[paste("truep",ii,sep='')]]
            ##
            z<-data.frame(resp=resp,p2=p2,p3=p3,p0=p0)
            om[ii+1]<-imv(z,p1="p2",p2="p3")
            om0[ii+1]<-imv(z,p1="p2",p2="p0")
        }
        c(om=mean(om*pctt.tab),om0=mean(om0*pctt.tab))
    }
    om<-list()
    for (i in 1:length(L)) om[[i]]<-f(L[[i]],pctt.tab[,i])
    om<-colMeans(do.call("rbind",om))
    c(unlist(arg),om)
}
simfun_graded<-function(np,a=1,J=15,K=5) {
    th<-rnorm(np)
    ##graded
    L<-list()
    for (j in 1:J) { #over items
        md<-0
        while (md<0.1) {
            b<-sort(rnorm(K-1))
            md<-min(diff(b))
        }
        p<-list()
        for (i in 1:length(b)) {
            k<-a*th-b[i]
            p[[i]]<-1/(1+exp(-k))
        }
        p<-do.call("cbind",p)
        p0<-1-p[,1]
        for (i in 1:(ncol(p)-1)) p[,i]<-p[,i]-p[,i+1]
        p<-cbind(p0,p)
        colnames(p)<-paste("truep",0:(ncol(p)-1),sep='')
        resp<-numeric()
        for (i in 1:np) resp[i]<-which(rmultinom(1,1,p[i,])[,1]>0)-1
        L[[j]]<-data.frame(item=paste0("item_",10+j),id=1:np,resp=resp,p)
    }
    data.frame(do.call("rbind",L))
}
##
simfun_pcm<-function(np,a=NULL,J=15,K=5) {
    th<-rnorm(np)
    if (is.null(a)) a<-rep(1,J)
    L<-list()
    for (j in 1:J) { #over items
        md<-0
        while (md<0.1) {
            b<-rnorm(K-1)
            md<-min(diff(b))
        }
        psi<-list()
        psi[[1]]<-rep(1,length(th))
        for (k in 1:(K-1)) {
            kern<-k*th-sum(b[1:k])
            psi[[k+1]]<-exp(a[j]*kern)
        }
        psi<-do.call("cbind",psi)
        den<-rowSums(psi)
        p<-psi/den
        colnames(p)<-paste("truep",0:(ncol(p)-1),sep='')
        resp<-numeric()
        for (i in 1:np) resp[i]<-which(rmultinom(1,1,p[i,])[,1]>0)-1
        L[[j]]<-data.frame(item=paste0("item_",10+j),id=1:np,resp=resp,p)
    }
    data.frame(do.call("rbind",L))
}





f<-function(nn=250,mod,J=10,K=5) {
    n<-runif(nn,min=log10(250),max=log10(5000))
    z<-expand.grid(round(10^n),J=J,K=K)
    argvals<-list()
    for (i in 1:nrow(z)) argvals[[i]]<-list(np=z[i,1],J=z[i,2],K=z[i,3],mod=mod)
    argvals
}

library(irtimv)
set.seed(867.5309^2)
library(parallel)
tab<-list()
for (J in c(5,10,20)) {
    argvals<-f(mod="graded",J=J)
    tab1<-mclapply(argvals,parfun,sim=simfun_graded,mc.cores=28)
    argvals<-f(mod="gpcm",J=J)
    tab2<-mclapply(argvals,parfun,sim=simfun_pcm,mc.cores=28)
    tab[[as.character(J)]]<-list(tab1,tab2)
}

save(tab,file="Nsim2_poly.Rdata")



##no log
pf<-function(tab) {
    cols<-c("blue","red")
    vals<-seq(0,.015,by=.005)
    plot(NULL,xlim=log10(c(100,10000)),
         ylim=range(vals),xaxt='n',
         yaxt='n',ylab="IMV(N,10N)",xlab="N respondents")
    for (ii in 1:length(tab)) {
        x1<-do.call("rbind",tab[[ii]])
        x1<-data.frame(x1)
        for (nm in c("np","J","K","om","om0")) x1[[nm]]<-as.numeric(x1[[nm]])
        z<-x1
        ##new
        z$ln<-log10(z$np)
        for (i in 1:length(vals)) abline(h=vals[i],lwd=.5,col='gray')
        xv<-c(100,2500,5000)
        axis(side=1,at=log10(xv),xv)
        axis(side=2,line=0,at=vals,vals)
        m<-loess(om0~ln,z)
        tmp<-cbind(m$x,m$fitted)
        tmp<-tmp[order(tmp[,1]),]
        lines(tmp,col=cols[ii],lty=2)
        text(tmp[nrow(tmp),1],tmp[nrow(tmp),2],unique(x1$mod),cex=.7,col=cols[ii],pos=4)
        ##
        m<-loess(om~ln,z)
        tmp<-cbind(m$x,m$fitted)
        tmp<-tmp[order(tmp[,1]),]
        lines(tmp,col=cols[ii],lty=1)
        text(tmp[nrow(tmp),1],tmp[nrow(tmp),2],unique(x1$mod),cex=.7,col=cols[ii],pos=4)
        if (ii==1 & unique(x1$J)==5) text(log10(2500),.0125,'Dashed lines represent IMV values\nfor true response probabilities\nrelative to estimates',cex=.7)
        mtext(side=3,line=0,paste(unique(x1$J),"items"))
    }
}

load("Nsim2_poly.Rdata")
pdf("/home/bd/Dropbox/Apps/Overleaf/IMV_IRT/nsim_poly.pdf",width=7,height=3)
par(mfrow=c(1,3),
    mgp=c(2,1,0),bty='n',cex.axis=.7,mar=c(3,3,1.3,.1),oma=rep(.3,4))
lapply(tab,pf)
dev.off()

