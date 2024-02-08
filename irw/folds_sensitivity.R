##get parfun() from 02_complexity.R

lf<-c('content_literacy_intervention.Rdata','chess_lnirt.Rdata','lsat.Rdata')

library(parallel)
L<-list()
for (fn in lf) {
    est<-list()
    for (nf in c(2,4,8,16,32)) {
        out<-mclapply(rep(fn,100),parfun,mc.cores=10,mc.set.seed=TRUE,ntimes=nf)
        out<-do.call("rbind",out)
        out<-cbind(nf,out)
        est[[paste(nf)]]<-out
    }
    L[[fn]]<-est
}

save(L,file="/home/bdomingu/Dropbox/projects/irt_meta/src/output/folds_sense.Rdata")


pdf("~/Dropbox/Apps/Overleaf/IMV_IRT/folds.pdf",width=7,height=5)

load(file="/home/bdomingu/Dropbox/projects/irt_meta/src/output/folds_sense.Rdata")
par(mfcol=c(3,3),mgp=c(2,1,0),mar=c(3,3,1,1),oma=c(.5,.5,1,.5))
for (i in 1:length(L)) {
    est<-L[[i]]
    x<-do.call("rbind",est)
    x<-x[,-2]
    x<-apply(x,2,as.numeric)
    boxplot(x[,5]~x[,1],ylim=c(0,.15),ylab="IMV(CTT,1PL)",xlab="k"); abline(h=0,lty=2,col='gray')
    abline(v=2)
    mtext(side=3,line=0,names(L)[i],cex=.7)
    boxplot(x[,7]~x[,1],ylim=c(-.008,.017),ylab="IMV(1PL,2PL)",xlab="k"); abline(h=0,lty=2,col='gray')
    abline(v=2)
    boxplot(x[,8]~x[,1],ylim=c(-.0035,.002),ylab="IMV(2PL,3PL)",xlab="k"); abline(h=0,lty=2,col='gray')
    abline(v=2)
}

dev.off()
