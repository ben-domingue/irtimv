
#for (fn in filenames) {
parfun<-function(fn,ntimes=4,nmax=10000) {
    #source("/home/bd/Dropbox/projects/irt_meta/src/00_funs.R")
    source("00_funs.R")
    load(fn)
    x<-x[!is.na(x$resp),]
    ##
    id<-unique(x$id)
    if (length(id)>nmax) x<-x[x$id %in% sample(id,nmax),]
    ##
    x$gr<-sample(1:ntimes,nrow(x),replace=TRUE)
    x.hold<-x
    items<-unique(x.hold$item)
    omega<-list()
    for (i in 1:ntimes) {
        x<-x.hold
        x$oos<-ifelse(x$gr==i,1,0)
        x0<-x[x$oos==0,]
        ##ctt p-value
        z<-by(x0$resp,x0$item,mean,na.rm=TRUE)
        z<-data.frame(item=names(z),p.ctt=as.numeric(z))
        x<-merge(x,z)
        ##
        xr<-rasch(x)
        x2<-twopl(x)
        x2<-x2[,c("item","id","p")]
        names(x2)[3]<-"p2"
        x<-merge(xr,x2)
        omega.tmp<-numeric()
        for (it in items) {
            omega.tmp[as.character(it)]<-imv(x[x$item==it,],p1="p",p2="p2")
        }
        omega[[i]]<-omega.tmp
    }
    z<-do.call("cbind",omega)
    om<-rowMeans(z)
    x<-x.hold
    mm<-aggregate(x$resp,list(x$item),mean,na.rm=TRUE)
    z<-merge(mm,om,by.x=1,by.y=0)
    names(z)<-c("item","p","omega")
    mm<-aggregate(x$resp,list(x$id),mean,na.rm=TRUE)
    names(mm)<-c("id","th")
    x<-merge(x,mm)
    L<-split(x,x$item)
    disc<-lapply(L,function(x) cor(x$resp,x$th,use='p'))
    disc<-data.frame(item=names(disc),disc=as.numeric(disc))
    merge(z,disc)
}

#source("/home/bd/Dropbox/projects/irt_meta/src/0_filenames.R")
source("0_filenames.R")
library(parallel)
set.seed(8610310)
tab<-mclapply(filenames,parfun,mc.cores=25)

#save(tab,file="items.Rdata")

load("items.Rdata")
z<-do.call("rbind",tab)
##
pdf("/home/bd/Dropbox/Apps/Overleaf/IMV_IRT/items.pdf",width=8,height=3)
par(mfrow=c(1,2),mgp=c(2,1,0),bty='n',cex.axis=.7,mar=c(3,3,.1,.1),oma=rep(.3,4))
plot(abs(z$p-.5),z$omega,pch=19,cex=.4,col='blue',xlab="|p-0.5|",ylab="IMV")
abline(h=0)
plot(z$disc,z$omega,pch=19,cex=.4,col='blue',xlab="Discrimination",ylab="IMV")
abline(h=0)
dev.off()
