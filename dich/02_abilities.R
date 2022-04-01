
parfun<-function(fn,ntimes=4,nmax=10000) {
    #source("/home/bd/Dropbox/projects/irt_meta/src/00_funs.R")
    source("00_funs.R")
    print(fn)
    load(fn)
    x<-x[!is.na(x$resp),]
    ##
    id<-unique(x$id)
    if (length(id)>nmax) x<-x[x$id %in% sample(id,nmax),]
    ##
    x$gr<-sample(1:ntimes,nrow(x),replace=TRUE)
    x.hold<-x
    omega<-list()
    for (i in 1:ntimes) {
        x<-x.hold
        x$oos<-ifelse(x$gr==i,1,0)
        ##
        mm<-by(x$resp,x$id,mean,na.rm=TRUE)
        ids<-names(mm[mm>0 & mm<1])
        x<-x[x$id %in% ids,]
        ##ctt p-value
        x0<-x[x$oos==0,]
        z<-by(x0$resp,x0$item,mean,na.rm=TRUE)
        z<-data.frame(item=names(z),p.ctt=as.numeric(z))
        x<-merge(x,z)
        ##
        tr<-list()
        ##
        xr<-rasch(x)
        xra<-rasch(x,ability="ML")
        xra<-xra[is.finite(xra$th),]
        xra<-xra[abs(xra$th)<7,]
        xra<-xra[,c("item","id","p")]
        names(xra)[3]<-"pml"
        xra<-xra[!is.na(xra$pml),]
        xra<-merge(xr,xra)
        tr$rasch<-imv(xra,p1="p",p2="pml") 
        xra<-rasch(x,ability="WLE")
        xra<-xra[is.finite(xra$th),]
        xra<-xra[abs(xra$th)<7,]
        xra<-xra[,c("item","id","p")]
        names(xra)[3]<-"wml"
        xra<-xra[!is.na(xra$wml),]
        xra<-merge(xr,xra)
        tr$rasch.wl<-imv(xra,p1="p",p2="wml") 
        ##
        x2<-twopl(x)
        x2a<-twopl(x,ability="ML")
        x2a<-x2a[is.finite(x2a$th),]
        x2a<-x2a[abs(x2a$th)<7,]
        x2a<-x2a[,c("item","id","p")]
        names(x2a)[3]<-"pml"
        x2a<-x2a[!is.na(x2a$pml),]
        x2a<-merge(x2,x2a)
        tr$two<-imv(x2a,p1="p",p2="pml") 
        x2a<-twopl(x,ability="WLE")
        x2a<-x2a[is.finite(x2a$th),]
        x2a<-x2a[abs(x2a$th)<7,]
        x2a<-x2a[,c("item","id","p")]
        names(x2a)[3]<-"wml"
        x2a<-x2a[!is.na(x2a$wml),]
        x2a<-merge(x2,x2a)
        tr$two.wl<-imv(x2a,p1="p",p2="wml") 
        ##
        #x3<-threepl(x)
        omega[[i]]<-unlist(tr)
    }
    tab<-c(
        length(unique(x.hold$id)),
        length(unique(x.hold$item)),
        mean(x.hold$resp,na.rm=TRUE),
        colMeans(do.call("rbind",omega))
    )
    print(fn)
    tab
}

#source("/home/bd/Dropbox/projects/irt_meta/src/0_filenames.R")
source("0_filenames.R")
library(parallel)

set.seed(8610310)
tab<-mclapply(filenames,parfun,mc.cores=25)
#tab<-lapply(filenames,parfun)
dump("tab","")

tab0<-do.call("rbind",tab)


library(xtable)
print(xtable(tab0[,c(4:7)],digits=2,display=c("f","e","e","e","e")))
