#/scratch/users/bdomingu/imv_irt

parfun<-function(fn,ntimes=4,nmax=10000) {
    library(irtimv)
    print(fn)
    load(fn)
    ##
    nms<-unique(df$item)
    if (all(nms %in% 1:length(nms))) df$item<-paste("item_",df$item,sep='')
    ##
    df<-df[!is.na(df$resp),]
    x<-df[df$resp %in% 0:1,]
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
        x0<-x[x$oos==0,]
        ##ctt p-value
        z<-by(x0$resp,x0$item,mean,na.rm=TRUE)
        z<-data.frame(item=names(z),p.ctt=as.numeric(z))
        x<-merge(x,z)
        ##
        tr<-list()
        ##
        xr<-rasch(x)
        tr$rasch<-imv(xr,p1="p.ctt",p2="p") 
        ##
        xr$guttman<-ifelse(xr$p>.5,.99,.01)
        tr$guttman<-imv(xr,p1="guttman",p2="p")
        ##
        x2<-twopl(x)
        if (length(x2)>0) {
            x2<-x2[,c("item","id","p")]
            names(x2)[3]<-"p2"
            xr<-merge(xr,x2,)
            tr$twopl<-imv(xr,p1="p",p2="p2")
        } else tr$twopl<-NA
        ##
        x3<-threepl(x)
        if (length(x3)>0) {
            x3<-x3[,c("item","id","p")]
            names(x3)[3]<-"p3"
            xr<-merge(xr,x3)
            tr$threepl<-imv(xr,p1="p2",p2="p3")
        } else tr$threepl<-NA
        ##
        omega[[i]]<-unlist(tr)
    }
    tab<-c(
        length(unique(x.hold$id)),
        length(unique(x.hold$item)),
        mean(x.hold$resp,na.rm=TRUE),
        colMeans(do.call("rbind",omega))
    )
    c(fn,tab)
}


library(parallel)
setwd("/scratch/users/bdomingu/imv_irt/irw")

                                        #get lf
source("/home/users/bdomingu/imv_irw/filelist.R")
tab<-mclapply(lf,parfun,mc.cores=10,mc.set.seed=8160310)
#tab<-lapply(lf,parfun)

save(tab,file="/scratch/users/bdomingu/imv_irt/complexity.Rdata")
