parfun<-function(fn,ntimes=4,nmax=10000,nmin=500) {
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
    if (length(id)<nmin) return(NULL)
    ##
    x$gr<-sample(1:ntimes,nrow(x),replace=TRUE)
    x.hold<-x
    omega<-numeric()
    for (i in 1:ntimes) {
        x<-x.hold
        x$oos<-ifelse(x$gr==i,1,0)
        ##
        x1<-twopl(x)
        errtest<-try(x2<-twopl.2f(x,return.model=TRUE))
        ##
        if (length(x2)>0) {
            ##
            m<-x2[[2]]
            co<-coef(m)
            rho<-co$GroupPars[4]
            co<-co[-length(co)]
            m<-do.call("rbind",co)
            load.cor<-cor(m[,1],m[,2])
            ##
            x2<-x2[[1]]
            mad<-mean(abs(x2$th1-x2$th2))
            x1<-x1[,c("item","id","p","resp")]
            names(x1)[3]<-"p1"
            x2<-x2[,c("item","id","p")]
            names(x2)[3]<-"p2"
            x<-merge(x1,x2)
            omega[i]<-imv(x,p1="p1",p2="p2")
        } else {
            omega[i]<-NA
            mad<-NA
        }
    }
    return(c(fn,imv=mean(omega),n=length(unique(x.hold$id)),mad=mad,rho=rho,load=load.cor))
}

library(parallel)
setwd("/scratch/users/bdomingu/imv_irt/irw")

                                        #get lf
source("/home/users/bdomingu/imv_irw/filelist.R")
#tab<-mclapply(lf,parfun,mc.cores=20,mc.set.seed=8160310,nmax=25000)
#save(tab,file="/scratch/users/bdomingu/imv_irt/mirt.Rdata")

#exclude<-c("eurpar2_mudfold.Rdata","state_c3_2007_7_responses.Rdata")
#lf<-lf[!(lf %in% exclude)]

tab<-list()
for (i in 67:89) {#for (i in 1:length(lf)) {
    print(lf[i])
    tab[[i]]<-parfun(lf[i],nmax=25000)
                                        #save(tab,file="/scratch/users/bdomingu/imv_irt/mirt.Rdata")
    dump("tab","")
}

save(tab,file="mirt.Rdata")

z<-do.call("rbind",tab)

load("mirt.Rdata")
z<-sapply(tab,class)
z<-tab[z!='try-error']
