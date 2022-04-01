setwd("/home/bd/Dropbox/projects/rt_meta/data/0_raw/coomans")
lf<-c("addition.RData","division.RData","letterchaos.RData","multiplication.RData","subtraction.RData")


L<-list()
for (fn in lf) {
    load(fn)
    x<-res_max2
    x<-x[order(x$days),]
    id<-paste(x$user_id,x$item_id)
    x<-x[!duplicated(id),]
    ##
    x<-x[,c("user_id","item_id","response_in_milliseconds","correct_answered","days")]
    names(x)<-c("id","item","rt","resp","days")
    x$id<-paste(x$id,x$days) #round(x$days/10))
    #x$id<-paste(x$id,x$days)
    #tab<-table(x$id)
    #x<-x[x$id %in% names(tab)[tab>9],]
    #print(table(x$resp))
    #print(table(table(x$item)))
    #print(table(x$id))
    #hist(x$rt)
    x$rt<-x$rt/1000
    print(table(x$rt<20))
    x$rt<-log(x$rt)
    L[[sub(".RData","",fn,fixed=TRUE)]]<-x
}
bigL<-L
rm("L")

data.frame(rbind(bigL$addition,bigL$subtraction))->bigL[['add.subtract']]
data.frame(rbind(bigL$multiplication,bigL$division))->bigL[['multiply.divide']]
for (ii in 1:length(bigL)) {
    x<-bigL[[ii]]
    x$days<-NULL
    save(x,file=paste("/home/bd/Dropbox/projects/irt_meta/data/",names(bigL)[ii],".Rdata",sep=''))
}
