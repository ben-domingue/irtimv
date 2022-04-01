
library(psych)
library(irtimv)
filenames<-datafiles()
L<-list()
nmax<-10000

for (fn in filenames) {
    print(fn)
    load(fn)
    x<-x[!is.na(x$resp),]
    id<-unique(x$id)
    if (length(id)>nmax) x<-x[x$id %in% sample(id,nmax),]
    ##
    resp<-makeresponse(x)
    resp$id<-NULL
    cm<-colMeans(is.na(resp))
    if (mean(cm)<.1) L[[fn]]<-alpha(resp)$total
}
names(L)<-names(filenames)

