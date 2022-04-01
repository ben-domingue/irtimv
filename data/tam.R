library(TAM)
x<-data.geiser
names(x)<-paste("item_",1:ncol(x),sep='')
id<-1:nrow(x)
L<-list()
for (i in 1:ncol(x)) L[[i]]<-data.frame(id=id,item=names(x)[i],resp=x[,i])
x<-data.frame(do.call("rbind",L))
save(x,file="/home/bd/Dropbox/projects/irt_meta/data/geiser.Rdata")
