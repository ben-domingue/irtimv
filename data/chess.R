library(foreign)
x<-read.spss("/home/bd/Dropbox/projects/rt_meta/data/0_raw/chess/chess.sav",to.data.frame=TRUE)
L<-list()
for (i in 1:40) {
    nms<-paste(c("A","AR","B","BR"),i,sep='')
    tmp<-x[,nms]
    z1<-data.frame(id=1:nrow(tmp),resp=tmp[,1],rt=tmp[,2],item=paste('a',i))
    L[[paste(i,'a')]]<-z1
    z1<-data.frame(id=1:nrow(tmp),resp=tmp[,3],rt=tmp[,4],item=paste('b',i))
    L[[paste(i,'b')]]<-z1
}
x<-data.frame(do.call("rbind",L))

save(x,file="/home/bd/Dropbox/projects/irt_meta/data/chess.Rdata")
