x<-read.csv("~/Dropbox/projects/rt_meta/data/0_raw/ldt/LDT_alldata_long.csv")
x$rt<-log(x$rt)
x$resp<-x$acc
x$item<-x$word
x$id<-x$subj

x<-x[,c("id","item","resp","rt")]
save(x,file=paste("/home/bd/Dropbox/projects/irt_meta/data/roar.Rdata",sep=""))
