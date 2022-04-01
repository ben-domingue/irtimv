library(GDINA)

resp<-frac20$dat
L<-list()
for (i in 1:ncol(resp)) L[[i]]<-data.frame(item=paste0("item_",i),id=1:nrow(resp),resp=as.numeric(resp[,i]))
x<-data.frame(do.call("rbind",L))

save(x,file="frac20.Rdata")


##analysis
mod1 <- GDINA(frac20$dat,frac20$Q,model="DINA")
mod1
