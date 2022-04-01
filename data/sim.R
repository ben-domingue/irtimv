set.seed(867.5309^2)
th<-rnorm(10000)

##rasch
b<-rnorm(50)
p<-outer(th,b,"-")
p<-1/(1+exp(-p))
resp<-p
for (i in 1:ncol(resp)) resp[,i]<-rbinom(nrow(resp),1,p[,i])
L<-list()
for (i in 1:ncol(resp)) L[[i]]<-data.frame(item=paste0("item_",i),id=1:nrow(resp),resp=as.numeric(resp[,i]))
x<-data.frame(do.call("rbind",L))
save(x,file="sim_rasch.Rdata")

##2PL
a<-exp(rnorm(50,mean=0,sd=.1))
p<-outer(th,b,"-")
for (i in 1:ncol(p)) p[,i]<-a[i]*p[,i]
p<-1/(1+exp(-p))
resp<-p
for (i in 1:ncol(resp)) resp[,i]<-rbinom(nrow(resp),1,p[,i])
L<-list()
for (i in 1:ncol(resp)) L[[i]]<-data.frame(item=paste0("item_",i),id=1:nrow(resp),resp=as.numeric(resp[,i]))
x<-data.frame(do.call("rbind",L))
save(x,file="sim_2pl.Rdata")

##3PL
c<-runif(50,min=0,max=.25)
p<-outer(th,b,"-")
for (i in 1:ncol(p)) p[,i]<-a[i]*p[,i]
for (i in 1:ncol(p)) p[,i]<-c[i]+(1-c[i])*1/(1+exp(-p[,i]))
resp<-p
for (i in 1:ncol(resp)) resp[,i]<-rbinom(nrow(resp),1,p[,i])
L<-list()
for (i in 1:ncol(resp)) L[[i]]<-data.frame(item=paste0("item_",i),id=1:nrow(resp),resp=as.numeric(resp[,i]))
x<-data.frame(do.call("rbind",L))
save(x,file="sim_3pl.Rdata")
