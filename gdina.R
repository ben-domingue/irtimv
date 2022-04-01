library(GDINA)
## dat <- sim10GDINA$simdat
## Q <- sim10GDINA$simQ

dat<-frac20$dat
Q<-frac20$Q
colnames(dat)<-paste("X",1:ncol(dat),sep="")


dina<-function(dat,Q) {
    mod1<-GDINA(dat = dat, Q = Q, model = "GDINA")
    pr<-mod1$LC.prob
    th<-personparm(mod1)
    th<-apply(th,1,function(x) paste(x,collapse=''))
    resp<-matrix(NA,ncol=nrow(pr),nrow=length(th))
    index<-match(th,colnames(pr))
    for (j in 1:ncol(resp)) {
        p<-pr[j,]
        resp[,j]<-p[index]
    }
    L<-list()
    for (i in 1:ncol(dat)) L[[i]]<-data.frame(id=1:nrow(dat),item=colnames(dat)[i],dina=resp[,i])
    x<-data.frame(do.call("rbind",L))
    x
}
p<-dina(dat,Q=Q)

irt<-function(dat) {
    library(mirt)
    resp<-data.frame(dat)
    m<-mirt(resp,1)
    co<-coef(m)
    print(co)
    nms<-names(co)
    co<-do.call("rbind",co[-length(co)])
    item<-data.frame(item=nms[-length(nms)],easy=co[,2],load=co[,1],gues=co[,3])
    th.est<-fscores(m,response.pattern=resp)
    index<-grep("^F",colnames(th.est))
    stud<-data.frame(id=1:nrow(resp),th=th.est[,index])
    y<-merge(stud,item)
    kk<-y$load*y$th+y$easy
    kk<-exp(kk)
    y$irt<-y$gues+(1-y$gues)*kk/(1+kk)
    L<-list()
    for (i in 1:ncol(resp)) L[[i]]<-data.frame(id=1:nrow(resp),item=names(resp)[i],resp=resp[,i])
    x<-data.frame(do.call("rbind",L))
    x<-merge(x,y)    
    x
}
p0<-irt(dat)

x<-merge(p,p0)

library(irtimv)
om<-imv(x,p1="irt",p2="dina")
