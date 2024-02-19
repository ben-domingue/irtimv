##example analysis of one dataset
set.seed(170301)
library(mirt); library(redivis); library(irw)
dataset <- redivis::user("stanford_templates")$
    dataset("item_response_warehouse",version='v2.0')
df <- dataset$table("content_literacy_intervention")$to_data_frame()
df$item<-paste("item_",df$item,sep='')
##cross-validation for models estimated in mirt
ntimes<-4
df$gr<-sample(1:ntimes,nrow(df),replace=TRUE)
omega<-numeric()
for (i in 1:ntimes) {
    x<-df
    x$oos<-ifelse(x$gr==i,1,0)
    x0<-x[x$oos==0,]
    resp0<-data.frame(irw::long2resp(x0))
    id<-resp0$id
    resp0$id<-NULL
    m0<-mirt(resp0,1,'Rasch')
    ni<-ncol(resp0)
    s<-paste("F=1-",ni,"
             PRIOR = (1-",ni,", a1, lnorm, 0.0, 1.0)",sep="")
    model<-mirt.model(s)
    m1<-mirt(resp0,model,itemtype=rep("2PL",ni),
             method="EM",
             technical=list(NCYCLES=10000))
    ##
    z0<-irw::getp(m0,x=x[x$oos==1,],id=id)
    z1<-irw::getp(m1,x=x[x$oos==1,],id=id)
    z0<-z0[,c("item","id","resp","p")]
    names(z0)[4]<-'p1'
    z1<-z1[,c("item","id","p")]
    names(z1)[3]<-'p2'
    z<-merge(z0,z1)
    omega[i]<-imv(z$resp,z$p1,z$p2)
}
mean(omega)
