

##########################################
load("misfit_poly.Rdata")
for (i in 1:length(tab)) {
    z<-tab[[i]]
    zz<-split(z,paste(z$n,z$K,z$J))
    zz<-lapply(zz,colMeans)
    z<-data.frame(do.call("rbind",zz))
    z$id<-paste(z$n,z$K,z$J)
    tab[[i]]<-z
}
x<-tab[[1]]
y<-tab[[2]][,c("id","om0")]
names(y)[2]<-"om.pcm"
z<-merge(x,y,by='id')

par(mgp=c(2,1,0),mar=c(5,3,.1,.1),bty='n',oma=rep(.3,4))
plot(NULL,xlim=c(1-3,nrow(z)),ylim=c(0,.1),xlab="",xaxt="n",ylab="IMV")
for (i in seq(-.025,.025,by=.025)) abline(h=i,lwd=1,col='gray')
axis(side=1,at=1:nrow(z),labels=rep("",nrow(z)))
mtext(side=1,at=1:nrow(z),z$J,line=.75,cex=.8)
mtext(side=1,line=.75,at=1-1.3,"J",cex=.8)
mtext(side=1,at=1:nrow(z),z$n,line=1.75,cex=.8)
mtext(side=1,line=1.75,at=1-1.3,"N",cex=.8)
mtext(side=1,at=1:nrow(z),z$K,line=2.75,cex=.8)
mtext(side=1,line=2.75,at=1-1.3,"K",cex=.8)
##
lines(1:nrow(z),z$om0,type='b',col='blue',lwd=2,pch=19)
text(1,z$om0[1],pos=2,paste("IMV(CTT,Gr)\nDGM=GR"),col='blue',cex=.8)
lines(1:nrow(z),z$om.pcm,type='b',col='red',lwd=2,pch=19)
text(1,z$om.pcm[1],pos=3,paste("IMV(CTT,PCM)\nDGM=PCM\n"),col='red',cex=.8)


##########################################
load("misfit_poly.Rdata")
for (i in 1:length(tab)) {
    z<-tab[[i]]
    zz<-split(z,paste(z$n,z$K,z$J))
    zz<-lapply(zz,colMeans)
    z<-data.frame(do.call("rbind",zz))
    z$id<-paste(z$n,z$K,z$J)
    tab[[i]]<-z
}
x<-tab[[1]]
y<-tab[[2]][,c("id","om1")]
names(y)[2]<-"om.pcm"
z<-merge(x,y,by='id')

par(mgp=c(2,1,0),mar=c(5,3,.1,.1),bty='n',oma=rep(.3,4))
plot(NULL,xlim=c(1-3,nrow(z)),ylim=c(0,.001),xlab="",xaxt="n",ylab="IMV")
for (i in seq(-.025,.025,by=.025)) abline(h=i,lwd=1,col='gray')
axis(side=1,at=1:nrow(z),labels=rep("",nrow(z)))
mtext(side=1,at=1:nrow(z),z$J,line=.75,cex=.8)
mtext(side=1,line=.75,at=1-1.3,"J",cex=.8)
mtext(side=1,at=1:nrow(z),z$n,line=1.75,cex=.8)
mtext(side=1,line=1.75,at=1-1.3,"N",cex=.8)
mtext(side=1,at=1:nrow(z),z$K,line=2.75,cex=.8)
mtext(side=1,line=2.75,at=1-1.3,"K",cex=.8)
##
lines(1:nrow(z),z$om1,type='b',col='blue',lwd=2,pch=19)
text(1,z$om1[1],pos=2,paste("IMV(PCM,Gr), DGM=GR"),col='blue',cex=.5)
lines(1:nrow(z),z$om.pcm,type='b',col='red',lwd=2,pch=19)
text(1,z$om.pcm[1],pos=2,paste("IMV(Gr,PCM), DGM=PCM"),col='red',cex=.5)
