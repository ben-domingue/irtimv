##get tab from 01_grm.R

par(mfrow=c(2,1),mgp=c(2,1,0),oma=rep(.5,4),mar=c(3,3,1,1))
d<-density(tab[,3])
plot(d,sub='',main='',ylab='',yaxt='n',xlab="IMV(CTT,GRM)",xlim=c(-0.05,.35))
polygon(c(d$x,rev(d$x)),c(d$y,rep(0,length(d$x))),col='blue')
abline(v=.15,col='gray',lwd=3)
abline(v=.1,col='gray',lwd=3)
d<-density(tab[,4])
plot(d,sub='',main='',ylab='',yaxt='n',xlab="IMV(GRM,PCM)",xlim=c(-0.05,.35))
polygon(c(d$x,rev(d$x)),c(d$y,rep(0,length(d$x))),col='red')
