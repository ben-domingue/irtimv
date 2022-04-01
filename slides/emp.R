par(mgp=c(2,1,0))
plot(log10(tab0[,1]),tab0[,2],xlab="Log10(N respondents)",ylab="N items",pch=19,col='blue')

par(mgp=c(2,1,0))
d<-density(tab0[,4])
plot(d,sub='',main='',ylab='',yaxt='n',xlab="IMV(CTT,1PL)")
polygon(c(d$x,rev(d$x)),c(d$y,rep(0,length(d$x))),col='blue')
abline(v=.1,lwd=3)

par(mgp=c(2,1,0),oma=rep(.5,4))
d<-density(tab0[,6])
plot(d,sub='',main='',ylab='',yaxt='n',xlab="IMV(1PL,2PL)")
polygon(c(d$x,rev(d$x)),c(d$y,rep(0,length(d$x))),col='red')
abline(v=.01,lwd=3)

par(mgp=c(2,1,0),oma=rep(.5,4))
d<-density(tab0[,7])
plot(d,sub='',main='',ylab='',yaxt='n',xlab="IMV(2PL,3PL)")
polygon(c(d$x,rev(d$x)),c(d$y,rep(0,length(d$x))),col='green')
abline(v=5e-4,lwd=3)

## par(mgp=c(2,1,0),oma=rep(.5,4))
## d<-density(tab0[,4])
## plot(d,sub='',main='',ylab='',yaxt='n',xlab="IMV",ylim=c(0,20))
## polygon(c(d$x,rev(d$x)),c(d$y,rep(0,length(d$x))),col='blue')
## ##
## d<-density(tab0[,6])
## polygon(c(d$x,rev(d$x)),c(d$y,rep(0,length(d$x))),col='red')
## ##
## d<-density(tab0[,7])
## polygon(c(d$x,rev(d$x)),c(d$y,rep(0,length(d$x))),col='green')


