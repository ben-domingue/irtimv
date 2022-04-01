##no log
load("Nsim3.Rdata")    
tabL<-tab#[as.character(c(25,50,400))]
vals<-seq(0,.2,by=.025)
par(mfrow=c(3,1),
    mgp=c(2,1,0),bty='n',cex.axis=1,mar=c(3,3,1.3,.1),oma=rep(.3,4))
for (ii in 1:length(tabL)) {
    tab<-tabL[[ii]]
    z<-data.frame(do.call("rbind",tab))
    names(z)[1:2]<-c("m","n")
    ##new
    z$n<-as.numeric(z$n)
    z$omega<-as.numeric(z$omega)
    z$omega0<-as.numeric(z$omega0)
    z$ln<-log10(z$n)
    L<-split(z,z$m)
    plot(NULL,xlim=log10(c(100,10000)),
         ylim=range(vals),xaxt='n',
         yaxt='n',ylab="IMV(CTT p-value, IRT p-value)",xlab="N respondents")
    for (i in 1:length(vals)) abline(h=vals[i],lwd=.5,col='gray')
    xv<-c(100,2500,5000)
    axis(side=1,at=log10(xv),xv)
    axis(side=2,line=0,at=vals,vals)
    cols<-c("black","red","blue")
    pf<-function(x,y,col,txt) {
        m<-loess(y~x)
        tmp<-cbind(m$x,m$fitted)
        tmp<-tmp[order(tmp[,1]),]
        lines(tmp,col=cols[i],lwd=2)
        text(tmp[nrow(tmp),1],tmp[nrow(tmp),2],txt,cex=1,col=col,pos=4)
    }
    for (i in 1:length(L)) {
        y<-L[[i]]
        ##
        pf(y$ln,y$omega0,col=cols[i],paste(names(L)[i]))
        #pf(y$ln,y$overfit,col=cols[i],paste(names(L)[i],"overfit"))
        #pf(y$ln,y$oracle,col=cols[i],paste(names(L)[i],"oracle"))
    }
    #if (ii==1) text(log10(2500),.0225,'Dashed lines represent IMV values\nfor true response probabilities\nrelative to estimates',cex=.7)
    mtext(side=3,line=0,paste(names(tabL)[ii],"items"))
}
vals<-seq(-.05,.05,by=.025)
for (ii in 1:length(tabL)) {
    tab<-tabL[[ii]]
    z<-data.frame(do.call("rbind",tab))
    names(z)[1:2]<-c("m","n")
    ##new
    z$n<-as.numeric(z$n)
    z$omega<-as.numeric(z$omega)
    z$omega0<-as.numeric(z$omega0)
    z$ln<-log10(z$n)
    L<-split(z,z$m)
    plot(NULL,xlim=log10(c(100,10000)),
         ylim=range(vals),xaxt='n',
         yaxt='n',ylab="IMV",xlab="N respondents")
    for (i in 1:length(vals)) abline(h=vals[i],lwd=.5,col='gray')
    xv<-c(100,2500,5000)
    axis(side=1,at=log10(xv),xv)
    axis(side=2,line=0,at=vals,vals)
    cols<-c("black","red","blue")
    pf<-function(x,y,col,txt,lty=1) {
        m<-loess(y~x)
        tmp<-cbind(m$x,m$fitted)
        tmp<-tmp[order(tmp[,1]),]
        lines(tmp,col=cols[i],lty=lty,lwd=2)
        text(tmp[nrow(tmp),1],tmp[nrow(tmp),2],txt,cex=1,col=col,pos=4)
    }
    for (i in 1:length(L)) {
        y<-L[[i]]
        ##
        #pf(y$ln,y$omega0,col=cols[i],paste(names(L)[i],"omega0"))
        pf(y$ln,y$overfit,col=cols[i],paste(names(L)[i]),lty=2)
        pf(y$ln,y$oracle,col=cols[i],paste(names(L)[i]))
    }
    #if (ii==1) text(log10(2500),.0225,'Dashed lines represent IMV values\nfor true response probabilities\nrelative to estimates',cex=.7)
    #mtext(side=3,line=0,paste(names(tabL)[ii],"items"))
}
legend("topright",bty='n',lty=c(1,2),c("Oracle","Overfit"))
