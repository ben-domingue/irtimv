makeresponse<-function (x) 
{
    nms <- unique(x$item)
    if (all(nms %in% 1:length(nms))) 
        x$item <- paste("item_", x$item, sep = "")
    id <- unique(x$id)
    L <- split(x, x$item)
    out <- list()
    for (i in 1:length(L)) {
        z <- L[[i]]
        index <- match(z$id, id)
        resp <- rep(NA, length(id))
        resp[index] <- z$resp
        out[[i]] <- resp
    }
    resp <- do.call("cbind", out)
    resp <- data.frame(resp)
    names(resp) <- names(L)
    resp$id <- id
    nr <- apply(resp, 2, function(x) length(table(x)))
    resp <- resp[, nr > 1]
    resp <- resp[rowSums(!is.na(resp)) > 1, ]
    resp
}
simdat<-function(r,
                 tau,
                 np=1000,ni=50
                 ) {
    library(MASS)
    th<-mvrnorm(np,c(0,0),matrix(c(1,r,r,1),2,2))
    b<-rnorm(ni)
    a<-rnorm(ni*2,sd=.3)
    a<-exp(a)
    a<-matrix(a,ncol=2,nrow=ni,byrow=FALSE)
    ##tau
    mm<-quantile(a[,1],tau)
    a[,2]<-ifelse(a[,1]>mm,0,a[,2])
    ##
    p<-th %*% t(a)
    for (i in 1:length(b)) p[,i]<-p[,i]+b[i]
    p<-1/(1+exp(-p))
    resp<-p
    for (i in 1:ncol(p)) resp[,i]<-rbinom(np,1,p[,i])
    ##
    df<-list()
    for (i in 1:ncol(p)) df[[i]]<-data.frame(id=1:np,item=paste('item_',i,sep=''),resp=resp[,i],p=p[,i],a1=a[i,1],a2=a[i,2],b=b[i])
    x<-data.frame(do.call("rbind",df))
    resp <- makeresponse(x)
    library(mirt)
    index <- grep("id", names(resp))
    ni <- ncol(resp) - 1
    s <- paste("F1=1-", ni, ",\nF2=1-", ni,
               "\nCOV=F1*F2",
               "\nPRIOR = (1-",ni, ", a1, lnorm, 0.0, 0.0),(1-", ni, ", a2, lnorm, 0.0, 0.0)", 
               sep = "")
    model <- mirt.model(s)
    test <- try(m <- mirt(resp[, -index], model, itemtype = rep("2PL", 
                                                                ni), method = "EM", technical = list(NCYCLES = 10000)))
    co <- coef(m)
    co<-do.call("rbind",co[-length(co)])
    cn<-colnames(resp)
    co<-data.frame(item=cn[-length(cn)],co)
    true<-data.frame(item=paste('item_',1:ni,sep=''),a1.true=a[,1],a2.true=a[,2])
    items<-merge(co,true)
    ##
    plot(items$a1.true,items$a1)
    legend("bottomright",legend=tau)
    plot(items$a2.true,items$a2)
    plot(items$a1,items$a2)
    legend("bottomright",legend=round(cor(items$a1,items$a2),2))
    fs<-fscores(m)
    plot(th[,1],fs[,1])
    legend("topleft",legend=r)
    plot(th[,2],fs[,2])
    plot(fs)
    legend("bottomright",legend=round(cor(fs[,1],fs[,2]),2))
    ##
}


par(mfrow=c(5,6),mgp=c(2,1,0),mar=c(3,3,1,1),oma=rep(.5,4))
simdat(r=0,tau=.5,np=2500)
simdat(r=0,tau=1,np=2500)
simdat(r=.5,tau=.5,np=2500)
simdat(r=.5,tau=.75,np=2500)
simdat(r=.5,tau=1,np=25000)
