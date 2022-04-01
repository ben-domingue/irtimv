set.seed(867.5309^2)
th<-rnorm(10000)

library(mirt)

##see mirt simdata man page
a <- matrix(rlnorm(30,.2,.3))
# for the graded model, ensure that there is enough space between the intercepts,
# otherwise closer categories will not be selected often (minimum distance of 0.3 here)
diffs <- t(apply(matrix(runif(30*4, .3, 1), 30), 1, cumsum))
diffs <- -(diffs - rowMeans(diffs))
d <- diffs + rnorm(30)
dat <- simdata(a, d, Theta=matrix(th,ncol=1), itemtype = 'graded')
L<-list()
for (i in 1:ncol(dat)) L[[i]]<-data.frame(item=paste0("item_",i),id=1:nrow(dat),resp=as.numeric(dat[,i]))
x<-data.frame(do.call("rbind",L))

save(x,file="cr__sim_grm.Rdata")

#save(x,file="cr__sim_pcm.Rdata")
#save(x,file="cr__sim_nrm.Rdata")
