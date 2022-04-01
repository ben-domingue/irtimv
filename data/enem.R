## ##see https://domingue-lab.slack.com/archives/DA48K3GMA/p1645727377783969
## x<-read.csv("enem_test_little.csv")
## x$sequence_number<-NULL
## x$item<-x$itemkey
## x$itemkey<-NULL
## x$id<-as.character(x$id)
## x$item<-paste("item_",x$item,sep='')

## save(x,file="enem.Rdata")

##see https://domingue-lab.slack.com/archives/DA48K3GMA/p1645737357261899

x<-read.csv("enem_imv.csv")
x$item<-x$itemkey
x$itemkey<-NULL
x$id<-as.character(x$id)
x$item<-paste("item_",x$item,sep='')
books<-unique(x$booklet)

y<-x
for (book in books) {
    x<-y[y$booklet==book,]
    save(x,file=paste("enem_",book,".Rdata",sep=''))
}
