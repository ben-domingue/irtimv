#/home/bd/Dropbox/projects/irt_meta/data/state

#################################################################################################
##dichotomous items

list.files(pattern="*_2007_*")->lf
for (i in 1:length(lf)) {
    lf[i]->fn
    read.table(fn,header=TRUE)->x
    names(x)<-tolower(names(x))
    grep("^mc",names(x))->index
    x[,index]->x
    #
    #sample(1:ncol(x),3)->rem.items
    #x[,-rem.items]->x
    #
    L<-list()
    for (j in 1:ncol(x)) L[[j]]<-data.frame(item=names(x)[j],id=1:nrow(x),resp=as.numeric(x[,j]))
    x<-data.frame(do.call("rbind",L))
    save(x,file=paste("/home/bd/Dropbox/projects/irt_meta/data/",gsub(".txt",".Rdata",fn),sep=""))
}


list.files(pattern="*_2007_*")->lf
for (i in 1:length(lf)) {
    lf[i]->fn
    read.table(fn,header=TRUE)->x
    names(x)<-tolower(names(x))
    grep("^cr",names(x))->index
    x[,index]->x
    #
    #sample(1:ncol(x),3)->rem.items
    #x[,-rem.items]->x
    #
    L<-list()
    for (j in 1:ncol(x)) L[[j]]<-data.frame(item=names(x)[j],id=1:nrow(x),resp=as.numeric(x[,j]))
    x<-data.frame(do.call("rbind",L))
    save(x,file=paste("/home/bd/Dropbox/projects/irt_meta/data/poly/cr__",gsub(".txt",".Rdata",fn),sep=""))
}
