x<-read.table("/home/bd/Dropbox/projects/rt_meta/data/0_raw/piaac/Prgusap1_2017.csv",sep="|",header=TRUE)
x$id<-1:nrow(x)

#taken from annex 17.1 of this, http://www.oecd.org/skills/piaac/PIAAC_Technical_Report_2nd_Edition_Full_Report.pdf
#and http://www.oecd.org/skills/piaac/International%20Codebook_PIAAC%20Public-use%20File%20(PUF)%20Variables%20and%20Values.xlsx
items<-c("C301C05S", "C300C02S", "D302C02S", "D311701S", "E321001S", 
"E321002S", "C308117S", "C308119S", "C308120S", "C308121S", "C305215S", 
"C305218S", "D315512S", "C308118S", "D304710S", "D304711S", "C308116S", 
"E327001S", "E327002S", "E327003S", "E327004S", "D307401S", "D307402S", 
"C309319S", "C309320S", "C309321S", "C309322S", "E322001S", "E322002S", 
"E322005S", "C313412S", "C313414S", "E322003S", "C310406S", "C310407S", 
"E320001S", "E320003S", "E320004S", "E322004S", "D306110S", "D306111S", 
"C313410S", "C313411S", "C313413S", "E323003S", "E323004S", "E318001S", 
"E318003S", "E329002S", "E329003S", "E323002S", "E323005S", "M301C05S", 
"P330001S", "N302C02S", "M300C02S", "N306110S", "N306111S", "M313410S", 
"M313411S", "M313412S", "M313413S", "M313414S", "P324002S", "P324003S", 
"M305215S", "M305218S", "P317001S", "P317002S", "P317003S", "M310406S", 
"M310407S", "M309319S", "M309320S", "M309321S", "M309322S", "C600C04S", 
"C601C06S", "E645001S", "C615602S", "C615603S", "C624619S", "C624620S", 
"C604505S", "C605506S", "C605507S", "C605508S", "E650001S", "C623616S", 
"C623617S", "E657001S", "C619609S", "E632001S", "E632002S", "E646002S", 
"C620610S", "C620612S", "C613520S", "C614601S", "C618607S", "C618608S", 
"E635001S", "C607510S", "E655001S", "C602502S", "C602503S", "C608513S", 
"C602501S", "C606509S", "C611516S", "C611517S", "C622615S", "E665001S", 
"E665002S", "E636001S", "C617605S", "C617606S", "E660003S", "E660004S", 
"E641001S", "E661001S", "E661002S", "C612518S", "E651002S", "E664001S", 
"E634001S", "E634002S", "E644002S", "M600C04S", "P601C06S", "P614601S", 
"P645001S", "M615602S", "M615603S", "P640001S", "M620610S", "M620612S", 
"P666001S", "M623616S", "M623617S", "M623618S", "M624619S", "M624620S", 
"M618607S", "M618608S", "M604505S", "M610515S", "P664001S", "M602501S", 
"M602502S", "M602503S", "P655001S", "U01A000S", "U01B000S", "U03A000S", 
"U06A000S", "U06B000S", "U21X000S", "U04A000S", "U19A000S", "U19B000S", 
"U07X000S", "U02X000S", "U16X000S", "U11B000S", "U23X000S")

resp<-x[,items]
for (i in 1:ncol(resp)) {
    resp[,i]<-as.numeric(ifelse(resp[,i] %in% c(7,"N"),NA,resp[,i]))
}
nn<-apply(resp,2,function(x) length(unique(x[!is.na(x)])))
resp<-resp[,nn==2]

nms<-names(resp)
for (i in 1:length(nms)) {
    nm<-nms[i]
    n<-nchar(nm)
    nm<-substr(nm,1,n-1)
    nms[i]<-nm
}
nms2<-paste(nms,"T",sep="")
test<-nms2 %in% names(x)
resp<-resp[,test]
rt<-x[,nms2[test]]
for (i in 1:ncol(rt)) {
    z<-rt[,i]
    z<-as.numeric(z)
    z<-ifelse(z<=0,NA,z)
    log(z/1000)->rt[,i]
}



##get rid of empty rows
rs<-rowSums(is.na(resp))
test<-rs<ncol(resp)
resp<-resp[test,]
rt<-rt[test,]
x<-x[test,]

##create long data
item<-names(resp)
L<-list()
for (i in 1:ncol(rt)) {
    L[[i]]<-data.frame(resp=resp[,i],rt=rt[,i],id=x$id,item=item[i],age=x$AGEG5LFSEXT)#,th=th[,1],diff=as.numeric(-1*co[i]))
}
x<-data.frame(do.call("rbind",L))

save(x,file="/home/bd/Dropbox/projects/irt_meta/data/piaac.Rdata")
