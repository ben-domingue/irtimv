library(irtimv)
filenames<-datafiles()
set.seed(8610310)
ntimes<-4

tab<-list()
for (fn in filenames) {
    load(fn)
    x<-x[!is.na(x$resp),]
    ##
    id<-unique(x$id)
    if (length(id)>10000) x<-x[x$id %in% sample(id,10000),]
    ##
    x$gr<-sample(1:ntimes,nrow(x),replace=TRUE)
    x.hold<-x
    omega<-numeric()
    for (i in 1:ntimes) {
        x<-x.hold
        x$oos<-ifelse(x$gr==i,1,0)
        ##
        x1<-twopl(x)
        x2<-twopl.2f(x)
        ##
        if (length(x2)>0) {
            x1<-x1[,c("item","id","p","resp")]
            names(x1)[3]<-"p1"
            x2<-x2[,c("item","id","p")]
            names(x2)[3]<-"p2"
            x<-merge(x1,x2,)
            omega[i]<-imv(x,p1="p1",p2="p2")
        } else omega[i]<-NA
    }
    tab[[fn]]<-mean(omega)
    print(tab)
}



tab0<-do.call("rbind",tab)
rownames(tab0)<-names(filenames)[match(filenames,rownames(tab0))]

tab0 <-
structure(c(-0.00083072870999166255, -0.0018789890320939204, 
-0.011241244867316081, 0.0045353914326731254, -0.0010734981596351488, 
-0.0014699271325901368, -0.00078814478499708563, 0.00038339364217100143, 
-0.0018906879787935413, 0.00063864311965843512, 0.0015465589885209944, 
0.00039355245287583625, 0.0056956048548743787, 0.00065641667330385836, 
0.00017896845020024449, 0.00070315272204829319, 0.00075616532369736009, 
0.00036607832411757082, 0.00025734596761010838, 0.0032836371965234393, 
-0.017361965912017455, -0.0091679043790901683, -0.012069833610684283, 
-2.5865552447165173e-05, 0.00021653531833070968, -0.00023759012836956371
), .Dim = c(26L, 1L), .Dimnames = list(c("ENEM Grey", "Mobility", 
"WIRS", "Rotation", "PISA US", "Chess", "Frac20", "Abortion", 
"Wordsum", "State Assessment 8", "Lexile 4", "State Assessment 9", 
"Anxiety", "State Assessment 7", "State Assessment 6", "State Assessment 10", 
"State Assessment 5", "State Assessment 3", "State Assessment 4", 
"Lexile 8", "Multiply/Divide", "LSAT", "Add/Subtract", "Letter Chaos", 
"PIAAC US", "ECLS DCCS"), NULL))

library(xtable)
print(xtable(tab0,digits=2,display=c("f","e")))
