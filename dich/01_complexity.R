
#for (fn in filenames) {
parfun<-function(fn,ntimes=4,nmax=10000) {
    load(fn)
    x<-x[!is.na(x$resp),]
    ##
    id<-unique(x$id)
    if (length(id)>nmax) x<-x[x$id %in% sample(id,nmax),]
    ##
    x$gr<-sample(1:ntimes,nrow(x),replace=TRUE)
    x.hold<-x
    omega<-list()
    for (i in 1:ntimes) {
        x<-x.hold
        x$oos<-ifelse(x$gr==i,1,0)
        x0<-x[x$oos==0,]
        ##ctt p-value
        z<-by(x0$resp,x0$item,mean,na.rm=TRUE)
        z<-data.frame(item=names(z),p.ctt=as.numeric(z))
        x<-merge(x,z)
        ##
        tr<-list()
        ##
        xr<-rasch(x)
        tr$rasch<-imv(xr,p1="p.ctt",p2="p") 
        ##
        xr$guttman<-ifelse(xr$p>.5,.99,.01)
        tr$guttman<-imv(xr,p1="guttman",p2="p")
        ##
        x2<-twopl(x)
        if (length(x2)>0) {
            x2<-x2[,c("item","id","p")]
            names(x2)[3]<-"p2"
            xr<-merge(xr,x2,)
            tr$twopl<-imv(xr,p1="p",p2="p2")
        } else tr$twopl<-NA
        ##
        x3<-threepl(x)
        if (length(x3)>0) {
            x3<-x3[,c("item","id","p")]
            names(x3)[3]<-"p3"
            xr<-merge(xr,x3)
            tr$threepl<-imv(xr,p1="p2",p2="p3")
        } else tr$threepl<-NA
        ##
        omega[[i]]<-unlist(tr)
    }
    tab<-c(
        length(unique(x.hold$id)),
        length(unique(x.hold$item)),
        mean(x.hold$resp,na.rm=TRUE),
        colMeans(do.call("rbind",omega))
    )
    print(fn)
    tab
}

library(parallel)
library(irtimv)
filenames<-datafiles()
set.seed(8610310)
tab<-mclapply(filenames,parfun,mc.cores=2)

####################################################################
##adding stuff
library(irtimv)
filenames<-datafiles()
index<-which(!(names(filenames) %in% names(tab)))

for (i in index) {
    nm<-filenames[[i]]
    fancy.nm<-names(filenames)[i]
    tab[[fancy.nm]]<-parfun(nm)
}
index<-match(names(filenames),names(tab))
tab<-tab[index]
dump('tab','')

tab <-
list(`ENEM Grey` = c(10000, 45, 0.25337999999999999, rasch = 0.020028870227060493, 
guttman = 0.58450587164839496, twopl = 0.006887054964443395, 
threepl = 0.0054283088825382532), Mobility = c(8445, 8, 0.31867969212551805, 
rasch = 0.065717806411400542, guttman = 0.58490027571058611, 
twopl = 0.00026883313922742636, threepl = -0.00023851550575580021
), WIRS = c(1005, 6, 0.33051409618573796, rasch = 0.022553045708743551, 
guttman = 0.4701639164939766, twopl = 0.019321060092411961, threepl = -0.00031368163553458243
), Rotation = c(519, 24, 0.3346178548490687, rasch = 0.069858361585638989, 
guttman = 0.59946755569792254, twopl = 0.0070131955740518328, 
threepl = 0.0027714082064922065), `PISA US` = c(10000, 60, 0.47238197410322047, 
rasch = 0.10033035942372108, guttman = 0.55160124510117714, twopl = 0.0093856949740003699, 
threepl = -0.00088313361720114504), Chess = c(258, 80, 0.48599605522682449, 
rasch = 0.067375601077640485, guttman = 0.71028649471395244, 
twopl = 0.0061851741328664746, threepl = -0.0013566783929591943
), Frac20 = c(536, 20, 0.53386194029850742, rasch = 0.31742020383857628, 
guttman = 0.70372773224745722, twopl = 0.010960187221428512, 
threepl = -0.0029255405485571779), Abortion = c(379, 4, 0.5712401055408971, 
rasch = 0.47224011608302341, guttman = 0.63263026963478453, twopl = -0.0022620453375500815, 
threepl = -0.0060699809594142451), Wordsum = c(1547, 10, 0.61460923325045358, 
rasch = 0.030560649094484235, guttman = 0.64089046470870237, 
twopl = 0.0046302679557068309, threepl = 0.00094325237526051407
), `State Assessment 8` = c(10000, 56, 0.63456460108220136, rasch = 0.072274450505157767, 
guttman = 0.56037644107550888, twopl = 0.011445340461818367, 
threepl = 0.0018104365267782902), `Lexile 4` = c(9876, 54, 0.63927022850369897, 
rasch = 0.071669062159264552, guttman = 0.60065418447961294, 
twopl = 0.01027227230134679, threepl = 0.0021661228280709585), 
    `State Assessment 9` = c(10000, 56, 0.65319647270882553, 
    rasch = 0.068241825668713577, guttman = 0.57658470731618094, 
    twopl = 0.010740048535102159, threepl = 0.0016419663908304001
    ), Anxiety = c(5401, 50, 0.6505218598817416, rasch = 0.089444621005583258, 
    guttman = 0.55918820987166118, twopl = 0.01135339513277183, 
    threepl = -0.00017288113270530698), `State Assessment 7` = c(10000, 
    55, 0.65937919358790131, rasch = 0.080020922406646253, guttman = 0.57415013042478591, 
    twopl = 0.010250491434363389, threepl = 0.0017765225282430255
    ), `State Assessment 6` = c(10000, 56, 0.66792532067375732, 
    rasch = 0.070170606813750147, guttman = 0.596664480906415, 
    twopl = 0.011073332125075885, threepl = 0.0012141987850192916
    ), `State Assessment 10` = c(10000, 55, 0.67660188186458692, 
    rasch = 0.080052088155030621, guttman = 0.58849354633081119, 
    twopl = 0.011470476164010604, threepl = 0.0020281274187622621
    ), `State Assessment 5` = c(10000, 56, 0.67983850463514783, 
    rasch = 0.080472845699770923, guttman = 0.58353857529551978, 
    twopl = 0.0071801280676863661, threepl = 0.0012456614111219109
    ), `State Assessment 3` = c(10000, 32, 0.70365028327407508, 
    rasch = 0.068642267603362159, guttman = 0.61051096127805948, 
    twopl = 0.008125127746502956, threepl = 0.0011179779187823979
    ), `State Assessment 4` = c(10000, 56, 0.70030321058127643, 
    rasch = 0.075543620161581171, guttman = 0.63275331277510882, 
    twopl = 0.0061663077515680753, threepl = 0.00085997864498564265
    ), `Lexile 8` = c(10000, 70, 0.73159015932025595, rasch = 0.086584215307857038, 
    guttman = 0.72691134759914977, twopl = 0.0059152512395350037, 
    threepl = 0.001782323340244561), `Multiply/Divide` = c(10000, 
    60, 0.74768656420032564, rasch = 0.015861282515874583, guttman = 0.57201399106570883, 
    twopl = -0.001346974850350019, threepl = 0.00027462046206912525
    ), LSAT = c(1000, 5, 0.76380000000000003, rasch = 0.0059158472445236445, 
    guttman = 0.60835435122443182, twopl = -0.0031917164674028701, 
    threepl = 0.00027234323735070298), `Add/Subtract` = c(10000, 
    60, 0.78662273709568853, rasch = 0.013212575031146706, guttman = 0.60943192279496661, 
    twopl = -0.0022563049340102869, threepl = 0.00030563960353443409
    ), `Letter Chaos` = c(10000, 10, 0.87090188543292901, rasch = 0.041784733046445907, 
    guttman = 0.11092314452458218, twopl = -0.0025058097473252584, 
    threepl = -0.00044035474402929001), `PIAAC US` = c(2870, 
    104, 0.93558021925805246, rasch = 0.028732449630462752, guttman = 0.026398136338232372, 
    twopl = 0.00037702631813167517, threepl = -0.00010329858437367763
    ), `ECLS DCCS` = c(10000, 30, 0.94682053933258847, rasch = 0.0048624894254079695, 
    guttman = 0.026884887974063906, twopl = 0.00081288068860974701, 
    threepl = -2.608837024001443e-05))




tab0<-do.call("rbind",tab)
tab0<-rbind(tab0,colMeans(tab0))

cor(abs(tab0[,3]-.5),tab0[,4])


library(xtable)
print(xtable(tab0,digits=2,display=c("f","d","d","f","e","e","e","e")))
