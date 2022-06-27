rasch.mod<-function (x, ability = "EAP")  ##modified for use here
{
    resp <- makeresponse(x[x$oos == 0, ])
    library(mirt)
    index <- grep("id", names(resp))
    ni <- ncol(resp) - 1
    m <- mirt(resp[, -index], 1, "Rasch")
    m
}
twopl.mod<-function (x, ability = "EAP") 
{
    resp <- makeresponse(x[x$oos == 0, ])
    library(mirt)
    index <- grep("id", names(resp))
    ni <- ncol(resp) - 1
    s <- paste("F=1-", ni, "\n             PRIOR = (1-", ni, 
        ", a1, lnorm, 0.2, 0.2)", sep = "")
    model <- mirt.model(s)
    test <- try(m <- mirt(resp[, -index], model, itemtype = rep("2PL", 
        ni), method = "EM", technical = list(NCYCLES = 10000)))
    if (class(test) != "try-error") {
        m
    }
    else NULL
}

#for (fn in filenames) {
parfun<-function(fn,ntimes=4,nmax=10000) {
    print(fn)
    load(fn)
    x<-x[!is.na(x$resp),]
    ##
    id<-unique(x$id)
    if (length(id)>nmax) x<-x[x$id %in% sample(id,nmax),]
    ##
    x$oos<-0
    m1<-rasch.mod(x)
    m2<-twopl.mod(x)
    ##
    llrt<-function(m1,m2) {
        cs<-m1@Fit$logLik-m2@Fit$logLik
        an<-anova(m1,m2)
        ii<-match("Bayes_Factor",colnames(an))
        bf<-an[2,ii]
        ii<-match("df",colnames(an))
        pc<-pchisq(-2*cs,df=an[2,ii],lower.tail=FALSE)
        c(bf,pc)
    }
    lrt.pv<-llrt(m1,m2)
    aic.del<-m1@Fit$AIC-m2@Fit$AIC
                                        ##rmsea
    test<-try(rm1<-M2(m1))
    if (class(test)=="try-error") {
        rm1<-M2(m1,impute=5)
    }
    test<-try(rm2<-M2(m2))
    if (class(test)=="try-error") {
        rm2<-M2(m2,impute=5)
    }
    ##
    c(rm1$RMSEA[1],rm2$RMSEA[1],aic.del,lrt.pv)
}

library(parallel)
library(irtimv)
filenames<-datafiles()
set.seed(8610310)

tab<-mclapply(filenames,parfun,mc.cores=2)
tab<-do.call("rbind",tab)

dump("tab","")

library(xtable)
print(xtable(tab[,-4],digits=2,
             display=c("f","e","e","d","e"))
      )


tab <-
structure(c(0.0367288016101504, 0.0726022070639924, 0.158463970628628, 
0.0837564588198274, 0.00639878866604544, 0.0443509281593288, 
0.106313968556422, 0.146817017968216, 0.0710826299330813, 0.0352461228484821, 
0.0389860413333287, 0.0365222467142664, 0.0597824003352224, 0.0342851364592972, 
0.0367828282603275, 0.038907391108543, 0.0319212373458758, 0.0473726411190759, 
0.030571290725784, 0.0426865172779742, 0.000724821320438922, 
0, 0.00122155899608234, 0.0167202143891138, 0.00753958498371857, 
0.0506862357043264, 0.0227839545253679, 0.0453503364684245, 0.128951788080327, 
0.0731841487377268, 0.00305244915838179, 0.0277088778694163, 
0.087144686321806, 0.0893635003925225, 0.052109250379518, 0.0167458951288647, 
0.0222756748886695, 0.018792470633322, 0.04898143156606, 0.0181970885036033, 
0.0157064940836208, 0.0191029551677319, 0.0176379678697331, 0.0292989621214154, 
0.0159065206185485, 0.0323395477515798, 9.9322325412673e-05, 
0, 0.00017799461397782, 0.00199774678066126, 0.00982991154150433, 
0.0218367205899599, 5614.08434315358, 707.02607769466, 143.624983783167, 
158.695523145616, 2116.57910417509, 308.408514239454, 191.175770037407, 
-7.9967389294593, 162.581416779207, 11337.6976012647, 10852.7560805904, 
11693.5977809314, 5563.1599218988, 10344.1844246951, 12489.4789608171, 
12289.2111683759, 8264.86949867534, 5262.728844369, 7955.65492187871, 
12714.0926974608, -71.1583066391395, -13.1396818093226, -64.1170466262047, 
271.222217726063, -286.373312602016, 1593.07888444765, 0, 1.19899058584508e-122, 
6.42552395534619e-27, 2.90031902928291e-39, 0, 1.7052446752355e-88, 
1.69315768897577e-23, 3365316937987.13, 7.58534968510173e-34, 
0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 1.51360456750625e-17, 43.0232586081693, 
2.11653356218413e-18, 1.23966229629114e-32, 1.78395607338849e+117, 
0, 0, 2.01797841669111e-151, 2.25799616495893e-31, 4.24336942926443e-31, 
0, 2.77841360556458e-56, 4.94687946924352e-38, 1, 3.8310937917689e-34, 
0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0.873774481220604, 1, 0.664048979984688, 
5.03292142327728e-57, 1, 0), .Dim = c(26L, 5L), .Dimnames = list(
    c("ENEM Grey", "Mobility", "WIRS", "Rotation", "PISA US", 
    "Chess", "Frac20", "Abortion", "Wordsum", "State Assessment 8", 
    "Lexile 4", "State Assessment 9", "Anxiety", "State Assessment 7", 
    "State Assessment 6", "State Assessment 10", "State Assessment 5", 
    "State Assessment 3", "State Assessment 4", "Lexile 8", "Multiply/Divide", 
    "LSAT", "Add/Subtract", "Letter Chaos", "PIAAC US", "ECLS DCCS"
    ), NULL))
