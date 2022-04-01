datafiles<-function() {
    filenames <-
        list(#ENEM="enem.Rdata",
            `ENEM Grey`="enem_grey.Rdata",
            #`ENEM Yellow`="enem_yellow.Rdata",
            #`ENEM Blue`="enem_blue.Rdata",
            #`ENEM Pink`="enem_pink.Rdata",
            Mobility="mobility.Rdata",
             WIRS="wirs.Rdata",
             Rotation="geiser.Rdata",
             `PISA US` = "pisa.Rdata",
             Chess = "chess.Rdata", 
             Frac20="frac20.Rdata",
             Abortion="abortion.Rdata",
             Wordsum="wordsum.Rdata",
             `State Assessment 8` = "c1_2007_8_responses.Rdata",
             `Lexile 4` = "duval4.Rdata", 
             `State Assessment 9` = "c1_2007_9_responses.Rdata",
             `Anxiety`="tma.Rdata",
             `State Assessment 7` = "c1_2007_7_responses.Rdata", 
             `State Assessment 6` = "c1_2007_6_responses.Rdata",
             `State Assessment 10` = "c1_2007_10_responses.Rdata", 
             `State Assessment 5` = "c1_2007_5_responses.Rdata",
             `State Assessment 3` = "c1_2007_3_responses.Rdata", 
             `State Assessment 4` = "c1_2007_4_responses.Rdata",
             `Lexile 8` = "duval8.Rdata", 
             `Multiply/Divide` = "multiply.divide.Rdata",
             LSAT="lsat.Rdata",
             `Add/Subtract` = "add.subtract.Rdata", 
                                        #`ROAR LDT` = "roar.Rdata",
             `Letter Chaos` = "letterchaos.Rdata", 
             `PIAAC US` = "piaac.Rdata",
             `ECLS DCCS` = "ecls_dccs.Rdata"
                                        #,`ECLS Flanker` = "ecls_flanker.Rdata" ##just too easy
             )
    filenames
}




## m<-list()
## for (fn in filenames) {
##     load(fn)
##     x<-x[!is.na(x$resp),]
##     m[[fn]]<-mean(x$resp)
## }

## index<-order(as.numeric(m))
## filenames<-filenames[index]
## dump("filenames","")
