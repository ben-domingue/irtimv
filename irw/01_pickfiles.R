##this is beset with directory problems


f<-function(fn) {
    print(fn)
    load(fn)
    df<-df[!is.na(df$resp),]
    ##dichotomous
    df<-df[df$resp %in% 0:1,]
    if (nrow(df)>0) {
        ##nperson
        person.n<-length(unique(df$id))
        ##nitems
        item.n<-length(unique(df$item))
        ##sparseneses
        n<-length(df$resp)
        per<-(sqrt(n)/person.n)*(sqrt(n)/item.n)
        ##
        c(fn=fn,person=person.n,item=item.n,sp=per)
    } else NULL
}

setwd("./irw/")
lf<-list.files(pattern="*.Rdata")
print(lf)
                                        #lapply(lf[1:4],f)

library(parallel)
L<-mclapply(lf,f,mc.cores=15)
#L<-lapply(lf,f)

save(L,file="/home/users/bdomingu/imv_irw/pickfiles.Rdata")

load("pickfiles.Rdata")
z<-do.call("rbind",L)
z<-data.frame(z)
for (i in 2:4) z[,i]<-as.numeric(z[,i])
z<-z[z$person>100,]
z<-z[z$item>3,]
z<-z[z$sp>.8 & z$sp<=1,]
nn<-z$person*z$item
z<-z[order(nn),]
lf<-z$fn

lf <-
c("polca_carcinoma.Rdata", "polca_values.Rdata", "dd_rotation.Rdata", 
"polca_cheating.Rdata", "abortion.Rdata", "slingshot.Rdata", 
"lsat.Rdata", "transreas_mokken.Rdata", "psychtools_blot.Rdata", 
"mpsycho_zareki.Rdata", "trees_sirt.Rdata", "wirs.Rdata", "janssen2_tam.Rdata", 
"verbagg.Rdata", "experimental_iq.Rdata", "chess_lnirt.Rdata", 
"eurpar2_mudfold.Rdata", "frac20.Rdata", "pks_probability.Rdata", 
"balance_mokken.Rdata", "geiser_tam.Rdata", "thurstonian_ddiffusion.Rdata", 
"oli_psych_course.Rdata", "measuring_lexical_quality.Rdata", 
"g308_sirt.Rdata", "wordsum.Rdata", "bpi_mplus.Rdata", "differences_spotting_study2.Rdata", 
"csedm_data_challenge_spring2019full.Rdata", "cdm_timss03.Rdata", 
"naep_multilcirt.Rdata", "mpsycho_rwdq.Rdata", "4thgrade_math_sirt.Rdata", 
"quantshort.Rdata", "psychtools_ability.Rdata", "csedm_data_challenge_fall2019full.Rdata", 
"difNLR_msatb.Rdata", "cdm_pisa00R.Rdata", "vocab_assessment_3_to_8_year_old_children.Rdata", 
"differences_spotting_study1.Rdata", "mpsycho_Rmotivation.Rdata", 
"mpsycho_wilmer.Rdata", "mpsycho_wilpat.Rdata", "content_literacy_intervention.Rdata", 
"mcmi_mokken.Rdata", "cdm_hr.Rdata", "mpsycho_YouthDep.Rdata", 
#"roar_lexical.Rdata",
"mobility.Rdata", "art.Rdata", "cdm_ecpe.Rdata", 
"fims_tam.Rdata", "approaches_to_text_study2.Rdata", "big5_sirt.Rdata", 
"pirlsmissing_sirt.Rdata", "Mental_Health_Questionnaire_for_the_Elderly.Rdata", 
"psychtools_epi.Rdata", "hte_irw.Rdata", "tma.Rdata", "RDatasets_gssabortion.Rdata", 
"credentialform_lnirt.Rdata", "duval4.Rdata", "psychoneurotic_inventory.Rdata", 
"vocabulary_iq.Rdata", "duval8.Rdata", "state_c1_2007_3_responses.Rdata", 
"state_c3_2007_7_responses.Rdata", "state_c3_2007_6_responses.Rdata", 
"state_c3_2007_8_responses.Rdata", "state_c3_2007_9_responses.Rdata", 
"state_c1_2007_10_responses.Rdata", "state_c3_2007_5_responses.Rdata", 
"state_c1_2007_7_responses.Rdata", "state_c1_2007_6_responses.Rdata", 
"state_c1_2007_4_responses.Rdata", "state_c1_2007_5_responses.Rdata", 
"state_c1_2007_8_responses.Rdata", "state_c1_2007_9_responses.Rdata", 
"enem_2013_1mil_lc.Rdata", "enem_2019_1mil_lc.Rdata", "enem_2013_1mil_mt.Rdata", 
"enem_2013_1mil_ch.Rdata", "enem_2013_1mil_cn.Rdata", "enem_2014_1mil_ch.Rdata", 
"enem_2019_1mil_ch.Rdata", "enem_2019_1mil_mt.Rdata", "enem_2018_1mil_mt.Rdata", 
"enem_2019_1mil_cn.Rdata", "enem_2014_1mil_cn.Rdata", "enem_2018_1mil_cn.Rdata")
