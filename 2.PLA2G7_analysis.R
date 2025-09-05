## COMPLETE R script for PLA2G7 Analysis

### FORMATTING ### 

## Formatting phenotype data for Lp-PLA2 analysis
# this script contains renaming of traits directly downloaded from the UKB
# Visualisation of distribution and transoformation for non-normal traits included at the end of the script 
# Covariate file contains age at recruitment, Sex, and PC1-5 
# Each pheno file was downloaded from the DNAnexus platform based on category (ie blood chemistry, NMR, immune etc)
# note for the rvtest inputs, FID, IID, fatid and matid headers need to be included (this is not shown in this script)
# note for SBP, DBP and pulse which have two measurements for each instance, 
  # mean was taken of measure 1 and measure 2 (body.csv)
# setup
library(openxlsx)
library(dplyr)

# 7. ICD coding -----------
  # 1 for diagnoses, 0 for no record of diagnosis 
d<-read.csv('ICD_participant.csv.crdownload')
ICDall<-d[,1:3]
rm(d)
write.csv(d, 'icd_participant.csv')

d<-read.csv('icd_participant.csv')
ICDall<-d[,1:4]

#hypertension codes all start with 'I1', this returns 162,488 individuals 
test_hypertensive<-ICDall[grepl("I1", ICDall$Diagnoses...ICD10), , drop = TRUE] 

#162,488
test_stroke<-ICDall[grepl("I6", ICDall$Diagnoses...ICD10), , drop = FALSE] #( agree w data showcase 25,722)

#21,190
test_vessel<-ICDall[grepl("I7", ICDall$Diagnoses...ICD10), , drop = FALSE]
#44432
test_t2d<-ICDall[grepl("E11", ICDall$Diagnoses...ICD10), , drop = FALSE]
#4279
test_parkinson<-ICDall[grepl("G20", ICDall$Diagnoses...ICD10), , drop = FALSE]
#2,922
test_degenerative<-ICDall[grepl("G25", ICDall$Diagnoses...ICD10), , drop = FALSE]
#70,716
test_heart<-ICDall[grepl("I2", ICDall$Diagnoses...ICD10), , drop = TRUE] 

# 14377
test_hyperdyslipidemia<-ICDall[grepl("E78.5", ICDall$Diagnoses...ICD10), , drop = TRUE] 

# 77444
test_hypercholesterolemia<-ICDall[grepl("E78.0", ICDall$Diagnoses...ICD10), , drop = TRUE] 

# combine lipidemia and hcolesterolmia 
test_alllipid<-c(test_hypercholesterolemia,test_hyperdyslipidemia)
test_alllipid<-unique(test_alllipid)
# 40,272
test_obesity<-ICDall[grepl("E66", ICDall$Diagnoses...ICD10), , drop = TRUE] 

#244
test_CKD<-ICDall[grepl("N18.1", ICDall$Diagnoses...ICD10), , drop = TRUE] 

#7771
test_NAFLD<-ICDall[grepl("K76.0", ICDall$Diagnoses...ICD10), , drop = TRUE] 

###ICD9
#1195
test_heart9<-ICDall[grepl("41", ICDall$Diagnoses...ICD9), , drop = TRUE] 
#2073
test_hypertensive9<-ICDall[grepl("40", ICDall$Diagnoses...ICD9), , drop = TRUE] 
#575
test_stroke9<-ICDall[grepl("43", ICDall$Diagnoses...ICD9), , drop = TRUE] 
#350
test_t2d9<-ICDall[grepl("250", ICDall$Diagnoses...ICD9), , drop = TRUE] 
#472
test_vessel9<-ICDall[grepl("44", ICDall$Diagnoses...ICD9), , drop = TRUE] 
#12
test_parkinson9<-ICDall[grepl("332", ICDall$Diagnoses...ICD9), , drop = TRUE] 
#9
test_degenerative9<-ICDall[grepl("333", ICDall$Diagnoses...ICD9), , drop = TRUE] 
# 
test_alllipid9<-ICDall[grepl("272.4", ICDall$Diagnoses...ICD10), , drop = TRUE] 

# obesity E66
test_obesity9<-ICDall[grepl("278", ICDall$Diagnoses...ICD9), , drop = TRUE] 

#kidney N18.1
test_CKD9<-ICDall[grepl("585", ICDall$Diagnoses...ICD9), , drop = TRUE] 

#NAFLD K76.0
test_NAFLD9<-ICDall[grepl("571", ICDall$Diagnoses...ICD9), , drop = TRUE] 


t2d_true<-c(test_t2d$Participant.ID,test_t2d9$Participant.ID)
ICDall$t2d<-ifelse(ICDall$Participant.ID %in% t2d_true ,1, 0) #t2d diagnosis participant  

heart_true<-c(test_heart$Participant.ID,test_heart9$Participant.ID)
ICDall$heart<-ifelse(ICDall$Participant.ID %in% heart_true ,1, 0) 

hypertension_true<-c(test_hypertensive$Participant.ID,test_hypertensive9$Participant.ID)
ICDall$hypertension<-ifelse(ICDall$Participant.ID %in% hypertension_true ,1, 0) 

stroke_true<-c(test_stroke$Participant.ID,test_stroke9$Participant.ID)
ICDall$stroke<-ifelse(ICDall$Participant.ID %in% stroke_true ,1, 0) 

vessel_true<-c(test_vessel$Participant.ID,test_vessel9$Participant.ID)
ICDall$vessel<-ifelse(ICDall$Participant.ID %in% vessel_true ,1, 0) 

parkinson_true<-c(test_parkinson$Participant.ID,test_parkinson9$Participant.ID)
ICDall$parkinson<-ifelse(ICDall$Participant.ID %in% parkinson_true ,1, 0) 

degenerative_true<-c(test_degenerative$Participant.ID,test_degenerative9$Participant.ID)
ICDall$degenerative<-ifelse(ICDall$Participant.ID %in% degenerative_true ,1, 0) 

alllipid_true<-c(test_hypercholesterolemia$Participant.ID,test_hyperdyslipidemia$Participant.ID, test_alllipid9$Participant.ID)
ICDall$alllipid<-ifelse(ICDall$Participant.ID %in% alllipid_true ,1, 0) 

obesity<-c(test_obesity$Participant.ID,test_obesity9$Participant.ID)
ICDall$obesity<-ifelse(ICDall$Participant.ID %in% obesity ,1, 0) 

CKD_true<-c(test_CKD$Participant.ID,test_CKD9$Participant.ID)
ICDall$CKD<-ifelse(ICDall$Participant.ID %in% CKD_true ,1, 0) 

NAFLD_true<-c(test_NAFLD$Participant.ID,test_NAFLD9$Participant.ID)
ICDall$NAFLD<-ifelse(ICDall$Participant.ID %in% NAFLD_true ,1, 0) 

write.csv(ICDall, 'ICD_updated.csv', row.names = FALSE) 
#medication reformatting 
d<-read.csv('medication_participant.csv')
d$Medication.for.cholesterol..blood.pressure.or.diabetes...Instance.0<-as.factor(d$Medication.for.cholesterol..blood.pressure.or.diabetes...Instance.0)
nomed<-c("Do not know ","Prefer not to answer","")
d$med1<-ifelse(d$Medication.for.cholesterol..blood.pressure.or.diabetes...Instance.0 %in% nomed ,0, 1)

write.csv(d,'medication.csv')


# 8. rank transformed data-------
rankT<-function(var) {
  n<-sum(!is.na(var))
  y<-qnorm((rank(var, na.last="keep")-0.5)/n)
  return(y)
}
x<-colnames(d)[grep('log',colnames(d))] #16 previously log transformed

rankTimpfatmass<-rankT(d$imp_fatmass)
rankTglucose<-rankT(d$Glucose)
rankTCRP<-rankT(d$CRP)
rankTLPA<-rankT(d$LPA)
rankTtri<-rankT(d$Triglycerides)
rankThba1c<-rankT(d$Hba1c)
rankTNMRtri<-rankT(d$NMR_triglycerides)
rankTbasophil<-rankT(d$basophil)
rankTbasophilperc<-rankT(d$basophilperc)
rankTeosinophil<-rankT(d$eosinophil)
rankTeosinophilperc<-rankT(d$eosinophilperc)
rankTlymphocyte<-rankT(d$lymphocyte)
rankTmonocyte<-rankT(d$monocyte)
rankTneutrophil<-rankT(d$neutrophil)
rankTreticulocyte<-rankT(d$Reticulocyte)
rankTWBCleuk<-rankT(d$WBCleuk)


rankT_dat<-as.data.frame(cbind(d$ID,rankTimpfatmass,rankTglucose,rankTCRP,rankTLPA,rankTtri,rankThba1c,
                               rankTNMRtri,rankTbasophil,rankTbasophilperc,rankTeosinophil,rankTeosinophilperc,rankTlymphocyte,
                               rankTmonocyte,rankTneutrophil,rankTreticulocyte,rankTWBCleuk,d$Sex))
colnames(rankT_dat)<-c("ID", colnames(rankT_dat)[-c(1,18)],"Sex")
rankTVars<-colnames(rankT_dat)[-c(1,18)]


########################################################################################

# DESCRIPTIVES -------
# summary tables
# split into case controls, calculate max, min, median, mean, sd, and number of NAs for each 

## CONTINUOUS VARIABLES 
cont.vars<-colnames(continuous)[-1]
cont.descriptives <- function(x) {
  cont.df <- data.frame(
    maximum = max(x, na.rm = TRUE),
    minimum = min(x, na.rm = TRUE),
    median = median(x, na.rm = TRUE),
    mean = mean(x, na.rm = TRUE),
    sd = sd(x, na.rm = TRUE),
    NAs = sum(is.na(x))
  )
  return(cont.df)
}

continuous.pla2g7carriers <- subset(continuous, continuous$pla2g6carrier==1)
results_cont_cases <- lapply(continuous.pla2g7carriers[, 3:75], cont.descriptives)
summary_continuous.pla2g7carriers <- do.call(rbind, results_cont_cases)
summary_continuous.pla2g7carriers$trait<-rownames(summary_continuous.pla2g7carriers)

continuous.pla2g7controls <- subset(continuous, continuous$pla2g6carrier==0)
results_cont_control <- lapply(continuous.pla2g7controls[, 3:75], cont.descriptives)
summary_continuous.pla2g7controls <- do.call(rbind, results_cont_control)
summary_continuous.pla2g7controls$trait<-rownames(summary_continuous.pla2g7controls)

continuous.pcsk9carriers <- subset(continuous, continuous$pcsk9carrier==1)
results_cont_cases.pcsk9 <- lapply(continuous.pcsk9carriers[, 3:75], cont.descriptives)
summary_continuous.pcsk9carriers <- do.call(rbind, results_cont_cases.pcsk9)
summary_continuous.pcsk9carriers$trait<-rownames(summary_continuous.pcsk9carriers)

continuous.pcsk9controls <- subset(continuous, continuous$pcsk9carrier==0)
results_cont_control.pcsk9 <- lapply(continuous.pcsk9controls[, 3:75], cont.descriptives)
summary_continuous.pcsk9controls <- do.call(rbind, results_cont_control.pcsk9)
summary_continuous.pcsk9controls$trait<-rownames(summary_continuous.pcsk9controls)


## BINARY VARIABLES 

binary.descriptives <- function(x) {
  tmp <- as.data.frame(table(x, useNA = "no"))
  case_count <- if (any(tmp$x == 1)) subset(tmp, x == 1)$Freq else 0
  control_count <- if (any(tmp$x == 0)) subset(tmp, x == 0)$Freq else 0
  na_count <- sum(is.na(x))
  
  binary.df <- data.frame(
    case = case_count,
    control = control_count,
    NAs = na_count
  )
  
  return(binary.df)
}


binary.pla2g7carriers <- subset(binary, binary$pla2g6carrier==1)
results_bin_cases <- lapply(binary.pla2g7carriers[, 3:15], binary.descriptives)
summary_binary.pla2g7carriers <- do.call(rbind, results_bin_cases)
summary_binary.pla2g7carriers$trait<-row.names(summary_binary.pla2g7carriers)

binary.pla2g7controls <- subset(binary, binary$pla2g6carrier==0)
results_bin_control <- lapply(binary.pla2g7controls[, 3:15], binary.descriptives)
summary_binary.pla2g7controls <- do.call(rbind, results_bin_control)
summary_binary.pla2g7controls$trait<-row.names(summary_binary.pla2g7controls)


binary.pcsk9carriers <- subset(binary, binary$pcsk9carrier==1)
results_binary_cases.pcsk9 <- lapply(binary.pcsk9carriers[, 3:15], binary.descriptives)
summary_binary.pcsk9carriers <- do.call(rbind, results_binary_cases.pcsk9)
summary_binary.pcsk9carriers$trait<-row.names(summary_binary.pcsk9carriers)

binary.pcsk9controls <- subset(binary, binary$pcsk9carrier==0)
results_bin_control.pcsk9 <- lapply(binary.pcsk9controls[, 3:15], binary.descriptives)
summary_binary.pcsk9controls <- do.call(rbind, results_bin_control.pcsk9)
summary_binary.pcsk9controls$trait<-row.names(summary_binary.pcsk9controls)


# Linear regression ------

## This is an example function for Continuous Variables 
lin_reg_pla2g7<-function(data) {
  results<-list()
  for (i in 4:75){
    temp_model<-lm(data[,i]~pla2g6carrier+sex+age.x+PC1+PC2+PC3+PC4+PC5, data=data,na.action=na.exclude)
    temp_res<-summary(temp_model)
    conf<-confint.default(temp_model)
    results[[i]]<-as.data.frame(cbind(temp_res$coefficients,conf,nobs(temp_model)))
    variable<-colnames(data)[i]
    results[[i]]$trait<-variable
  }
  return(results)
  colnames(results)<-c("beta","stderr","tvalue","pvalue","lowerCI","uppderCI","nobs","N","trait")
}

PLA2G7_cont <- lin_reg_pla2g7(data.in.cont)
res_PLA2G7_cont <-as.data.frame(do.call(rbind,PLA2G7_cont))
PLA2G7_rankT<-lin_reg_pla2g7(rankT_dat)
res_PLA2G7_rankT <-as.data.frame(do.call(rbind,PLA2G7_rankT))


## This is an example function for Continuous Variables 
glm_reg_pla2g7<-function(data) {
  results<-list()
  for (i in 5:15){
    temp_model<-glm(data[,i]~pla2g6carrier+sex+age+PC1+PC2+PC3+PC4+PC5, data=data, family="binomial",na.action=na.exclude)
    temp_res<-summary(temp_model)
    conf<-confint.default(temp_model)
    results[[i]]<-as.data.frame(cbind(temp_res$coefficients,conf,nobs(temp_model)))
    variable<-colnames(data)[i]
    results[[i]]$trait<-variable
  }
  return(results)
  colnames(results)<-c("beta","stderr","tvalue","pvalue","lowerCI","uppderCI","nobs","N","trait")
}

pla2g7_bin<- glm_reg_pla2g7(data.in.binary)
res_pla2g7_bin <-as.data.frame(do.call(rbind,pla2g7_bin))


###PLOTTING----------

# forest plot for linear regression
# forest plot will plot beta estimate, 95% CI 
require(ggplot2)
require(readxl)
require(dplyr)
data<-read_xlsx('forestplot_linregcont.xlsx',sheet=2)



# format data 
data<-data %>% 
  mutate(significant=ifelse(`P-value`<0.05,TRUE, FALSE)) %>% 
  mutate(fill_var = ifelse(significant, Domain, NA)) %>% 
  mutate(Domain=as.factor(Domain)) %>% 
  arrange(Domain)

# plot 
ggplot(data, aes(x=Estimate, y=Trait, color=Domain, fill=fill_var)) + 
  geom_point(shape=21)+
  geom_errorbarh(aes(xmin=lowerCI,xmax=upperCI), height=0.1)+
  facet_wrap(~Domain, scales="free")+
  scale_color_manual(
    values=c(
      "Blood chemistry"="lightblue",
      "Immune"="pink",
      "Impedence"="brown",
      "Measurements"="darkgreen",
      "NMR"="purple",
      "Telomeres"="darkgrey")
  )+
  scale_fill_manual(values=c(
    "Blood chemistry"="lightblue",
    "Immune"="pink",
    "NMR"="purple"),
    na.value = "white",   # fill non-significant polygons as white
    na.translate = FALSE  # do not create legend key for NA
  ) +
  geom_vline(xintercept=0, linetype="dashed", colour="darkgray")+
  guides(fill="none")+
  theme_minimal()


ggsave("linreg_continuous.jpeg")

## binary 
# plot ORs

data<-read_xlsx('forestplot_linregcont.xlsx',sheet=2)

data<-data %>% 
  mutate(significant=ifelse(`P-value`<0.05,TRUE, FALSE)) 

# plot 
ggplot(data, aes(x=OR, y=trait)) + 
  geom_point(shape=21)+
  geom_errorbarh(aes(xmin=OR_lowerCI,xmax=OR_upperCI), height=0.1)+
  geom_vline(xintercept=1, linetype="dashed", colour="lightgray")+
  theme_minimal() +
  ylab("Disease Diagnosis") 
ggsave("linreg_binary.jpeg")



