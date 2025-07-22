rm(list=ls())
pacman::p_load(RODBC, dplyr, tidyverse, openxlsx, tidyselect)
pacman::p_load("foreign", "stringr", "magrittr", "reshape2", "data.table", "psych", "vtable", "corrplot", "haven", 
               "survey", "ggplot2", "dplyr", "tidyr", "sf", "RColorBrewer")
#conn <- odbcConnect("IDMAzureDatabricks_DSN") 
options(max.print=1000000)
setwd("C:/Users/yingyili/OneDrive - Bill & Melinda Gates Foundation/Pathways/Modeling/India 2020 DHS")

### READ-IN TRUNCATED UP DATASET
load("Data/BR.RData")
load("Data/KR.RData")
load("Data/PR.RData")
load("Data/IR.RData")
load("Data/HH.RData")

######################################################################## KR FILE

##anemic
table(KR$hw57)
KR$kid.anemia <- ifelse(KR$hw57=="not anemic", 0, 1)
table(KR$kid.anemia)
prop.table(table(KR$kid.anemia))

# Wasting, stunting, underweight for all children
KR$hhid<-paste(KR$v001,KR$v002)
class(KR$hw5)
class(KR$hw8)
class(KR$hw11)
KR$haz<-KR$hw5/100; KR$haz<-ifelse(KR$haz>90,NA,KR$haz)
KR$waz<-KR$hw8/100; KR$waz<-ifelse(KR$waz>90,NA,KR$waz)
KR$whz<-KR$hw11/100; KR$whz<-ifelse(KR$whz>90,NA,KR$whz)

KR<- KR %>% mutate(stunt.cat1 =  case_when(
  haz < -3 ~ "Severely stunted",
  (haz >= -3 & haz < -2) ~ "Moderately stunted",
  haz >= -2 ~ "Not stunted"))
prop.table(table(KR$stunt.cat1))
# KR$stunt.cat1<-factor(KR$stunt.cat1, c("Severely stunted","Moderately stunted", "Not stunted"))
KR$stunt.cat2<-ifelse(KR$haz < -2,1,0)

KR<- KR %>% mutate(waste.cat =  case_when(
  whz < -2            ~ "Wasted",
  whz >= -2             ~ "Not wasted"))
KR$waste.cat<-factor(KR$waste.cat, c("Wasted", "Not wasted"))
prop.table(table(KR$waste.cat))
KR$waste.cat<-ifelse(KR$whz < -2,1,0)
#no enough variation to look into severe wasting

KR<- KR %>% mutate(unwgt.cat1 =  case_when(
  waz < -3 ~ "Severely underweight",
  (waz >= -3 & waz < -2) ~ "Moderately underweight",
  waz >= -2 ~ "Not underweight"))
prop.table(table(KR$unwgt.cat1))
# KR$unwgt.cat1 <- factor(KR$unwgt.cat1, c("Severely underweight","Moderately underweight", "Not underweight"))
KR$unwgt.cat2 <- ifelse(KR$unwgt.cat1!="Not underweight",1,0)
prop.table(table(KR$unwgt.cat1))
prop.table(table(KR$unwgt.cat2))

# Breastfeeding
# label define M4      
# 93 "Ever breastfed, not currently breastfeeding"
# 94 "Never breastfed"
# 95 "Still breastfeeding"
# 96 "Breastfed until died"
# 97 "Inconsistent"
# 98 "Don't know"

#is this variable available for all kids? 
sum(is.na(KR$m4))
table(KR$b19[!is.na(KR$m4)])
KR$breastfeed <- ifelse(KR$m4 ==94, 0, 1)
KR$breastfeed <-ifelse((KR$b19 < 24 & KR$b9 == "respondent"), KR$breastfeed, NA)
KR$breastfeed[KR$m4==98] <- NA
table(KR$breastfeed)
prop.table(table(KR$breastfeed))
#not a good variation 

#see what feeding questions are avaialble in India 2020 DHS 
v41 <- KR %>% dplyr:: select(starts_with("v41"))
lapply(v41, table)

KR <- KR %>%
  mutate(comp.feed = case_when(KR$v409 == "yes" | 
                                 KR$v410 == "yes" | 
                                 KR$v411 == "yes" | 
                                 KR$v411a == "yes" | 
                                 KR$v412a == "yes" | 
                                 KR$v412c == "yes" | 
                                 KR$v413 == "yes"  | 
                                 KR$v414a == "yes" |
                                 KR$v414e == "yes" | 
                                 KR$v414f == "yes" | 
                                 KR$v414g == "yes" | 
                                 KR$v414i == "yes" | 
                                 KR$v414j == "yes" | 
                                 KR$v414k == "yes" | 
                                 KR$v414l == "yes" | 
                                 KR$v414m == "yes" | 
                                 KR$v414n == "yes" | 
                                 KR$v414o == "yes" | 
                                 KR$v414p == "yes" | 
                                 KR$v414s == "yes" | 
                                 KR$v414t == "yes" | 
                                 KR$v414v == "yes" ~1, TRUE ~ 0))
KR$comp.feed<-ifelse((KR$b19 < 24 & KR$b9 == "respondent"),KR$comp.feed, NA)
table(KR$comp.feed)

KR$breastfeed.excl<-ifelse((KR$breastfeed==1 & KR$comp.feed==0),1,0)
table(KR$breastfeed.excl)

KR$breastfeed.n<-ifelse((KR$b19 < 24 & KR$b9 == "respondent"),1,0)
table(KR$breastfeed.n)

kr.mom<- KR %>% group_by(caseid) %>% summarize(
                                               k.breast.cnt=sum(breastfeed, na.rm=T), 
                                               k.breastcomp.cnt=sum(comp.feed, na.rm=T),
                                               k.breastex.cnt=sum(breastfeed.excl, na.rm=T),
                                               k.stunt.cnt=sum(stunt.cat2, na.rm=T), 
                                               k.waste.cnt = sum(waste.cat, na.rm=T), 
                                               k.unwgt.cnt = sum(unwgt.cat2, na.rm=T),
                                               k.anemia.cnt=sum(kid.anemia, na.rm=T))

######################################################################## BR FILE

# Immunization - zero-dose
#Similar to Cata-Preta et al, 2021, prevalence of zero-dose children was defined as those who had not received any doses of the 
#four vaccines (four basic vaccines: BCG, polio (OPV and IPV, doses given at birth were not considered in the analyses), DPT, and MCV). 
#The immunization cascade was generated as a score ranging from 0 to 4. Each type of vaccine accounts for one point in the cascade regardless of the number of doses. 
#For example, a child was coded as "1" if either one, two or three doses of DPT/polio vaccine had been received. 
BR[,ChildAge:= (v008-b3)] # in months
summary(BR[is.na(h2),]$ChildAge)
summary(BR[is.na(h4),]$b8)
summary(BR[is.na(h4) & b5=="yes",]$ChildAge)

BR[,ZeroDose:= ifelse(h2== "no" & h3== "no" & h4== "no" & h5== "no" & h6== "no" & h7== "no" & h8== "no" & h9== "no", 1, 0)]
BR$ZeroDose <- ifelse(BR$h2=="don't know" |BR$h3=="don't know"|BR$h4=="don't know" |BR$h5=="don't know" |BR$h6=="don't know" |BR$h7=="don't know" |BR$h8=="don't know" |BR$h9=="don't know",NA,BR$ZeroDose)
table(BR$ZeroDose)
prop.table(table(BR$ZeroDose)) #6% very low 

######################## 
BR[!(is.na(h2))& h2!="don't know",bcg:=ifelse(h2 %in% c("vaccination date on card","reported by mother","vaccination marked on card"),1,0)]
BR[!(is.na(h3))& h3!="don't know",dpt1:=ifelse(h3 %in% c("vaccination date on card","reported by mother","vaccination marked on card"),1,0)]
BR[!(is.na(h4))& h4!="don't know",opv1:=ifelse(h4 %in% c("vaccination date on card","reported by mother","vaccination marked on card"),1,0)]
BR[!(is.na(h5))& h5!="don't know",dpt2:=ifelse(h5 %in% c("vaccination date on card","reported by mother","vaccination marked on card"),1,0)]
BR[!(is.na(h6))& h6!="don't know",opv2:=ifelse(h6 %in% c("vaccination date on card","reported by mother","vaccination marked on card"),1,0)]
BR[!(is.na(h7))& h7!="don't know",dpt3:=ifelse(h7 %in% c("vaccination date on card","reported by mother","vaccination marked on card"),1,0)]
BR[!(is.na(h8))& h8!="don't know",opv3:=ifelse(h8 %in% c("vaccination date on card","reported by mother","vaccination marked on card"),1,0)]
BR[!(is.na(h9))& h9!="don't know",mcv1:=ifelse(h9 %in% c("vaccination date on card","reported by mother","vaccination marked on card"),1,0)]
BR[,dpt:= ifelse(dpt1==1 |dpt2==1 | dpt3==1, 1, 0)]
BR[,opv:= ifelse(opv1==1 |opv2==1 | opv3==1, 1, 0)]
BR[,score:= (bcg + opv + dpt + mcv1)]
BR[,score2:= (bcg + opv1 + opv2 + opv3 + dpt1 + dpt2 + dpt3 + mcv1)]
BR[,dpt12mcv1:= ifelse(dpt1==1 & dpt2==1 & mcv1==1 & dpt3==0, 1, 0)]
table(BR$score)

# Child death U5, U1
#b3 - Date of birth of child (CMC)
#b5 - Child is alive (1 = Yes, 0 = No)
#b7 - Age at death in months (imputed)
BR$hhid<-paste(BR$v001,BR$v002)
BR$u1mort<-ifelse(BR$b7<13 & BR$b5=="no",1,0)
BR$u5mort<-ifelse(BR$b5=="no",1,0)

length(unique(BR$caseid))
br.mom <- BR %>% group_by(caseid) %>% summarize(k.dpt1.cnt = sum(dpt1, na.rm=T),
                                                k.u1mort.cnt = sum(u1mort, na.rm=T), 
                                                k.u5mort.cnt = sum(u5mort, na.rm=T))

######################################################################## IR file



#label variable v208     "Births in last five years"
#Pregnancy loss: 
# v234: last pregnancy: need to find a denominator among those who had a last pregnancy??? 
# table(IR$s234)
# prop.table(table(IR$s234))
# past five years: vcal_7
IR$cal <- substr(IR$vcal_7, IR$v018, IR$v018+59)
IR$preg_5yr <- ifelse(grepl("P", IR$cal), 1, 0)
table(IR$preg_5yr)
#still birth: A baby who dies after 28 weeks of pregnancy, but before or during birth,
x <- IR$cal
IR$preg.live.cnt   <- lengths(regmatches(x, gregexpr("B", x)))
IR$preg.miscar.cnt <- lengths(regmatches(x, gregexpr("M", x)))
IR$preg.abort.cnt  <- lengths(regmatches(x, gregexpr("A", x)))
IR$preg.still.cnt  <- lengths(regmatches(x, gregexpr("S", x)))
table(IR$preg.live.cnt)
table(IR$preg.miscar.cnt)
table(IR$preg.abort.cnt)
table(IR$preg.still.cnt)

#ever experience preg loss for those who had a pregnancy event in the 5-year calendar 
IR$preg.loss <- ifelse(IR$preg.miscar.cnt  >0 |
                         IR$preg.abort.cnt >0 |
                         IR$preg.still.cnt >0, 1, 0)
table(IR$preg.loss)
prop.table(table(IR$preg.loss)) # 16%
#recode denominator to those who ever had pregnancy in calendar data 
# IR$preg.loss[IR$preg_5yr==0]       <- "0: No preg in the last 5 years"
# IR$preg.miscar.cnt[IR$preg_5yr==0] <- NA
# IR$preg.abort.cnt[IR$preg_5yr==0]  <- NA
# IR$preg.still.cnt[IR$preg_5yr==0]  <- NA
# table(IR$preg.miscar.cnt)
# table(IR$preg.abort.cnt)
# table(IR$preg.still.cnt)
# prop.table(table(IR$preg.miscar.cnt))
# prop.table(table(IR$preg.abort.cnt))
# prop.table(table(IR$preg.still.cnt))



# Recode FP
sum(is.na(IR$v313))
sum(is.na(IR$v359))
IR$fp.mod.now <- ifelse(IR$v313 == "modern method",1,0)
IR$fp.any.now <- ifelse(IR$v313 == "no method",0,1)
IR$fp.dis.mod.5yr<-ifelse((IR$v359=="diaphragm" |
                             IR$v359=="emergency contraception" |
                             IR$v359=="female condom" |
                             IR$v359=="injections"|
                             IR$v359=="iud" | 
                             IR$v359=="lactational amenorrhea (lam)" | 
                             IR$v359=="male condom" |
                             IR$v359=="pill"),1,0)
IR$fp.dis.any.5yr<-ifelse((IR$fp.dis.mod.5yr==1 | 
                             IR$v359=="other traditional" |
                             IR$v359=="periodic abstinence" | 
                             IR$v359=="withdrawal"),1,0)
IR$fp.dis.mod.5yr[is.na(IR$fp.dis.mod.5yr)] <- 0
IR$fp.dis.any.5yr[is.na(IR$fp.dis.any.5yr)] <- 0
IR$fp.mod.ever<-ifelse((IR$fp.mod.now==1 | IR$fp.dis.mod.5yr==1),1,0)
IR$fp.any.ever<-ifelse((IR$fp.any.now==1 | IR$fp.dis.any.5yr==1),1,0)
prop.table(table(IR$fp.mod.now))
prop.table(table(IR$fp.any.now))
prop.table(table(IR$fp.dis.mod.5yr))
prop.table(table(IR$fp.dis.any.5yr))
prop.table(table(IR$fp.mod.ever))
prop.table(table(IR$fp.any.ever))
sum(is.na(IR$fp.mod.now))
sum(is.na(IR$fp.any.now))
sum(is.na(IR$fp.mod.ever))
sum(is.na(IR$fp.any.ever))

# Recode home births
sum(is.na(IR$m15_1))
IR$hb1 <- ifelse(grepl("home", IR$m15_1), 1, 0)
IR$hb2 <- ifelse(grepl("home", IR$m15_2), 1, 0)
IR$hb3 <- ifelse(grepl("home", IR$m15_3), 1, 0)
IR$hb4 <- ifelse(grepl("home", IR$m15_4), 1, 0)
IR$hb5 <- ifelse(grepl("home", IR$m15_5), 1, 0)
IR$hb6 <- ifelse(grepl("home", IR$m15_6), 1, 0)
IR$hb.ever <- ifelse(IR$hb1==1 | 
                       IR$hb2==1 |
                       IR$hb3==1 |
                       IR$hb4==1 | 
                       IR$hb5==1 |
                       IR$hb6==1, 1, 0)
IR$hb.ever[!is.na(IR$m15_1) & !is.na(IR$m15_2) & !is.na(IR$m15_3) &
             !is.na(IR$m15_4) & !is.na(IR$m15_5) & !is.na(IR$m15_1) & is.na(IR$hb.ever)] <- 0
table(IR$hb.ever)
prop.table(table(IR$hb.ever))
sum(is.na(IR$hb.ever))

# Women's weight
table(IR$v445)
IR$v445[IR$v445=="flagged cases"] <- NA
IR$v445 <- as.numeric(IR$v445)
IR$bmi<-IR$v445/100
IR$bmi<-ifelse(IR$bmi>90,NA,IR$bmi)
summary(IR$bmi)
IR<- IR %>% mutate(bmi.cat = case_when(
  (bmi < 18.5) ~ "Underweight",
  (bmi >= 18.5 & bmi < 25) ~ "Normal",
  (bmi >= 25 ) ~ "Overweight/Obese"))
IR$bmi.cat<-factor(IR$bmi.cat, c("Underweight","Normal", "Overweight/Obese"))
prop.table(table(IR$bmi.cat))

IR$underweight <- ifelse(IR$bmi.cat=="Underweight", 1, 0)
IR$overweight <- ifelse(IR$bmi.cat=="Overweight/Obese", 1, 0)
sum(is.na(IR$bmi.cat))
sum(is.na(IR$underweight))
sum(is.na(IR$overweight))

# Number of ANC visits: for the MOST RECENT BIRTHS 
# m13: Timing of 1st antenatal check (months)
table(IR$m13_1)
IR$m13_1[IR$m13_1=="don't know"] <- NA
IR$m13_1 <- as.numeric(IR$m13_1)
table(IR$m14_1)
summary(IR$m13_1)
IR$anc.1stvisit <- IR$m13_1
#for no ANC, set to 10, == birth
IR$anc.1stvisit[IR$m14_1=="no antenatal visits"] <- 10
table(IR$anc.1stvisit)
sum(is.na(IR$anc.1stvisit))

IR$anc.month<-ifelse(IR$anc.1stvisit<4,"Month 1-3",
                     ifelse(IR$anc.1stvisit>3 & IR$anc.1stvisit<7,"Month 4-6",
                            ifelse(IR$anc.1stvisit>6 & IR$anc.1stvisit<10, "Month 7-9", "No ANC")))
IR$anc.month <- factor(IR$anc.month, c("No ANC","Month 1-3","Month 4-6", "Month 7-9"))
table(IR$anc.month)

#m14: Number of antenatal visits during pregnancy
table(IR$m14_1)
table(IR$m14_2)
table(IR$m14_3)
table(IR$m14_4)
table(IR$m14_5)
IR$m14_1[IR$m14_1=="don't know"] <- NA
IR$m14_1[IR$m14_1=="no antenatal visits"] <- "0"
IR$m14_1 <- as.numeric(IR$m14_1)
summary(IR$m14_1)
IR$anc.num <- IR$m14_1 
IR$anc.4plus <-ifelse(IR$anc.num>=4, 1, 0)
# IR$anc.4plus[is.na(IR$anc.4plus)] <- "0: No births in the past 5 years"
table(IR$anc.4plus)
prop.table(table(IR$anc.4plus))

# 
# IR$anc.any <- ifelse(IR$m14_1 == 0, "1: no ANC", "2: Any ANC")
# table(IR$anc.any) #no variation 
# prop.table(table(IR$anc.any))
IR$anc.1st3month <- ifelse(IR$anc.month=="Month 1-3", 1, 0)
# IR$anc.1st3month[is.na(IR$anc.1st3month)] <- "0: No births in the past 5 years"
table(IR$anc.1st3month)
prop.table(table(IR$anc.1st3month))

# Woman had health check after birth
table(IR$m66_1)
temp1<-ifelse(IR$m66_1=="yes" & !is.na(IR$m66_1),1,0)
temp2<-ifelse(IR$m62_1=="yes" & !is.na(IR$m62_1),1,0)
IR$mom.hlthck<-ifelse((temp1==1|temp2==1), 1, 0)
# IR$mom.hlthck<-ifelse((is.na(IR$m62_1) & is.na(IR$m66_1)),"0: No births in the past 5 years",IR$mom.hlthck)
table(IR$mom.hlthck)
prop.table(table(IR$mom.hlthck))

# Baby had health check after birth
temp3<-ifelse(IR$m70_1=="yes" & !is.na(IR$m70_1),1,0)
temp4<-ifelse(IR$m74_1=="yes" & !is.na(IR$m74_1),1,0)
IR$baby.hlthck<-ifelse((temp3==1|temp4==1), "2: yes", "1: No")
IR$baby.hlthck<-ifelse((is.na(IR$m70_1) & is.na(IR$m74_1)),"0: No births in the past 5 years",IR$baby.hlthck)
table(IR$baby.hlthck)
prop.table(table(IR$baby.hlthck))

# Wasting, stunting, underweight for last child
class(IR$hw5_1)
class(IR$hw8_1)
class(IR$hw11_1)
table(IR$hw5_1)
table(IR$hw8_1)
table(IR$hw11_1)
IR$hw5_1[IR$hw5_1=="flagged cases"] <- NA
IR$hw8_1[IR$hw8_1=="flagged cases"] <- NA
IR$hw11_1[IR$hw11_1=="flagged cases"] <- NA
IR$hw5_1 <- as.numeric(IR$hw5_1)
IR$hw8_1 <- as.numeric(IR$hw8_1)
IR$hw11_1 <- as.numeric(IR$hw11_1)
IR$haz.last<-IR$hw5_1/100; IR$haz.last<-ifelse(IR$haz.last>90,NA,IR$haz.last) #height/age standard deviation (AG 2/28)
IR$whz.last<-IR$hw11_1/100; IR$whz.last<-ifelse(IR$whz.last>90,NA,IR$whz.last) #weight/height standard deviation (AG 2/28)
IR$waz.last<-IR$hw8_1/100; IR$waz.last<-ifelse(IR$waz.last>90,NA,IR$waz.last) #weight/age standard deviation (AG 2/28)

IR<- IR %>% mutate(stunt.cat1.last =  case_when(
  haz.last < -3 ~ "Severely stunted",
  (haz.last >= -3 & haz.last < -2) ~ "Moderately stunted",
  haz.last >= -2 ~ "Not stunted"))
# IR$stunt.cat1.last<-factor(IR$stunt.cat1.last, c("Severely stunted","Moderately stunted", "Not stunted"))
IR$stunt.cat2.last<-ifelse(IR$haz.last < -2,1,0)
prop.table(table(IR$stunt.cat1.last))
prop.table(table(IR$stunt.cat2.last))

IR<- IR %>% mutate(waste.cat.last =  case_when(
  whz.last < -2 ~ 1,
  whz.last >= -2 ~ 0))
prop.table(table(IR$waste.cat.last))

IR<- IR %>% mutate(unwgt.cat1.last =  case_when(
  waz.last < -3 ~ "Severely underweight",
  (waz.last >= -3 & waz.last < -2) ~ "Moderately underweight",
  waz.last >= -2 ~ "Not underweight"))
# IR$unwgt.cat1.last<-factor(IR$unwgt.cat1.last, c("Severely underweight","Moderately underweight", "Not underweight"))
IR$unwgt.cat2.last <- ifelse(IR$waz.last < -2,1,0)
prop.table(table(IR$unwgt.cat1.last))
prop.table(table(IR$unwgt.cat2.last))

# Breastfeeding last/youngest child who was born in the last 2 years 
# IR$breastfed.last <-ifelse(IR$m4_1 == "ever breastfed, not currently breastfeeding" | IR$m4_1=="still breastfeeding",1,0)
# IR$breastfed.last <-ifelse((IR$b19_01 < 24 & IR$b9_01 == "respondent"),IR$breastfed.last, NA) #child is <24 months, child lives with respondent (AG added 2/28)
# prop.table(table(IR$breastfed.last))
# sum(is.na(IR$breastfed.last))

# ##for the last/ youngest child
# sum(is.na(IR$v409))
# table(IR$b19_01[!is.na(IR$v409)]) # this question was only asked for kids who were < 24 months 
# IR <- IR %>%
#   mutate(comp.feed.last = case_when(IR$v409 == "yes" | 
#                                       IR$v410 == "yes" | 
#                                       IR$v411 == "yes" | 
#                                       IR$v411a == "yes" | 
#                                       IR$v412a == "yes" | 
#                                       IR$v412c == "yes" | 
#                                       IR$v413 == "yes"  | 
#                                       IR$v414a == "yes" |
#                                       IR$v414e == "yes" | 
#                                       IR$v414f == "yes" | 
#                                       IR$v414g == "yes" | 
#                                       IR$v414i == "yes" | 
#                                       IR$v414j == "yes" | 
#                                       IR$v414k == "yes" | 
#                                       IR$v414l == "yes" | 
#                                       IR$v414m == "yes" | 
#                                       IR$v414n == "yes" | 
#                                       IR$v414o == "yes" | 
#                                       IR$v414p == "yes" | 
#                                       IR$v414s == "yes" | 
#                                       IR$v414t == "yes" | 
#                                       IR$v414v == "yes" ~1, TRUE ~ 0))
# IR$comp.feed.last<-ifelse((IR$b19_01 < 24 & IR$b9_01 == "respondent"),IR$comp.feed.last, NA) ##these are questions on other feed types, like water or solids
# table(IR$comp.feed.last)
# # very close to the kid file 
# 
# IR$breastfed.last.excl<-ifelse((IR$breastfed.last==1 & IR$comp.feed.last==0),1,0)
# table(IR$breastfed.last.excl)

#anemia 
table(IR$v455)
table(IR$v456)
class(IR$v456)
table(IR$v213)
# Any anemia:
# Number of non-pregnant women whose hemoglobin count is less than 12.0 grams per deciliter (g/dl) (v456 < 120)
# plus number of pregnant women whose count is less than 11.0 g/dl (v456 < 110)
IR <- IR %>% 
  mutate(anemia=case_when(
    v213=="yes" & v456 <110 ~ 1,
    v213!="yes" & v456 <120 ~ 1
  ))
table(IR$anemia)
IR$anemia[is.na(IR$anemia) & IR$v455=="measured"] <- 0 
table(IR$anemia)
prop.table(table(IR$anemia))
sum(is.na(IR$anemia))
# 
# ##menstrual hygiene: MISSING A LOT
# table(IR$v215)
# table(IR$v226)
# table(IR$v227)
# IR$v215 <- as.numeric(IR$v215)
# IR$time_last_period <- ifelse(IR$v215 >=100 & IR$v215 <=399, 1, 0)
# table(IR$time_last_period)
# #who used appropriate and hygenic materials during their last menstruation
# #Women who use locally prepared napkins, sanitary napkins, menstrual cups, or tampons 
# table(IR$s260a) #Cloth
# table(IR$s260b) #Locally prepared napkins
# table(IR$s260c) #Sanitary napkins
# table(IR$s260d) #Tampons
# table(IR$s260e) #Menstrual cup
# table(IR$s260f) #Nothing
# table(IR$s260x) #Others 
# table(IR$s264)
# table(IR$s265)
# 
# IR$menstru_hygenic <- ifelse(IR$s260b=="yes" | IR$s260c=="yes" | IR$s260d=="yes" | IR$s260e=="yes", 1, 0)
# IR$menstru_hygenic[is.na(IR$s260a) & is.na(IR$s260b) & is.na(IR$s260c) & is.na(IR$s260d) &
#                      is.na(IR$s260e) & is.na(IR$s260f) & is.na(IR$s260x)] <- NA
# table(IR$menstru_hygenic)
# prop.table(table(IR$menstru_hygenic))
# sum(is.na(IR$menstru_hygenic))
# 
# IR$menstru_bath <- ifelse(IR$s264=="yes" & IR$s265=="yes", 1, 0)
# IR$menstru_bath[is.na(IR$s264) & is.na(IR$s265)] <- NA
# table(IR$menstru_bath)
# prop.table(table(IR$menstru_bath)) # no much variation 

## STI (self-reported)
table(IR$v525)
table(IR$v763a)
table(IR$v763b)
table(IR$v763c)
IR$sti <- ifelse(IR$v763a=="yes" | IR$v763b =="yes" | IR$v763c=="yes", 1, 0)
table(IR$sti)
IR$sti[is.na(IR$v763a) & is.na(IR$v763b) &is.na(IR$v763c)] <- NA
table(IR$sti)
prop.table(table(IR$sti))
sum(is.na(IR$sti))

##hypertension 
table(IR$s728b)
prop.table(table(IR$s728b))
table(IR$sb18s)
table(IR$sb18d)
sum(is.na(IR$sb18d))
IR$sb18s <- as.numeric(IR$sb18s)
IR$sb25s <- as.numeric(IR$sb25s)
IR$sb29s <- as.numeric(IR$sb29s)
IR$mean_systolic <- rowMeans(IR[c('sb18s', 'sb25s', 'sb29s')], na.rm = TRUE)
summary(IR$mean_systolic)
IR$sb18d <- as.numeric(IR$sb18d)
IR$sb25d <- as.numeric(IR$sb25d)
IR$sb29d <- as.numeric(IR$sb29d)
IR$mean_diastolic <- rowMeans(IR[c('sb18d', 'sb25d', 'sb29d')], na.rm = TRUE)
summary(IR$mean_diastolic)
#The new guidelines define hypertension as a systolic blood pressure of =130 mmHg 
#or diastolic blood pressure of =80 mmHg or treatment
IR$hypertension <- ifelse(IR$mean_systolic>=140 | IR$mean_diastolic>=90, 1, 0)
IR$hypertension[is.na(IR$mean_systolic) & is.na(IR$mean_diastolic)] <- NA
table(IR$hypertension)
prop.table(table(IR$hypertension))
sum(is.na(IR$hypertension))

#blood glucose 
table(IR$sb56)
table(IR$sb57)
table(IR$sb74)
sum(is.na(IR$sb74))
sum(is.na(IR$sb56))
sum(is.na(IR$sb74))
IR$sb74 <- as.numeric(IR$sb74)
summary(IR$sb74)
table(IR$sb74)
IR$blood_glucose <- ifelse(IR$sb56=="yes" | IR$sb57=="yes", 1, 0)
IR$blood_glucose[IR$sb74>=140] <- 1
table(IR$blood_glucose)
prop.table(table(IR$blood_glucose))
sum(is.na(IR$blood_glucose))
 
##exam: too little variation 
# table(IR$sb79)
# table(IR$sb80)
# table(IR$sb81)


ir.out <- IR %>% select(c("caseid", "v001","v002", "v025",  
                          'preg.loss',
                          'hb.ever',
                          'anc.4plus',
                          'anc.1st3month',
                          'mom.hlthck',
                          # 'stunt.cat1.last',
                          'stunt.cat2.last',
                          'waste.cat.last',
                          # 'unwgt.cat1.last',
                          'unwgt.cat2.last',
                          'fp.mod.now',
                          'fp.any.now',
                          'fp.mod.ever',
                          'fp.any.ever',
                          'overweight',
                          'underweight',
                          # 'bmi.cat',
                          'sti',
                          'blood_glucose',
                          'hypertension',
                          'anemia'))

colnames(kr.mom)
colnames(br.mom)
lapply(kr.mom[, 2:8], hist)
lapply(br.mom[, 2:3], hist)
#make all KR and BR outcomes binary 

kr.mom$k.breast.cnt[kr.mom$k.breast.cnt>1] <- 1
kr.mom$k.breastcomp.cnt[kr.mom$k.breastcomp.cnt>1] <- 1
kr.mom$k.breastex.cnt[kr.mom$k.breastex.cnt>1] <- 1
kr.mom$k.stunt.cnt[kr.mom$k.stunt.cnt>1] <- 1
kr.mom$k.waste.cnt[kr.mom$k.waste.cnt>1] <- 1
kr.mom$k.unwgt.cnt[kr.mom$k.unwgt.cnt>1] <- 1
kr.mom$k.anemia.cnt[kr.mom$k.anemia.cnt>1] <- 1

br.mom$k.dpt1.cnt[br.mom$k.dpt1.cnt>1] <- 1
br.mom$k.u1mort.cnt[br.mom$k.u1mort.cnt>1] <- 1
br.mom$k.u5mort.cnt[br.mom$k.u5mort.cnt>1] <- 1

lapply(kr.mom[, 2:8], table)
lapply(br.mom[, 2:4], table)

outcomes <- merge(ir.out, kr.mom, by="caseid", all.x = TRUE)
outcomes <- merge(outcomes, br.mom, by="caseid", all.x=T)

## Make KR and BR outcomes 'Not applicable' category 
colSums(is.na(outcomes))
table(outcomes$preg.loss)

colSums(is.na(outcomes))
table(outcomes$v025)

lapply(outcomes, class)
lapply(outcomes, table)

##Adi update on file path:
save(outcomes, file = "India_constructed_data/1_UP_DHS_outcomes.RData")



















