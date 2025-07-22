
################################################################################
# GENERATE OUTCOME VARIABLES
################################################################################

###################################
# RUN SETUP
# IR <- IR1
# KR <- KR1
# BR <- BR1


###################################
# DEFINE FUNCTION
###################################

gen_outcome_variables <- function(IR, KR, BR){

  IR[,str:=substr(vcal_1, v018, v018+59)]
  IR[,STL:=ifelse(grepl('TPPPPPP',str),1,NA)]
  IR[,LB:=ifelse(grepl('BPPPPPPP',str),1,NA)]

  ###################################
  # CALCULATE STILLBIRTHS BY CASEID

  ###################################
  # GET STILL BIRTHS HELPER FUNCTION
  getStillbirths <- function(individ,id_vars,recode=7){
    setDT(individ)

    #tmp=individ[v228 %in% 1,some,with=F]}
    #tmp[v233>6,] # v233: months of pregnancy.

    tmp=data.table::melt(individ, measure.vars=patterns('^bord[_]','^b0[_]','^b3[_]','^b4[_]','^b5[_]','^b6[_]','^b7[_]','^b8[_]','^b11[_]','^m14[_]','^m15[_]','^m17[_]'),
                         value.name=c('bord','b0','dob',"sex",'alive',"deathAge",'deathAgeMo','CurrAge',"b11","m14","m15","m17"),variable.name='ReverseOrder',
                         id.vars=id_vars)
  }


  stl_var_list = c("caseid", "v000", "v001", "v002", "v003", "v004", "v005", "v006", "v007", "v008", "v008a", "v009", "v010", "v011", "v012", "v013", "v014", "v015", "v016", "v017",
                   "v018", "v019", "v019a", "v020", "v021", "v022", "v023", "v024", "v025", "v026", "v027", "v028", "v029", "v030", "v031", "v032", "v034", "v040",
                   "sm508va_4", "sm508va_5", "sm508va_6", "str", "STL", "LB")

  some <- names(IR)[(names(IR) %in% stl_var_list)]

  stl <- getStillbirths(IR, id_vars=some)

  stl[,id:=1:dim(stl)[1]]
  stl[,start:=str_locate_all(str,"TPPPPPP")[[1]][1],by=c("id")]
  stl[,MomAgeSTL:=ceiling((v012 - (start/12))),]
  stl[,MomAge:=ceiling((dob-v011)/12),]

  stl$period = 60
  stl$tu <- stl$v008
  stl$tl <- stl$v008 - stl$period
  stl <- stl[tl <= dob & dob < tu,]  #5  years, due to limitation of stillbirth data

  stl[,index:= paste(caseid,v000,v007)]
  stl[,diff:=MomAgeSTL-MomAge]

  sub <- stl[STL==1,]
  setkey(sub,index)

  sub$rep<- NA
  sub$rep[1] <- 1
  for (i in 2:dim(sub)[1]){
    sub$rep[i] <- ifelse(sub$index[i]==sub$index[i-1],0,1)
  }
  setkey(sub,index)

  sub <- sub[rep==1,]
  sub[,MomAge:=MomAgeSTL]
  sub[,deathAge:=NA]
  sub[,CurrAge:=NA]
  sub[,deathAgeMo:=NA]
  sub[,sex:=NA]
  sub[,dob:=NA]
  sub[,alive:="STL"]

  dim(sub)
  # stl <- rbind(stl,sub[,-67])  # "rep"
  stl <- rbind(stl, sub[,-c("rep")])

  setkey(stl,index)

  stl[alive=="STL",stl:=1]
  stl[is.na(stl),stl:=0]

  stl.out <- stl %>% group_by(caseid) %>% summarize(stl.cnt=sum(stl, na.rm=T))

  ###################################
  # RECODE FP VARS
  IR$fp.mod.now <- ifelse(IR$v313 == "modern method",1,0)
  IR$fp.dis.5yr <- ifelse((IR$v359=="pill"|IR$v359=="iud"|IR$v359=="injections"|IR$v359=="male condom"|
                           IR$v359=="implants/norplant"|IR$v359=="female condom"|IR$v359=="emergency contraception"|
                           IR$v359=="other modern method"),1,0)
  IR$fp.dis.5yr[is.na(IR$fp.dis.5yr)] <- 0
  IR$fp.mod.ever <- ifelse((IR$fp.mod.now==1 | IR$fp.dis.5yr==1),1,0)

  ###################################
  # RECODE HOME BIRTHS
  IR[,hb.1:= ifelse(m15_1== "home" | m15_1== "respondent's home" | m15_1== "other home" | m15_1== "other",1,0)]
  IR[,hb.2:= ifelse(m15_2== "home" | m15_2== "respondent's home" | m15_2== "other home" | m15_2== "other",1,0)]
  IR[,hb.3:= ifelse(m15_3== "home" | m15_3== "respondent's home" | m15_3== "other home" | m15_3== "other",1,0)]
  IR[,hb.4:= ifelse(m15_4== "home" | m15_4== "respondent's home" | m15_4== "other home" | m15_4== "other",1,0)]
  IR[,hb.5:= ifelse(m15_5== "home" | m15_5== "respondent's home" | m15_5== "other home" | m15_5== "other",1,0)]

  IR$hb.ever[(IR$hb.1==1 | IR$hb.2==1 | IR$hb.3==1 | IR$hb.4==1 | IR$hb.5==1)]<- 1
  IR$hb.ever[(IR$hb.1==0 & IR$hb.2==0 & IR$hb.3==0 & IR$hb.4==0 & IR$hb.5==0)]<-0
  IR$hb.ever <- ifelse((is.na(IR$hb.1))&(is.na(IR$hb.2))&(is.na(IR$hb.3))&(is.na(IR$hb.4))&(is.na(IR$hb.5)),NA,IR$hb.ever)

  ###################################
  # CALCULATE WOMEN'S WEIGHT (BMI)
  IR <- IR %>%
    mutate(v445=as.numeric(v445))

  IR$bmi<-IR$v445/100
  IR$bmi<-ifelse(IR$bmi>90,NA,IR$bmi)
  IR<- IR %>% mutate(bmi.cat1 = case_when(
    (bmi < 16) ~ "Severely thin",
    (bmi >= 16 & bmi < 17) ~ "Moderately thin",
    (bmi >= 17 & bmi < 18.5) ~ "Mildly thin",
    (bmi >= 18.5 & bmi < 25) ~ "Normal",
    (bmi >= 25 & bmi < 30) ~ "Overweight",
    (bmi >= 30) ~ "Obese"))
  IR$bmi.cat1<-factor(IR$bmi.cat1, c("Severely thin", "Moderately thin", "Mildly thin","Normal", "Overweight","Obese"))
  IR<- IR %>% mutate(bmi.cat2 = case_when(
    (bmi < 18.5) ~ "Underweight",
    (bmi >= 18.5 & bmi < 25) ~ "Normal",
    (bmi >= 25 & bmi < 30) ~ "Overweight",
    (bmi >= 30) ~ "Obese"))
  IR$bmi.cat2<-factor(IR$bmi.cat2, c("Underweight", "Normal", "Overweight", "Obese"))

  ###################################
  # NUMBER OF ANC VISITS
  IR <- IR %>%
    mutate(m13_1=as.numeric(m13_1),
           m14_1=as.numeric(m14_1))

  IR$anc.1stvisit <- ifelse(IR$m13_1 == 98, 5, IR$m13_1) # Impute DNK to the mean of 5
  IR$anc.1stvisit[IR$m14_1 == 0] <- 10 # if had no visits set to 10 (birth)
  IR$anc.num <- ifelse(IR$m14_1 == 98, 3, IR$m14_1) # Impute DNK to the mean of 3
  IR$anc.4plus <-ifelse(IR$anc.num >= 4, 1, 0)
  IR$anc.month<-ifelse(IR$anc.1stvisit < 4, "Month 1-3",
                       ifelse(IR$anc.1stvisit > 3 & IR$anc.1stvisit < 7,"Month 4-6",
                              ifelse(IR$anc.1stvisit > 6 & IR$anc.1stvisit < 10, "Month 7-9", "No ANC")))
  IR$anc.month <- factor(IR$anc.month, c("No ANC","Month 1-3","Month 4-6", "Month 7-9"))
  IR$anyanc<-ifelse(IR$m14_1 == 0,0,1)
  IR$anc.1sttri<-ifelse(IR$anc.month == "Month 1-3", 1, 0)

  ###################################
  # WOMAN HAD HEALTH CHECK AFTER BIRTH
  temp1 <- ifelse(IR$m66_1=="yes" & !is.na(IR$m66_1), 1, 0)
  temp2 <- ifelse(IR$m62_1=="yes" & !is.na(IR$m62_1), 1, 0)
  IR$wom.hlthck <- ifelse((temp1==1|temp2==1), 1, 0)
  IR$wom.hlthck <- ifelse((is.na(IR$m62_1) & is.na(IR$m66_1)), NA, IR$wom.hlthck)

  ###################################
  # BABY HAD HEALTH CHECK AFTER BIRTH
  temp3<-ifelse(IR$m70_1=="yes" & !is.na(IR$m70_1),1,0)
  temp4<-ifelse(IR$m74_1=="yes" & !is.na(IR$m74_1),1,0)
  IR$baby.hlthck<-ifelse((temp3==1|temp4==1),1,0)
  IR$baby.hlthck<-ifelse((is.na(IR$m70_1) & is.na(IR$m74_1)),NA,IR$baby.hlthck)

  ###################################
  # WASTING, STUNTING, UNDERWEIGHT FOR LAST CHILD
  IR <- IR %>%
    mutate(hw5_1=as.numeric(hw5_1),
           hw8_1=as.numeric(hw8_1),
           hw11_1=as.numeric(hw11_1))

  IR$haz.last <- IR$hw5_1/100; IR$haz.last<-ifelse(IR$haz.last>90,NA,IR$haz.last) #height/age standard deviation (AG 2/28)
  IR$waz.last <- IR$hw8_1/100; IR$waz.last<-ifelse(IR$waz.last>90,NA,IR$waz.last) #weight/age standard deviation (AG 2/28)
  IR$whz.last <- IR$hw11_1/100; IR$whz.last<-ifelse(IR$whz.last>90,NA,IR$whz.last) #weight/height standard deviation (AG 2/28)

  # STUNTING
  IR<- IR %>% mutate(stunt.cat1.last =  case_when(
    haz.last < -3 ~ "Severely stunted",
    (haz.last >= -3 & haz.last < -2) ~ "Moderately stunted",
    haz.last >= -2 ~ "Not stunted"))
  IR$stunt.cat1.last<-factor(IR$stunt.cat1, c("Severely stunted","Moderately stunted", "Not stunted"))
  IR$stunt.cat2.last<-ifelse(IR$haz.last < -2,1,0)

  # WASTING
  IR<- IR %>% mutate(waste.cat1.last =  case_when(
    whz.last < -3 ~ "Severely wasted",
    (whz.last >= -3 & whz.last < -2) ~ "Moderately wasted",
    whz.last >= -2 ~ "Not wasted"))
  IR$waste.cat1.last<-factor(IR$waste.cat1, c("Severely wasted","Moderately wasted", "Not wasted"))
  IR$waste.cat2.last<- ifelse(IR$whz.last < -2,1,0)

  # UNDER/OVER WEIGHT
  IR<- IR %>% mutate(unwgt.cat1.last =  case_when(
    waz.last < -3 ~ "Severely underweight",
    (waz.last >= -3 & waz.last < -2) ~ "Moderately underweight",
    waz.last >= -2 ~ "Not underweight"))
  IR$unwgt.cat1.last<-factor(IR$unwgt.cat1, c("Severely underweight","Moderately underweight", "Not underweight"))
  IR$undwgt.last<- ifelse(IR$waz.last < -2,1,0)
  IR$ovrwgt.last<-ifelse(IR$waz.last >2,1,0)

  ###################################
  # BREASTFEEDING LAST CHILD
  IR <- IR %>%
    mutate(b19_01=as.numeric(b19_01))

  IR$breastfed <- ifelse(IR$m4_1 == "ever breastfed, not currently breastfeeding" | IR$m4_1=="still breastfeeding",1,0)
  IR$breastfed2 <- ifelse((IR$b19_01 < 24 & IR$b9_01 == "respondent"), IR$breastfed, NA) #child is <24 months, child lives with respondent (AG added 2/28)

  IR <- IR %>%
    mutate(comp.feed = case_when(IR$v409 == "yes" | IR$v410 == "yes" | IR$v411 == "yes" | IR$v411 == "yes" | IR$v412a == "yes" |
                                 IR$v412c == "yes" | IR$v413 == "yes" | IR$v414e == "yes"  | IR$v414f == "yes" |
                                 IR$v414g == "yes" | IR$v414h == "yes" | IR$v414i == "yes" | IR$v414j == "yes" |
                                 IR$v414k == "yes" | IR$v414l == "yes" | IR$v414m == "yes" | IR$v414n == "yes" |
                                 IR$v414o == "yes" | IR$v414p == "yes" | IR$v414s == "yes" | IR$v414v == "yes" ~1,
                                 TRUE ~ 0))
  # IR$comp.feed <- ifelse((IR$b19_01 < 24 & IR$b9_01 == "respondent"), IR$comp.feed, NA) ##these are questions on other feed types, like water or solids

  IR$breastfed2.excl<-ifelse((IR$breastfed2==1 & IR$comp.feed==0),1,0)

  # SELECT ALL INDIVIDUAL RECODE OUTCOME VARIABLES
  ir.out<-IR %>% select(c("v001","v002","caseid","baby.hlthck","wom.hlthck","bmi","bmi.cat1","bmi.cat2","hb.1","fp.mod.now",
                          "fp.mod.ever","anc.1stvisit","anc.num","anc.4plus","anc.month","anyanc","anc.1sttri","haz.last","waz.last","whz.last",
                          "stunt.cat1.last","stunt.cat2.last","waste.cat1.last","waste.cat2.last","unwgt.cat1.last","undwgt.last","ovrwgt.last",
                          "breastfed","breastfed2","breastfed2.excl"))


  ######################################################################
  # KR OUTCOMES DATA CLEANING
  ######################################################################
  KR$hhid<-paste(KR$v001, KR$v002)

  ###################################
  # WASTING, STUNTING, AND UNDERWEIGHT FOR ALL CHILDREN
  KR <- KR %>%
    mutate(hw5=as.numeric(hw5),
           hw8=as.numeric(hw8),
           hw11=as.numeric(hw11))

  KR$haz<-KR$hw5/100; KR$haz<-ifelse(KR$haz>90,NA,KR$haz)
  KR$waz<-KR$hw8/100; KR$waz<-ifelse(KR$waz>90,NA,KR$waz)
  KR$whz<-KR$hw11/100; KR$whz<-ifelse(KR$whz>90,NA,KR$whz)

  # STUNTING
  KR<- KR %>% mutate(stunt.cat1 =  case_when(
    haz < -3 ~ "Severely stunted",
    (haz >= -3 & haz < -2) ~ "Moderately stunted",
    haz >= -2 ~ "Not stunted"))
  KR$stunt.cat1<-factor(KR$stunt.cat1, c("Severely stunted","Moderately stunted", "Not stunted"))
  KR$stunt.cat2<-ifelse(KR$haz < -2,1,0)

  # WASTING
  KR<- KR %>% mutate(waste.cat1 =  case_when(
    whz < -3 ~ "Severely wasted",
    (whz >= -3 & whz < -2) ~ "Moderately wasted",
    whz >= -2 ~ "Not wasted"))
  KR$waste.cat1<-factor(KR$waste.cat1, c("Severely wasted","Moderately wasted", "Not wasted"))
  KR$waste.cat2<- ifelse(KR$whz < -2,1,0)

  # UNDER/OVER WEIGHT
  KR<- KR %>% mutate(unwgt.cat1 =  case_when(
    waz < -3 ~ "Severely underweight",
    (waz >= -3 & waz < -2) ~ "Moderately underweight",
    waz >= -2 ~ "Not underweight"))
  KR$unwgt.cat1<-factor(KR$unwgt.cat1, c("Severely underweight","Moderately underweight", "Not underweight"))
  KR$undwgt <- ifelse(KR$waz < -2,1,0)
  KR$ovrwgt <-ifelse(KR$waz >2,1,0)

  ###################################
  # BREASTFEEDING
  KR$breastfeed <- ifelse(KR$m4 == "ever breastfed, not currently breastfeeding" | KR$m4=="still breastfeeding",1,0)
  KR$breastfeed <- ifelse((KR$b19 < 24 & KR$b9 == "respondent"), KR$breastfeed, NA)

  KR <- KR %>%
    mutate(comp.feed = case_when(KR$v409 == "yes" | KR$v410 == "yes" | KR$v411 == "yes" | KR$v411 == "yes" | KR$v412a == "yes" |
                                 KR$v412c == "yes" | KR$v413 == "yes" | KR$v414e == "yes"  | KR$v414f == "yes" |
                                 KR$v414g == "yes" | KR$v414h == "yes" | KR$v414i == "yes" | KR$v414j == "yes" |
                                 KR$v414k == "yes" | KR$v414l == "yes" | KR$v414m == "yes" | KR$v414n == "yes" |
                                 KR$v414o == "yes" | KR$v414p == "yes" | KR$v414s == "yes" | KR$v414v == "yes" ~1,
                                 TRUE ~ 0))
  KR$comp.feed <- ifelse((KR$b19 < 24 & KR$b9 == "respondent"), KR$comp.feed, NA)

  KR$breastfeed.excl <- ifelse((KR$breastfeed==1 & KR$comp.feed==0),1,0)

  KR$breastfeed.n<-ifelse((KR$b19 < 24 & KR$b9 == "respondent"),1,0)

  kr.mom<- KR %>%
    dplyr::group_by(caseid) %>%
    dplyr::summarize(k.rost = length(caseid),
              k.meas = length(caseid[(!is.na(whz)&!is.na(haz)&!is.na(waz))]),
              k.breast = sum(breastfeed.n, na.rm=T),
              breast.cnt = sum(breastfeed, na.rm=T),
              breastex.cnt = sum(breastfeed.excl, na.rm=T),
              stunt.cnt = sum(stunt.cat2, na.rm=T),
              waste.cnt = sum(waste.cat2, na.rm=T),
              unwgt.cnt = sum(undwgt, na.rm=T),
              ovrwgt.cnt = sum(ovrwgt, na.rm=T),
              haz.mean=mean(haz, na.rm=T),
              waz.mean=mean(waz, na.rm=T),
              whz.mean=mean(whz, na.rm=T))

  kr.out<- KR %>% select(c("caseid", "haz", "waz", "whz", "stunt.cat1", "stunt.cat2", "waste.cat1", "waste.cat2", "unwgt.cat1", "undwgt", "ovrwgt", "breastfeed", "breastfeed.excl")) %>%
    distinct()


  ######################################################################
  # BR DATA CLEANING
  ######################################################################
  BR$hhid<-paste(BR$v001, BR$v002)

  # Immunization - zero-dose
  #Similar to Cata-Preta et al, 2021, prevalence of zero-dose children was defined as those who had not received any doses of the
  #four vaccines (four basic vaccines: BCG, polio (OPV and IPV, doses given at birth were not considered in the analyses), DPT, and MCV).
  #The immunization cascade was generated as a score ranging from 0 to 4. Each type of vaccine accounts for one point in the cascade regardless of the number of doses.
  #For example, a child was coded as "1" if either one, two or three doses of DPT/polio vaccine had been received.

  BR[,ChildAge:= (v008-b3)] # in months
  # summary(BR[is.na(h2),]$ChildAge)
  # summary(BR[is.na(h4),]$b8)
  # summary(BR[is.na(h4) & b5=="yes",]$ChildAge)

  BR[,ZeroDose:= ifelse(h2== "no" & h3== "no" & h4== "no" & h5== "no" & h6== "no" & h7== "no" & h8== "no" & h9== "no",1,0)]
  BR$ZeroDose <- ifelse((is.na(BR$h2))|(is.na(BR$h3))|(is.na(BR$h4))|(is.na(BR$h5))|(is.na(BR$h6))|(is.na(BR$h7))|(is.na(BR$h8))|(is.na(BR$h9)),NA,BR$ZeroDose)
  BR$ZeroDose <- ifelse(BR$h2=="don't know" |BR$h3=="don't know"|BR$h4=="don't know" |BR$h5=="don't know" |BR$h6=="don't know" |BR$h7=="don't know" |BR$h8=="don't know" |BR$h9=="don't know",NA,BR$ZeroDose)

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

  # Child death U5, U1
  #b3 - Date of birth of child (CMC)
  #b5 - Child is alive (1 = Yes, 0 = No)
  #b7 - Age at death in months (imputed)
  BR$u1mort<-ifelse(BR$b7<13 & BR$b5=="no",1,0)
  BR$u5mort<-ifelse(BR$b5=="no",1,0)

  br.mom <- BR %>%
    group_by(caseid) %>%
    summarize(b.rost=length(caseid), zerodosecnt = sum(ZeroDose, na.rm=T), dpt1cnt = sum(dpt1, na.rm=T),
                                                  avgscore=mean(score, na.rm=T), u1mortcnt = sum(u1mort, na.rm=T), u5mortcnt = sum(u5mort, na.rm=T))

  br.out<- BR %>% select(c("caseid", "v001", "v002", "ZeroDose", "dpt1", "score", "u1mort", "u5mort"))

  ###################################
  # COMBINE IR, KR, BR TO CREATE OUTCOMES DATAFRAME
  ###################################

  outcomes <- base::merge(ir.out, kr.mom, by="caseid", all.x = TRUE)
  outcomes <- base::merge(outcomes, br.mom, by="caseid", all.x=T)
  outcomes <- base::merge(outcomes, stl.out, by="caseid", all.x=T)

  #### Define and group outcomes for use in age exploration file ####

  ## Make binary versions of some outcomes
  # Underweight
  outcomes$underweight <- ifelse(outcomes$bmi.cat2 == "Underweight", 1,
                                 ifelse(!is.na(outcomes$bmi.cat2), 0, NA))

  # Zero dose
  outcomes$zerodose.yn <- ifelse(outcomes$zerodosecnt > 0 & !is.na(outcomes$zerodosecnt), 1,
                                 ifelse(outcomes$zerodosecnt == 0, 0, NA))

  # DPT1
  outcomes$dpt1.yn <- ifelse(outcomes$dpt1cnt > 0 & !is.na(outcomes$dpt1cnt), 1,
                             ifelse(outcomes$dpt1cnt == 0, 0, NA))

  # Any deaths of kids under 1
  outcomes$u1mort.yn <- ifelse(outcomes$u1mortcnt > 0 & !is.na(outcomes$u1mortcnt), 1,
                               ifelse(outcomes$u1mortcnt == 0, 0, NA))

  # Any deaths of kids under 5
  outcomes$u5mort.yn <- ifelse(outcomes$u5mortcnt > 0 & !is.na(outcomes$u5mortcnt), 1,
                               ifelse(outcomes$u5mortcnt == 0, 0, NA))

  return(outcomes)
}

###################################
# SAVE OUTCOMES DF
###################################

# # outcomes_file <- paste0(config::get("outcomes_file"))
# dir.create(dirname(outcomes_file), showWarnings = F, recursive = T)
# saveRDS(outcomes, file = outcomes_file)






