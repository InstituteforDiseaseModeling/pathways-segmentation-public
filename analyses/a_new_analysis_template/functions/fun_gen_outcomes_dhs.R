


################################################################################
# GENERATE OUTCOME VARIABLES | DHS
################################################################################

# PROGRAM:
# PURPOSE:
# DATA INPUTS:
# DATA OUTPUTS:
# AUTHOR:
# DATA LAST MODIFIED:
# NOTES:


###################################
# RUN SETUP


###################################
# TABLE OF CONTENTS
# 1 | GENERAL
# 2 | ANC/PNC
# 3 | BMI
# 4 | BREASTFEEDING
# 5 | CHILD MORTALITY
# 6 | FAMILY PLANNING
# 7 | HOME BIRTHS
# 8 | IMMUNIZATION
# 9 | MALNOURISHMENT
# 10 | MENSTRUAL HEALTH
# 11 | CHILD HEALTH
# 12 | SUMMARIZE


###################################
# DEFINE FUNCTION

gen_outcome_variables_dhs <- function(IR=NULL, KR=NULL, BR=NULL, DHS=7){


  # CONVERT INPUTS TO DATA.TABLE
  IR <- data.table(IR)
  KR <- data.table(KR)
  BR <- data.table(BR)


  ######################################################################
  # 1 | GENERAL

  # INDIVIDUAL
  IR[,str:=substr(vcal_1, v018, v018+59)]
  IR[,STL:=ifelse(grepl('TPPPPPP',str),1,NA)]
  IR[,LB:=ifelse(grepl('BPPPPPPP',str),1,NA)]


  # BIRTH
  BR$hhid <- paste(BR$v001, BR$v002)
  BR[,child.age:= (v008-b3)] #MONTHS
  KR[,child.age:= (v008-b3)] #MONTHS


  # KID
  KR$hhid<-paste(KR$v001, KR$v002)


  # FILTER TO ONLY CHILDREN <= 5 IN KR FILE (DATA QUALITY)
  KR <- KR %>%
    dplyr::filter(child.age < 61)



  ######################################################################
  # 2 | ANC/PNC

  # ANC VISITS
  # Number of antenatal visits during last pregnancy
  IR <- IR %>%
    dplyr::group_by(survey) %>%
    dplyr::mutate(anc.total = case_when((m14_1 == 98 | m14_1 == "don't know") ~ NA,
                                        (m14_1 == "no antenatal visits" | m14_1 == 0) ~ 0,
                                        is.na(m14_1) ~ NA,
                                        TRUE ~ as.integer(m14_1)),
                  anc.mean = as.integer(mean(as.integer(anc.total), na.rm=TRUE)),
                  anc.total = ifelse((m14_1 == 98 | m14_1 == "don't know"), anc.mean, anc.total),
                  anc.less4.last = ifelse(anc.total >= 4, 0, 1)) %>%
    ungroup()


  # Timing of 1st antenatal check (months)
  IR <- IR %>%
    dplyr::group_by(survey) %>%
    dplyr::mutate(anc.1stvisit = case_when((m13_1 == 98 | m13_1 == "don't know") ~ NA,
                                           is.na(m13_1) ~ NA,
                                           TRUE ~ as.integer(m13_1)),
                  anc.1st.mean = as.integer(mean(as.integer(anc.1stvisit), na.rm=TRUE)),
                  anc.1stvisit = ifelse((m13_1 == 98 | m13_1 == "don't know"), anc.1st.mean, anc.1stvisit),
                  no.anc.1st.tri = ifelse(anc.1stvisit < 4, 0, 1)) %>%
    ungroup()


  # CATEGORICAL OUTCOME FOR ANC 1ST VISIT
  IR$anc.month <- ifelse(IR$anc.1stvisit < 4, "Month 1-3",
                         ifelse(IR$anc.1stvisit > 3 & IR$anc.1stvisit < 7,"Month 4-6",
                                ifelse(IR$anc.1stvisit > 6 & IR$anc.1stvisit < 10, "Month 7-9", "No ANC")))
  IR$anc.month <- factor(IR$anc.month, c("No ANC","Month 1-3","Month 4-6", "Month 7-9"))


  # WOMAN HAD HEALTH CHECK AFTER BIRTH
  temp1 <- ifelse(IR$m66_1=="yes" & !is.na(IR$m66_1), 1, 0)
  temp2 <- ifelse(IR$m62_1=="yes" & !is.na(IR$m62_1), 1, 0)
  IR$wom.nohlthck <- ifelse((temp1==1|temp2==1), 0, 1)
  IR$wom.nohlthck <- ifelse((is.na(IR$m62_1) & is.na(IR$m66_1)), NA, IR$wom.nohlthck)


  # BABY HAD HEALTH CHECK AFTER BIRTH
  temp3<-ifelse(IR$m70_1=="yes" & !is.na(IR$m70_1), 1,0)
  temp4<-ifelse(IR$m74_1=="yes" & !is.na(IR$m74_1), 1,0)
  IR$baby.nohlthck<-ifelse((temp3==1|temp4==1),0, 1)
  IR$baby.nohlthck<-ifelse((is.na(IR$m70_1) & is.na(IR$m74_1)), NA, IR$baby.nohlthck)



  ######################################################################
  # 3 | BMI

  # BMI
  IR$bmi <- as.numeric(IR$v445)/100
  IR$bmi <- ifelse(IR$bmi > 90, NA, IR$bmi)


  # WOMAN UNDERWEIGHT
  IR <- IR %>%
    dplyr::mutate(woman.underweight = case_when(bmi < 18.5 ~ 1,
                                                is.na(bmi) ~ NA,
                                                TRUE ~ 0))


  #
  IR$overweight.obese <- NA


  #
  IR$obese <- NA



  ######################################################################
  # 4 | BREASTFEEDING

  # CATEGORICAL OUTCOME FOR LATEST CHILD BREASTFED
  IR <- IR %>%
    dplyr::mutate(no.breastfeed.last = case_when(m4_1 %in% c("never breastfed") ~ 1,
                                                 is.na(m4_1) ~ NA,
                                                 TRUE ~ 0))

  KR <- KR %>%
    dplyr::mutate(no.breastfeed.n = case_when(m4 %in% c("never breastfed") ~ 1,
                                              is.na(m4) ~ NA,
                                              TRUE ~ 0))


  # ALTERNATE DEFINITION FOR LAST CHILD BREASTFED
  IR <- IR %>%
    dplyr::mutate(no.breastfeed2.last = ifelse((b19_01 < 24 & b9_01 == "respondent"), no.breastfeed.last, NA))

  KR <- KR %>%
    dplyr::mutate(no.breastfeed2.n = ifelse((b19 < 24 & b9 == "respondent"), no.breastfeed.n, NA))



  ######################################################################
  # 5 | CHILD MORTALITY

  # CHILD UNDER 1 DIED
  BR$u1mort <- ifelse(BR$b7<13 & BR$b5=="no", 1, 0)


  # CHILD UNDER 5 DIED
  BR$u5mort<-ifelse(BR$b5=="no", 1, 0)


  # STILLBIRTHS
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
  stl.out$stl.yn <- ifelse(stl.out$stl.cnt > 0, 1, 0)



  ######################################################################
  # 6 | FAMILY PLANNING

  # NOT CURRENTLY USING MODERN FAMILY PLANNING METHOD
  # Current use by method type
  IR <- IR %>%
    dplyr::mutate(nofp.mod.now = ifelse(v313 == "modern method", 0, 1))

  # Last method discontinued in last 5 years
  IR <- IR %>%
    dplyr::mutate(nofp.dis.5yr = case_when(v359 %in% c("pill", "iud", "injections", "male condom", "implants/norplant", "female condom", "emergency contraception", "other modern method") ~ 0,
                                           is.na(v359) ~ NA,
                                           TRUE ~ 1))

  # Decision to interpret NA as "Did not discontinue a modern method in last 5yrs"
  IR$nofp.dis.5yr[is.na(IR$nofp.dis.5yr)] <- 1

  # NEVER USE OF MODERN FAMILY PLANNING METHOD
  # Per input by Darcy, let's say if you're currently using or discontinued a modern method in last 5yrs, then 0, otherwise we assume you have never used a modern method
  IR <- IR %>%
    # dplyr::mutate(nofp.mod.ever = ifelse((nofp.mod.now==1 & nofp.dis.5yr==1), 1, 0))
    dplyr::mutate(nofp.mod.ever = ifelse((nofp.mod.now==0 | nofp.dis.5yr==0), 0, 1))



  ######################################################################
  # 7 | HOME BIRTHS

  # LATEST BIRTH WAS HOME BIRTH
  IR <- IR %>%
    dplyr::mutate(home.birth.last = case_when(m15_1 %in% c("home", "parents' home", "respondent's home", "other home", "other") ~ 1,
                                              is.na(m15_1) ~ NA,
                                              TRUE ~ 0))

  # ANY BIRTH WAS A HOME BIRTH
  BR <- BR %>%
    dplyr::mutate(home.birth = case_when(m15 %in% c("home", "parents' home", "respondent's home", "other home", "other") ~ 1,
                                         is.na(m15) ~ NA,
                                         TRUE ~ 0))



  ######################################################################
  # 8 | IMMUNIZATION

  # *** Pentavalent ***
  # DPT 1, 2, 3 either source
  KR <- KR %>%
    mutate(dpt1 = case_when(h3%in%c("vaccination date on card","vaccination marked on card","reported by mother") ~ 1, h3%in%c("no","don't know") ~ 0  )) %>%
    mutate(dpt2 = case_when(h5%in%c("vaccination date on card","vaccination marked on card","reported by mother") ~ 1, h5%in%c("no","don't know") ~ 0  )) %>%
    mutate(dpt3 = case_when(h7%in%c("vaccination date on card","vaccination marked on card","reported by mother") ~ 1, h7%in%c("no","don't know") ~ 0  )) %>%
    mutate(dptsum = dpt1 + dpt2 + dpt3) %>%
    mutate(dpt.full = case_when(is.na(dptsum) ~ NA, dptsum == 3 ~ 0, TRUE ~ 1)) %>%
    mutate(zero.dose = case_when(is.na(dptsum) ~ NA, dptsum == 0 ~ 1, TRUE ~ 0))


  # *** Measles ***
  # Measles either source
  KR <- KR %>%
    mutate(measles1 = case_when(h9 %in% c("vaccination date on card", "vaccination marked on card", "reported by mother") ~ 1, h9 %in% c("no", "don't know")  ~ 0  )) %>%
    mutate(measles2 = case_when(h9a %in% c("vaccination date on card", "vaccination marked on card", "reported by mother") ~ 1, h9a %in% c("no", "don't know")  ~ 0  )) %>%
    mutate(measlessum = measles1 + measles2) %>%
    mutate(measles.full = case_when(is.na(measlessum) ~ NA, measlessum == 2 ~ 0, TRUE ~ 1))


  # *** Polio ***
  # Polio 0, 1, 2, 3 either source
  KR <- KR %>%
    mutate(polio1 = case_when(h4%in%c("vaccination date on card","vaccination marked on card","reported by mother") ~ 1, h4%in%c("no","don't know") ~ 0  )) %>%
    mutate(polio2 = case_when(h6%in%c("vaccination date on card","vaccination marked on card","reported by mother") ~ 1, h6%in%c("no","don't know") ~ 0  )) %>%
    mutate(polio3 = case_when(h8%in%c("vaccination date on card","vaccination marked on card","reported by mother") ~ 1, h8%in%c("no","don't know") ~ 0  )) %>%
    mutate(poliosum=polio1 + polio2 + polio3) %>%
    mutate(polio.full = case_when(is.na(poliosum) ~ NA, poliosum == 3 ~ 0, TRUE ~ 1))


  ######################################################################
  # 9 | MALNOURISHMENT

  # STANDARD MALNOURISHMENT VARIABLES IN IR FILE

  if (DHS == 7){


    # STUNTING
    IR <- IR %>%
      dplyr::mutate(haz.last = case_when(as.numeric(hw5_1)/100 > 90 ~ NA,
                                         is.na(hw5_1) ~ NA,
                                         TRUE ~ as.numeric(hw5_1)/100)) %>%
      dplyr::mutate(stunt.cat2.last = ifelse(haz.last < -2, 1, 0))

    # WASTING
    IR <- IR %>%
      dplyr::mutate(whz.last = case_when(as.numeric(hw11_1)/100 > 90 ~ NA,
                                         is.na(hw11_1) ~ NA,
                                         TRUE ~ as.numeric(hw11_1)/100)) %>%
      dplyr::mutate(waste.cat2.last = ifelse(whz.last < -2, 1, 0))

    # UNDERWEIGHT/OVERWEIGHT
    IR <- IR %>%
      dplyr::mutate(waz.last = case_when(as.numeric(hw8_1)/100 > 90 ~ NA,
                                         is.na(hw8_1) ~ NA,
                                         TRUE ~ as.numeric(hw8_1)/100)) %>%
      dplyr::mutate(undwgt.last = ifelse(waz.last < -2, 1, 0),
                    ovrwgt.last = ifelse(waz.last > 2, 1, 0))


  } else if (DHS == 8){


    # STUNTING
    IR <- IR %>%
      dplyr::mutate(haz.last = case_when(as.numeric(hw70_1)/100 > 90 ~ NA,
                                         is.na(hw70_1) ~ NA,
                                         TRUE ~ as.numeric(hw70_1)/100)) %>%
      dplyr::mutate(stunt.cat2.last = ifelse(haz.last < -2, 1, 0))

    # WASTING
    IR <- IR %>%
      dplyr::mutate(whz.last = case_when(as.numeric(hw72_1)/100 > 90 ~ NA,
                                         is.na(hw72_1) ~ NA,
                                         TRUE ~ as.numeric(hw72_1)/100)) %>%
      dplyr::mutate(waste.cat2.last = ifelse(whz.last < -2, 1, 0))

    # UNDERWEIGHT/OVERWEIGHT
    IR <- IR %>%
      dplyr::mutate(waz.last = case_when(as.numeric(hw71_1)/100 > 90 ~ NA,
                                         is.na(hw71_1) ~ NA,
                                         TRUE ~ as.numeric(hw71_1)/100)) %>%
      dplyr::mutate(undwgt.last = ifelse(waz.last < -2, 1, 0),
                    ovrwgt.last = ifelse(waz.last > 2, 1, 0))


  }


  # STANDARD MALNOURISHMENT VARIABLES IN KR FILE

  if (DHS == 7){


    # STUNTING
    KR <- KR %>%
      dplyr::mutate(haz = case_when(as.numeric(hw5)/100 > 90 ~ NA,
                                    is.na(hw5) ~ NA,
                                    TRUE ~ as.numeric(hw5)/100)) %>%
      dplyr::mutate(stunt.cat2 = ifelse(haz < -2, 1, 0))

    # WASTING
    KR <- KR %>%
      dplyr::mutate(whz = case_when(as.numeric(hw11)/100 > 90 ~ NA,
                                    is.na(hw11) ~ NA,
                                    TRUE ~ as.numeric(hw11)/100)) %>%
      dplyr::mutate(waste.cat2 = ifelse(whz < -2, 1, 0))

    # UNDERWEIGHT/OVERWEIGHT
    KR <- KR %>%
      dplyr::mutate(waz = case_when(as.numeric(hw8)/100 > 90 ~ NA,
                                    is.na(hw8) ~ NA,
                                    TRUE ~ as.numeric(hw8)/100)) %>%
      dplyr::mutate(undwgt = ifelse(waz < -2, 1, 0),
                    ovrwgt = ifelse(waz > 2, 1, 0))


  } else if (DHS == 8){


    # STUNTING
    KR <- KR %>%
      dplyr::mutate(haz = case_when(as.numeric(hw70)/100 > 90 ~ NA,
                                    is.na(hw70) ~ NA,
                                    TRUE ~ as.numeric(hw70)/100)) %>%
      dplyr::mutate(stunt.cat2 = ifelse(haz < -2, 1, 0))

    # WASTING
    KR <- KR %>%
      dplyr::mutate(whz = case_when(as.numeric(hw72)/100 > 90 ~ NA,
                                    is.na(hw72) ~ NA,
                                    TRUE ~ as.numeric(hw72)/100)) %>%
      dplyr::mutate(waste.cat2 = ifelse(whz < -2, 1, 0))

    # UNDERWEIGHT/OVERWEIGHT
    KR <- KR %>%
      dplyr::mutate(waz = case_when(as.numeric(hw71)/100 > 90 ~ NA,
                                    is.na(hw71) ~ NA,
                                    TRUE ~ as.numeric(hw71)/100)) %>%
      dplyr::mutate(undwgt = ifelse(waz < -2, 1, 0),
                    ovrwgt = ifelse(waz > 2, 1, 0))


  }



  ######################################################################
  # 10 | MENSTRUAL HEALTH

  # #
  # IR$mens.nopriv <- ifelse(IR$s239c=="non",1,0)
  #
  # #
  # IR$mens.noprod <- ifelse(IR$s239d=="yes",0,1)
  #
  # #
  # IR$mens.noboth <- ifelse(IR$mens.nopriv==1 & IR$mens.noprod==1,1,0)



  ######################################################################
  # 11 | CHILD HEALTH

  # DIARRHEAL ILLNESS
  # Had diarrhea recently (LAST CHILD)
  IR <- IR %>%
    dplyr::mutate(diarrhea.2wks.last = case_when(h11_1 == "yes, last 24 hours" ~ 1,
                                                 h11_1 == "yes, last two weeks" ~ 1,
                                                 h11_1 == "no" ~ 0,
                                                 h11_1 == "don't know" ~ NA,
                                                 is.na(h11_1) ~ NA,
                                                 v137 == 0 ~ NA))

  # Had diarrhea recently (ANY CHILD UNDER 5)
  KR <- KR %>%
    dplyr::mutate(diarrhea.2wks = case_when(h11 == "yes, last 24 hours" ~ 1,
                                            h11 == "yes, last two weeks" ~ 1,
                                            h11 == "no" ~ 0,
                                            h11 == "don't know" ~ NA,
                                            is.na(h11) ~ NA,
                                            v137 == 0 ~ NA))


  # FEBRILE ILLNESS
  # Had fever in last two weeks (LAST CHILD)
  IR <- IR %>%
    dplyr::mutate(fever.2wks.last = case_when(h22_1 == "yes" ~ 1,
                                              h22_1 == "no" ~ 0,
                                              h22_1 == "don't know" ~ NA,
                                              is.na(h22_1) ~ NA,
                                              v137 == 0 ~ NA))

  # Had fever in last two weeks (ANY CHILD UNDER 5)
  KR <- KR %>%
    dplyr::mutate(fever.2wks = case_when(h22 == "yes" ~ 1,
                                         h22 == "no" ~ 0,
                                         h22 == "don't know" ~ NA,
                                         is.na(h22) ~ NA,
                                         v137 == 0 ~ NA))


  # COUGH
  # Had cough in last two weeks (LAST CHILD)
  IR <- IR %>%
    dplyr::mutate(cough.2wks.last = case_when(h31_1 == "yes, last 24 hours" ~ 1,
                                              h31_1 == "yes, last two weeks" ~ 1,
                                              h31_1 == "no" ~ 0,
                                              h31_1 == "don't know" ~ NA,
                                              is.na(h31_1) ~ NA,
                                              v137 == 0 ~ NA))

  # Had cough in last two weeks (ANY CHILD UNDER 5)
  KR <- KR %>%
    dplyr::mutate(cough.2wks = case_when(h31 == "yes, last 24 hours" ~ 1,
                                         h31 == "yes, last two weeks" ~ 1,
                                         h31 == "no" ~ 0,
                                         h31 == "don't know" ~ NA,
                                         is.na(h31) ~ NA,
                                         v137 == 0 ~ NA))


  # FEVER OR COUGH (ALIGNS WITH TREATMENT QUESTION)
  IR <- IR %>%
    dplyr::mutate(fever.cough.2wks.last = ifelse(fever.2wks.last == 1 | cough.2wks.last == 1, 1, 0))

  KR <- KR %>%
    dplyr::mutate(fever.cough.2wks = ifelse(fever.2wks == 1 | cough.2wks == 1, 1, 0))


  # PROBLEM IN CHEST
  # Problem in the chest or blocked or running nose
  IR <- IR %>%
    dplyr::mutate(chest.prob.last = case_when(h31c_1 == "both" ~ 1,
                                              h31c_1 == "chest only" ~ 1,
                                              h31c_1 == "nose only" ~ 0,
                                              h31c_1 == "other" ~ 0,
                                              h31c_1 == "don't know" ~ NA,
                                              is.na(h31c_1) ~ NA,
                                              v137 == 0 ~ NA))

  KR <- KR %>%
    dplyr::mutate(chest.prob = case_when(h31c == "both" ~ 1,
                                         h31c == "chest only" ~ 1,
                                         h31c == "nose only" ~ 0,
                                         h31c == "other" ~ 0,
                                         h31c == "don't know" ~ NA,
                                         is.na(h31c) ~ NA,
                                         v137 == 0 ~ NA))


  # DIFFICULTY BREATHING
  # Short, rapid breaths
  IR <- IR %>%
    dplyr::mutate(diff.breath.last = case_when(h31b_1 == "yes" ~ 1,
                                               h31b_1 == "no" ~ 0,
                                               h31b_1 == "don't know" ~ NA,
                                               is.na(h31b_1) ~ NA,
                                               v137 == 0 ~ NA))

  KR <- KR %>%
    dplyr::mutate(diff.breath = case_when(h31b == "yes" ~ 1,
                                          h31b == "no" ~ 0,
                                          h31b == "don't know" ~ NA,
                                          is.na(h31b) ~ NA,
                                          v137 == 0 ~ NA))


  # ARI SYMPTOMS
  IR <- IR %>%
    dplyr::mutate(ari.last = ifelse(chest.prob.last == 1 & diff.breath.last == 1, 1, 0))

  KR <- KR %>%
    dplyr::mutate(ari = ifelse(chest.prob == 1 & diff.breath == 1, 1, 0))


  # HEALTH SEEKING FOR ILLNESS: TREATMENT FOR FEVER/COUGH
  # Fever/cough: (treatment sought at Xi)
  kr_var <- KR %>%
    dplyr::select(survey, caseid, starts_with("h32")) %>%
    reshape2::melt(id.vars=c("survey", "caseid")) %>%
    dplyr::mutate(care = case_when((variable == "h32a" & value == "yes") ~ 1, # PUBLIC HOSPITAL = X1 ....
                                   (variable == "h32b" & value == "yes") ~ 1, # PUBLIC CLINIC
                                   (variable == "h32c" & value == "yes") ~ 1, # PUBLIC HEALTH CENTER
                                   (variable == "h32d" & value == "yes") ~ 1, # PUBLIC MOBILE
                                   (variable == "h32e" & value == "yes") ~ 1, # PUBLIC VILLAGE MIDWIFE
                                   (variable == "h32f" & value == "yes") ~ 1, # PUBLIC OTHER
                                   (variable == "h32g" & value == "yes") ~ 1, # UKBM VILLAGE HEALTH POST
                                   (variable == "h32h" & value == "yes") ~ 1, # HEALTH POST
                                   (variable == "h32i" & value == "yes") ~ 1, # UKBM OTHER
                                   (variable == "h32j" & value == "yes") ~ 1, # PRIVATE HOSPITAL
                                   (variable == "h32k" & value == "yes") ~ 1, # PRIVATE PHARMACY/DRUG STORE
                                   (variable == "h32l" & value == "yes") ~ 1, # GENERAL PRACTITIONER
                                   (variable == "h32m" & value == "yes") ~ 1, # PEDIATRICIAN
                                   (variable == "h32n" & value == "yes") ~ 1, # MIDWIFE
                                   (variable == "h32o" & value == "yes") ~ 1, # NURSE
                                   (variable == "h32p" & value == "yes") ~ 1, # CLINIC
                                   (variable == "h32q" & value == "yes") ~ 1, # PRIVATE OTHER
                                   is.na(value) ~ NA,
                                   TRUE ~ 0)) %>%
    dplyr::group_by(survey, caseid) %>%
    dplyr::mutate(care.cnt = ifelse(all(is.na(care)), NA, sum(care, na.rm=TRUE)),
                  no.fever.cough.care.yn = ifelse(care.cnt == 0, 1, 0)) %>%
    dplyr::select(survey, caseid, care.cnt, no.fever.cough.care.yn) %>%
    distinct()



  ######################################################################
  # 12 | SUMMARIZE

  # INDIVIDUAL
  ir.out <- IR


  # BIRTH
  br.out <- BR %>%
    group_by(survey, caseid) %>%
    dplyr::summarize(b.rost = length(caseid),
                     u1mort.cnt = ifelse(all(is.na(u1mort)), NA, sum(u1mort, na.rm=TRUE)),
                     u5mort.cnt = ifelse(all(is.na(u5mort)), NA, sum(u5mort, na.rm=TRUE)),
                     home.birth.cnt = ifelse(all(is.na(home.birth)), NA, sum(home.birth, na.rm=TRUE))) %>%
    mutate(u1mort.yn = ifelse(u1mort.cnt > 0, 1, 0),
           u5mort.yn = ifelse(u5mort.cnt > 0, 1, 0),
           home.birth.yn = ifelse(home.birth.cnt > 0, 1, 0)) %>%
    dplyr::select(survey, caseid, u1mort.cnt, u5mort.cnt, home.birth.cnt, u1mort.yn, u5mort.yn, home.birth.yn)

  # KID
  kr.out <- KR %>%
    group_by(survey, caseid) %>%
    dplyr::summarize(k.rost = length(caseid),
                     no.breastfeed.cnt = ifelse(all(is.na(no.breastfeed.n)), NA, sum(no.breastfeed.n, na.rm=TRUE)),
                     no.breastfeed2.cnt = ifelse(all(is.na(no.breastfeed2.n)), NA, sum(no.breastfeed2.n, na.rm=TRUE)),
                     ovrwgt.cnt = ifelse(all(is.na(ovrwgt)), NA, sum(ovrwgt, na.rm=TRUE)),
                     undwgt.cnt = ifelse(all(is.na(undwgt)), NA, sum(undwgt, na.rm=TRUE)),
                     stunt.cat2.cnt = ifelse(all(is.na(stunt.cat2)), NA, sum(stunt.cat2, na.rm=TRUE)),
                     waste.cat2.cnt = ifelse(all(is.na(waste.cat2)), NA, sum(waste.cat2, na.rm=TRUE)),
                     meas.full.cnt = ifelse(all(is.na(measles.full)), NA, sum(measles.full, na.rm=TRUE)),
                     polio.full.cnt = ifelse(all(is.na(polio.full)), NA, sum(polio.full, na.rm=TRUE)),
                     dpt.full.cnt = ifelse(all(is.na(dpt.full)), NA, sum(dpt.full, na.rm=TRUE)),
                     zerodose.cnt = ifelse(all(is.na(zero.dose)), NA, sum(zero.dose, na.rm=TRUE)),
                     diarrhea.cnt = ifelse(all(is.na(diarrhea.2wks)), NA, sum(diarrhea.2wks, na.rm=TRUE)),
                     fever.cnt = ifelse(all(is.na(fever.2wks)), NA, sum(fever.2wks, na.rm=TRUE)),
                     cough.cnt = ifelse(all(is.na(cough.2wks)), NA, sum(cough.2wks, na.rm=TRUE)),
                     chest.prob.cnt = ifelse(all(is.na(chest.prob)), NA, sum(chest.prob, na.rm=TRUE)),
                     diff.breath.cnt = ifelse(all(is.na(diff.breath)), NA, sum(diff.breath, na.rm=TRUE)),
                     ari.cnt = ifelse(all(is.na(ari)), NA, sum(ari, na.rm=TRUE))) %>%
    dplyr::mutate(no.breastfeed.yn = ifelse(no.breastfeed.cnt > 0, 1, 0),
                  no.breastfeed2.yn = ifelse(no.breastfeed2.cnt > 0, 1, 0),
                  ovrwgt.yn = ifelse(ovrwgt.cnt > 0, 1, 0),
                  undwgt.yn = ifelse(undwgt.cnt > 0, 1, 0),
                  stunt.cat2.yn = ifelse(stunt.cat2.cnt > 0, 1, 0),
                  waste.cat2.yn = ifelse(waste.cat2.cnt > 0, 1, 0),
                  meas.full.yn = ifelse(meas.full.cnt > 0, 1, 0),
                  polio.full.yn = ifelse(polio.full.cnt > 0, 1, 0),
                  dpt.full.yn = ifelse(dpt.full.cnt > 0, 1, 0),
                  zerodose.yn = ifelse(zerodose.cnt > 0, 1, 0),
                  fever.yn = ifelse(fever.cnt > 0, 1, 0),
                  cough.yn = ifelse(cough.cnt > 0, 1, 0),
                  diff.breath.yn = ifelse(diff.breath.cnt > 0, 1, 0),
                  chest.prob.yn = ifelse(chest.prob.cnt > 0, 1, 0),
                  ari.yn = ifelse(ari.cnt > 0, 1, 0)) %>%
    dplyr::select(survey, caseid,
                  no.breastfeed.cnt, no.breastfeed2.cnt, ovrwgt.cnt, undwgt.cnt, stunt.cat2.cnt, waste.cat2.cnt, meas.full.cnt, dpt.full.cnt, polio.full.cnt, zerodose.cnt, diarrhea.cnt, fever.cnt, cough.cnt, diff.breath.cnt, chest.prob.cnt, ari.cnt,
                  no.breastfeed.yn, no.breastfeed2.yn, ovrwgt.yn, undwgt.yn, stunt.cat2.yn, waste.cat2.yn, meas.full.yn, dpt.full.yn, polio.full.yn, zerodose.yn, fever.yn, cough.yn, diff.breath.yn, chest.prob.yn, ari.cnt) %>%
    base::merge(kr_var, by=c("survey", "caseid"), all.x=TRUE)


  # JOIN TOGETHER
  outcomes <- ir.out %>%
    base::merge(kr.out, by=c("survey", "caseid"), all.x=TRUE) %>%
    base::merge(br.out, by=c("survey", "caseid"), all.x=TRUE) %>%
    base::merge(stl.out, by=c("caseid"), all.x=TRUE)


  # DROP SOME INTERMEDIARY VARIABLES
  drop_vars <- c("anc.1st.mean", "anc.mean")
  outcomes <- outcomes %>% dplyr::select(-all_of(drop_vars))


  return(outcomes)
}
