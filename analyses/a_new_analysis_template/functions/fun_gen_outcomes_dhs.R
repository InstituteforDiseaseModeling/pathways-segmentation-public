


################################################################################
# GENERATE OUTCOME VARIABLES | DHS
################################################################################


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
# 12 | MATERNAL HEALTH
# 13 | SUMMARIZE

gen_outcome_variables_dhs <- function(IR=NULL, KR=NULL, BR=NULL, dhs = 8) {

  ######################################################################
  # 1 | GENERAL ----

  ## CALENDAR STRING PARSING ----
  # Calendar string for stillbirth detection
  IR[,str:=substr(vcal_1, v018, v018+59)]
  IR[,STL:=ifelse(grepl('TPPPPPP',str),1,NA)]
  IR[,LB:=ifelse(grepl('BPPPPPPP',str),1,NA)]

  ## CHILD AGE ----
  # Birth file - child age in months
  BR$hhid <- paste(BR$v001, BR$v002)
  BR[,child.age:= (v008-b3)]

  # Kids file - child age in months
  KR[,child.age:= (v008-b3)]
  KR$hhid<-paste(KR$v001, KR$v002)

  # Filter to only children <= 5 in KR file (data quality)
  KR <- KR %>%
    dplyr::filter(child.age < 61)


  ######################################################################
  # 2 | ANC/PNC ----

  ## ANC.TOTAL ----
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


  # ANC 8+
  IR <- IR %>% dplyr::mutate(anc.8plus.last = ifelse(anc.total >= 8, 0, 1))

  ## ANC.1STVISIT ----
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

  ## UNASSISTED.DEL ----
  # Medically unassisted delivery Y/N
  BR <- BR %>%
    mutate(
      unassisted.del = case_when(
        is.na(m3a) & is.na(m3b) ~ NA_integer_,
        m3a == 1 | m3b == 1 ~ 0,
        TRUE ~ 1
      )
    )

  ## ANC.MONTH ----
  # Categorical outcome for ANC 1st visit
  IR <- IR %>%
    dplyr::mutate(anc.month = case_when(
      anc.1stvisit < 4 ~ "Month 1-3",
      (anc.1stvisit > 3 & IR$anc.1stvisit < 7) ~ "Month 4-6",
      (anc.1stvisit > 6 & IR$anc.1stvisit < 10) ~ "Month 7-9",
      m14_1==0 ~ "No ANC"))
  IR$anc.month <- factor(IR$anc.month, c("No ANC","Month 1-3","Month 4-6", "Month 7-9"))

  ## WOM.NOHLTHCK ----
  # Woman had health check after birth
  temp1 <- ifelse(IR$m66_1=="yes" & !is.na(IR$m66_1), 1, 0)
  temp2 <- ifelse(IR$m62_1=="yes" & !is.na(IR$m62_1), 1, 0)
  IR$wom.nohlthck <- ifelse((temp1==1|temp2==1), 0, 1)
  IR$wom.nohlthck <- ifelse((is.na(IR$m62_1) & is.na(IR$m66_1)), NA, IR$wom.nohlthck)

  ## BABY.NOHLTHCK ----
  # Baby had health check after birth
  temp3<-ifelse(IR$m70_1=="yes" & !is.na(IR$m70_1), 1,0)
  temp4<-ifelse(IR$m74_1=="yes" & !is.na(IR$m74_1), 1,0)
  IR$baby.nohlthck<-ifelse((temp3==1|temp4==1),0, 1)
  IR$baby.nohlthck<-ifelse((is.na(IR$m70_1) & is.na(IR$m74_1)), NA, IR$baby.nohlthck)


  ######################################################################
  # 3 | BMI ----

  ## BMI ----
  # Body mass index
  IR$bmi <- as.numeric(IR$v445)/100
  IR$bmi <- ifelse(IR$bmi > 90, NA, IR$bmi)

  ## WOMAN.UNDERWEIGHT ----
  # Woman is underweight
  IR <- IR %>%
    dplyr::mutate(woman.underweight = case_when(bmi < 18.5 ~ 1,
                                                is.na(bmi) ~ NA,
                                                TRUE ~ 0))


  ######################################################################
  # 4 | BREASTFEEDING ----

  ## NO.BREASTFEED.LAST ----
  # Latest child breastfed
  IR <- IR %>%
    dplyr::mutate(no.breastfeed.last = case_when(m4_1 %in% c("never breastfed") ~ 1,
                                                 is.na(m4_1) ~ NA,
                                                 TRUE ~ 0))
  KR <- KR %>%
    dplyr::mutate(no.breastfeed.n = case_when(m4 %in% c("never breastfed") ~ 1,
                                              is.na(m4) ~ NA,
                                              TRUE ~ 0))

  ## NO.BREASTFEED2.LAST ----
  # Alternate definition for last child breastfed
  IR <- IR %>%
    dplyr::mutate(no.breastfeed2.last = ifelse((b19_01 < 24 & b9_01 == "respondent"), no.breastfeed.last, NA))
  KR <- KR %>%
    dplyr::mutate(no.breastfeed2.n = ifelse((b19 < 24 & b9 == "respondent"), no.breastfeed.n, NA))


  ######################################################################
  # 5 | CHILD MORTALITY ----

  ## U1MORT ----
  # Child under 1 died
  BR$u1mort <- ifelse(BR$b7<12 & BR$b5=="no", 1, 0)

  ## U5MORT ----
  # Child under 5 died
  BR$u5mort<-ifelse(BR$b7<60 & BR$b5=="no", 1, 0)

  ## STL (STILLBIRTHS) ----
  # Function to extract stillbirths
  getStillbirths <- function(individ,id_vars,recode=7){
    setDT(individ)

    tmp=data.table::melt(individ, measure.vars=patterns('^bord[_]','^b0[_]','^b3[_]','^b4[_]','^b5[_]','^b6[_]','^b7[_]','^b8[_]','^b11[_]','^m14[_]','^m15[_]','^m17[_]'),
                         value.name=c('bord','b0','dob',"sex",'alive',"deathAge",'deathAgeMo','CurrAge',"b11","m14","m15","m17"),variable.name='ReverseOrder',
                         id.vars=id_vars)
  }

  # Define variables for stillbirth analysis
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
  stl <- stl[tl <= dob & dob < tu,]

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

  stl <- rbind(stl, sub[,-c("rep")])

  setkey(stl,index)

  stl[alive=="STL",stl:=1]
  stl[is.na(stl),stl:=0]

  stl.out <- stl %>% group_by(caseid) %>% summarize(stl.cnt=sum(stl, na.rm=T))
  stl.out$stl.yn <- ifelse(stl.out$stl.cnt > 0, 1, 0)


  ######################################################################
  # 6 | FAMILY PLANNING ----

  ## NOFP.MOD.NOW ----
  # Not currently using modern family planning method
  IR <- IR %>%
    dplyr::mutate(nofp.mod.now = ifelse(v313 == "modern method", 0, 1))

  ## NOFP.DIS.5YR ----
  # Last method discontinued in last 5 years
  IR <- IR %>%
    dplyr::mutate(nofp.dis.5yr = case_when(v359 %in% c("pill", "iud", "injections", "male condom", "implants/norplant", "female condom", "emergency contraception", "other modern method", "lactational amenorrhea (lam)", "standard days method (sdm)") ~ 0,
                                           is.na(v359) ~ NA,
                                           TRUE ~ 1))
  IR$nofp.dis.5yr[is.na(IR$nofp.dis.5yr)] <- 1

  ## NOFP.MOD.EVER ----
  # Never use of modern family planning method
  IR <- IR %>%
    dplyr::mutate(nofp.mod.ever = ifelse((nofp.mod.now==0 | nofp.dis.5yr==0), 0, 1))


  ######################################################################
  # 7 | HOME BIRTHS ----

  ## HOME.BIRTH.LAST ----
  # Latest birth was home birth
  IR <- IR %>%
    dplyr::mutate(home.birth.last = case_when(m15_1 %in% c("home", "parents' home", "her home", "other home") ~ 1,
                                              is.na(m15_1) ~ NA,
                                              TRUE ~ 0))

  ## HOME.BIRTH.EVER ----
  # Any birth was a home birth (births 2-5)
  IR <- IR %>%
    dplyr::mutate(home.birth.2 = case_when(m15_2 %in% c("her home", "other home") ~ 1,
                                           is.na(m15_2) ~ NA,
                                           TRUE ~ 0))
  IR <- IR %>%
    dplyr::mutate(home.birth.3 = case_when(m15_3 %in% c("her home", "other home") ~ 1,
                                           is.na(m15_3) ~ NA,
                                           TRUE ~ 0))
  IR <- IR %>%
    dplyr::mutate(home.birth.4 = case_when(m15_4 %in% c("her home", "other home") ~ 1,
                                           is.na(m15_4) ~ NA,
                                           TRUE ~ 0))
  IR <- IR %>%
    dplyr::mutate(home.birth.5 = case_when(m15_5 %in% c("her home", "other home") ~ 1,
                                           is.na(m15_5) ~ NA,
                                           TRUE ~ 0))
  IR <- IR %>%
    mutate(home.birth.ever = case_when((home.birth.last==1|home.birth.2==1|home.birth.3==1|home.birth.4==1|home.birth.5==1) ~ 1,
                                       is.na(home.birth.last) & is.na(home.birth.2) & is.na(home.birth.3) & is.na(home.birth.4) & is.na(home.birth.5) ~ NA,
                                       TRUE ~ 0))

  ## FP_UNMET_TOT ----
  # Unmet contraceptive need
  IR <- IR %>% mutate(fp_unmet_tot = ifelse(v626a==1|v626a==2, 1, 0))

  ## HOME.BIRTH (BR FILE) ----
  # Any birth was a home birth
  BR <- BR %>%
    dplyr::mutate(home.birth = case_when(m15 %in% c("home", "parents' home", "her home", "other home") ~ 1,
                                         is.na(m15) ~ NA,
                                         TRUE ~ 0))


  ######################################################################
  # 8 | IMMUNIZATION ----

  ## VAC.DOC.YN ----
  # Most recent child has a health card and/or other vaccination document
  IR <- IR %>% mutate(vac.doc = h1a_1)
  IR <- IR %>% mutate(vac.doc.yn = ifelse(vac.doc == "does not have either card or other document", 0, 1))

  ## DPT VACCINATION ----
  # DPT 1, 2, 3 either source
  KR <- KR %>%
    mutate(dpt1 = case_when(h3%in%c("vaccination date on card","vaccination marked on card","reported by mother") ~ 1, h3%in%c("no","don't know") ~ 0)) %>%
    mutate(dpt2 = case_when(h5%in%c("vaccination date on card","vaccination marked on card","reported by mother") ~ 1, h5%in%c("no","don't know") ~ 0)) %>%
    mutate(dpt3 = case_when(h7%in%c("vaccination date on card","vaccination marked on card","reported by mother") ~ 1, h7%in%c("no","don't know") ~ 0)) %>%
    mutate(dptsum = dpt1 + dpt2 + dpt3) %>%
    mutate(dpt.full = case_when(is.na(dptsum) ~ NA, dptsum == 3 ~ 0, TRUE ~ 1)) %>%
    mutate(zero.dose = case_when(is.na(dptsum) ~ NA, dptsum == 0 ~ 1, TRUE ~ 0))

  ## MEASLES VACCINATION ----
  # Measles either source
  KR <- KR %>%
    mutate(measles1 = case_when(h9 %in% c("vaccination date on card", "vaccination marked on card", "reported by mother") ~ 1, h9 %in% c("no", "don't know")  ~ 0  )) %>%
    mutate(measles2 = case_when(h9a %in% c("vaccination date on card", "vaccination marked on card", "reported by mother") ~ 1, h9a %in% c("no", "don't know")  ~ 0  )) %>%
    mutate(measlessum = measles1 + measles2) %>%
    mutate(measles.full = case_when(is.na(measlessum) ~ NA, measlessum == 2 ~ 0, TRUE ~ 1))

  ## POLIO VACCINATION ----
  # Polio 0, 1, 2, 3 either source
  KR <- KR %>%
    mutate(polio1 = case_when(h4%in%c("vaccination date on card","vaccination marked on card","reported by mother") ~ 1, h4%in%c("no","don't know") ~ 0  )) %>%
    mutate(polio2 = case_when(h6%in%c("vaccination date on card","vaccination marked on card","reported by mother") ~ 1, h6%in%c("no","don't know") ~ 0  )) %>%
    mutate(polio3 = case_when(h8%in%c("vaccination date on card","vaccination marked on card","reported by mother") ~ 1, h8%in%c("no","don't know") ~ 0  )) %>%
    mutate(poliosum=polio1 + polio2 + polio3) %>%
    mutate(polio.full = case_when(is.na(poliosum) ~ NA, poliosum == 3 ~ 0, TRUE ~ 1))

  ## INDIVIDUAL VACCINES ----
  # Basis for questions: All children 0–59 months

  ### MEASLES ----
  KR <- KR %>%
    mutate(measles1 = case_when(h9 %in% c("vaccination date on card", "vaccination marked on card", "reported by mother") ~ 1, h9 %in% c("no", "don't know")  ~ 0  )) %>%
    mutate(measles2 = case_when(h9a %in% c("vaccination date on card", "vaccination marked on card", "reported by mother") ~ 1, h9a %in% c("no", "don't know")  ~ 0  )) %>%
    mutate(measlessum = measles1 + measles2) %>%
    mutate(measles.all = case_when(is.na(measlessum) ~ NA, measlessum == 2 ~ 1, TRUE ~ 0)) %>%
    mutate(measles.none = case_when(is.na(measlessum) ~ NA, measlessum == 0 ~ 1, TRUE ~ 0))

  ### DPT ----
  KR <- KR %>%
    mutate(dpt1 = case_when(h3%in%c("vaccination date on card","vaccination marked on card","reported by mother") ~ 1, h3%in%c("no","don't know") ~ 0  )) %>%
    mutate(dpt2 = case_when(h5%in%c("vaccination date on card","vaccination marked on card","reported by mother") ~ 1, h5%in%c("no","don't know") ~ 0  )) %>%
    mutate(dpt3 = case_when(h7%in%c("vaccination date on card","vaccination marked on card","reported by mother") ~ 1, h7%in%c("no","don't know") ~ 0  )) %>%
    mutate(dptsum = dpt1 + dpt2 + dpt3) %>%
    mutate(dpt.all = case_when(is.na(dptsum) ~ NA, dptsum == 3 ~ 1, TRUE ~ 0)) %>%
    mutate(dpt.none = case_when(is.na(dptsum) ~ NA, dptsum == 0 ~ 1, TRUE ~ 0))

  ### POLIO ----
  KR <- KR %>%
    mutate(polio1 = case_when(h4%in%c("vaccination date on card","vaccination marked on card","reported by mother") ~ 1, h4%in%c("no","don't know") ~ 0  )) %>%
    mutate(polio2 = case_when(h6%in%c("vaccination date on card","vaccination marked on card","reported by mother") ~ 1, h6%in%c("no","don't know") ~ 0  )) %>%
    mutate(polio3 = case_when(h8%in%c("vaccination date on card","vaccination marked on card","reported by mother") ~ 1, h8%in%c("no","don't know") ~ 0  )) %>%
    mutate(poliosum=polio1 + polio2 + polio3) %>%
    mutate(polio.all = case_when(is.na(poliosum) ~ NA, poliosum == 3 ~ 0, TRUE ~ 1)) %>%
    mutate(polio.none = case_when(is.na(poliosum) ~ NA, poliosum == 0 ~ 1, TRUE ~ 0))

  ### BCG ----
  KR <- KR %>%
    mutate(bcg1 = case_when(h2 %in% c("vaccination date on card", "vaccination marked on card", "reported by mother") ~ 1, h2 %in% c("no", "don't know")  ~ 0  )) %>%
    mutate(bcg.all = case_when(is.na(bcg1) ~ NA, bcg1 == 1 ~ 1, TRUE ~ 0)) %>%
    mutate(bcg.none = case_when(is.na(bcg1) ~ NA, bcg1 == 0 ~ 1, TRUE ~ 0))

  ### HEP B ----
  KR <- KR %>%
    mutate(hepb1 = case_when(h50 %in% c("vaccination date on card", "vaccination marked on card", "reported by mother") ~ 1, h50 %in% c("no", "don't know")  ~ 0  )) %>%
    mutate(hepb.all = case_when(is.na(hepb1) ~ NA, hepb1 == 1 ~ 1, TRUE ~ 0)) %>%
    mutate(hepb.none = case_when(is.na(hepb1) ~ NA, hepb1 == 0 ~ 1, TRUE ~ 0))

  ### PENTAVALENT ----
  KR <- KR %>%
    mutate(pentavalent1 = case_when(h51%in%c("vaccination date on card","vaccination marked on card","reported by mother") ~ 1, h51%in%c("no","don't know") ~ 0  )) %>%
    mutate(pentavalent2 = case_when(h52%in%c("vaccination date on card","vaccination marked on card","reported by mother") ~ 1, h52%in%c("no","don't know") ~ 0  )) %>%
    mutate(pentavalent3 = case_when(h53%in%c("vaccination date on card","vaccination marked on card","reported by mother") ~ 1, h53%in%c("no","don't know") ~ 0  )) %>%
    mutate(pentavalentsum=pentavalent1 + pentavalent2 + pentavalent3) %>%
    mutate(pentavalent.all = case_when(is.na(pentavalentsum) ~ NA, pentavalentsum == 3 ~ 1, TRUE ~ 0)) %>%
    mutate(pentavalent.none = case_when(is.na(pentavalentsum) ~ NA, pentavalentsum == 0 ~ 1, TRUE ~ 0))

  ### PNEUMOCOCCAL ----
  KR <- KR %>%
    mutate(pneumo1 = case_when(h54%in%c("vaccination date on card","vaccination marked on card","reported by mother") ~ 1, h54%in%c("no","don't know") ~ 0  )) %>%
    mutate(pneumo2 = case_when(h55%in%c("vaccination date on card","vaccination marked on card","reported by mother") ~ 1, h55%in%c("no","don't know") ~ 0  )) %>%
    mutate(pneumo3 = case_when(h56%in%c("vaccination date on card","vaccination marked on card","reported by mother") ~ 1, h56%in%c("no","don't know") ~ 0  )) %>%
    mutate(pneumosum=pneumo1 + pneumo2 + pneumo3) %>%
    mutate(pneumo.all = case_when(is.na(pneumosum) ~ NA, pneumosum == 3 ~ 1, TRUE ~ 0)) %>%
    mutate(pneumo.none = case_when(is.na(pneumosum) ~ NA, pneumosum == 0 ~ 1, TRUE ~ 0))

  ### ROTAVIRUS ----
  KR <- KR %>%
    mutate(rota1 = case_when(h57%in%c("vaccination date on card","vaccination marked on card","reported by mother") ~ 1, h57%in%c("no","don't know") ~ 0  )) %>%
    mutate(rota2 = case_when(h58%in%c("vaccination date on card","vaccination marked on card","reported by mother") ~ 1, h58%in%c("no","don't know") ~ 0  )) %>%
    mutate(rota3 = case_when(h59%in%c("vaccination date on card","vaccination marked on card","reported by mother") ~ 1, h59%in%c("no","don't know") ~ 0  )) %>%
    mutate(rotasum=rota1 + rota2 + rota3) %>%
    mutate(rota.all = case_when(is.na(rotasum) ~ NA, rotasum == 3 ~ 1, TRUE ~ 0)) %>%
    mutate(rota.none = case_when(is.na(rotasum) ~ NA, rotasum == 0 ~ 1, TRUE ~ 0))

  ### YELLOW FEVER ----
  KR <- KR %>%
    mutate(yellowfever1 = case_when(syf %in% c("vaccination date on card", "vaccination marked on card", "reported by mother") ~ 1, syf %in% c("no", "don't know")  ~ 0  )) %>%
    mutate(yellowfever.all = case_when(is.na(yellowfever1) ~ NA, yellowfever1 == 1 ~ 1, TRUE ~ 0)) %>%
    mutate(yellowfever.none = case_when(is.na(yellowfever1) ~ NA, yellowfever1 == 0 ~ 1, TRUE ~ 0))

  ### MENINGITIS ----
  KR <- KR %>%
    mutate(meningitis1 = case_when(smg %in% c("vaccination date on card", "vaccination marked on card", "reported by mother") ~ 1, smg %in% c("no", "don't know")  ~ 0  )) %>%
    mutate(meningitis.all = case_when(is.na(meningitis1) ~ NA, meningitis1 == 1 ~ 1, TRUE ~ 0)) %>%
    mutate(meningitis.none = case_when(is.na(meningitis1) ~ NA, meningitis1 == 0 ~ 1, TRUE ~ 0))

  ## SUMMARY VAX MEASURES ----

  ### BASIC.ANTIGEN.FULL.12M ----
  # Basic antigen coverage (children age 12 months+)
  # One dose BCG, 3 dose polio, 3 dose DPT, 1 dose measles
  KR <- KR %>%
    mutate(
      basic.antigen.full.12m = case_when(
        is.na(h1) ~ NA_real_,
        b19 < 12 | b19 > 24~ NA_real_,
        b19 >= 12 &  bcg.all == 1 & dpt.all == 1 & polio.all == 1 & measles1 == 1 ~ 1,
        b19 >= 12 & b19 <= 24~ 0))

  KR <- KR %>%
    mutate(
      not.basic.antigen.full.12m = case_when(
        is.na(h1) ~ NA_real_,
        b19 < 12 | b19 > 24 ~ NA_real_,
        b19 >= 12 & b19 <= 24 &
          (bcg.all == 0 | polio.all == 0 | measles1 == 0 | dpt.all == 0) ~ 1,
        b19 >= 12 & b19 <= 24 ~ 0
      )
    )

  ### FULL.VAX.SCHEDULE.12M ----
  # Fully vaccinated 12 months (children age 12 months+)
  KR <- KR %>%
    mutate(
      full.vax.schedule.12m = case_when(
        is.na(h1) ~ NA_real_,
        b19 < 12 | b19 > 24 ~ NA_real_,
        b19 >= 12 & b19 <= 24 &
          (bcg.all == 1 &
             hepb.all == 1 &
             pentavalent.all == 1 &
             polio.all == 1 &
             pneumo.all == 1 &
             measles1 == 1 &
             yellowfever.all == 1 &
             meningitis.all == 1) ~ 1,
        b19 >= 12 & b19 <= 24 ~ 0
      )
    )

  KR <- KR %>%
    mutate(
      not.full.vax.schedule.12m = case_when(
        is.na(h1) ~ NA_real_,
        b19 < 12 | b19 > 24 ~ NA_real_,
        b19 >= 12 & b19 <= 24 &
          (bcg.all == 0 |
             hepb.all == 0 |
             pentavalent.all == 0 |
             polio.all == 0 |
             pneumo.all == 0 |
             measles1 == 0 |
             yellowfever.all == 0 |
             meningitis.all == 0) ~ 1,
        b19 >= 12 & b19 <= 24 ~ 0
      )
    )

  ### FULL.VAX.SCHEDULE.24M ----
  # Fully vaccinated 24 months (second dose of measles)
  KR <- KR %>%
    mutate(
      full.vax.schedule.24m = case_when(
        is.na(h1) ~ NA_real_,
        b19 < 24 | b19 > 36 ~ NA_real_,
        b19 >= 24 &  bcg.all == 1 & hepb.all == 1 & pentavalent.all == 1 & polio.all == 1 &
          pneumo.all == 1 & measles.all == 1 & yellowfever.all == 1 & meningitis.all == 1 ~ 1,
        b19 >= 24 & b19 <= 36 ~ 0))

  KR <- KR %>%
    mutate(
      not.full.vax.schedule.24m = case_when(
        is.na(h1) ~ NA_real_,
        b19 < 24 | b19 > 36 ~ NA_real_,
        b19 >= 24 &  bcg.all == 0 | hepb.all == 0 | pentavalent.all == 0 | polio.all == 0 |
          pneumo.all == 0 | measles.all == 0 | yellowfever.all == 0 | meningitis.all == 0 ~ 1,
        b19 >= 24 & b19 <= 36 ~ 0))

  ### ZERO.DOSE.4VAX.12M ----
  # Global zero dose definition (no dose BCG, polio, DPT and MCV)
  KR <- KR %>%
    mutate(
      zero.dose.4vax.12m = case_when(
        b19 < 12 | b19 > 24 ~ NA_real_,
        b19 >= 12 &  bcg1 == 0 & dpt1 == 0 & dpt2 == 0 & dpt3 ==0 &
          polio1 == 0 & polio2 == 0 &polio3 == 0 & measles1 == 0 ~ 1,
        b19 >= 12 & b19 <= 24 ~ 0))

  ### ZERO.DOSE.OPERATIONAL ----
  # Operational zero dose definition (no DPT1)
  KR <- KR %>%
    mutate(
      zero.dose.operational = case_when(
        b19 < 12 | b19 > 24 ~ NA_real_,
        b19 >= 12 &  dpt1 == 0 ~ 1,
        b19 >= 12 & b19 <= 24 ~ 0))


  ######################################################################
  # 9 | MALNOURISHMENT ----

  ## CHILD MINIMUM DIETARY DIVERSITY ----
  KR <- KR %>%
    mutate(
      across(c(v469e, v469f, v469x), ~ na_if(., 8), .names = "{.col}_"),
      across(c(v469e_, v469f_, v469x_), ~ coalesce(., 0)),
      totmilkf = v469e_ + v469f_ + v469x_,
      nt_fed_milk = case_when(
        (totmilkf >= 2 | m4 == "still breastfeeding") & (b19>=6 & b19<=23) ~ 1,
        (totmilkf < 2 | m4 != "still breastfeeding") & (b19>=6 & b19<=23) ~ 0,
        TRUE ~ NA_real_))

  ### COUNTRY SPECIFIC FOOD ----
  KR <- KR %>%
    mutate(food1  = case_when(v414a=="yes"  ~ 1 , v414a!="yes" ~ 0)) %>%
    mutate(food2  = case_when(v414b=="yes"  ~ 1 , v414b!="yes" ~ 0)) %>%
    mutate(food3  = case_when(v414c=="yes"  ~ 1 , v414c!="yes" ~ 0)) %>%
    mutate(food4  = case_when(v414d=="yes"  ~ 1 , v414d!="yes" ~ 0)) %>%
    mutate(nt_formula  = case_when(v411a=="yes"  ~ 1 , v411a!="yes"~ 0)) %>%
    mutate(nt_milk  = case_when(v411=="yes"  ~ 1 , v411!="yes"~ 0)) %>%
    mutate(nt_liquids= case_when(v410=="yes" | v412c=="yes" | v413=="yes"  ~ 1 , v410!="yes" | v412c!="yes" | v413!="yes"  ~ 0)) %>%
    mutate(nt_bbyfood  = case_when(v412a=="yes"  ~ 1 , v412a!="yes"~ 0)) %>%
    mutate(nt_grains  = case_when(v412a=="yes" | v414e=="yes" ~ 1 , v412a!="yes" | v414e!="yes" ~ 0)) %>%
    mutate(nt_vita = case_when(v414i=="yes" | v414j=="yes" | v414k=="yes" ~ 1 , v414i!="yes" | v414j!="yes" | v414k!="yes" ~ 0)) %>%
    mutate(nt_frtveg  = case_when(v414l=="yes"  ~ 1 , v414l!="yes"~ 0)) %>%
    mutate(nt_root  = case_when(v414f=="yes" ~ 1, v414f!="yes" ~ 0)) %>%
    mutate(nt_nuts  = case_when(v414o=="yes"  ~ 1 , v414o!="yes"~ 0)) %>%
    mutate(nt_meatfish  = case_when((v414h=="yes" | v414m=="yes" | v414n=="yes") ~ 1, !(v414h=="yes" | v414m=="yes" | v414n=="yes") ~ 0)) %>%
    mutate(nt_eggs  = case_when(v414g=="yes"  ~ 1 , v414g!="yes"~ 0)) %>%
    mutate(nt_dairy  = case_when(v414p=="yes" | v414v=="yes" ~ 1 , v414p!="yes" | v414v!="yes" ~ 0)) %>%
    mutate(nt_solids = case_when( nt_bbyfood=="yes" | nt_grains=="yes" | nt_vita=="yes" | nt_frtveg=="yes" | nt_root=="yes" | nt_nuts=="yes" | nt_meatfish=="yes" |
                                    nt_eggs=="yes" | nt_dairy=="yes" | v414s=="yes" ~ 1 ,
                                  nt_bbyfood!="yes" | nt_grains!="yes" | nt_vita!="yes" | nt_frtveg!="yes" | nt_root!="yes" | nt_nuts!="yes" | nt_meatfish!="yes" |
                                    nt_eggs!="yes" | nt_dairy!="yes" | v414s!="yes" ~ 0) )

  KR <- KR %>%
    mutate(group1 = case_when(m4=="still breastfeeding"  ~ 1 ,
                              m4!="still breastfeeding" ~ 0)) %>%
    mutate(group2 = case_when(nt_formula==1 | nt_milk==1 | nt_dairy==1  ~ 1 ,
                              nt_formula!=1 | nt_milk!=1 | nt_dairy!=1 ~ 0)) %>%
    mutate(group3  = case_when(nt_grains==1 | nt_root==1 | nt_bbyfood==1 ~ 1 , nt_grains!=1 | nt_root!=1 | nt_bbyfood!=1 ~ 0)) %>%
    mutate(group4  = case_when(nt_vita==1  ~ 1 , nt_vita!=1 ~ 0)) %>%
    mutate(group5  = case_when(nt_frtveg==1 ~ 1 , nt_frtveg!=1~ 0)) %>%
    mutate(group6  = case_when(nt_eggs==1 ~ 1 , nt_eggs!=1~ 0)) %>%
    mutate(group7  = case_when(nt_meatfish==1 ~ 1 , nt_meatfish!=1~ 0)) %>%
    mutate(group8  = case_when(nt_nuts==1 ~ 1 , nt_nuts!=1~ 0)) %>%
    mutate(food_nonmiss = rowSums(!is.na(dplyr::select(., group1:group8))),
           foodsum = case_when(
             food_nonmiss == 0 ~ NA_real_,
             TRUE ~ rowSums(dplyr::select(., group1:group8), na.rm = TRUE))) %>%
    mutate(nt_mdd  = case_when((b19>=6 & b19<24) & foodsum<5 ~ 0 ,
                               (b19>=6 & b19<24) & foodsum>=5~ 1))

  ### MIN MEAL FREQUENCY ----
  KR <- KR %>%
    mutate(feedings = case_when(m39 > 0 & m39 < 8 ~ totmilkf + m39, TRUE ~ NA_real_)) %>%
    mutate(nt_mmf = case_when(b19 < 6 ~ NA_real_,
                              (m4 == "still breastfeeding" & b19 >= 6 & b19 <= 8 & m39 >= 2 & m39 <= 7) |
                                (m4 == "still breastfeeding" & b19 >= 9 & b19 <= 23 & m39 >= 3 & m39 <= 7) ~ 1,
                              (m4 != "still breastfeeding" & b19 >= 6 & b19 <= 23 & feedings >= 4) ~ 1, TRUE ~ 0))

  ### NO.MAD ----
  # Min acceptable diet
  KR <- KR %>%
    mutate(foodsum2 = nt_grains + nt_root + nt_nuts + nt_meatfish + nt_vita + nt_frtveg + nt_eggs) %>%
    mutate(nt_mad = case_when(b19 < 6 ~ NA_real_,
                              (m4 == "still breastfeeding" & nt_mdd == 1 & nt_mmf == 1) ~ 1,
                              (m4 != "still breastfeeding" & foodsum2 >= 4 & nt_mmf == 1 & totmilkf >= 2) ~ 1, TRUE ~ 0)) %>%
    mutate(no.mad = ifelse(nt_mad == 1, 0, 1))

  ## NOVITA.6M ----
  # No vitamin A supplements
  IR <- IR %>% mutate(novita.6m = case_when(h34_1 == "yes" ~ 0,
                                            h34_1 == "no" ~ 1,
                                            h34_1 == "don't know" ~ NA_real_,
                                            v137 == 0 ~ NA_real_))

  ## MICRONUTRIENT.12M ----
  # In the last 12 months given micronutrients
  BR <- BR %>%
    dplyr::mutate(micronutrient.12m = case_when(
      h80a %in% c("no","don't know") ~ 1,
      h80a== "yes" ~ 0))

  ## STUNTING AND WASTING (IR FILE) ----
  if (dhs == 7){

    # HAZ.LAST / STUNT.CAT2.LAST
    IR <- IR %>%
      dplyr::mutate(haz.last = case_when(as.numeric(hw5_1)/100 > 90 ~ NA,
                                         is.na(hw5_1) ~ NA,
                                         TRUE ~ as.numeric(hw5_1)/100)) %>%
      dplyr::mutate(stunt.cat2.last = ifelse(haz.last < -2, 1, 0))

    # WHZ.LAST / WASTE.CAT2.LAST
    IR <- IR %>%
      dplyr::mutate(whz.last = case_when(as.numeric(hw11_1)/100 > 90 ~ NA,
                                         is.na(hw11_1) ~ NA,
                                         TRUE ~ as.numeric(hw11_1)/100)) %>%
      dplyr::mutate(waste.cat2.last = ifelse(whz.last < -2, 1, 0))

  } else if (dhs == 8){

    # HAZ.LAST / STUNT.CAT2.LAST
    IR <- IR %>%
      dplyr::mutate(haz.last = case_when(as.numeric(hw70_1)/100 > 90 ~ NA,
                                         is.na(hw70_1) ~ NA,
                                         TRUE ~ as.numeric(hw70_1)/100)) %>%
      dplyr::mutate(stunt.cat2.last = ifelse(haz.last < -2, 1, 0))

    # WHZ.LAST / WASTE.CAT2.LAST
    IR <- IR %>%
      dplyr::mutate(whz.last = case_when(as.numeric(hw72_1)/100 > 90 ~ NA,
                                         is.na(hw72_1) ~ NA,
                                         TRUE ~ as.numeric(hw72_1)/100)) %>%
      dplyr::mutate(waste.cat2.last = ifelse(whz.last < -2, 1, 0))

  }

  ## WAZ.LAST / UNDWGT.LAST / OVRWGT.LAST ----
  # Underweight/Overweight (IR)
  IR <- IR %>%
    dplyr::mutate(waz.last = case_when(as.numeric(hw71_1)/100 > 90 ~ NA,
                                       is.na(hw71_1) ~ NA,
                                       TRUE ~ as.numeric(hw71_1)/100)) %>%
    dplyr::mutate(undwgt.last = ifelse(waz.last < -2, 1, 0),
                  ovrwgt.last = ifelse(waz.last > 2, 1, 0))

  ## STUNTING AND WASTING (KR FILE) ----
  if (dhs == 7){

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

  } else if (dhs == 8){

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

  }

  ## UNDWGT / OVRWGT (KR FILE) ----
  # Underweight/Overweight
  KR <- KR %>%
    dplyr::mutate(waz = case_when(as.numeric(hw71)/100 > 90 ~ NA,
                                  is.na(hw71) ~ NA,
                                  TRUE ~ as.numeric(hw71)/100)) %>%
    dplyr::mutate(undwgt = ifelse(waz < -2, 1, 0),
                  ovrwgt = ifelse(waz > 2, 1, 0))


  ######################################################################
  # 10 | MENSTRUAL HEALTH ----

  ## MENS.NOPRIV ----
  # Able to change in privacy during last menstrual cycle
  if(dhs == 8)
    IR$mens.nopriv <- ifelse(IR$v248=="no",1,0)

  ## MENS.NOPRODUCT ----
  # Proper product
  IR <- IR %>%
    mutate(mens.product = ifelse(v247a=="yes"|v247b=="yes"|v247c=="yes"|v247d=="yes", 1, 0))

  IR$mens.noproduct = ifelse(IR$mens.product==0, 1, 0)

  ## MENS.NOEITHER ----
  # Either no product or no privacy
  IR <- IR %>%
    mutate(mens.noeither = ifelse(mens.noproduct==1 |mens.nopriv==1, 1, 0))


  ######################################################################
  # 11 | CHILD HEALTH ----

  ## NO.EBF.N ----
  # No exclusive breastfeeding
  KR <- KR %>%
    mutate(bf_curr = case_when(m4 == "still breastfeeding" ~ 1,
                               m4 %in% c("ever breastfed, not currently breastfeeding", "never breastfed", "inconsistent", "don't know") ~ 0))

  # Consuming other foods
  KR <- KR %>%
    mutate(
      water = if_else(v409 == "yes", 1, 0),
      liquids = if_else(v409a == "yes" | v410 == "yes" | v410a == "yes" | v412c == "yes" | v413 == "yes" | v413a == "yes" | v413b == "yes" | v413c == "yes" | v413d == "yes",1, 0),
      milk = if_else(v411 == "yes" | v411a == "yes", 1, 0),
      solids = if_else(v414a == "yes" | v414b == "yes" | v414c == "yes" | v414d == "yes" | v414e == "yes" | v414f == "yes" | v414g == "yes" | v414h == "yes" | v414i == "yes" | v414j == "yes" | v414k == "yes" | v414l == "yes" | v414m == "yes" | v414n == "yes" | v414o == "yes" |
                         v414p == "yes" | v414r == "yes" | v414s == "yes" | v414t == "yes" | v414u == "yes" | v414v == "yes" | v414w == "yes" | m39a == "yes", 1, 0))

  # Breastfeeding status
  KR <- KR %>%
    mutate(
      bf_status = case_when(
        bf_curr == 0 ~ 0,
        solids == 1    ~ 5,
        milk == 1      ~ 4,
        liquids == 1   ~ 3,
        water == 1     ~ 2,
        TRUE ~ 1 ))

  KR <- KR %>%
    mutate(no.ebf.n = case_when(b19 < 6 & b9 == "respondent" & bf_status == 1  ~ 0 ,
                                b19 < 6 & b9 == "respondent" & bf_status != 1 ~ 1 ))

  ## NOBARESKIN ----
  # No skin to skin contact
  IR <- IR %>% mutate(nobareskin = ifelse(m77_1 == "put on chest, touching bare skin", 0, 1))

  ## COMP.FEED ----
  # Child given complementary feeding (dependency for breastfed2.noexcl)
  IR <- IR %>%
    mutate(comp.feed = case_when(IR$v409 == "yes" | IR$v410 == "yes" | IR$v411 == "yes" | IR$v411 == "yes" |
                                   IR$v412c == "yes" | IR$v413 == "yes" | IR$v414e == "yes"  | IR$v414f == "yes" |
                                   IR$v414g == "yes" | IR$v414h == "yes" | IR$v414i == "yes" | IR$v414j == "yes" |
                                   IR$v414k == "yes" | IR$v414l == "yes" | IR$v414m == "yes" | IR$v414n == "yes" |
                                   IR$v414o == "yes" | IR$v414p == "yes" | IR$v414s == "yes" | IR$v414v == "yes" ~1,
                                 TRUE ~ 0))

  ## BREASTFEED.LAST / BREASTFEED2.LAST / BREASTFED2.NOEXCL ----
  # Child did not receive exclusive breastfeeding (dependency variable)
  IR <- IR %>%
    dplyr::mutate(breastfeed.last = case_when(m4_1 %in% c("never breastfed") ~ 0,
                                              is.na(m4_1) ~ NA,
                                              TRUE ~ 1))
  IR <- IR %>%
    dplyr::mutate(breastfeed2.last = ifelse((b19_01 < 24 & b9_01 == "respondent"), breastfeed.last, NA))

  IR$breastfed2.noexcl<-ifelse((IR$breastfeed2.last==1 & IR$comp.feed==0),0,1)

  ## NO.IMM.BREASTFEED.LAST.YN ----
  # Last child immediately breastfed
  IR <- IR %>% mutate(no.imm.breastfeed.last.yn = case_when(
    m34_1 %in% c("0", "100") ~ 0,
    is.na(m34_1) ~ NA_integer_,
    TRUE ~ 1))

  ## LOW.BIRTHWGT ----
  # Low birthweight last child
  IR <- IR %>% mutate(low.birthwgt = ifelse(m19_1 >= 2500 & m19_1 < 9995, 0, 1))
  IR <- IR %>% mutate(low.birthwgt = ifelse(m19_1 == 9998 | m19_1 == 9996, NA, low.birthwgt))

  ## NOTX.WORMS.6M ----
  # Not treated for worms
  IR <- IR %>% mutate(notx.worms.6m = case_when(h43_1 == "yes" ~ 0,
                                                h43_1 == "no" ~ 1,
                                                h43_1 == "don't know" ~ NA_real_,
                                                v137 == 0 ~ NA_real_))

  ## DIARRHEA.2WKS ----
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

  ## FEVER.2WKS ----
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

  ## COUGH.2WKS ----
  # Had cough in last two weeks (LAST CHILD UNDER 5)
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

  ## FEVER.COUGH.2WKS ----
  # Fever or cough (aligns with treatment question)
  IR <- IR %>%
    dplyr::mutate(fever.cough.2wks.last = ifelse(fever.2wks.last == 1 | cough.2wks.last == 1, 1, 0))

  KR <- KR %>%
    dplyr::mutate(fever.cough.2wks = ifelse(fever.2wks == 1 | cough.2wks == 1, 1, 0))

  ## CHEST.PROB ----
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

  ## DIFF.BREATH ----
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

  ## ARI ----
  # ARI symptoms
  IR <- IR %>%
    dplyr::mutate(ari.last = ifelse(chest.prob.last == 1 & diff.breath.last == 1, 1, 0))

  KR <- KR %>%
    dplyr::mutate(ari = ifelse(chest.prob == 1 & diff.breath == 1, 1, 0))

  ## NO.FEVER.COUGH.CARE.YN ----
  # Health seeking for illness: treatment for fever/cough
  kr_var <- KR %>%
    dplyr::select(survey, caseid, starts_with("h32")) %>%
    reshape2::melt(id.vars=c("survey", "caseid")) %>%
    dplyr::mutate(care = case_when((variable == "h32a" & value == "yes") ~ 1,
                                   (variable == "h32b" & value == "yes") ~ 1,
                                   (variable == "h32c" & value == "yes") ~ 1,
                                   (variable == "h32d" & value == "yes") ~ 1,
                                   (variable == "h32e" & value == "yes") ~ 1,
                                   (variable == "h32f" & value == "yes") ~ 1,
                                   (variable == "h32g" & value == "yes") ~ 1,
                                   (variable == "h32h" & value == "yes") ~ 1,
                                   (variable == "h32i" & value == "yes") ~ 1,
                                   (variable == "h32j" & value == "yes") ~ 1,
                                   (variable == "h32k" & value == "yes") ~ 1,
                                   (variable == "h32l" & value == "yes") ~ 1,
                                   (variable == "h32m" & value == "yes") ~ 1,
                                   (variable == "h32n" & value == "yes") ~ 1,
                                   (variable == "h32o" & value == "yes") ~ 1,
                                   (variable == "h32p" & value == "yes") ~ 1,
                                   (variable == "h32q" & value == "yes") ~ 1,
                                   (variable == "h32nb" & value == "yes") ~ 1,
                                   (variable == "h32nc" & value == "yes") ~ 1,
                                   is.na(value) ~ NA,
                                   TRUE ~ 0)) %>%
    dplyr::group_by(survey, caseid) %>%
    dplyr::mutate(care.cnt = ifelse(all(is.na(care)), NA, sum(care, na.rm=TRUE)),
                  no.fever.cough.care.yn = ifelse(care.cnt == 0, 1, 0)) %>%
    dplyr::select(survey, caseid, care.cnt, no.fever.cough.care.yn) %>%
    distinct()


  ######################################################################
  # 12 | MATERNAL HEALTH ----

  ## OVERWEIGHT.OBESE ----
  # Woman is overweight or obese
  IR$bmi<-IR$v445/100
  IR$bmi<-ifelse(IR$bmi>90,NA,IR$bmi)
  IR<- IR %>% mutate(bmi.cat1 = case_when(
    (bmi < 16) ~ "Severely thin",
    (bmi >= 16 & bmi < 17) ~ "Moderately thin",
    (bmi >= 17 & bmi < 18.5) ~ "Mildly thin",
    (bmi >= 18.5 & bmi < 25) ~ "Normal",
    (bmi >= 25 & bmi < 30) ~ "Overweight",
    (bmi >= 30) ~ "Obese"))

  IR <- IR %>%mutate(overweight.obese = if_else(bmi.cat1 %in% c("Overweight", "Obese"), 1, 0))

  ## PREG.TERMIN ----
  # Ever terminated a pregnancy
  IR$preg.termin <- IR$v228
  IR <- IR %>% dplyr::mutate(preg.termin = case_when(
    preg.termin == "yes" ~ 1,
    preg.termin == "no" ~ 0,
  ))

  ## STI.ANY ----
  # Any STI's in the last 12 months
  IR<- IR %>% mutate(sti.any = case_when(
    v763a=="yes"~1,
    v763b=="yes"~1,
    v763c=="yes"~1,
    v763a=="no" & v763b=="no" & v763c=="no"~0,
    is.na(v763a)~ NA_integer_,
    is.na(v763b)~  NA_integer_,
    is.na(v763c)~ NA_integer_))

  # STI's yes/no (dependency variable)
  IR <- IR %>% mutate(sti.yn = case_when(
    v763a == "yes" | v763b == "yes" | v763c == "yes" | v763d == "yes" ~ 1,
    is.na(v763a) & is.na(v763b) & is.na(v763c) & is.na(v763d) ~ NA_integer_,
    TRUE ~ 0 ))

  ## HIV.NEVER.TESTED ----
  # Never tested for HIV
  IR<-IR %>% mutate(hiv.never.tested = case_when(
    v781=="no"~ 1,
    v781 == "yes"~0,
    is.na(v781)~NA_integer_))

  ## NO_SKILLED_ASSIST_BIN ----
  # Who assisted delivery
  BR <- BR %>%
    mutate(across(m3a:m3n, ~ ifelse(.x == "yes", 1,
                                    ifelse(.x == "no", 0, NA_integer_))))
  BR <- BR %>%
    mutate(
      who.assisted.del = case_when(
        m3a == "yes" ~ "Doctor",
        m3b == "yes" ~ "Nurse/midwife",
        m3c == "yes" ~ "Auxilary midwife",
        m3d == "yes" | m3e == "yes" | m3f == "yes" ~ "Community health extension worker",
        m3g == "yes" ~ "Traditional birth attendant",
        m3h == "yes" | m3i == "yes" | m3j == "yes" | m3k == "yes" | m3l == "yes" | m3m == "yes" ~ "Relative/other",
        m3n == "yes" ~ "No one",
        m3a == NA ~ "NA - last birth >5 yrs",
        is.na(m3a) ~ "NA - last birth >5 yrs",
        TRUE ~ NA_character_))

  BR <- BR %>%
    mutate(
      no_skilled_assist_bin = case_when(
        who.assisted.del %in% c(
          "Auxilary midwife",
          "Nurse/midwife",
          "Doctor",
          "Community health extension worker"
        ) ~ 0,
        is.na(who.assisted.del) ~ NA_real_,
        TRUE ~ 1
      )
    )

  ## NO.PRENATAL.TETNUS ----
  # Tetanus toxoid (coded as none during pregnancy)
  BR <- BR %>%
    dplyr::mutate(no.prenatal.tetnus = case_when(m1 == 0 ~ 1,
                                                 m1 %in% c(1:8) ~ 0))

  ## UNMET.FP.NEED ----
  # Unmet contraceptive need
  IR<-IR %>%
    dplyr::mutate(unmet.fp.need = case_when(
      v626a=="unmet need for limiting" ~ "Unmet need for limiting",
      v626a=="unmet need for spacing" ~ "Unmet need for spacing",
      v626a=="no unmet need" ~ "No unmet need",
      v626a=="using for limiting" | v626a=="using for spacing" ~ "Using contraception",
      v626a=="never had sex" | v626a=="not married and no sex in last 30 days" | v626a=="infecund, menopausal"  ~ "NA (infecund, menopausal, no sex)"))

  ## UNMET.FP.NEED_YN ----
  # Unmet need - binary
  IR<-IR %>%
    dplyr::mutate(unmet.fp.need_yn = case_when(
      v626a=="unmet need for limiting" | v626a=="unmet need for spacing" ~ 1,
      v626a=="no unmet need" | v626a=="using for limiting" | v626a=="using for spacing" ~ 0,
      v626a=="never had sex" | v626a=="not married and no sex in last 30 days" | v626a=="infecund, menopausal"  ~ NA))


  ######################################################################
  # 13 | SUMMARIZE ----

  ## INDIVIDUAL (IR) ----
  ir.out <- IR

  ## BIRTH (BR) ----
  br.out <- BR %>%
    group_by(survey, caseid) %>%
    dplyr::summarize(b.rost = length(caseid),
                     u1mort.cnt = ifelse(all(is.na(u1mort)), NA, sum(u1mort, na.rm=TRUE)),
                     u5mort.cnt = ifelse(all(is.na(u5mort)), NA, sum(u5mort, na.rm=TRUE)),
                     unassisted.del.cnt = ifelse(all(is.na(unassisted.del)), NA, sum(unassisted.del, na.rm=TRUE)),
                     no.prenatal.tetnus.cnt = ifelse(all(is.na(no.prenatal.tetnus)), NA, sum(no.prenatal.tetnus, na.rm=TRUE)),
                     micronutrient.12m.cnt = ifelse(all(is.na(micronutrient.12m)), NA, sum(micronutrient.12m, na.rm=TRUE)),
                     home.birth.cnt = ifelse(all(is.na(home.birth)), NA, sum(home.birth, na.rm=TRUE)),
                     no_skilled_assist.cnt = ifelse(all(is.na(no_skilled_assist_bin)), NA, sum(no_skilled_assist_bin, na.rm=TRUE))) %>%
    mutate(u1mort.yn = ifelse(u1mort.cnt > 0, 1, 0),
           u5mort.yn = ifelse(u5mort.cnt > 0, 1, 0),
           unassisted.del.yn = ifelse(unassisted.del.cnt > 0, 1, 0),
           no.prenatal.tetnus.yn = ifelse(no.prenatal.tetnus.cnt > 0, 1, 0),
           micronutrient.12m.yn = ifelse(micronutrient.12m.cnt > 0, 1, 0),
           no_skilled_assist.yn = ifelse(no_skilled_assist.cnt > 0, 1, 0),
           home.birth.yn = ifelse(home.birth.cnt > 0, 1, 0)) %>%
    dplyr::select(survey, caseid, no_skilled_assist.yn, unassisted.del.yn , unassisted.del.cnt, no.prenatal.tetnus.yn, no.prenatal.tetnus.cnt, u1mort.cnt, u5mort.cnt, home.birth.cnt, u1mort.yn, u5mort.yn, home.birth.yn, micronutrient.12m.yn)

  ## KID (KR) ----
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
                     zero.dose.operational.cnt = ifelse(all(is.na(zero.dose.operational)), NA, sum(zero.dose.operational, na.rm=TRUE)),
                     zero.dose.4vax.12m.cnt = ifelse(all(is.na(zero.dose.4vax.12m)), NA, sum(zero.dose.4vax.12m, na.rm=TRUE)),
                     not.full.vax.schedule.24m.cnt = ifelse(all(is.na(not.full.vax.schedule.24m)), NA, sum(not.full.vax.schedule.24m, na.rm=TRUE)),
                     not.full.vax.schedule.12m.cnt = ifelse(all(is.na(not.full.vax.schedule.12m)), NA, sum(not.full.vax.schedule.12m, na.rm=TRUE)),
                     full.vax.schedule.24m.cnt = ifelse(all(is.na(full.vax.schedule.24m)), NA, sum(full.vax.schedule.24m, na.rm=TRUE)),
                     full.vax.schedule.12m.cnt = ifelse(all(is.na(full.vax.schedule.12m)), NA, sum(full.vax.schedule.12m, na.rm=TRUE)),
                     measles.all.cnt = ifelse(all(is.na(measles.all)), NA, sum(measles.all, na.rm=TRUE)),
                     measles.none.cnt = ifelse(all(is.na(measles.none)), NA, sum(measles.none, na.rm=TRUE)),
                     polio.all.cnt = ifelse(all(is.na(polio.all)), NA, sum(polio.all, na.rm=TRUE)),
                     polio.none.cnt = ifelse(all(is.na(polio.none)), NA, sum(polio.none, na.rm=TRUE)),
                     dpt.all.cnt = ifelse(all(is.na(dpt.all)), NA, sum(dpt.all, na.rm=TRUE)),
                     dpt.none.cnt = ifelse(all(is.na(dpt.none)), NA, sum(dpt.none, na.rm=TRUE)),
                     bcg.all.cnt = ifelse(all(is.na(bcg.all)), NA, sum(bcg.all, na.rm=TRUE)),
                     bcg.none.cnt = ifelse(all(is.na(bcg.none)), NA, sum(bcg.none, na.rm=TRUE)),
                     hepb.all.cnt = ifelse(all(is.na(hepb.all)), NA, sum(hepb.all, na.rm=TRUE)),
                     hepb.none.cnt = ifelse(all(is.na(hepb.none)), NA, sum(hepb.none, na.rm=TRUE)),
                     pneumo.all.cnt = ifelse(all(is.na(pneumo.all)), NA, sum(pneumo.all, na.rm=TRUE)),
                     pneumo.none.cnt = ifelse(all(is.na(pneumo.none)), NA, sum(pneumo.none, na.rm=TRUE)),
                     rota.all.cnt = ifelse(all(is.na(rota.all)), NA, sum(rota.all, na.rm=TRUE)),
                     rota.none.cnt = ifelse(all(is.na(rota.none)), NA, sum(rota.none, na.rm=TRUE)),
                     yellowfever.all.cnt = ifelse(all(is.na(yellowfever.all)), NA, sum(yellowfever.all, na.rm=TRUE)),
                     yellowfever.none.cnt = ifelse(all(is.na(yellowfever.none)), NA, sum(yellowfever.none, na.rm=TRUE)),
                     meningitis.all.cnt = ifelse(all(is.na(meningitis.all)), NA, sum(meningitis.all, na.rm=TRUE)),
                     meningitis.none.cnt = ifelse(all(is.na(meningitis.none)), NA, sum(meningitis.none, na.rm=TRUE)),
                     pentavalent.all.cnt = ifelse(all(is.na(pentavalent.all)), NA, sum(pentavalent.all, na.rm=TRUE)),
                     pentavalent.none.cnt = ifelse(all(is.na(pentavalent.none)), NA, sum(pentavalent.none, na.rm=TRUE)),
                     pos.basic.antigen.full.12m.cnt = ifelse(all(is.na(basic.antigen.full.12m)), NA, sum(basic.antigen.full.12m, na.rm=TRUE)),
                     not.basic.antigen.full.12m.cnt = ifelse(all(is.na(not.basic.antigen.full.12m)), NA, sum(not.basic.antigen.full.12m, na.rm=TRUE)),
                     no.ebf.cnt = ifelse(all(is.na(no.ebf.n)), NA, sum(no.ebf.n, na.rm=TRUE)),
                     ari.cnt = ifelse(all(is.na(ari)), NA, sum(ari, na.rm=TRUE)),
                     no.mad.cnt = ifelse(all(is.na(no.mad)), NA, sum(no.mad, na.rm = TRUE))) %>%
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
                  zero.dose.operational.yn = ifelse(zero.dose.operational.cnt > 0, 1, 0),
                  zero.dose.4vax.12m.yn = ifelse(zero.dose.4vax.12m.cnt > 0, 1, 0),
                  not.full.vax.schedule.24m.yn = ifelse(not.full.vax.schedule.24m.cnt > 0, 1, 0),
                  not.full.vax.schedule.12m.yn = ifelse(not.full.vax.schedule.12m.cnt > 0, 1, 0),
                  pos.full.vax.schedule.24m.yn = ifelse(full.vax.schedule.24m.cnt > 0, 1, 0),
                  pos.full.vax.schedule.12m.yn = ifelse(full.vax.schedule.12m.cnt > 0, 1, 0),
                  pos.measles.all.yn = ifelse(measles.all.cnt > 0, 1, 0),
                  measles.none.yn = ifelse(measles.none.cnt > 0, 1, 0),
                  pos.polio.all.yn = ifelse(polio.all.cnt > 0, 1, 0),
                  polio.none.yn = ifelse(polio.none.cnt > 0, 1, 0),
                  pos.dpt.all.yn = ifelse(dpt.all.cnt > 0, 1, 0),
                  dpt.none.yn = ifelse(dpt.none.cnt > 0, 1, 0),
                  pos.bcg.all.yn = ifelse(bcg.all.cnt > 0, 1, 0),
                  bcg.none.yn = ifelse(bcg.none.cnt > 0, 1, 0),
                  pos.hepb.all.yn = ifelse(hepb.all.cnt > 0, 1, 0),
                  hepb.none.yn = ifelse(hepb.none.cnt > 0, 1, 0),
                  pos.pneumo.all.yn = ifelse(pneumo.all.cnt > 0, 1, 0),
                  pneumo.none.yn = ifelse(pneumo.none.cnt > 0, 1, 0),
                  pos.rota.all.yn = ifelse(rota.all.cnt > 0, 1, 0),
                  rota.none.yn = ifelse(rota.none.cnt > 0, 1, 0),
                  pos.yellowfever.all.yn = ifelse(yellowfever.all.cnt > 0, 1, 0),
                  yellowfever.none.yn = ifelse(yellowfever.none.cnt > 0, 1, 0),
                  pos.meningitis.all.yn = ifelse(meningitis.all.cnt > 0, 1, 0),
                  meningitis.none.yn = ifelse(meningitis.none.cnt > 0, 1, 0),
                  pos.pentavalent.all.yn = ifelse(pentavalent.all.cnt > 0, 1, 0),
                  pentavalent.none.yn = ifelse(pentavalent.none.cnt > 0, 1, 0),
                  pos.basic.antigen.full.12m.yn = ifelse(pos.basic.antigen.full.12m.cnt > 0, 1, 0),
                  basic.antigen.full.none.12m.yn = ifelse(not.basic.antigen.full.12m.cnt > 0, 1, 0),
                  no.ebf.yn = ifelse(no.ebf.cnt > 0, 1, 0),
                  ari.yn = ifelse(ari.cnt > 0, 1, 0),
                  no.mad.yn = ifelse(no.mad.cnt > 0, 1, 0)) %>%
    dplyr::select(survey, caseid,
                  no.breastfeed.cnt, no.breastfeed2.cnt, ovrwgt.cnt, undwgt.cnt, stunt.cat2.cnt, waste.cat2.cnt, meas.full.cnt, dpt.full.cnt, polio.full.cnt, zerodose.cnt, diarrhea.cnt, fever.cnt, cough.cnt, diff.breath.cnt, chest.prob.cnt, ari.cnt, no.mad.cnt,
                  no.breastfeed.yn, no.breastfeed2.yn, ovrwgt.yn, undwgt.yn, stunt.cat2.yn, waste.cat2.yn, meas.full.yn, dpt.full.yn, polio.full.yn, zerodose.yn, fever.yn, cough.yn, diff.breath.yn, chest.prob.yn,
                  zero.dose.operational.yn, zero.dose.4vax.12m.yn, not.full.vax.schedule.24m.yn, not.full.vax.schedule.12m.yn,
                  pos.full.vax.schedule.24m.yn, pos.full.vax.schedule.12m.yn, pos.measles.all.yn, pos.polio.all.yn, pos.dpt.all.yn,
                  pos.bcg.all.yn, pos.hepb.all.yn, pos.pneumo.all.yn, pos.rota.all.yn, pos.yellowfever.all.yn, pos.meningitis.all.yn,
                  no.mad.yn, no.ebf.yn, measles.none.yn, polio.none.yn, dpt.none.yn, bcg.none.yn, hepb.none.yn, pneumo.none.yn,
                  rota.none.yn, yellowfever.none.yn, meningitis.none.yn, pos.basic.antigen.full.12m.yn, basic.antigen.full.none.12m.yn,
                  pentavalent.none.yn, pos.pentavalent.all.yn, ari.yn, bcg.all.cnt,zero.dose.4vax.12m.cnt, no.ebf.cnt) %>%
    base::merge(kr_var, by=c("survey", "caseid"), all.x=TRUE)

  ## JOIN TOGETHER ----
  outcomes <- ir.out %>%
    base::merge(kr.out, by=c("survey", "caseid"), all.x=TRUE) %>%
    base::merge(br.out, by=c("survey", "caseid"), all.x=TRUE) %>%
    base::merge(stl.out, by=c("caseid"), all.x=TRUE)

  ## DROP INTERMEDIARY VARIABLES ----
  drop_vars <- c("anc.1st.mean", "anc.mean") # here you can drop any intermediate variables you don't wish to keep.
  outcomes <- outcomes %>% dplyr::select(-all_of(drop_vars))

  return(outcomes)

}




