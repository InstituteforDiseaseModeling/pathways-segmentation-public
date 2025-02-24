# GENERATE OUTCOME VARIABLES ###################################################

# PROGRAM:
# PURPOSE:
# DATA INPUTS:
# DATA OUTPUTS:
# AUTHOR:
# DATA LAST MODIFIED:
# NOTES:

# RUN SETUP ####################################################################
# IR <- IR1
# KR <- KR1
# BR <- BR1


# TABLE OF CONTENTS ############################################################
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
# 11 | SUMMARIZE


# DEFINE FUNCTION ##############################################################

gen_outcome_variables <- function(IR, KR, BR, northern_nigeria = TRUE){


  # 1 | GENERAL #################################################################

  ## INDIVIDUAL -----
  IR[,str:=substr(vcal_1, v018, v018+59)]
  IR[,STL:=ifelse(grepl('TPPPPPP',str),1,NA)]
  IR[,LB:=ifelse(grepl('BPPPPPPP',str),1,NA)]


  ## BIRTH ----
  BR$hhid <- paste(BR$v001, BR$v002)
  BR[,ChildAge:= (v008-b3)]


  ## KID ----
  KR$hhid<-paste(KR$v001, KR$v002)


  # 2 | ANC/PNC #################################################################

  ## NUMBER OF ANC VISITS ----
  # CALCULATE THE MEAN NUMBER OF ANC VISITS AND IMPUTE AS DNK IN ANC OUTCOME VARIABLES
  IR = IR %>%
    mutate(anc.num = case_when(
      m14_1 == 98 ~ round((mean(m14_1, na.rm = TRUE)),  0),
      TRUE ~ m14_1),
      # BINARY OUTCOME FOR ANC.NUM >= 4
      anc.less4 = ifelse(anc.num >= 4, 0, 1),
      anc.none = ifelse(anc.num>0, 0, 1))#,
      # BINARY OUTCOME FOR ANC.NUM >= 8
      #anc.8plus = ifelse(IR$anc.num >= 8, 1, 0))


  # CALCULATE THE MEAN MONTH OF 1ST ANC VISIT AND IMPUTE AS DNK IN ANC OUTCOME VARIABLES
  IR = IR %>%
    mutate(anc.1stvisit = case_when(
      m13_1 == 98 ~ round(mean(m13_1, na.rm = TRUE), 0),
      m14_1 == 0 ~ 10,
      TRUE ~ m13_1),
      # CATEGORICAL OUTCOME FOR ANC 1ST VISIT
      anc.month = factor(case_when(
        anc.1stvisit < 4 ~ "Month 1-3",
        anc.1stvisit > 3 & anc.1stvisit < 7 ~ "Month 4-6",
        anc.1stvisit > 6 & anc.1stvisit < 10 ~ "Month 7-9",
        TRUE ~ 'No ANC'),
        levels=c("No ANC","Month 1-3","Month 4-6", "Month 7-9") ))

  ## WOMAN HAD HEALTH CHECK AFTER BIRTH ----
  IR = IR %>%
    mutate(wom.nohlthck1 = case_when(
      (m66_1=="yes" & !is.na(m66_1)) | (m62_1=="yes" & !is.na(m62_1)) ~ 0,
      is.na(m62_1) & is.na(m66_1) ~ NA,
      TRUE ~ 1
    ))

  ## BABY HAD HEALTH CHECK AFTER BIRTH ----
  IR = IR %>%
    mutate(baby.nohlthck1 = case_when(
      m70_1=="yes" & !is.na(m70_1) | m74_1=="yes" & !is.na(m74_1) ~ 0,
      is.na(m70_1) & is.na(m74_1) ~ NA,
      TRUE ~ 1
    ))

  # 3 | BMI ####################################################################

  ## BMI ----
  IR = IR %>%
    mutate(bmi1 = case_when(
      v445/100 <=90 ~  v445/100 ,
      TRUE ~ NA
    ))

  # Underweight ----
  IR$underweight <- NA

  # Overweight or obese ----
  IR$overweight.obese <- NA

  # Obese ----
  IR$obese <- NA

  # 4 | BREASTFEEDING  #########################################################

  ## CATEGORICAL OUTCOME FOR LATEST CHILD BREASTFED  ----
  IR = IR %>%
    mutate(no.breastfed = ifelse(m4_1 == "ever breastfed, not currently breastfeeding" |
                                   m4_1=="still breastfeeding", 0, 1),
           # ALTERNATE DEFINITION FOR LAST CHILD BREASTFED
           no.breastfed2 = ifelse((b19_01 < 24 & b9_01 == "respondent"), no.breastfed, NA))

  KR = KR %>%
    mutate(breastfeed.n  = ifelse((b19 < 24 & b9 == "respondent"), 1, 0))

  # 5 | CHILD MORTALITY ########################################################

  ## CHILD UNDER 1 DIED ----
  BR = BR %>%
    mutate(u1mort = ifelse(b7<13 & b5=="no", 1, 0),

           # CHILD UNDER 5 DIED
           u5mort = ifelse(b5=="no", 1, 0))


  ## STILLBIRTHS ----
  getStillbirths <- function(individ,id_vars,recode=7){
    setDT(individ)

    tmp=data.table::melt(individ, measure.vars=patterns('^bord[_]','^b0[_]',
                                                        '^b3[_]','^b4[_]','^b5[_]',
                                                        '^b6[_]','^b7[_]','^b8[_]',
                                                        '^b11[_]','^m14[_]','^m15[_]',
                                                        '^m17[_]'),
                         value.name=c('bord','b0','dob',"sex",'alive',"deathAge",
                                      'deathAgeMo','CurrAge',"b11","m14","m15","m17"),
                         variable.name='ReverseOrder',
                         id.vars=id_vars)
  }


  stl_var_list = c("caseid", "v000", "v001", "v002", "v003", "v004", "v005",
                   "v006", "v007", "v008", "v008a", "v009", "v010", "v011",
                   "v012", "v013", "v014", "v015", "v016", "v017", "v018",
                   "v019", "v019a", "v020", "v021", "v022", "v023", "v024",
                   "v025", "v026", "v027", "v028", "v029", "v030", "v031",
                   "v032", "v034", "v040", "sm508va_4", "sm508va_5", "sm508va_6",
                   "str", "STL", "LB")

  some <- IR %>% dplyr::select(any_of(stl_var_list)) %>% names()

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
  stl.out$stl.yn<-ifelse(stl.out$stl.cnt > 0,1,0)


  # 6 | FAMILY PLANNING ########################################################

  ## CURRENTLY USING MODERN FAMILY PLANNING METHODS  ----
  IR$nofp.mod.now <- ifelse(IR$v313 == "modern method", 0, 1)


  ## EVER USED MODERN FAMILY PLANNING METHODS  ----
  # Code all modern methods as 1, and use of all other methods or not method
  # (NAs) as 0.
  IR = IR %>%
    mutate(
      # Method discontinued in the last 5 years (1 if a method was discontinued, 0 otherwise)
      nofp.dis.5yr =
        case_when(v359=="pill"|v359=="iud"|v359=="injections"|
                    v359=="male condom"| v359=="implants/norplant"|
                    v359=="female condom"|v359=="emergency contraception"|
                    v359=="other modern method" ~ 1,
                  is.na(v359) ~ 0,
                 TRUE ~ 0),
      # Ever
      nofp.mod.ever = case_when( # Not using now and did not discontinue in past 5 years
                                nofp.mod.now==1 & nofp.dis.5yr==0 ~ 1,
                                # All other cases
                                # A) Using now AND did not discontinue (is using now)
                                # B) Using now and did discontinue (has used in past 5 yrs)
                                TRUE ~ 0))

  # 7 | HOME BIRTHS ############################################################

  # LATEST BIRTH WAS HOME BIRTH
  IR[,hb.1:= ifelse(m15_1== "home" | m15_1== "respondent's home" |
                      m15_1== "other home" | m15_1== "other", 0,1)]

  # 8 | IMMUNIZATION ###########################################################
  ## *** Pentavalent *** ----
  # DPT 1, 2, 3 either source
  KR <- KR %>%
    mutate(dpt1 = case_when(h3%in%c("vaccination date on card","vaccination marked on card",
                                    "reported by mother") ~ 1,
                            h3%in%c("no","don't know") ~ 0  )) %>%
    mutate(dpt2 = case_when(h5%in%c("vaccination date on card","vaccination marked on card",
                                    "reported by mother") ~ 1,
                            h5%in%c("no","don't know") ~ 0  )) %>%
    mutate(dpt3 = case_when(h7%in%c("vaccination date on card","vaccination marked on card",
                                    "reported by mother") ~ 1,
                            h7%in%c("no","don't know") ~ 0  )) %>%
    mutate(dptsum = dpt1 + dpt2 + dpt3) %>%
    mutate(dpt.full = case_when(is.na(dptsum) ~ NA,
                                dptsum == 3 ~ 0,
                                TRUE ~ 1)) %>%
    mutate(zero.dose = case_when(is.na(dptsum) ~ NA,
                                 dptsum == 0 ~ 1, TRUE ~ 0))

  ## *** Measles *** ----
  # Measles either source
  KR <- KR %>%
    mutate(measles1 = case_when(h9%in%c("vaccination date on card","vaccination marked on card",
                                        "reported by mother") ~ 1,
                                h9%in%c("no","don't know")  ~ 0  ),
           measles2 = case_when(h9a%in%c("vaccination date on card","vaccination marked on card",
                                         "reported by mother") ~ 1,
                                h9a%in%c("no","don't know")  ~ 0  ) ,
           measlessum=measles1 + measles2,
           measles.full = case_when(is.na(measlessum) ~ NA, measlessum == 2 ~ 0, TRUE ~ 1))


  ## *** Polio *** ----
  # Polio 0, 1, 2, 3 either source
  KR <- KR %>%
    mutate(polio1 = case_when(h4%in%c("vaccination date on card",
                                      "vaccination marked on card","reported by mother") ~ 1,
                              h4%in%c("no","don't know") ~ 0  ),
           polio2 = case_when(h6%in%c("vaccination date on card",
                                      "vaccination marked on card","reported by mother") ~ 1,
                              h6%in%c("no","don't know") ~ 0  )) %>%
    mutate(polio3 = case_when(h8%in%c("vaccination date on card",
                                      "vaccination marked on card","reported by mother") ~ 1,
                              h8%in%c("no","don't know") ~ 0  ),
           poliosum=polio1 + polio2 + polio3,
           polio.full = case_when(is.na(poliosum) ~ NA, poliosum == 3 ~ 0, TRUE ~ 1))


  # 9 | MALNOURISHMENT #########################################################

  # Note: The DHS reports The DHS reports the HAZ/WAZ/WHZ as
  # standard deviations * 100, so -2sd would represent -200 here.
  #height/age standard deviation (AG 2/28).
  # Note: Setting values higher than 90*100 to NAs because missing values
  # were coded as 9998 (therefore 9998/100 = 99.98 and > 90)

  # STANDARD MALNOURISHMENT VARIABLES IN KID FILE ----
  KR = KR %>% mutate(
    haz = case_when(hw5/100 > 90 ~ NA,
                    TRUE ~ hw5/100 ),
    waz = case_when(hw8/100 > 90 ~ NA,
                    TRUE ~ hw8/100),
    whz = case_when(hw11/100 > 90 ~ NA,
                    TRUE ~ hw11/100))

  # STANDARD MALNOURISHMENT VARIABLES IN WOMEN'S FILE ----
  IR = IR %>%
    mutate(
      # Height/age standard deviation
      haz.last = case_when(hw5_1/100 > 90 ~ NA,
                           TRUE ~ hw5_1/100),
      #weight/age standard deviation (AG 2/28)
      waz.last = case_when(hw8_1/100 > 90 ~ NA,
                           TRUE ~ hw8_1/100),
      #weight/height standard deviation (AG 2/28)
      whz.last = case_when(hw11_1/100 > 90 ~ NA,
                           TRUE ~ hw11_1/100 ))

  # KID IS UNDERWEIGHT
  # Because waz is calculated as standard deviations from the mean, then a kid is
  # underweight if waz.last <-2 (under 2 sds under the mean), and overweight if
  # waz.last > 2*sds from the mean
  KR$undwgt <- ifelse(KR$waz < -2, 1, 0)

  # LAST KID IS UNDERWEIGHT
  IR$undwgt.last<- ifelse(IR$waz.last < -2, 1, 0)

  # CHILD IS OVERWEIGHT
  KR$ovrwgt <- ifelse(KR$waz > 2, 1, 0)

  # LAST CHILD IS OVERWEIGHT
  IR$ovrwgt.last <- ifelse(IR$waz.last >2, 1, 0)

  # CATEGORICAL OUTCOME FOR KID STUNTING
  KR <- KR %>% mutate(stunt.cat1 =  factor(case_when(
    haz < -3 ~ "Severely stunted",
    (haz >= -3 & haz < -2) ~ "Moderately stunted",
    haz >= -2 ~ "Not stunted"), c("Severely stunted", "Moderately stunted", "Not stunted"),
  ordered = TRUE))

  # BINARY OUTCOME FOR KID STUNTING
  KR$stunt.cat2 <- ifelse(KR$haz < -2, 1, 0)

  # CATEGORICAL OUTCOME FOR LAST CHILD STUNTING
  IR <- IR %>% mutate(stunt.cat1.last =  factor(case_when(
    haz.last < -3 ~ "Severely stunted",
    (haz.last >= -3 & haz.last < -2) ~ "Moderately stunted",
    haz.last >= -2 ~ "Not stunted"), levels = c("Severely stunted", "Moderately stunted", "Not stunted"),
    ordered = TRUE))

  # BINARY OUTCOME FOR LAST CHILD STUNTING
  IR$stunt.cat2.last <- ifelse(IR$haz.last < -2, 1, 0)

  # CATEGORICAL OUTCOME FOR CHILD WASTING
  KR<- KR %>% mutate(waste.cat1 =  factor(case_when(
    whz < -3 ~ "Severely wasted",
    (whz >= -3 & whz < -2) ~ "Moderately wasted",
    whz >= -2 ~ "Not wasted"), levels = c("Severely wasted", "Moderately wasted", "Not wasted"),
    ordered = TRUE))

  # BINARY OUTCOME FOR CHILD WASTING
  KR$waste.cat2 <- ifelse(KR$whz < -2, 1, 0)

  # Individual Dietary Diversity Score (IDDS)
  #I used the FANTA definition (12-groups) of DDS
  idds = IR1 %>%
    dplyr::select(caseid, starts_with('s653')) %>%
    rowwise() %>%
    mutate(idds =
             sum(s653a =='yes', # cereal grains
             (s653c == 'yes') , # roots
             (s653d  == 'yes' | s653b  == 'yes' | s653g  == 'yes') , # veg
             (s653e  == 'yes' | s653f  == 'yes'),  # fruit
             (s653h  == 'yes' | s653i  == 'yes') , # meat
             s653j  == 'yes',  # egg
             s653k  == 'yes' , # fish
             s653l  == 'yes' , # pulses
             s653n  == 'yes' , # milk
             (s653m  == 'yes' | s653p == 'yes' | s653q  == 'yes') , # fat
             (s653r  == 'yes'  | s653t  == 'yes' ) ,  #sugar
             (s653o  == 'yes' | s653s  == 'yes' | s653u  == 'yes' | s653v  == 'yes')# miscellaneous (without alcohol and tobacco)
             )) %>%
    dplyr::select(caseid, idds) %>%
    mutate(idds.7plus = case_when(idds>6~ 1,
                                  TRUE ~ 0))

  # 10 | MENSTRUAL HEALTH ######################################################
  #
  for(var in c('s239c', 's239d'))
  if(var %in% colnames(IR)){

    IR$mens.nopriv <- ifelse(IR$s239c=="non",1,0)
  } else (message(paste(var, 'is not in the dataset, so this menstrual health variable will not be created.')))

  # 11 | SUMMARIZE #############################################################

  ## INDIVIDUAL -----
  ir.out <- IR %>%
    left_join(idds, by='caseid')

  ## BIRTH -----
  br.out <- BR %>%
    group_by(caseid) %>%
    dplyr::summarize(b.rost = length(caseid),
                     u1mortcnt = sum(u1mort, na.rm=TRUE),
                     u5mortcnt = sum(u5mort, na.rm=TRUE)) %>%
    mutate(u1mort.yn = ifelse(u1mortcnt > 0, 1, 0),
           u5mort.yn = ifelse(u5mortcnt > 0, 1, 0)) %>%
    dplyr::select(caseid, u1mortcnt, u5mortcnt, u1mort.yn, u5mort.yn)

  ## KID ----
  kr.out <- KR %>%
    group_by(caseid) %>%
    dplyr::summarize(k.rost = length(caseid),
                     breastfeed.cnt = sum(breastfeed.n, na.rm=TRUE),
                     ovrwgt.cnt = sum(ovrwgt, na.rm=TRUE),
                     undwgt.cnt = sum(undwgt, na.rm=TRUE),
                     stunt.cat2.cnt = sum(stunt.cat2, na.rm=TRUE),
                     waste.cat2.cnt = sum(waste.cat2, na.rm=TRUE),
                     meas.full.cnt = ifelse(all(is.na(measles.full)), NA, sum(measles.full, na.rm=TRUE)),
                     polio.full.cnt = ifelse(all(is.na(polio.full)), NA, sum(polio.full, na.rm=TRUE)),
                     dpt.full.cnt = ifelse(all(is.na(dpt.full)), NA, sum(dpt.full, na.rm=TRUE)),
                     zerodose.cnt = ifelse(all(is.na(zero.dose)), NA, sum(zero.dose, na.rm=TRUE))) %>%
    mutate(breastfeed.yn = ifelse(breastfeed.cnt > 0, 1, 0),
           ovrwgt.yn = ifelse(ovrwgt.cnt > 0, 1, 0),
           undwgt.yn = ifelse(undwgt.cnt > 0, 1, 0),
           stunt.cat2.yn = ifelse(stunt.cat2.cnt > 0, 1, 0),
           waste.cat2.yn = ifelse(waste.cat2.cnt > 0, 1, 0),
           meas.full.yn = ifelse(meas.full.cnt > 0, 1, 0),
           polio.full.yn = ifelse(polio.full.cnt > 0, 1, 0),
           dpt.full.yn = ifelse(dpt.full.cnt > 0, 1, 0),
           zerodose.yn = ifelse(zerodose.cnt > 0, 1, 0)) %>%
    dplyr::select(caseid, ovrwgt.cnt, undwgt.cnt, stunt.cat2.cnt, waste.cat2.cnt,
                  breastfeed.cnt, meas.full.cnt, dpt.full.cnt, polio.full.cnt,
                  zerodose.cnt, ovrwgt.yn, undwgt.yn, stunt.cat2.yn, waste.cat2.yn,
                  breastfeed.yn, meas.full.yn, dpt.full.yn, polio.full.yn, zerodose.yn)

  # JOIN TOGETHER ##############################################################
  outcomes <- ir.out %>%
    base::merge(kr.out, by=c("caseid"), all.x=TRUE) %>%
    base::merge(br.out, by=c("caseid"), all.x=TRUE) %>%
    base::merge(stl.out, by=c("caseid"), all.x=TRUE)


  if(northern_nigeria == TRUE){

    outcomes <- outcomes %>%
    ## Filter to Northern Nigeria states only ----------------------------------
    dplyr::filter(v023 %>% str_detect('nc ') |
             v023 %>% str_detect('ne ') |
             v023 %>% str_detect('nw ')
    )
  }


  return(outcomes)
}
