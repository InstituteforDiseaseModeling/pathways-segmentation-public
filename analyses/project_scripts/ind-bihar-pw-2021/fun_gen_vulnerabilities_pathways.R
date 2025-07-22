


################################################################################
# GENERATE VULNERABILITY FACTORS | PATHWAYS
################################################################################


###################################
# DEFINE FUNCTION
gen_vulnerability_factors_pathways <- function(survey=NULL){

  pacman::p_load(RODBC, dplyr, tidyverse, openxlsx, tidyselect, zscorer)

  woman <- survey


  # woman <- bihar %>% dplyr::filter(ITYP == 1)

  ### These are original string variables, if researchers need to use text responses
  #source( "C:/Users/yingyili/OneDrive - Bill & Melinda Gates Foundation/Pathways/Modeling/India 2021 Pathways/utility.R")
  #country = "bihar"
  #query_string = paste0("SELECT * FROM idm_general.pathways.pathways_", country, "_ityp_1_clean")
  #pathways <- query_db(query_string, dsn = conn)
  #data <- integers_to_strings(pathways, country=country, dsn=conn)

  #working directory: save all output data into this constructed data folder
  # setwd("C:/Users/yingyili/OneDrive - Bill & Melinda Gates Foundation/Pathways/Modeling/India 2021 Pathways/Data/Bihar_constructed_data")

  # Woman's age (for adjustment)
  woman <- woman %>%
    mutate(her.age = case_when(
      S02 <=19            ~ 1,
      S02 >=20 & S02 <=24 ~ 2,
      S02 >=25 & S02 <=29 ~ 3,
      S02 >=30            ~ 4
    ))
  table(woman$her.age)

  #### 1. Woman and her past experiences ####

  # Husband's age
  woman <- woman %>%
    mutate(his.age = case_when(
      S03a_1 <=24               ~ 1,
      S03a_1 >=25 & S03a_1 <=29 ~ 2,
      S03a_1 >=30 & S03a_1 <=34 ~ 3,
      S03a_1 >=35               ~ 4
    ))
  table(woman$his.age)

  ##age at major life events
  sum(is.na(woman$C13m_1))
  sum(is.na(woman$C13m_2))
  sum(is.na(woman$C13m_3))
  sum(is.na(woman$C13m_4))
  table(woman$C13m_1)
  table(woman$C13m_2)
  table(woman$C13m_3)
  table(woman$C13m_4)
  #median: 18-19
  woman$age.1stcohab.her <- ifelse((woman$C13m_1>18),1,0)
  woman$age.1stsex.her   <- ifelse((woman$C13m_2>18),1,0)
  woman$age.1stpreg.her  <- ifelse((woman$C13m_3>18),1,0)
  woman$age.1stbrth.her  <- ifelse((woman$C13m_4>18),1,0)
  #recode implausible values
  woman$age.1stcohab.her[woman$C13m_1==3] <- NA
  woman$age.1stsex.her[woman$C13m_2==3] <- NA
  woman$age.1stpreg.her[woman$C13m_3==3] <- NA
  woman$age.1stbrth.her[woman$C13m_4==3] <- NA
  table(woman$age.1stcohab.her)
  table(woman$age.1stsex.her)
  table(woman$age.1stpreg.her)
  table(woman$age.1stbrth.her)

  ### PARTNER'S AGE AT KEY MILESTONES
  sum(is.na(woman$C13p_1))
  sum(is.na(woman$C13p_2))
  sum(is.na(woman$C13p_3))
  sum(is.na(woman$C13p_4))
  table(woman$C13p_1)
  table(woman$C13p_2)
  table(woman$C13p_3)
  table(woman$C13p_4)
  #median: 21
  woman$age.1stcohab.his <- ifelse((woman$C13p_1>21),0,1)
  woman$age.1stsex.his   <- ifelse((woman$C13p_2>21),0,1)
  woman$age.1stpreg.his  <- ifelse((woman$C13p_3>21),0,1)
  woman$age.1stbrth.his  <- ifelse((woman$C13p_4>21),0,1)
  #recode implausible values
  woman$age.1stcohab.his[woman$C13p_1==3] <- NA
  woman$age.1stsex.his[woman$C13p_2==3] <- NA
  woman$age.1stpreg.his[woman$C13p_3==3] <- NA
  woman$age.1stbrth.his[woman$C13p_4==3] <- NA
  table(woman$age.1stcohab.his)
  table(woman$age.1stsex.his)
  table(woman$age.1stpreg.his)
  table(woman$age.1stbrth.his)

  #parent death
  table(woman$E02_1) #mother
  table(woman$E02_2) #father
  #don't know -> missing
  woman <- woman %>%
    mutate(death.parent = case_when(
      E02_1 == 2 | E02_2 ==2 ~ 1,
      E02_1 == 1 & E02_2 ==1 ~ 0
    ))
  woman <- woman %>%
    mutate(death.mom = case_when(
      E02_1 == 2  ~ 1,
      E02_1 == 1  ~ 0
    ))
  woman <- woman %>%
    mutate(death.dad = case_when(
      E02_2 ==2 ~ 1,
      E02_2 ==1 ~ 0
    ))
  table(woman$death.parent)
  table(woman$death.mom)
  table(woman$death.dad)
  prop.table(table(woman$death.parent))
  prop.table(table(woman$death.mom))
  prop.table(table(woman$death.dad))
  #one-in-five women have experienced parental death
  woman$death.parent[is.na(woman$death.parent)] <- 0
  woman$death.mom[is.na(woman$death.mom)] <- 0
  woman$death.dad[is.na(woman$death.dad)] <- 0

  #husband wasting
  summary(woman$IO30_1)
  woman$his.wasting <- ifelse((woman$IO30_1<=23),1,0)
  table(woman$his.wasting)

  ### WHO RAISED THE WOMAN RESPONDENT?
  table(woman$E01)
  E01 <- woman %>% dplyr::select(starts_with("E01"))
  apply(E01, 2, table)
  #raised by single mom?
  woman$single.mom <- ifelse((woman$E01_1 ==1 & woman$E01_2==0),1,0)
  table(woman$single.mom)
  #raised by BOTH biological parents?
  woman$bio.parent <- ifelse((woman$E01_1 ==1 & woman$E01_2==1),1,0)
  table(woman$bio.parent)

  ### HER CHILDHOOD HOME ENVIRONMENT
  #feeling safe?
  table(woman$E04)
  woman$childhood.home.safe <- ifelse((woman$E04 ==5),1,0)
  table(woman$childhood.home.safe)

  #anyone feel stressed?
  E05 <- woman %>% dplyr::select(starts_with("E05"))
  apply(E05, 2, table)
  woman$childhood.home.stress <- ifelse((  woman$E05_1_1 ==1 |
                                             woman$E05_2_1 ==1 |
                                             woman$E05_3_1 ==1 |
                                             woman$E05_4_1 ==1 |
                                             woman$E05_5_1 ==1 |
                                             woman$E05_1_2 ==1 |
                                             woman$E05_2_2 ==1 |
                                             woman$E05_3_2 ==1 |
                                             woman$E05_4_2 ==1 |
                                             woman$E05_5_2 ==1),1,0)
  table(woman$childhood.home.stress)

  ### DISTANCE TO NATAL/PARENT HOME
  woman$dist.natal <- woman$E06
  sum(is.na(woman$E06))
  table(woman$E06)
  table(woman$dist.natal)
  prop.table(table(woman$dist.natal))
  woman$dist.natal[woman$E06==98] <- NA
  woman$dist.natal[woman$E06==4 | woman$E06==5] <- 3
  woman$dist.natal[woman$S03_3==1 | woman$S03_5==1] <- 1 #living w. parent
  woman$dist.natal[woman$E02_1==2 & woman$E02_2==2] <- 3 #both parents died
  table(woman$dist.natal)
  sum(is.na(woman$dist.natal))

  # her education
  table(woman$E07a)
  woman <- woman %>%
    mutate(her.edulevel = case_when(E07a=="1" ~ 1,
                                    E07a=="1a" ~ 2,
                                    E07a=="2" ~ 2,
                                    E07a=="3" ~ 3,
                                    E07a=="4" ~ 4))
  table(woman$her.edulevel)

  # his education
  table(woman$E07c)
  woman <- woman %>%
    mutate(his.edulevel = case_when(E07c=="1" ~ 1,
                                    E07c=="1a" ~ 2,
                                    E07c=="2" ~ 2,
                                    E07c=="3" ~ 3,
                                    E07c=="4" ~ 4))
  table(woman$his.edulevel)

  ### CASTE
  table(woman$E38)
  sum(is.na(woman$E38))
  table(woman$E38a)
  sum(is.na(woman$E38a))
  #1=The Scheduled Castes and Scheduled Tribes are officially designated groups and the most disadvantaged SES
  woman <- woman %>%
    mutate(caste = case_when(
      E38 == 1 | E38==2  ~ 1,
      E38 == 3           ~ 2,
      E38 == 4           ~ 3
    ))
  table(woman$caste)
  prop.table(table(woman$caste))
  expss::val_lab(woman$caste) = expss::num_lab("
            1 Scheduled castes/tribes
            2 OBC
            3 General")
  table(woman$caste)

  ### RELIGION: hindu or not
  table(woman$E39)
  woman$hindu <- ifelse((woman$E39==1),1,0)
  table(woman$hindu)

  #internalized gender norms
  ### FERTILITY PREFERENCES & DESIRES
  table(woman$C38)
  table(woman$C39)
  table(woman$C40_1)
  table(woman$C40_2)
  sum(is.na(woman$C40_1))
  sum(is.na(woman$C40_2))
  woman$fert.more <-  ifelse((woman$C38 >1),0,1)
  woman$fert.child <- ifelse((woman$C39 >2),1,0)
  woman$male.pref <- ifelse((woman$C40_1 > woman$C40_2),1,0)
  woman$male.pref[is.na(woman$C40_1 )] <- 0
  table(woman$fert.child)
  table(woman$fert.more)
  table(woman$male.pref)
  sum(is.na(woman$male.pref))

  ###BEATING WIFE JUSTIFIED?
  sum(is.na(woman$A13_1))
  sum(is.na(woman$A13_2))
  sum(is.na(woman$A13_3))
  sum(is.na(woman$A13_4))
  table(woman$A13_1)
  table(woman$A13_2)
  table(woman$A13_3)
  table(woman$A13_4)
  woman$ipv.accept <- ifelse((woman$A13_1==1 |
                                woman$A13_2==1 |
                                woman$A13_3==1 |
                                woman$A13_4==1 ),1,0)
  table(woman$ipv.accept)

  ###GENDER ROLE: WOMEN SHOULD DO X, Y, Z?
  sum(is.na(woman$B01_1))
  sum(is.na(woman$B01_2))
  sum(is.na(woman$B01_3))
  sum(is.na(woman$B01_4))
  sum(is.na(woman$B01_5))
  sum(is.na(woman$B01_6))
  sum(is.na(woman$B01_7))
  sum(is.na(woman$B01_8))
  table(woman$B01_1)
  table(woman$B01_2)
  table(woman$B01_3)
  table(woman$B01_4)
  table(woman$B01_5)
  table(woman$B01_6)
  table(woman$B01_7)
  table(woman$B01_8)
  woman <- woman %>% dplyr::mutate(B01_sum = rowMeans(across(starts_with('B01_')), na.rm = TRUE))
  summary(woman$B01_sum)
  table(woman$B01_sum)
  woman$gender.role <-  ifelse((woman$B01_sum >3),1,0)
  table(woman$gender.role)

  #### 2. Household relationships ####

  ###FAMILY SIZE: TOTAL HH MEMBERS
  S03 <- woman %>% dplyr::select(starts_with("S03_"))
  apply(S03, 2, table) #quick tab to see if any column has no response
  woman <- woman %>% dplyr::mutate(hh.size = rowSums(across(starts_with('S03_')), na.rm = TRUE))
  table(woman$hh.size)
  woman$hh.size <- woman$hh.size + 1 #plus woman herself
  summary(woman$hh.size)
  table(woman$hh.size)

  #re-categorize to <=5, 6, 7, 8, 9, 10+
  woman$hh.size[woman$hh.size<5] <- 5
  woman$hh.size[woman$hh.size>10] <- 10
  table(woman$hh.size)
  #recode to start with 1
  woman$hh.size <- woman$hh.size - 4
  table(woman$hh.size)

  ### COHABITING ADULTS
  table(woman$S03)
  S03a <- woman %>% dplyr::select(starts_with("S03a_"))
  apply(S03a, 2, table) #quick view about what hh memebers' age look like
  #decide whether each column is an adult (anyone who is >=18 years); if yes, count the sum
  adult_count <- function(row) {
    # Count the number of elements >=18
    result <- sum(row >=18, na.rm = T)
    return(result)
  }
  woman$cohab.adult.sum <-  apply(dplyr::select(woman, starts_with("S03a_")), 1, adult_count)
  table(woman$cohab.adult.sum)
  #recategorize: <=2, 3, 4, 5, 6+
  woman$cohab.adult.sum[woman$cohab.adult.sum<2] <- 2
  woman$cohab.adult.sum[woman$cohab.adult.sum>6] <- 6
  table(woman$cohab.adult.sum)
  #recode to start with 1
  woman$cohab.adult.sum <- woman$cohab.adult.sum - 1
  table(woman$cohab.adult.sum)

  ### COHABITING MINORS (<18 years) who are NOT woman's biological children
  #decide whether each column is a child (anyone who is <18 years); if yes, count the sum
  child_count <- function(row) {
    # Count the number of elements <12
    result <- sum(row <18, na.rm = T)
    return(result)
  }
  woman$cohab.minor.sum <-  apply(dplyr::select(woman, starts_with("S03a_")
                                                & !starts_with("S03a_7")
                                                & !starts_with("S03a_8")), 1, child_count)
  woman$cohab.minor <- ifelse((woman$cohab.minor.sum > 0),1,0)
  table(woman$cohab.minor)

  ### HOUSEHOLD HEAD
  table(woman$S07)
  woman$head.female <- ifelse((woman$S07==29 |
                                 woman$S07==3  |
                                 woman$S07==4),1,0)
  table(woman$head.female)
  table(woman$female_head_household) # consistent with the pre-constructed variable

  ### FEMALE INFLUENCER? (any female who can help when child is sick?)
  table(woman$S03_3[!is.na(woman$S08)]) #mother
  table(woman$S03_4[!is.na(woman$S08)]) #mother-in-law
  table(woman$S03_13[!is.na(woman$S08)]) #sister
  table(woman$S03_14[!is.na(woman$S08)]) #sister-in-law
  table(woman$S03_18[!is.na(woman$S08)]) #grandma
  table(woman$S03_22[!is.na(woman$S08)]) #aunt - no 'yes' response
  table(woman$S03_24[!is.na(woman$S08)]) #female cousin - no 'yes' response
  table(woman$S03_28[!is.na(woman$S08)]) #other adult female
  table(woman$S09)
  #create a binary variable
  #0=no female in HH/there is female, but no female influencer
  #1=yes (MOTSLY mother-in-law, n=1303, >95%)
  woman <- woman %>%
    mutate(female.help = case_when(
      S08==2 | is.na(S08)                                                                        ~ 0,
      S09=="99"                                                                                  ~ 0,
      S08== 1                                                                                    ~ 1,
      S09=="14_1" | S09=="14_3" | S09=="18_1" | S09=="22_1" | S09=="28_1" | S09=="3" | S09=="4"  ~ 1
    ))
  table(woman$female.help)

  ###WOMEN DOES NOT WANT A CHILD : can she neogotiate?
  table(woman$B07)
  woman$negotiate.preg <- woman$B07
  table(woman$negotiate.preg)
  prop.table(table(woman$negotiate.preg))

  ###HUSBAND SUPPORT
  #need to recode item 3,4,6,7,10
  woman$A09_1_3  <- 6 - woman$A09_1_3
  woman$A09_1_4  <- 6 - woman$A09_1_4
  woman$A09_1_6  <- 6 - woman$A09_1_6
  woman$A09_1_7  <- 6 - woman$A09_1_7
  woman$A09_1_10 <- 6 - woman$A09_1_10
  woman <- woman %>% dplyr::mutate(A09_1_sum = rowMeans(across(starts_with('A09_1_')), na.rm = TRUE))
  table(woman$A09_1_sum)
  woman$support.his <- ifelse((woman$A09_1_sum==5),1,0)
  table(woman$support.his)

  ### SOCIAL NETWORK SUPPORT: WITHIN & BEYOND THE HOUSEHOLD
  #quick tab to see what these variables look like
  A04 <- woman %>%dplyr::select(starts_with("A04_"))
  apply(A04, 2, table)

  #decide whether each A04 column (EXCEPT husband--no variation) is an adult in the FIRST circle; if yes, count the sum
  first_count <- function(row) {
    # Count the number of elements >=18
    result <- sum(row == 1, na.rm = T)
    return(result)
  }
  woman$A04_sum <-  apply(dplyr::select(woman, starts_with("A04_")
                                        & !starts_with("A04_s")
                                        & !A04_1
                                        & !starts_with("A04_F")
                                        & !starts_with("A04_R")), 1, first_count)
  table(woman$A04_sum)
  sum(is.na(woman$A04_sum))
  woman$support.first.hh <- ifelse((woman$A04_sum > 0),1,0)
  table(woman$support.first.hh)

  ### CHILD HEALTH DECISIONS (which household member can decide?)
  A02a <- woman %>%dplyr::select(starts_with("A02a_"), A02b)
  apply(A02a, 2, table)

  #can woman herself decide child health?
  woman$her.decide.child <- ifelse((woman$A02a_29==1),1,0)
  table(woman$her.decide.child)

  #is he the the one who decides the most?
  table(woman$A02b)
  A02a1 <- subset(A02a, is.na(A02b))
  #missing N=473, when only one A02a is selected
  #see among these 473, who is in charge of child health?
  apply(A02a1, 2, table) #most is A02a_1, husband
  woman <- woman %>%
    mutate(he.decide.child.most = case_when(
      A02a_1  == 1 & is.na(A02b) ~ 1, #only one hh member is selected, husband can decide
      A02a_29 == 1 & is.na(A02b) ~ 0,  #only one hh member is selected, woman herself can decide
      A02a_4  == 1 & is.na(A02b) ~ 0, #only one hh member is selected, mom-in-law can decide
      A02a_6  == 1 & is.na(A02b) ~ 0,  #only one hh member is selected, dad-in-law can decide
      A02b    == "1" ~ 1,
      A02b    == "29" ~ 0,
      A02b    == "4" ~ 0,
      A02b    == "6" ~ 0,
    ))
  table(woman$he.decide.child.most)
  sum(is.na(woman$he.decide.child.most))
  woman$he.decide.child.most[is.na(woman$he.decide.child.most)] <- 0
  sum(is.na(woman$he.decide.child.most))

  ### HH BIG DECISIONS (which household member can decide?)
  A03a <- woman %>% dplyr::select(starts_with("A03a_"), A03b)
  apply(A03a, 2, table)
  #first, can woman herself decide?
  woman$her.decide.hh <- ifelse((woman$A03a_29==1),1,0)
  table(woman$her.decide.hh)

  #is he the the one who decides the most?
  table(woman$A03b) #woman herself is a very small category--need to collapse
  A03a1 <- subset(A03a, is.na(A03b))
  #missing N=473, when only one A02a is selected
  #see among these 473, who is in charge of child health?
  apply(A03a1, 2, table) #most is A02a_1, husband
  #so let's create a 4 categorical variable decide.child_most: husband, woman herself, mom-in-law, dad-in law/other adults
  woman <- woman %>%
    mutate(he.decide.hh.most = case_when(
      A03a_1 == 1 & is.na(A03b) ~ 1, #only one hh member is selected, husband can decide
      A03a_4 == 1 & is.na(A03b) ~ 0, #only one hh member is selected, mom-in-law can decide
      A03a_6 == 1 & is.na(A03b) ~ 0,  #only one hh member is selected, dad-in-law can decide
      A03b   == "1" ~ 1,
      A03b   == "4" ~ 0,
      A03b   == "6" ~ 0,
    ))
  table(woman$he.decide.hh.most)
  woman$he.decide.hh.most[is.na(woman$he.decide.hh.most)] <- 0
  sum(is.na(woman$he.decide.hh.most))

  ##women's agency
  ###LAND OWNERSHIP: own or jointly
  table(woman$E21)
  woman <- woman %>%
    mutate(land.own = case_when(
      E21 == 4 | E21==5 ~ 0,
      E21 == 2          ~ 0,
      E21 == 1| E21 ==3 ~ 1
    ))
  table(woman$land.own)
  prop.table(table(woman$land.own))

  ### BANK
  table(woman$E25)
  sum(is.na(woman$E25))
  woman <- woman %>%
    mutate(bank.own = case_when(
      E25 == 1  ~ 1,
      E25 == 3  ~ 1,
      E25 == 2  ~ 0,
      E25 == 4 | E25==5  ~ 0,
      E25 == 97 ~ 0
    ))
  table(woman$bank.own)

  #mobile bank account: 0=no one in the HH, 1=someone in the HH (own is too small, so collapse with others)
  table(woman$E25a)
  sum(is.na(woman$E25a))
  woman <- woman %>%
    mutate(mobilebank.own = case_when(
      E25a == 1  ~ 1,
      E25a == 4  ~ 0,
      E25a == 5  ~ 0,
      E25a == 97 ~ 0
    ))
  table(woman$mobilebank.own)

  ###Woman owns a cellphone? 1=own, 2=shared, 0=No
  table(woman$IO19)
  sum(is.na(woman$IO19))
  woman <- woman %>%
    mutate(cell.own = case_when(
      IO19 == 1  ~ 1,
      IO19 == 3  ~ 1,
      IO19 == 2  ~ 0,
      IO19 == 4 | IO19==5  ~ 0,
    ))
  table(woman$cell.own)

  #owns smart phone?
  table(woman$IO20)
  sum(is.na(woman$IO20))
  woman$smartcell.own <- ifelse((woman$IO20==1),1,0)
  woman$smartcell.own[is.na(woman$IO20)] <- 0
  table(woman$smartcell.own)
  sum(is.na(woman$smartcell.own))

  ##household stressor/instability
  sum(is.na(woman$A11_1))
  sum(is.na(woman$A11_2))
  sum(is.na(woman$A11_3))
  sum(is.na(woman$A11_4))
  sum(is.na(woman$A11_5))
  table(woman$A11_1)
  table(woman$A11_2)
  table(woman$A11_3)
  table(woman$A11_4)
  table(woman$A11_5)
  woman$house.stress <- ifelse((woman$A11_1==1 |
                                  woman$A11_2==1 |
                                  woman$A11_3==1 |
                                  woman$A11_4==1 |
                                  woman$A11_5==1),1,0)
  table(woman$house.stress)
  prop.table(table(woman$house.stress))
  #see which stress variable is driving the overall stress level
  table(woman$A11_1[woman$house.stress==1])
  table(woman$A11_2[woman$house.stress==1])
  table(woman$A11_3[woman$house.stress==1])
  table(woman$A11_4[woman$house.stress==1])
  table(woman$A11_5[woman$house.stress==1])

  #create an alternative stress variable
  woman$house.stress_ <- ifelse(woman$A11_4==1, 1, 0)
  table(woman$house.stress_ )

  ###HOUSEHOLD CONFLICT (reasons are missing a lot)
  table(woman$A14)
  woman$house.conflict <- ifelse((woman$A14==1),0,1)
  table(woman$house.conflict)

  #### 3. Household economics ####

  #woman working status
  table(woman$E08_29) #self working?
  woman <- woman %>%
    mutate(her.work.year = case_when(E08_29==0 ~ 1,
                                     E09_29==2 | E09_29==3 ~ 2,
                                     E09_29==1 ~ 3))
  table(woman$her.work.year)

  #woman working type
  woman <- woman %>%
    mutate(her.work.agri = case_when(E08_29==0 ~ 1,
                                     E10_1_29==1 | E10_2_29==1 ~ 3))
  woman$her.work.agri[is.na(woman$her.work.agri)] <- 2
  table(woman$her.work.agri)
  sum(is.na(woman$her.work.agri))

  #husband working type
  woman$his.work.agri <- ifelse((woman$E10_1_1==1 | woman$E10_2_1==1),1,0)
  table(woman$his.work.agri)
  sum(is.na(woman$his.work.agri))

  #husband migrant worker?
  woman$his.work.migrant <- ifelse((woman$E15==1),1,0)
  table(woman$his.work.migrant)

  #woman herself working throughout the year?
  table(woman$E09_29)
  sum(is.na(woman$E09_29))
  woman$house.workher <- ifelse((woman$E09_29==1),1,0)
  woman$house.workher[is.na(woman$E09_29)] <- NA
  table(woman$house.workher)

  #any other HH member who has work?
  work_count <- function(row) {
    # Count the number of elements <12
    result <- sum(row ==1, na.rm = T)
    return(result)
  }
  woman$other.work <-  apply(dplyr::select(woman, (is.numeric) & starts_with("E08_") & !E08_29 & !E08_1), 1, work_count)
  table(woman$other.work)

  ### LOAN
  table(woman$E30)
  # don't know category is too small, so collapse with no
  woman$loan <- ifelse((woman$E30==1),1,0)
  table(woman$loan)

  #Wealth index based on assets, following DHS wealth index method
  woman$land <-ifelse(woman$E21==5,0,1)
  sum(is.na(woman$land))

  ### ELECTRICITY everyday?
  woman$elec <- ifelse((woman$E24==1),1,0)
  table(woman$elec)
  woman$elec[is.na(woman$elec)] <- 0
  sum(is.na(woman$elec))

  #anyone in HH owns smart phone? (phone alone, no variation)
  table(woman$IO20)
  woman$smartcell.hh <- ifelse((woman$IO20==1),1,0)
  table(woman$smartcell.hh)
  woman$smartcell.hh[is.na(woman$smartcell.hh)] <- 0
  sum(is.na(woman$smartcell.hh))

  #anyone in HH has a mobile bank account? (bank alone, no variation)
  woman$bank.hh <- ifelse(woman$E25a==5 | woman$E25a==97,0,1)
  table(woman$bank.hh)
  sum(is.na(woman$bank.hh))

  ### TRANSPORTATION
  table(woman$E35_1)
  table(woman$E35_2)
  table(woman$E35_3) #car, very little variation
  table(woman$E35_4) #motor boat, very little variation
  woman$transport.hh <- ifelse((  woman$E35_1==1 |
                                    woman$E35_2==1 |
                                    woman$E35_3==1 |
                                    woman$E35_4==1),1,0)
  table(woman$transport.hh)
  sum(is.na(woman$transport.hh))

  woman$transport.hh_ <- ifelse((  woman$E35_1==1 |
                                     woman$E35_2==1 ),1,0)
  table(woman$transport.hh_)
  sum(is.na(woman$transport.hh_))


  #number of bedrooms: couples and biological children, more than 2 bedrooms
  woman$bedrooms2 <- ifelse(woman$IO16>=2, 1, 0)
  table(woman$bedrooms2)
  sum(is.na(woman$bedrooms2))

  #toilet: (1) flush toilet and (2) not shared
  woman$WASH.toilet <- ifelse((woman$IO21==1 & woman$IO22==2 ),1,0)
  table(woman$WASH.toilet)
  sum(is.na(woman$WASH.toilet))

  #pipe water
  woman$watpipe <- ifelse(woman$IO26==1,1,0)
  table(woman$watpipe )
  sum(is.na(woman$watpipe))

  #materials of roof: 1=finished roof, 0=unfinished
  table(woman$IO5)
  woman$roof <- ifelse((woman$IO5==8 | woman$IO5==9 | woman$IO5==10 |
                          woman$IO5==11 | woman$IO5==12 | woman$IO5==13),1,0)
  table(woman$roof)
  prop.table(table(woman$roof))
  sum(is.na(woman$roof))

  #walls: 1=finished roof, 0=unfinished
  table(woman$IO6)
  woman$wall <- ifelse((woman$IO6==10 | woman$IO6==11 | woman$IO6==12 |
                          woman$IO6==13 | woman$IO6==14 | woman$IO6==15 | woman$IO6==16),1,0)
  table(woman$wall)
  prop.table(table(woman$wall))
  sum(is.na(woman$wall))

  #floor
  table(woman$IO7)
  woman$floor <- ifelse((woman$IO7==6 | woman$IO7==7 | woman$IO7==8 |
                           woman$IO7==9 ),1,0)
  table(woman$floor)
  prop.table(table(woman$floor))
  sum(is.na(woman$floor))

  #NO sitting water
  woman$sitwaterno      <- ifelse(woman$IO8_4==1,1,0)
  sum(is.na(woman$sitwaterno))

  #CLOSE sewer
  woman$sewerclose  <- ifelse(woman$IO9==4,1,0)
  table(woman$sewerclose)
  sum(is.na(woman$sewerclose))

  #kitchen
  table(woman$IO11)
  woman$kitchen.room <- ifelse((woman$IO11==1),0,1)
  table(woman$kitchen.room)
  sum(is.na(woman$kitchen.room))

  #kitchen is in a separate room from sleeping area
  table(woman$IO15)
  woman$kitchen.bed <- ifelse((woman$IO15==4 ),1,0)
  table(woman$kitchen.bed)
  sum(is.na(woman$kitchen.bed))

  #dwelling type
  table(woman$IO3)
  woman$dwell.deattach <- ifelse((woman$IO3==3),1,0)
  table(woman$dwell.deattach)
  prop.table(table(woman$dwell.deattach))
  sum(is.na(woman$dwell.deattach))

  ## HOUSEHOLD POSESSIONS
  woman$house.asset <- rowSums(cbind(woman$land,
                                     woman$elec,
                                     woman$smartcell.hh,
                                     woman$bank.hh,
                                     woman$transport.hh,
                                     woman$bedrooms2,
                                     woman$WASH.toilet,
                                     woman$watpipe,
                                     woman$roof,
                                     woman$wall,
                                     woman$floor,
                                     woman$sitwaterno,
                                     woman$kitchen.room,
                                     woman$kitchen.bed,
                                     woman$dwell.deattach), na.rm = T )

  table(woman$house.asset)
  summary(woman$house.asset)
  #recode: <=6, 7, 8, 9, 10, 11+
  woman$house.asset[woman$house.asset<6] <- 6
  woman$house.asset[woman$house.asset>11] <- 11
  table(woman$house.asset)
  #recode to start with 1
  woman$house.asset <- woman$house.asset - 5
  table(woman$house.asset)

  #output a file for house.asset only
  house.asset <- woman %>% dplyr::select(caseid,
                                  house.asset,
                                  land,
                                  elec,
                                  smartcell.hh,
                                  bank.hh,
                                  transport.hh,
                                  bedrooms2,
                                  WASH.toilet,
                                  watpipe,
                                  roof,
                                  wall,
                                  floor,
                                  sitwaterno,
                                  kitchen.room,
                                  kitchen.bed,
                                  dwell.deattach
  )
  # saveRDS(house.asset, file = "house.asset.rds")



  #### 4. Life in the community ####

  #decide whether each A04 column is an adult in the FIRST circle; if yes, count the sum
  first_count <- function(row) {
    # Count the number of elements >=18
    result <- sum(row == 1, na.rm = T)
    return(result)
  }
  woman$A04_comm_sum <-  apply(dplyr::select(woman, starts_with("A04_F") | starts_with("A04_R")), 1, first_count)
  table(woman$A04_comm_sum)
  sum(is.na(woman$A04_comm_sum))
  woman$support.first.comm <- ifelse((woman$A04_comm_sum > 0),1,0)
  table(woman$support.first.comm)

  ###NON-HUSBAND SUPPORT (CHOOSE A RANDOM "First circle" response)
  #need to recode item 3,4,
  woman$A09_2_3  <- 6 - woman$A09_2_3
  woman$A09_2_4  <- 6 - woman$A09_2_4
  woman <- woman %>% dplyr::mutate(A09_2_sum = rowMeans(across(starts_with('A09_2')), na.rm = TRUE))
  table(woman$A09_2_sum)
  sum(is.na(woman$A09_2_sum)) #244 women did not have OTHER
  woman$support.other <- ifelse((woman$A09_2_sum==5),1,0)
  table(woman$support.other)

  ### COMMUNITY BELONGING
  #need to recode item 3,4,5
  woman$A10_3  <- 6 - woman$A10_3
  woman$A10_4  <- 6 - woman$A10_4
  woman$A10_5  <- 6 - woman$A10_5
  woman <- woman %>% dplyr::mutate(A10_sum = rowMeans(across(starts_with('A10_')), na.rm = TRUE))
  table(woman$A10_sum)
  woman$support.comm <- ifelse((woman$A10_sum==5),1,0)
  table(woman$support.comm)

  ###COMMUNITY HEALTH WORKER in FIRST CYCLE?
  table(woman$A05)
  table(woman$A06)
  sum(is.na(woman$A06)) #correct: missing=29 (who dont have chw living around)
  woman <- woman %>%
    mutate(support.first.chw = case_when(
      A06==1 ~ 1,
      A06==2 ~ 1,
      A06==3 ~ 1,
      A06==4 | A05==2 ~ 0 #no chw, or outside of the trust circle
    ))
  table(woman$support.first.chw)

  ### Anganwadi Worker/AWW in FIRST CYCLE?
  table(woman$A07)
  table(woman$A08)
  sum(is.na(woman$A08)) #correct: missing=52 (who dont have AWW living around)
  woman <- woman %>%
    mutate(support.first.aww = case_when(
      A08==1 ~ 1,
      A08==2 ~ 1,
      A08==3 ~ 1,
      A08==4 | A07==2 ~ 0 #no aww, or outside of the trust circle
    ))
  table(woman$support.first.aww)

  ### WOMEN SELF-HELP GROUP SAVINGS
  table(woman$E27)
  table(woman$E28)
  table(woman$E29)
  woman$saving.group <-  ifelse((woman$E27==1 & woman$E28==1),1,0)
  table(woman$saving.group)

  ### TRUST IN DOCTORS/ASHA
  table(woman$C33)
  table(woman$C34)
  sum(is.na(woman$C33))
  sum(is.na(woman$C34))
  woman$trust.doctor <-  ifelse((woman$C33==3),1,0)
  woman$trust.asha   <-  ifelse((woman$C34==3),1,0)
  woman$trust.asha[is.na(woman$C34)] <- 0
  table(woman$trust.doctor)
  sum(is.na(woman$trust.doctor))
  table(woman$trust.asha)

  #### 5. Healthcare and mental models ####

  ### PREGNANCY PREARENESS
  table(woman$C14)
  woman$prepare.preg <- ifelse((woman$C14==1),1,0)
  table(woman$prepare.preg)

  #Prepareness for sexual debut
  table(woman$C05)
  woman$prepare.sex <- ifelse((woman$C05==2),1,0)
  table(woman$prepare.sex)

  ###RECOGNITION OF CHILD ILLNESS
  table(woman$B06)
  woman$percept.child.sick <- woman$B06
  table(woman$percept.child.sick)

  ### PREFERED SITE OF BIRTH DELIVERY
  #1=home, 2=private hospital, 3=public hospital
  sum(is.na(woman$C22))
  table(woman$C22)
  class(woman$C22)
  woman$deliver.home.prefer <- ifelse((woman$C22==1),1,0)
  table(woman$deliver.home.prefer)

  ###AWARENESS OF GOV INCENTIVES
  sum(is.na(woman$C23))
  table(woman$C23)
  class(woman$C23)
  woman <- woman %>%
    mutate(know.govincent = case_when(!is.na(C23) & C23!=97 ~ 1, C23 ==97 ~ 0 ))
  table(woman$know.govincent)

  ###PERCEPTION ABOUT SRH (e.g., contraception, birth delivery)
  table(woman$B02_1)
  table(woman$B02_2)
  table(woman$B02_3)
  table(woman$B02_4)
  table(woman$B02_5)
  table(woman$B02_6)
  table(woman$B02_7)
  table(woman$B02_8)
  sum(is.na(woman$B02_1))
  sum(is.na(woman$B02_2))
  sum(is.na(woman$B02_3))
  sum(is.na(woman$B02_4))
  sum(is.na(woman$B02_5))
  sum(is.na(woman$B02_6))
  sum(is.na(woman$B02_7))
  sum(is.na(woman$B02_8))
  woman <- woman %>% dplyr::mutate(B02_sum = rowMeans(across(starts_with('B02_')), na.rm = TRUE))
  table(woman$B02_sum)
  woman$percept.srh <-  ifelse((woman$B02_sum >3),1,0)
  table(woman$percept.srh)

  #### 6. Larger environment ####

  ### DISTANCE TO DOCTORS
  table(woman$C30)
  woman$doctor.distance <- woman$C30
  #collapse categories: >4 -> greater than one hour
  woman$doctor.distance[woman$C30>4] <- 4
  table(woman$doctor.distance)
  sum(is.na(woman$doctor.distance))

  ### BARRIERS TO ACCESS HEALTH FACILITIES
  table(woman$C32_1)
  table(woman$C32_2)
  table(woman$C32_3)
  table(woman$C32_4)
  #create a binary variable indicating whether woman has ANY BIG PROBLEM in accessing healthcare
  woman$medical.barrier <- ifelse((woman$C32_1 ==3 |
                                     woman$C32_2 ==3 |
                                     woman$C32_3 ==3 |
                                     woman$C32_4 ==3 ),1,0)
  table(woman$medical.barrier)

  ## WASH INDICATORS
  # For this study, we concentrated on three dependent variables defined for the study population,
  # which assign a value of 1 to households that use basic services (water, sanitation, and hygiene)
  # and 0 to those households that use limited or unimproved services, and had no facility of hand-washing,
  # practiced open defecation and used surface water for drinking.
  # REF: https://www.frontiersin.org/articles/10.3389/fenvs.2022.1013657/full

  #water: is the ware source (1) in house, and (2) purified?
  woman$WASH.water <- ifelse((woman$D01==1 & woman$D03_99==1),1,0)
  table(woman$WASH.water)

  #handwashing: (1) have a facility, and (2) soap & water
  woman$WASH.hand <- ifelse((woman$IO25!=4 & woman$IO26==1 & woman$IO27==1 ),1,0)
  table(woman$WASH.hand)

  ### CLIMATE
  table(woman$D09_1)
  table(woman$D09_2)
  table(woman$D09_3)
  table(woman$D09_4)
  #any YES to the above questions?
  woman$climate.impact <- ifelse((woman$D09_1==1 | woman$D09_2==1 | woman$D09_3==1 | woman$D09_4==1 ),1,0)
  table(woman$climate.impact)
  #impact on money?
  table(woman$D10)
  woman$climate.loss <- ifelse((woman$D10==5),0,1)
  table(woman$climate.loss)

  ### MEDIA EXPOSURE
  table(woman$E34_1) #internet: majority of media exposure
  table(woman$E34_2) #radio: very little variation
  table(woman$E34_3) #watch TV
  table(woman$E34_4) #newspaper: very little variation
  woman$media <- ifelse((woman$E34_1==4 &
                           woman$E34_2==4 &
                           woman$E34_3==4 &
                           woman$E34_4==4),0,1)
  table(woman$media)

  #alternatively, keep internet and TV
  woman$media_ <- ifelse((woman$E34_1==4 &
                            woman$E34_3==4),0,1)
  table(woman$media_)


  ### COOKING FUEL / AIR QUALITY
  table(woman$IO12)
  # categorize to "smoke-producing cooking fuel" (1) versus “non-smoke-producing fuels” (0)
  # REF: https://www.ncbi.nlm.nih.gov/pmc/articles/PMC8374520/
  #[1] Wood
  #[2] Charcoal
  #[3] LPG/Natural gas
  #[4] Straw/shrubs/grass
  #[5] Agricultural crop
  #[6] Animal dung
  #[7] Parafin/Kerosene
  #[8] Electricity
  woman <- woman %>%
    mutate(kitchen.gas = case_when(
      IO12==1 | IO12==2 | IO12==4 | IO12==5 | IO12==6 | IO12==7  ~ 0,
      IO12==3 | IO12==8                                          ~ 1
    ))
  table(woman$kitchen.gas)

  #kitchen ventilation
  table(woman$IO13)
  woman$kitchen.vent <- ifelse((woman$IO13==1 ),1,0)
  table(woman$kitchen.vent)

  #HH smoke
  table(woman$IO28)
  table(woman$IO28_1)
  table(woman$IO28_2)
  table(woman$IO28_3)
  table(woman$IO28_4)
  table(woman$IO28_5)
  table(woman$IO28_99)
  woman$house.smoke <- ifelse((woman$IO28_99==1),0,1)
  table(woman$house.smoke)
  prop.table(table(woman$house.smoke))


  ############### Set survey design matrix
  #SURVEY DESIGN: NEED TO RE-RUN EVERY TIME WHEN THERE IS UPDATE IN THE DATA
  # That is, run the suyvey design codes right before the analysis code
  summary(woman$randomdraw) #range: 0-1, correct
  woman$weight <- 1/woman$randomdraw
  summary(woman$weight)
  woman$psu <- substr(woman$Cal_PSUID,1,nchar(woman$Cal_PSUID)-5)

  # woman$wt <- 1
  # woman$strata <- "rural"

  woman <- woman %>%
    dplyr::mutate(wt = 1) %>%
    dplyr::mutate(strata = "rural") %>%
    dplyr::mutate(svy_id = paste(survey, substr(Cal_PSUID, 1, nchar(Cal_PSUID)-5))) %>%
    dplyr::mutate(svy_strata = district) %>%
    dplyr::mutate(state = "Bihar")

  #select vul predictors
  vul <- woman %>% dplyr::select(
    caseid,
    psu,
    weight,
    her.age,
    #1.Woman & past experiences
    his.age,
    age.1stcohab.her,
    age.1stsex.her,
    age.1stpreg.her,
    age.1stbrth.her,
    death.parent,
    death.mom,
    death.dad,
    single.mom,
    bio.parent,
    dist.natal,
    childhood.home.safe,
    childhood.home.stress,
    her.edulevel,
    his.edulevel,
    caste,
    hindu,
    male.pref,
    ipv.accept,
    gender.role,
    #2. Household relationships
    hh.size,
    cohab.adult.sum,
    cohab.minor,
    head.female,
    female.help,
    support.first.hh,
    negotiate.preg,
    support.his,
    her.decide.child,
    he.decide.child.most,
    her.decide.hh,
    he.decide.hh.most,
    land.own,
    bank.own,
    mobilebank.own,
    smartcell.own,
    house.stress,
    house.conflict,
    #3.Household economics
    her.work.year,
    her.work.agri,
    his.work.agri,
    his.work.migrant,
    other.work,
    loan,
    house.asset,
    #4.Health mental models
    prepare.sex,
    prepare.preg,
    percept.child.sick,
    deliver.home.prefer,
    know.govincent,
    percept.srh,
    #5. Life in the community
    support.first.comm,
    support.comm,
    support.first.chw,
    support.first.aww,
    trust.doctor,
    trust.asha,
    saving.group,
    #6. Larger environment & connectivity
    doctor.distance,
    medical.barrier,
    media,
    smartcell.hh,
    transport.hh,
    climate.impact,
    climate.loss,
    WASH.water,
    WASH.toilet,
    WASH.hand,
    kitchen.gas,
    kitchen.vent,
    house.smoke,
    ##some alternative variables
    media_,
    transport.hh_,
    house.stress_,
    survey,
    wt,
    strata,
    svy_id,
    svy_strata,
    state
  )
  colnames(vul) #76 indicators
  colSums(is.na(vul))

  ## Set survey design matrix
  # vul.des <-svydesign(id=~psu, weights=~weight, data=vul)
  return(vul)

}
