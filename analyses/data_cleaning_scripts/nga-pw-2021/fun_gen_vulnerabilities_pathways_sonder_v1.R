

################################################################################
# GENERATE VULNERABILITY FACTORS | PATHWAYS | SONDER
################################################################################


gen_vulnerability_factors_pathways_sonder <- function(NN=NULL){


  ######################################################################
  ######################################################################
  # 1 | WOMAN AND HER PAST EXPERIENCES

  ### 1.1. PARTNERS AGE

  NN <- NN %>%
    mutate(his.age = case_when(
      !is.na(T_S03a_100) ~ T_S03a_100,
      !is.na(T_S03a_200) ~ T_S03a_200,
      TRUE ~ NA))

  NN <- NN %>%
    mutate(his.age.cat = case_when(
      (his.age <=24) ~ 1,
      (his.age >=25 &his.age <=29) ~ 2,
      (his.age >=30 &his.age <=34) ~ 3,
      (his.age >=35) ~ 4))

  # AGE DIFFERENCE WITH PARTNER

  NN$age.gap <- NN$his.age - NN$S02

  NN <- NN %>%
    mutate(age.gap.cat = case_when(
      age.gap < 0 ~ "partner younger than woman",
      age.gap == 0 ~ "same age",
      age.gap > 0 ~ "partner older than woman",
      TRUE ~ "no partner "
    ))

  NN <- NN %>%
    mutate(age.gap.cat1 = case_when(
      abs(age.gap) < 5 ~ "small age gap",
      abs(age.gap) >= 5 & abs(age.gap) <= 10 ~ "some age gap",
      abs(age.gap) > 10 ~ "big age gap",
      TRUE ~ "no partner or don't know"
    ))

  # AGE DIFFERENCE CREATED BY DESIRE LINE (NOT USED BUT KEEPING)

  NN$age.diff.partner <- as.numeric(NN$age.diff.partner)

  NN <- NN %>%
    mutate(age.diff.cat = case_when(
      age.diff.partner < 0 ~ "partner younger than woman",
      age.diff.partner == 0 ~ "same age",
      age.diff.partner > 0 ~ "partner older than woman",
      TRUE ~ "no partner"
    ))


  NN <- NN %>%
    mutate(age.diff.cat1 = case_when(
      abs(age.diff.partner) < 5 ~ "small age gap",
      abs(age.diff.partner) >= 5 & abs(age.diff.partner) <= 10 ~ "some age gap",
      abs(age.diff.partner) > 10 ~ "big age gap",
      TRUE ~ "no partner"
    ))

  ### 1.2. AGE AT FIRST COHABITATION

  NN <- NN %>%
    mutate(age.1stcohab.cat = case_when(
      I_1_C13m_recode < 19 ~ "< 19 yrs",
      I_1_C13m_recode > 18 ~ "19+ yers",
      TRUE ~ "no applicable"))

  NN <- NN %>%
    mutate(early.1stcohab.19 = case_when(
      I_1_C13m_recode < 19 ~ 1,
      TRUE ~ 0))

  NN <- NN %>%
    mutate(early.1stcohab.16 = case_when(
      I_1_C13m_recode < 16 ~ 1,
      TRUE ~ 0))

  ### 1.3. AGE AT 1st SEX

  NN <- NN %>%
    mutate(early.sex.18 = case_when(
      I_3_C13m_recode < 18 ~ 1,
      TRUE ~ 0))

  NN <- NN %>%
    mutate(early.sex.16 = case_when(
      I_3_C13m_recode < 16 ~ 1,
      TRUE ~ 0))

  ### 1.4. AGE AT 1st PREGNANCY

  NN <- NN %>%
    mutate(early.preg.20 = case_when(
      I_4_C13m_recode < 20 ~ 1,
      TRUE ~ 0))

  NN <- NN %>%
    mutate(early.preg.18 = case_when(
      I_4_C13m_recode < 18 ~ 1,
      TRUE ~ 0))


  ### 1.5. AGE AT 1st BIRTH

  NN <- NN %>%
    mutate(early.birth.20 = case_when(
      I_5_C13m_recode < 20 ~ 1,
      TRUE ~ 0))

  NN <- NN %>%
    mutate(early.brith.18 = case_when(
      I_5_C13m_recode < 18 ~ 1,
      TRUE ~ 0))

  ### 1.6. RELATIONSHIP STATUS

  NN <- NN %>%
    mutate(pship.cat = case_when(
      (S03_O1 == 100 | S03_O2 == 200) & is.na(A01) ~ 1,
      (A01 == 1 | A01 == 2) ~ 2,
      TRUE ~ 0))


  NN <- NN %>%
    mutate(no.partner = case_when(
      (A01 == 3 | A01 == 4 | A01 == 5) ~  1,
      TRUE ~ 0))

  NN <- NN %>%
    mutate(death.partnter = case_when(
      A01==3 ~ 1,
      TRUE ~ 0))

  NN <- NN %>%
    mutate(partner.yn = case_when(
      (S03_O1==100 | S03_O2 ==200 | A01==1 | A01==2) ~ 1,
      TRUE ~ 0))

  ### 1.7. PARENT DEATH

  NN <- NN %>%
    mutate(death.parent = case_when(
      T_E02_1 == 2 | T_E02_2 ==2 ~ 1,
      T_E02_1 == 1 & T_E02_2 ==1 ~ 0))
  NN <- NN %>%
    mutate(death.mom = case_when(
      T_E02_1 == 2  ~ 1,
      T_E02_1 == 1  ~ 0))
  NN <- NN %>%
    mutate(death.dad = case_when(
      T_E02_2 ==2 ~ 1,
      T_E02_2 ==1 ~ 0))
  NN$death.parent[is.na(NN$death.parent)] <- 0
  NN$death.mom[is.na(NN$death.mom)] <- 0
  NN$death.dad[is.na(NN$death.dad)] <- 0

  ### 1.8. RAISED BT SINGLE MOM

  NN <- NN %>%
    mutate(single.mom = case_when(
      (E01_O1 == 1 & is.na(E01_O2)) ~ 1,
      TRUE ~ 0))

  ### 1.9. RAISED BY BOTH BIOLOGICAL PARENTS

  NN <- NN %>%
    mutate(bio.parent = case_when(
      (E01_O1 == 1 & E01_O2 == 2) ~ 1,
      TRUE ~ 0))

  ### 1.10. DISTANCE TO NATAL HOME

  NN$E06 <- ifelse(NN$E06 == 98, 2, NN$E06) #median being 2
  NN$dist.natal <- as.numeric(NN$E06)

  NN$dist.natal[NN$E06==4 | NN$E06==5] <- 4 #distance more than 10 hours
  NN$dist.natal[NN$E06==1 | NN$E06==2 |NN$E06==3] <- 3 #distance < 10 hours
  NN$dist.natal[NN$S03_O1==300 | NN$S03_O1==300 | NN$S03_O2==300 |NN$S03_O2==500] <- 1 #living w. parent
  NN$dist.natal[NN$T_E02_1==2 & NN$T_E02_2==2] <- 2 #both parents died

  ### 1.11. FEEL SAFE AT CHILDHOOD HOME

  NN$childhood.home.safe <- ifelse((NN$E04 ==5),1,0)

  ### 1.12. MULTIPLE STRESSORS - CHILDHOOD

  # Anyone (me or someone else in the HH) feel stressed? #from question E05 (multi-selection)
  E05 <- NN %>%
    dplyr::select(starts_with("T_E05"))

  # Experience hunger in childhood
  NN <- NN %>%
    mutate(childhood.hunger = case_when(
      (T_E05_1_O1 == 3) ~ 0,
      TRUE ~ 1))

  # Experience drop-out in childhood
  NN <- NN %>%
    mutate(childhood.drop = case_when(
      (T_E05_2_O1 == 3) ~ 0,
      TRUE ~ 1))

  # Experience relocate because of money
  NN <- NN %>%
    mutate(childhood.move = case_when(
      (T_E05_3_O1 == 3) ~ 0,
      TRUE ~ 1))

  # Experience having a chronically ill parent
  NN <- NN %>%
    mutate(childhood.illparent = case_when(
      (T_E05_4_O1 == 3) ~ 0,
      TRUE ~ 1))

  # Experience child labor because of money
  NN <- NN %>%
    mutate(childhood.labor = case_when(
      (T_E05_5_O1 == 3) ~ 0,
      TRUE ~ 1))

  # Create childhood stress
  NN <- NN %>%
    mutate(childhood.stress = case_when(
      (childhood.hunger == 1 | childhood.drop == 1 | childhood.move == 1 | childhood.illparent == 1 | childhood.labor == 1) ~ 1,
      TRUE ~ 0))

  NN <- NN %>%
    mutate(childhood.stress.sum = childhood.hunger + childhood.drop + childhood.move + childhood.illparent + childhood.labor)

  NN <- NN %>%
    mutate(childhood.stress.3plus = ifelse(childhood.stress.sum > 2, 1, 0))

  ### 1.13. WOMAN's EDUCATION

  NN$ed.lev <- as.numeric(NN$E07a) #NN$E07a #1= No school, 2=Primary, 3 = Secondary, 4 = 	Higher

  NN$ed.lev.cat <- as.character(NN$ed.lev)

  NN <- NN %>%
    mutate(ed.lev.yn = ifelse(ed.lev == 1, 0, 1))

  NN$E07d <- ifelse(NN$E07d == 97, 12, NN$E07d)

  NN <- NN %>%
    mutate(partner.edu = case_when(
      (E07d == 8 | E07d < 8) ~ 1,
      E07d > 8 ~ 2,
      TRUE ~ 0))

  ### 1.14. PARTNER'S EDUCATION LEVEL

  NN$husb.edu <- ifelse(NN$E07c == 97, 3, NN$E07c) #E07c: What is the highest level of school he attended?

  NN <- NN %>%
    mutate(husb.edu.lev = case_when(
      husb.edu == 1 ~ 1, #no school
      husb.edu == 2 ~ 2, #primary
      husb.edu == 3 ~ 3, #secondary
      husb.edu == 4 ~ 4, #higher
      TRUE ~ 5 #no partner
    ))

  NN <- NN %>%
    mutate(husb.edu.cat = ifelse((is.na(T_S03a_100)&is.na(T_S03a_200))&is.na(A01a), 0,
                                 ifelse(husb.edu == 1, 1, 2)))

  NN <- NN %>%
    mutate(husb.edu.yn = ifelse(husb.edu == 1, 0, 1))

  NN <- NN %>%
    mutate(husb.edu.secondary = ifelse(husb.edu==1 | husb.edu ==2, 0, 1))


  ### 1.15. FERTILITY PREFERENCE (MALE KID PREFERENCE)

  NN$male.pref <- ifelse((NN$T_C40_1 > NN$T_C40_2),1,0)

  ### 1.16. PERCEPTION ON IPV

  NN <- NN %>%
    mutate(dv.out = ifelse(T_A13_1 == 1, 1, 0),
           dv.negkid = ifelse(T_A13_2 == 1, 1, 0),
           dv.argue = ifelse(T_A13_3 == 1, 1, 0),
           dv.nosex = ifelse(T_A13_4 == 1, 1, 0))
  NN <- NN %>%
    mutate(dv.index = dv.out + dv.negkid + dv.argue + dv.nosex)

  NN <- NN %>%
    mutate(dv.index.bi = ifelse(dv.index == 0, 0, 1))

  ### 1.17. PERCEPTION ON GENDER ROLE

  NN <- NN %>%
    mutate(B01_sum = rowMeans(across(starts_with('T_B01_')), na.rm = TRUE))

  NN$gender.role <-  ifelse((NN$B01_sum >3),1,0)

  ### 1.18. RELIGION

  NN <- NN %>% #21=Christian - Catholic, 22=Christian - Protestant, 23=Christian - Other,24=Traditional African religion, 25=Muslim, 98=Other, specify: , 99=None
    mutate(muslim = case_when(
      (E39_O1 == 25 | E39_O2 == 25) ~ 1,
      TRUE ~ 0))

  ### 1.19. MEDIA EXPLOSURE

  NN$freq.net <- as.numeric(NN$T_E34_1)

  NN$freq.radio <- as.numeric(NN$T_E34_2)

  NN$freq.tv <- as.numeric(NN$T_E34_3)

  NN$freq.news <- as.numeric(NN$T_E34_4)

  NN$media <- ifelse((NN$T_E34_1==4 & NN$T_E34_2==4 & NN$T_E34_3==4 & NN$T_E34_4==4),0,1)

  NN$media.net.tv <- ifelse((NN$T_E34_1==4 &
                               NN$T_E34_3==4),0,1)

  ######################################################################
  ######################################################################
  # 2 | HOUSEHOLD RELATIONSHIPS

  ### 2.1. HOUSEHOLD SIZE

  S03 <- NN %>%
    dplyr::select(starts_with("S03_"))

  NN$num_people = rowSums(!is.na(NN[, grep("^S03_O[1-7]$", names(NN))]))

  NN$hh.size <- NN$num_people + 1 #plus woman herself

  NN$hh.size.4plus = ifelse(NN$hh.size >3, 1, 0)

  ### 2.2. NUMBER OF ADULTS IN HOUSEHOLD

  ### COHABITING ADULTS

  S03a <- NN %>%
    dplyr::select(starts_with("T_S03a_"))

  adult_count <- function(row) {
    result <- sum(row >=18, na.rm = T)
    return(result)
  }
  NN$cohab.adult.sum <-  apply(dplyr::select(NN, starts_with("T_S03a_")), 1, adult_count)

  NN <- NN %>%
    mutate(cohab.adult.cat = case_when(
      cohab.adult.sum == 0 ~ 0,
      cohab.adult.sum == 1 ~ 1,
      TRUE ~ 2))

  ### 2.3. NUMBER OF MINORS (WHO ARE NOT WOMANS BIOLICAL CHILDREN)

  child_count <- function(row) {
    result <- sum(row <18, na.rm = T)
    return(result)
  }
  NN$cohab.minor.sum <-  apply(dplyr::select(NN, starts_with("T_S03a_")
                                             & !starts_with("T_S03a_7")
                                             & !starts_with("T_S03a_8")), 1, child_count)

  NN$cohab.minor <- ifelse((NN$cohab.minor.sum > 0),1,0)

  ### 2.4. GENDER AND HEAD OF HOUSEHOLD

  NN <- NN %>%
    mutate(head.sex = case_when(
      S07 %in% c(300, 400, 1300, 1800, 2200, 2400, 2800, 9999) ~ "female",
      TRUE ~ "male"
    ))

  NN <- NN %>%
    mutate(head.female = ifelse(head.sex == "female", 1, 0))


  ### 2.5. DECISION MAKEING DYNAMIC ON CHILDS HEALTH

  NN <- NN %>%
    mutate(kidhlt = case_when(
      (A02a_O1 == 9999 & is.na(A02b)) ~ 1, #only one hh member is selected, woman herself can decide
      (A02a_O1 == 9999 & A02b == 100)|(A02a_O1 == 9999 & A02b == 200) ~ 2, #joined decision made and partner makes most decision
      (A02a_O1 == 9999 & A02b == 9999) ~ 3, #joined decision made and woman makes most decision,
      (A02a_O1 == 9999 & A02b == 99) ~ 4, #woman equally sits in the decision-making panel,
      TRUE ~ 5))

  NN <- NN %>%
    mutate(wd.kidhlt = case_when(
      (kidhlt == 1 | kidhlt ==  3 | kidhlt ==  4) ~ 1,
      TRUE ~ 0))

  NN <- NN %>%
    mutate(jdwd.kidhlt = case_when(
      (kidhlt == 1|kidhlt == 2|kidhlt == 3|kidhlt == 4) ~ 1,
      kidhlt == 5 ~ 0))

  ### 2.6. DECISION DYNAMIC ON OTHER FAMILY TOPICS

  NN <- NN %>%
    mutate(family = case_when(
      (A03a_O1 == 9999 & is.na(A03b)) ~ 1, #only one hh member is selected, woman helself can decide
      (A03a_O1 == 9999 & A03b == 100)|(A03a_O1 == 9999 & A03b == 200) ~ 2, #joined decision made and partner makes most decision
      (A03a_O1 == 9999 & A03b == 9999) ~ 3, #joined decision made and woman makes most decision
      (A03a_O1 == 9999 & A03b == 99) ~ 4, #woman equally sits in the decision-making panel
      TRUE ~ 5))

  NN <- NN %>%
    mutate(wd.family = case_when(
      (family == 1|family == 3|family == 4) ~ 1,
      TRUE ~ 0))

  NN <- NN %>%
    mutate(jdwd.family = case_when(
      (family == 1|family == 2|family == 3|family == 4) ~ 1,
      family == 5 ~ 0))

  ### 2.7. NEGOTIATION POWER IF NOT WANTING A CHILD

  NN <- NN %>%
    mutate(negotiate.preg = case_when(
      (B07==3) ~ 1,
      TRUE ~ 0))

  ### 2.8. POLYGAMY

  NN <- NN %>%
    mutate(polygamy = case_when(
      C10 %in% c(2,3,4,5,6) ~ 1,
      TRUE ~ 2))

  ### 2.9. NUMBER OF CHILDREN

  NN$num.child <- as.numeric(NN$T_C13_1)

  NN <- NN %>%
    mutate(num.child.4plus = case_when(
      num.child %in% c(1:3) ~ 0,
      num.child > 3 ~ 1))

  ### 2.10. HOUSE CONFLICTS

  NN$house.conflict <- ifelse((NN$A14 == 1), 0, 1)

  ######################################################################
  ######################################################################
  # 3 | HOUSEHOLD ECONOMICS AND LIVING CONDITIONS


  ### 3.1. MIGRATION

  NN <- NN %>%
    mutate(alwaysres.yn = case_when(
      E18 == 2 ~ 1,
      E18 == 1 ~ 0))

  ### 3.2. RECENTLY MIGRATED

  NN <- NN %>%
    mutate(hh.move = case_when(
      E18 == 2 ~ 1, #hh never moved
      E19 == 1 ~ 2, #moved within the last 6 months
      E19 == 2 ~ 3, #move 6 to 12 months ago
      E19 == 3 ~ 4, #moved 1 to 2 years ago
      E19 == 4 ~ 5, #moved 2 to 5 years ago
      E19 == 5 ~ 6 #moved more than 5 years ago
    ))

  NN <- NN %>%
    mutate(recent.move.yn = case_when(
      (hh.move == 2|hh.move == 3) ~ 1,
      TRUE ~ 0))

  ### 3.3. TYPE OF EARNING

  NN <- NN %>%
    mutate(woman.earn = case_when(
      E08_O1 == 2900 ~ 1,
      TRUE ~ 0))

  NN <- NN %>%
    mutate(woman.earn.season = case_when(
      T_E09_2900 == 2 | T_E09_2900 == 3 ~ "seasonal",
      T_E09_2900 == 1 ~ "year-around",
      TRUE ~ "not working"
    ))

  ### 3.4. WOMANS WORK

  NN <- NN %>%
    mutate(work.type = case_when(
      (T_E10_2900_O1 == 11|T_E10_2900_O2 == 11|T_E10_2900_O3 == 11|T_E10_2900_O4 == 11|T_E10_2900_O5 == 11) ~ 1, #Professional, technical, managerial
      (T_E10_2900_O1 == 12|T_E10_2900_O2 == 12|T_E10_2900_O3 == 12|T_E10_2900_O4 == 12|T_E10_2900_O5 == 12) ~ 2, #Clerical or sales
      (T_E10_2900_O1 == 13|T_E10_2900_O2 == 13|T_E10_2900_O3 == 13|T_E10_2900_O4 == 13|T_E10_2900_O5 == 13) ~ 3, #Agricultural (farming, livestock, fishing)
      (T_E10_2900_O1 == 14|T_E10_2900_O2 == 14|T_E10_2900_O3 == 14|T_E10_2900_O4 == 14|T_E10_2900_O5 == 14) ~ 4, #Household, domestic and services
      (T_E10_2900_O1 == 15|T_E10_2900_O2 == 15|T_E10_2900_O3 == 15|T_E10_2900_O4 == 15|T_E10_2900_O5 == 15) ~ 5, #Manual labor
      (T_E10_2900_O1 == 98|T_E10_2900_O2 == 98|T_E10_2900_O3 == 98|T_E10_2900_O4 == 98|T_E10_2900_O5 == 98) ~ 6, #other
      TRUE ~ 7))


  NN <- NN %>%
    mutate(work.type.cat = case_when(
      work.type == 1 ~ "professional",
      work.type == 3 ~ "agriculture",
      work.type %in% c(2,4,5,6) ~"other types",
      TRUE ~ "no work"))

  NN <- NN %>%
    mutate(work.type.prof = case_when(
      work.type == 1 ~ "professional",
      work.type %in% c(2:6) ~"non professional",
      TRUE ~ "no work"))

  ### 3.5. PARTNER MIGRANT WORKER

  NN <- NN %>%
    mutate(his.work.migrant = case_when(
      E15 == 1 ~ "yes",
      E15 == 2 ~ "no",
      TRUE ~ "not partnered"))

  ### 3.6. PARTNER WORK TYPE

  NN <- NN %>%
    mutate(his.work.type = case_when(
      (T_E10_100_O1 == 11|T_E10_100_O2 == 11|T_E10_100_O3 == 11|T_E10_100_O4 == 11|T_E10_100_O5 == 11|T_E10_100_O6 == 11|T_E10_200_O1 == 11|T_E10_200_O2 == 11) ~ 1, #Professional, technical, managerial
      (T_E10_100_O1 == 12|T_E10_100_O2 == 12|T_E10_100_O3 == 12|T_E10_100_O4 == 12|T_E10_100_O5 == 12|T_E10_100_O6 == 12|T_E10_200_O1 == 12|T_E10_200_O2 == 12) ~ 2, #Clerical or sales
      (T_E10_100_O1 == 13|T_E10_100_O2 == 13|T_E10_100_O3 == 13|T_E10_100_O4 == 13|T_E10_100_O5 == 13|T_E10_100_O6 == 13|T_E10_200_O1 == 13|T_E10_200_O2 == 13) ~ 3, #Agricultural (farming, livestock, fishing)
      (T_E10_100_O1 == 14|T_E10_100_O2 == 14|T_E10_100_O3 == 14|T_E10_100_O4 == 14|T_E10_100_O5 == 14|T_E10_100_O6 == 14|T_E10_200_O1 == 14|T_E10_200_O2 == 14) ~ 4, #Household, domestic and services
      (T_E10_100_O1 == 15|T_E10_100_O2 == 15|T_E10_100_O3 == 15|T_E10_100_O4 == 15|T_E10_100_O5 == 15|T_E10_100_O6 == 15|T_E10_200_O1 == 15|T_E10_200_O2 == 15) ~ 5, #Manual labor
      (T_E10_100_O1 == 98|T_E10_100_O2 == 98|T_E10_100_O3 == 98|T_E10_100_O4 == 98|T_E10_100_O5 == 98|T_E10_100_O6 == 98|T_E10_200_O1 == 98|T_E10_200_O2 == 98) ~ 6, #other
      TRUE ~ 7))

  NN <- NN %>%
    mutate(his.work.type.cat = case_when(
      his.work.type == 1 ~ "professional",
      his.work.type == 3 ~ "agriculture",
      his.work.type %in% c(2,4,5,6) ~"other types",
      TRUE ~ "no work"))

  NN <- NN %>%
    mutate(his.work.type.prof = case_when(
      his.work.type == 1 ~ "professional",
      his.work.type %in% c(2:6) ~"non professional",
      TRUE ~ "no work"))

  ### 3.7. FINANCIAL ASSISTANCE

  NN <- NN %>%
    mutate(save.grp = case_when(
      E27 == 1 ~ 2,
      E27 == 2 ~ 1))
  NN <- NN %>%
    mutate(hh.loan = case_when(
      E30 == 1 ~ 2,
      TRUE ~ 1))

  ### 3.8. OWN LAND

  NN$hh.land.own <- as.numeric(NN$E21)

  NN <- NN %>%
    mutate(hh.land.own.yn = case_when(
      E21 %in% c(1:4) ~ 0,
      E21 == 5 ~ 1))

  ### 3.9. CURRENT HOUSEHOLD STRESSORS

  NN$hh.hunger <- ifelse(NN$T_A11_1 == 1, 1, 0)

  NN$hh.dropout <- ifelse(NN$T_A11_2 == 1, 1, 0)
  NN$hh.relocate <- ifelse(NN$T_A11_3 == 1, 1, 0)
  NN$hh.illmemb <- ifelse(NN$T_A11_4 == 1, 1, 0)
  NN$hh.childlabor <- ifelse(NN$T_A11_5 == 1, 1, 0)

  NN <- NN %>%
    mutate(house.stress = case_when(
      (T_A11_1==1 | T_A11_2==1 | T_A11_3==1 | T_A11_4==1 | T_A11_5==1) ~ 1,
      TRUE ~ 0))

  ### 3.10. COOKING FUEL

  NN$cook.fuel <- ifelse((NN$IO12 == 98 | NN$IO12 == 99), 9, NN$IO12)

  NN <- NN %>%
    mutate(clean.fuel = case_when(
      cook.fuel %in% c(3,8) ~ 1,
      TRUE ~ 0))

  NN <- NN %>%
    mutate(cook.fuel.1 = case_when(
      cook.fuel ==1 ~ "wood",
      cook.fuel ==2 ~ "charcoal",
      cook.fuel %in% c(3,8) ~ "clean fuel",
      cook.fuel %in% c(4,5,7,9) ~ "others"))

  ### 3.11. WHERE TO COOK

  NN$hh.where.cook <- as.numeric(NN$IO11)

  ### 3.12. TIME REQUIRED TO GET WATER

  NN <- NN %>%
    mutate(hh.wat.time.yn = case_when(
      (D01 == 1 | D01 == 2) ~ 0,
      (D01 == 3 | D01 == 4 | D01 == 5) ~ 1))

  ### 3.13. WATER PURIFICATION METHOD

  NN <- NN %>%
    mutate(water.pur = case_when(
      D03_O1 == 99 ~ 0,
      TRUE ~ 1))

  ### 3.14. Toilet location

  NN$toilet.loc <- as.numeric(NN$IO23)

  NN <- NN %>%
    mutate(toilet.loc.cat = case_when(
      IO23 == 1|IO23 == 2|IO21 == 4 ~ 1, #in or close to the premises
      IO23 == 3 ~ 2, #less than 5 min walk
      TRUE ~ 3 #more than 5 min walk, or No facility/bush/field,
    ))

  ### 3.15. TOILET TYPE

  NN$toilet.type <- as.numeric(NN$IO21)
  NN <- NN %>%
    mutate(toilet.type = case_when(
      toilet.type==1 ~ 1,
      toilet.type==2 ~ 2,
      (toilet.type==3 | toilet.type==4) ~ 3))

  NN <- NN %>%
    mutate(toilet.flush = case_when(
      toilet.type==1 ~ 1,
      TRUE ~ 0))

  ### 3.16. TRANSPORTATION MEANS AND INDEX

  NN <- NN %>%
    mutate(bike = ifelse(T_E35_1 == 1, 1, 0)) %>%
    mutate(motor = ifelse(T_E35_2 == 1, 1, 0)) %>%
    mutate(car = ifelse(T_E35_3 == 1, 1, 0)) %>%
    mutate(boat = ifelse(T_E35_4 == 1, 1, 0))

  NN <- NN %>%
    mutate(transpo.index = bike + motor + car + boat)

  NN <- NN %>%
    mutate(transpo.index.yn = case_when(
      transpo.index == 0 ~ 0,
      TRUE ~ 1))

  ### 3.17. WOMEN OWNING A MOBILE/BANK ACCOUNT

  NN <- NN %>%
    mutate(w.bank = case_when(
      E25 %in% c(1,2,3) ~ 1, #woman has either own or joined bank account
      TRUE ~ 0))

  NN <- NN %>%
    mutate(w.mpesa = case_when(
      E25a %in% c(1,2,3) ~ 1, #women has either own or joined a mobile account
      TRUE ~ 0))

  NN <- NN %>%
    mutate(w.fin.account = w.bank + w.mpesa)

  NN <- NN %>%
    mutate(w.fin.account.yn = case_when(
      w.fin.account == 0 ~ 0,
      TRUE ~ 1))

  ### 3.18. HOUSEHOLD ASSET OWNERSHIP

  NN <- NN %>%
    mutate(cellphone = case_when(
      IO19 %in% c(1:4) ~ 1,
      TRUE ~ 0))

  NN <- NN %>%
    mutate(hh.bank = case_when(
      E25 %in% c(1,2,3,4) ~ 1, #HH has a bank account
      TRUE ~ 0))
  NN <- NN %>%
    mutate(hh.mpesa = case_when(
      E25a %in% c(1,2,3,4) ~ 1, #HH has a mobile account
      TRUE ~ 0))

  NN <- NN %>%
    mutate(hh.fin.account = case_when(
      (hh.bank == 1 | hh.mpesa == 1) ~ 1,
      TRUE ~ 0))

  NN <- NN %>%  #Create the index
    mutate(asset.index = motor + car + cellphone + hh.fin.account)

  NN <- NN %>%
    mutate(asset.index.yn = case_when(
      asset.index==0 ~ 0,
      TRUE ~ 1))

  ### 3.19. MATERIALS OF WALL

  NN$IO6 <- ifelse(NN$IO6 == 98, 3, NN$IO6)
  NN$wall <- as.numeric(NN$IO6)

  NN <- NN %>%
    mutate(wall.cat = case_when(
      wall %in% c(1,2,3,98) ~ "natural",
      wall %in% c(4:9) ~ "rudimentary",
      TRUE ~ "finished"))

  NN <- NN %>%
    mutate(wall.finish = ifelse(wall.cat == "finished", 1, 0))

  ######################################################################
  ######################################################################
  # 4 | HEALTH MENTAL MODELS

  ### 4.1. PERCEIVED BARRIERS TO HEALTHCARE

  NN <- NN %>%
    mutate(permit = ifelse(T_C32_1 == 3, 1, 0)) %>%
    mutate(money = ifelse(T_C32_2 == 3, 1, 0)) %>%
    mutate(distance = ifelse(T_C32_3 == 3, 1, 0)) %>%
    mutate(alone = ifelse(T_C32_4 == 3, 1, 0))

  NN <- NN %>%
    mutate(med.index = permit + money + distance + alone)

  NN <- NN %>%
    mutate(med.index.yn = case_when(
      med.index == 0 ~ 0, #not a problem for all
      med.index > 0 ~ 1))

  NN <- NN %>%
    mutate(med.index.2plus = case_when(
      med.index < 2 ~ 0,
      med.index =2 |med.index > 2  ~ 1))

  ### 4.2. PREPAREDNESS FOR SEX

  NN$prepare.sex <- ifelse((NN$C05 == 2), 1, 0)

  ### 4.3. PREPAREDNESS FOR PREGNANCY

  NN$prepare.preg <- ifelse((NN$C14 == 1), 1, 0)

  ### 4.5. RISK PERCEPTIONS OF CHILD ILLNESS

  NN$percept.child.sick <- as.numeric(NN$B06)

  ### 4.6. PREFERREDN PLACE OF DELIVERY

  NN <- NN %>%
    mutate(deliver.pubhos.prefer = case_when(
      C22 == 3 ~ 1,
      TRUE ~ 0))

  NN <- NN %>%
    mutate(deliver.hos.prefer = case_when(
      (C22 == 3 | C22 == 2) ~ 1,
      TRUE ~ 0))

  NN <- NN %>%
    mutate(deliver.home.prefer = case_when(
      C22 == 1 ~ 1,
      TRUE ~ 0))

  ### 4.7. PERCEPTION OF SRH

  NN <- NN %>%
    mutate(B02_sum = rowMeans(across(starts_with('T_B02_')), na.rm = TRUE))

  NN$percept.srh <-  ifelse((NN$B02_sum >3),0,1)

  ### 4.8. TRUST IN HEALTHCARE PROVIDERS

  NN$trust.doctor <-  ifelse((NN$C33 == 3), 1, 0)

  ######################################################################
  ######################################################################
  # 5 | SOCIAL SUPPORT

  first_count <- function(row) {
    # Count the number of elements >=18
    result <- sum(row == 1, na.rm = T)
    return(result)
  }
  NN$A04_comm_sum <-  apply(dplyr::select(NN, starts_with("A04_F") | starts_with("A04_R")), 1, first_count)
  NN$support.first.comm <- ifelse((NN$A04_comm_sum > 0),1,0)

  ### 5.1. LIFE IN THE COMMUNITY

  NN <- NN %>%
    mutate(comm.help = case_when(
      (T_A10_1 == 1 | T_A10_1 == 2) ~ 0, #lack of community support
      TRUE ~ 1 #with community suport
    ))

  NN <- NN %>%
    mutate(comm.trust = case_when(
      (T_A10_2 == 1 | T_A10_2 == 2) ~ 0, #lack of trust in community
      TRUE ~ 1 #have trust in community
    ))

  NN <- NN %>%
    mutate(comm.belong = case_when(
      (T_A10_3 == 4 | T_A10_3 == 5) ~ 0, #lack of belonging to community
      TRUE ~ 1 #feel belonging to community
    ))

  NN <- NN %>%
    mutate(comm.safe = case_when(
      (T_A10_4 == 4 | T_A10_4 == 5) ~ 0, #do not feel safe in the community
      TRUE ~ 1 #feel safe in the community
    ))

  NN <- NN %>%
    mutate(comm.fair = case_when(
      (T_A10_5 == 4 | T_A10_5 == 5) ~ 0, #being treated unfairly in the community
      TRUE ~ 1 #being treated fairly in the community
    ))

  NN <- NN %>%
    mutate(comm.index = comm.help + comm.trust + comm.belong + comm.safe + comm.fair)

  NN <- NN %>%
    mutate(comm.index.1plus = ifelse(comm.index >= 1, 1, 0))

  ### 5.2. SAVING GROUP

  NN <- NN %>%
    mutate(saving.group = case_when(
      (E27 == 1 | E28 == 1) ~ 1,
      TRUE ~ 0))

  ######################################################################
  ######################################################################
  # 6 | HUMAN AND NATURAL SYSTEMS

  ### 6.1. TIME REQUIRED TO GET TO HEALTH FACILITY

  NN$C30 <- ifelse(NN$C30 == 97, 2, NN$C30)
  NN$travtime.fac <- as.numeric(NN$C30)

  NN <- NN %>%
    mutate(travtime.fac.30less = case_when(
      travtime.fac == 1|travtime.fac == 2 ~ 1, #30 min or less
      TRUE ~ 0 #more than 30 minutes
    ))


  NN <- NN %>%
    mutate(travtime.fac.2hrplus = case_when(
      travtime.fac %in% c(5,6) ~ 1, #2hours or more
      TRUE ~ 0 #less than 2 hours
    ))


  ### 6.2. EXPOSURE TO THE WORLD BEYOND (SMART PHONE ETC)

  NN <- NN %>%
    mutate(smartcell.hh = case_when(
      IO20 == 1 ~ 1,
      TRUE ~ 0))

  ### 6.3.CLIMATE IMPACT ON COMMUNITY

  NN <- NN %>%
    mutate(climate.impact = case_when(
      (T_D09_1==1 | T_D09_2==1 | T_D09_3==1 | T_D09_4==1) ~ 1,
      TRUE ~ 0))

  ### 6.4. CLIMATE LOSS

  NN <- NN %>%
    mutate(climate.loss = case_when(
      D10 == 5 ~ 0,
      TRUE ~ 1))

  ### 6.5. HANDWASHING

  NN$wash.hand <- ifelse((NN$IO25!=4 & NN$IO26==1 & NN$IO27==1 ),1,0)

  ### 6.6. KITCHEN VENTILATION

  NN$kitchen.vent <- ifelse((NN$IO13 == 1), 1, 0)

  ### 6.7. HH MEMBER SMOKING

  NN <- NN %>%
    mutate(hh.smoke = case_when(
      IO28_O1 == 99 ~ 0,
      TRUE ~ 1))


  ######################################################################
  ######################################################################
  # 7 | CLEAN-UP

  # CREATE AGE GROUPS

  NN <- NN %>%
    mutate(her.age = case_when(
      (S02 <=19) ~ 1,
      (S02 >=20 & S02 <=24) ~ 2,
      (S02 >=25 & S02 <=29) ~ 3,
      (S02 >=30)  ~ 4))

  # CALCULATE WT FOR INDIVIDUAL

  NN$wt <- NN$base.wt

  # CREATE SEGMENTATION STRATA

  NN <- NN %>%
    mutate(strata = URBAN_RURA)

  return(NN)

}
