


################################################################################
# GENERATE OUTCOME VARIABLES | PATHWAYS
################################################################################


###################################
# DEFINE FUNCTION

gen_outcome_variables_pathways <- function(survey=NULL){

  pacman::p_load(RODBC, dplyr, tidyverse, tidyselect, zscorer)

  woman <- survey

  ################
  ### OUTCOMES ###
  ################

  ### HER ILLNESS IN THE PAST 3 MONTHS
  C27 <- woman %>% dplyr::select(starts_with("C27_"))
  apply(C27, 2, table)
  woman$her.ill_3mo <- ifelse((  woman$C27_1 ==1 |
                                   woman$C27_2 ==1 |
                                   woman$C27_3 ==1 |
                                   woman$C27_4 ==1 |
                                   woman$C27_5 ==1 |
                                   woman$C27_6 ==1 |
                                   woman$C27_7 ==1 |
                                   !is.na(woman$C27_98)),1,0)
  table(woman$her.ill_3mo)

  #total number of symptoms
  sick_count <- function(row) {
    # Count the number of elements >=18
    result <- sum(row ==1, na.rm = T)
    return(result)
  }
  woman$her.ill_sum <-  apply(dplyr::select(woman, where(is.numeric) & starts_with("C27_")), 1, sick_count)
  table(woman$her.ill_sum)

  #wasting: MID UPPER ARM CIRCUMFERENCE (MUAC).
  #MUAC is a useful indicator of malnutrition in ill patients (normal MUAC >23 cm in males, >22 cm in females).
  #WHO standards for mid-upper arm circumference (MUAC)-for-age show that in a well nourished population
  # there are very few children aged 6–60 months with a MUAC less than 115 mm. Children with a MUAC
  #less than 115 mm have a highly elevated risk of death compared to those who are above.

  #woman herself wasting
  IO30 <- woman %>%dplyr::select(starts_with("IO30"))
  summary(woman$IO30_29)
  woman$her.wasting <- ifelse((woman$IO30_29<=22),1,0)
  table(woman$her.wasting)

  ### STRESS/DEPRESSION
  #reverse code item 1
  table(woman$B08_1)
  woman$B08_1 <- 6- woman$B08_1
  table(woman$B08_1)
  woman <- woman %>% mutate(depress = rowMeans(across(starts_with('B08')), na.rm = TRUE))
  summary(woman$depress)

  #reproductive history
  #Number of pregnancies
  woman$preg.num <- woman$C13_1 + woman$C13_2 + woman$C13_3 + woman$C13_4
  table(woman$preg.num)
  summary(woman$preg.num)
  sd(woman$preg.num, na.rm=TRUE)

  #Number of biological children alive (for Bihar, we only know cohabiting)
  #decide whether each column is her son/daughter (regardless of age)
  S03a_7 <- woman %>%dplyr::select(starts_with("S03a_7"))
  S03a_8 <- woman %>%dplyr::select(starts_with("S03a_8"))
  woman$son1 <- ifelse(( !is.na(woman$S03a_7_1)),1,0)
  woman$son2 <- ifelse(( !is.na(woman$S03a_7_2)),1,0)
  woman$son3 <- ifelse(( !is.na(woman$S03a_7_3)),1,0)
  woman$son4 <- ifelse(( !is.na(woman$S03a_7_4)),1,0)
  woman$son5 <- ifelse(( !is.na(woman$S03a_7_5)),1,0)
  woman$dau1 <- ifelse(( !is.na(woman$S03a_8_1)),1,0)
  woman$dau2 <- ifelse(( !is.na(woman$S03a_8_2)),1,0)
  woman$dau3 <- ifelse(( !is.na(woman$S03a_8_3)),1,0)
  woman$dau4 <- ifelse(( !is.na(woman$S03a_8_4)),1,0)
  woman$dau5 <- ifelse(( !is.na(woman$S03a_8_5)),1,0)
  ##cohabiting bio children
  woman <- woman %>% mutate(cohab.sonsum = rowSums(across(starts_with('son')), na.rm = TRUE))
  woman <- woman %>% mutate(cohab.dausum = rowSums(across(starts_with('dau')), na.rm = TRUE))
  woman$cohab.kid.sum <- woman$cohab.sonsum + woman$cohab.dausum
  table(woman$cohab.kid.sum)
  sum(is.na(woman$cohab.kid.sum))

  #pregnancy outcomes
  #current preg
  woman$preg.current <- ifelse((woman$C35==3),1,0)
  table(woman$preg.current)

  #live births: fertility
  woman$preg.live <-  woman$C13_1
  table(woman$preg.live)
  summary(woman$preg.live)

  #still births
  woman$preg.still <- woman$C13_2
  table(woman$preg.still)
  summary(woman$preg.still)
  #abortion
  woman$preg.abort <- woman$C13_3
  table(woman$preg.abort)
  summary(woman$preg.abort)
  #miscarriage
  woman$preg.miscar <- woman$C13_4
  table(woman$preg.miscar)
  summary(woman$preg.miscar)
  #among all pregancies, whether woman ever experience loss (including stillbirth, abortion, miscarriage)
  #pregnancy loss
  woman$preg.loss.num <- woman$C13_2 + woman$C13_3 + woman$C13_4
  table(woman$preg.loss.num)
  woman$preg.loss <- ifelse((woman$preg.loss.num==0),0,1)
  table(woman$preg.loss)

  #Counselling received at clinic
  ###LAST PREG HIGH RISK?
  table(woman$C19_1)
  table(woman$C19_2)
  table(woman$C19_3)
  table(woman$C19_4)
  #yes=either 1
  woman$preg.risk <- ifelse((  woman$C19_1==1 |
                                 woman$C19_2==1 |
                                 woman$C19_3==1 |
                                 woman$C19_1==1),1,0)
  table(woman$preg.risk)

  #Number of ANC visitts
  #total number of ANC visits during last preg: this is a categorical var, 4=4 times or more
  table(woman$C17)
  woman$anc.total <- woman$C17
  woman$anc.total[woman$C17==97] <- NA #97=don't know
  table(woman$anc.total)
  #the start of ANC visits: this is a continuous variable, unit=months
  table(woman$C18)
  woman$anc.start <- woman$C18

  #family planning
  #Awareness of family planning? Do we have this in Bihar data?
  #currently avoiding preg?
  table(woman$C35)
  #current use of methods: a binary indicating whether using a modern method
  #NOT modern method: SDM & withdrawal
  table(woman$C36)
  #table(woman$C36_SP)
  woman$fp.current.modern <- ifelse(( woman$C36_1==1 |
                                        woman$C36_2==1 |
                                        woman$C36_3==1 |
                                        woman$C36_4==1 |
                                        woman$C36_5==1 |
                                        woman$C36_6==1|
                                        woman$C36_7==1 |
                                        woman$C36_9==1 |
                                        woman$C36_11==1),1,0)
  table(woman$fp.current.modern)
  table(woman$fp.current.modern)
  woman$fp.current.modern[woman$C35==2] <- 0
  table(woman$fp.current.modern)

  ##any method
  woman$fp.current.any  <- woman$fp.current.modern
  woman$fp.current.any[woman$C36_8==1 | woman$C36_10==1 | woman$C36_12==1] <- 1
  table(woman$fp.current.any)

  #ever use of methods: a binary indicating whether using a modern method
  #NOT modern method: SDM & withdrawal
  table(woman$C37)
  C37 <- woman %>%dplyr::select(starts_with("C37"))
  apply(C37, 2, table)
  woman$fp.ever.modern <- ifelse((  woman$C37_1==1 |
                                      woman$C37_2==1 |
                                      woman$C37_3==1 |
                                      woman$C37_4==1 |
                                      woman$C37_5==1 |
                                      woman$C37_6==1|
                                      woman$C37_7==1 |
                                      woman$C37_9==1 |
                                      woman$C37_11==1),1,0)
  table(woman$fp.ever.modern)
  table(woman$fp.ever.modern)

  ##any method
  woman$fp.ever.any  <- woman$fp.ever.modern
  woman$fp.ever.any[woman$C37_8==1 | woman$C37_10==1 | woman$C37_12==1] <- 1
  table(woman$fp.ever.any)

  #delivery
  ### PLACE OF DELIVERY OF HER CHILDREN
  C21 <- woman %>%dplyr::select(starts_with("C21_7_") | starts_with("C21_8_"))
  apply(C21, 2, table)
  #just generate a simple binary to see if woman has EVER deliver at home?
  woman$deliver.home <- ifelse((woman$C21_7_1==1 |
                                  woman$C21_7_2==1 |
                                  woman$C21_7_3==1 |
                                  woman$C21_7_4==1 |
                                  woman$C21_7_5==1 |
                                  woman$C21_8_1==1 |
                                  woman$C21_8_2==1 |
                                  woman$C21_8_3==1 |
                                  woman$C21_8_4==1 |
                                  woman$C21_8_5==1),1,0)
  table(woman$deliver.home)
  woman$deliver.home[is.na(woman$deliver.home)] <- 0
  table(woman$deliver.home)

  #newborn care
  ### BREASTFEEDING for the last child
  table(woman$C20_1)
  table(woman$C20_2)
  table(woman$C20_3)
  sum(is.na(woman$C20_1))
  sum(is.na(woman$C20_2))
  sum(is.na(woman$C20_3))
  woman$C20_3[woman$C20_3 > 2017] <- NA #recode 2018, 2019, 2020, 2021 to missing
  woman$breastfed <- ifelse((  woman$C20_1 >1 |
                                 woman$C20_2 >1 |
                                 woman$C20_3 >1 ),1,0)
  table(woman$breastfed)
  prop.table(table(woman$breastfed))

  #any child is wasting
  wasting_count <- function(row) {
    # Count the number of elements >=18
    result <- sum(row <=11.5, na.rm = T)
    return(result)
  }
  woman$kid.wasting <-  apply(dplyr::select(woman,
                                            starts_with("IO30_7") | starts_with("IO30_8")),
                              1, wasting_count)
  table(woman$kid.wasting)
  woman$kid.wasting[woman$kid.wasting==2] <- 1
  table(woman$kid.wasting)

  #stunting: height, measured for children under 5
  IO31 <- woman %>% dplyr::select(starts_with("IO31"))
  anthro <- woman %>% dplyr::select(caseid,
                                    starts_with("IO31_7"), starts_with("IO31_8"),
                                    starts_with("S03a_7"), starts_with("S03a_8"))
  #Stunting was defined as height-for-age z-scores less than -2 SD (-3 SD for severe stunting).
  # https://cran.r-project.org/web/packages/zscorer/readme/README.html
  #convert age to days
  anthro$age.son1 <- anthro$S03a_7_1 * 365.25
  anthro$age.son2 <- anthro$S03a_7_2 * 365.25
  anthro$age.son3 <- anthro$S03a_7_3 * 365.25
  anthro$age.son4 <- anthro$S03a_7_4 * 365.25
  anthro$age.son5 <- anthro$S03a_7_5 * 365.25
  anthro$age.dau1 <- anthro$S03a_8_1 * 365.25
  anthro$age.dau2 <- anthro$S03a_8_2 * 365.25
  anthro$age.dau3 <- anthro$S03a_8_3 * 365.25
  anthro$age.dau4 <- anthro$S03a_8_4 * 365.25
  anthro$age.dau5 <- anthro$S03a_8_5 * 365.25

  # height-for-age z-score: for each child, keep complete cases with height and age info
  son1 <- anthro %>% dplyr::select(caseid, IO31_7_1, age.son1) %>% dplyr::filter(!is.na(IO31_7_1) & !is.na(age.son1))
  son2 <- anthro %>% dplyr::select(caseid, IO31_7_2, age.son2) %>% dplyr::filter(!is.na(IO31_7_2) & !is.na(age.son2))
  son3 <- anthro %>% dplyr::select(caseid, IO31_7_3, age.son3) %>% dplyr::filter(!is.na(IO31_7_3) & !is.na(age.son3))
  son4 <- anthro %>% dplyr::select(caseid, IO31_7_4, age.son4) %>% dplyr::filter(!is.na(IO31_7_4) & !is.na(age.son4))
  son5 <- anthro %>% dplyr::select(caseid, IO31_7_5, age.son5) %>% dplyr::filter(!is.na(IO31_7_5) & !is.na(age.son5))
  dau1 <- anthro %>% dplyr::select(caseid, IO31_8_1, age.dau1) %>% dplyr::filter(!is.na(IO31_8_1) & !is.na(age.dau1))
  dau2 <- anthro %>% dplyr::select(caseid, IO31_8_2, age.dau2) %>% dplyr::filter(!is.na(IO31_8_2) & !is.na(age.dau2))
  dau3 <- anthro %>% dplyr::select(caseid, IO31_8_3, age.dau3) %>% dplyr::filter(!is.na(IO31_8_3) & !is.na(age.dau3))
  dau4 <- anthro %>% dplyr::select(caseid, IO31_8_4, age.dau4) %>% dplyr::filter(!is.na(IO31_8_4) & !is.na(age.dau4))
  dau5 <- anthro %>% dplyr::select(caseid, IO31_8_5, age.dau5) %>% dplyr::filter(!is.na(IO31_8_5) & !is.na(age.dau5))
  son <- mapply(c, son1, son2, son3, son4, son5)
  son <- as.data.frame(son)
  son$sex <- 1

  dau1 <- anthro %>% dplyr::select(caseid, IO31_8_1, age.dau1) %>% dplyr::filter(!is.na(IO31_8_1) & !is.na(age.dau1))
  dau2 <- anthro %>% dplyr::select(caseid, IO31_8_2, age.dau2) %>% dplyr::filter(!is.na(IO31_8_2) & !is.na(age.dau2))
  dau3 <- anthro %>% dplyr::select(caseid, IO31_8_3, age.dau3) %>% dplyr::filter(!is.na(IO31_8_3) & !is.na(age.dau3))
  dau4 <- anthro %>% dplyr::select(caseid, IO31_8_4, age.dau4) %>% dplyr::filter(!is.na(IO31_8_4) & !is.na(age.dau4))
  dau5 <- anthro %>% dplyr::select(caseid, IO31_8_5, age.dau5) %>% dplyr::filter(!is.na(IO31_8_5) & !is.na(age.dau5))
  dau1 <- anthro %>% dplyr::select(caseid, IO31_8_1, age.dau1) %>% dplyr::filter(!is.na(IO31_8_1) & !is.na(age.dau1))
  dau2 <- anthro %>% dplyr::select(caseid, IO31_8_2, age.dau2) %>% dplyr::filter(!is.na(IO31_8_2) & !is.na(age.dau2))
  dau3 <- anthro %>% dplyr::select(caseid, IO31_8_3, age.dau3) %>% dplyr::filter(!is.na(IO31_8_3) & !is.na(age.dau3))
  dau4 <- anthro %>% dplyr::select(caseid, IO31_8_4, age.dau4) %>% dplyr::filter(!is.na(IO31_8_4) & !is.na(age.dau4))
  dau5 <- anthro %>% dplyr::select(caseid, IO31_8_5, age.dau5) %>% dplyr::filter(!is.na(IO31_8_5) & !is.na(age.dau5))
  dau <- mapply(c, dau1, dau2, dau3, dau4, dau5)
  dau <- as.data.frame(dau)
  dau$sex <- 2

  kid <- mapply(c, son, dau)
  kid <- as.data.frame(kid)
  colnames(kid) <- c("caseid", "height", "age", "sex")
  summary(kid$height)
  summary(kid$age)
  summary(kid$sex)
  kid$height <- as.numeric(kid$height)
  kid$age    <- as.numeric(kid$age)
  kid$sex    <- as.numeric(kid$sex)
  summary(kid$height)
  summary(kid$age)
  summary(kid$sex)

  kid.young <- kid %>% subset(age < 730.5)
  kid.old   <- kid %>% subset(age >= 730.5)

  lfa <- addWGSR(data = kid.young,
                 sex = "sex",
                 firstPart = "height",
                 secondPart = "age",
                 index = "lfa")
  summary(lfa$lfaz)
  lfa$stunt <- ifelse((lfa$lfaz > -2),0,1)
  lfa$stunt[is.na(lfa$stunt)] <- 0
  table(lfa$stunt)

  hfa <- addWGSR(data = kid.old,
                 sex = "sex",
                 firstPart = "height",
                 secondPart = "age",
                 index = "hfa")
  summary(hfa$hfaz)
  hfa$stunt <- ifelse((hfa$hfaz > -2),0,1)
  hfa$stunt[is.na(hfa$stunt)] <- 0
  table(hfa$stunt)

  lfa <- lfa %>% dplyr::select(caseid, stunt)
  hfa <- hfa %>% dplyr::select(caseid, stunt)
  stunt <- mapply(c, lfa, hfa)
  stunt <- as.data.frame(stunt)
  length(unique(stunt$caseid))
  class(stunt$stunt)
  stunt$stunt <- as.numeric(stunt$stunt)

  stunt <- stunt %>%
    group_by(caseid) %>%
    mutate(stunt_sum = sum(stunt))
  stunt <- stunt %>% dplyr::select(caseid, stunt_sum)
  stunt <- unique( stunt[ , 1:2 ] )
  woman <- left_join(woman, stunt, by="caseid")
  woman$kid.stunt <- ifelse((woman$stunt_sum>0),1,0)
  table(woman$kid.stunt)

  ### EXPERIENCING OWN U5 CHILD DEATH
  table(woman$C13a)
  table(woman$C13b_1)
  table(woman$C13b_2)
  woman$death.u5 <- ifelse((woman$C13a==1),1,0)
  table(woman$death.u5)

  ### RECENT CHILD ILLNESS
  C24 <- woman %>% dplyr::select(starts_with("C24"))
  apply(C24, 2, table)
  woman$kid.ill_3mo <- ifelse((woman$C24_1==1 |
                                 woman$C24_2==1 |
                                 woman$C24_3==1 |
                                 woman$C24_4==1 |
                                 woman$C24_5==1 |
                                 woman$C24_6==1 |
                                 woman$C24_7==1 ),1,0)
  table(woman$kid.ill_3mo)
  #total number of symptoms
  sick_count <- function(row) {
    # Count the number of elements >=18
    result <- sum(row ==1, na.rm = T)
    return(result)
  }
  woman$kid.ill_sum <-  apply(dplyr::select(woman, where(is.numeric) & starts_with("C24")), 1, sick_count)
  table(woman$kid.ill_sum)

  ### ANY bio CHILD (including non-bio ones) HAS BEEN HOSPITALIZED FOR THE PAST YEAR?
  C26 <- woman %>%dplyr::select(starts_with("C26"))
  apply(C26, 2, table)
  apply(C26, 2, class)
  hospital_count <- function(row) {
    # Count the number of elements >=18
    result <- sum(row == 1, na.rm = T)
    return(result)
  }
  #her biological children
  woman$son.hosp_sum <-  apply(dplyr::select(woman, starts_with("C26_7")), 1, hospital_count)
  woman$dau.hosp_sum <-  apply(dplyr::select(woman, starts_with("C26_8")), 1, hospital_count)
  table(woman$son.hosp_sum)
  table(woman$dau.hosp_sum)
  woman$kid.hosp <- ifelse((woman$son.hosp_sum >0 | woman$dau.hosp_sum >0),1,0)
  table(woman$kid.hosp)

  ### PREFERED SITE FOR CHILD ILLNESS
  table(woman$C25)
  C25 <- woman %>%dplyr::select(starts_with("C25"))
  apply(C25, 2, table)
  #create a binary variable indicating whether she would take the child for medical treatment
  woman$kid.ill_doctor <- ifelse((woman$C25_1==1 | woman$C25_2==1),1,0)
  table(woman$kid.ill_doctor)

  ### PREFERED SITE FOR HER OWN ILLNESS
  table(woman$C28)
  C28 <- woman %>%dplyr::select(starts_with("C28"))
  apply(C28, 2, table)
  #create a binary variable indicating whether she would go for for medical treatment
  woman$her.ill_doctor <- ifelse((woman$C28_1==1 | woman$C28_2==1),1,0)
  table(woman$her.ill_doctor)

  outcomes <- woman %>% dplyr::select(
    caseid,
    her.ill_3mo,
    her.ill_sum,
    her.wasting,
    depress,
    preg.num,
    cohab.kid.sum,
    preg.current,
    preg.live,
    preg.loss,
    preg.risk,
    anc.total,
    anc.start,
    fp.current.modern,
    fp.current.any,
    fp.ever.modern,
    fp.ever.any,
    deliver.home,
    breastfed,
    kid.wasting,
    kid.stunt,
    death.u5,
    kid.ill_3mo,
    kid.ill_sum,
    kid.hosp,
    kid.ill_doctor,
    her.ill_doctor,
    survey
  )

}
