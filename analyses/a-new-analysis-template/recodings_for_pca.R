


################################################################################
# ADDITIONAL VULNERABILITY VARIABLE RECODINGS FOR PCA
################################################################################


# AGE AT FIRST BIRTH CATEGORY 2
IR <- IR %>%
  dplyr::mutate(age.1stbrth.cat2 = case_when(v212 < 18 ~ "<18",
                                             v212 >= 18 & v212 < 25 ~ "18-25",
                                             v212 >= 25 ~ "25+"))


#CATEGORICAL FACTOR FOR AGE AT FIRST MARRIAGE/COHABITATION
IR<- IR %>%
  dplyr::mutate(age.1stcohab.cat = case_when(
    v501 == "never in union" ~ "never in union",
    (v511 > 0 & v511 < 15) ~ "5-14",
    (v511 >= 15 & v511 < 20) ~ "15-19",
    (v511 >= 20) ~ "20+"))


# JOINT DECISION: FAMILY PLANNING
IR <- IR %>%
  dplyr::mutate(desc.fp = case_when(
    v213 == "yes" ~ "currently pregnant",
    (v632 %in% c("joint decision") | v632a %in% c("joint decision")) ~ "respondent or respondent/partner",
    (v632 %in% c("mainly respondent") | v632a %in% c("mainly respondent")) ~ "respondent or respondent/partner",
    (v632 %in% c("mainly husband, partner") | v632a %in% c("mainly husband, partner")) ~ "husband / partner alone",
    (v632 %in% c("other") | v632a %in% c("other")) ~ "not partnered / other",
    (marr.cohab == 0) ~ "not partnered / other"))


# EDUCATION LEVEL
IR <- IR %>%
  dplyr::mutate(ed.level = case_when(v106 == "no education" ~ "no education",
                                     v106 == "primary" ~ "primary",
                                     v106 %in% c("secondary", "higher") ~ "secondary +"))


# FERTILITY PREFERENCE
IR <- IR %>%
  dplyr::mutate(fertility.pref = case_when(v602 %in% c("sterilized (respondent or partner)", "declared infecund", "no more") ~ "sterilized/infecund/no more",
                                           v602 == "undecided" ~ "undecided",
                                           v602 == "have another" ~ "have another",
                                           TRUE ~ v602))


# CATEGORICAL FACTOR FOR JOINT DECISION-MAKING INDEX
IR <- IR %>%
  dplyr::mutate(jd.index.cat = case_when(
    jd.index == "not partnered" ~ "Not partnered",
    jd.index == 0 ~ "None",
    jd.index %in% c(1:3) ~ "1-3",
    jd.index %in% c(4:6) ~ "4-6"
  ))


# CATEGORICAL FACTOR FOR NUMBER OF LIVING CHILDREN
IR <- IR %>% dplyr::mutate(num.child.alive.cat = case_when(
  v218 %in% c(0:2) ~ "0-2",
  v218 %in% c(3:5) ~ "3-5",
  (v218 >=6 & !is.na(v218)) ~ "6+"))


# NUMBER OF KIDS < 5 IN THE HOUSE - RECODE
IR <- IR %>% mutate(num.kids.house.cat = case_when(
  num.kids.house %in% c(0:1) ~ "0-1",
  num.kids.house %in% c(2,3) ~ "2-3",
  num.kids.house >=4 ~ "4 or more"))


# HUSBAND/PARTNER'S EDUCATION LEVEL - CAT 1
IR <- IR %>%
  dplyr::mutate(partner.ed.level.cat1 = case_when(v701 %in% c("primary", "others") ~ "primary",
                                                  v701 %in% c("no education") ~ "no education",
                                                  v701 %in% c("higher", "secondary") ~ "secondary +",
                                                  marr.cohab == 0 ~ "not partnered",
                                                  TRUE ~ v701))


#re-categorizing traditional, other and protestant together given extremely small sample sizes
IR$religionrecode.cat <- IR$religion
IR<- IR %>% mutate(religionrecode.cat =case_when(
  religion=="traditional" ~ "other",
  religion=="catholic" ~ "other",
  religion=="other" ~ "other",
  religion=="protestant" ~ "protestant",
  religion=="orthodox" ~ "orthodox",
  religion=="muslim" ~ "muslim",
  religion=="christian" ~ "other"))


# SLUM SUM
HH <- HH %>%
  rowwise() %>%
  dplyr::mutate(slum.sum = sum(floor, water, latrine, living, na.rm=T)) %>%
  dplyr::mutate(slum.sum = case_when(slum.sum == 0 ~ "0",
                                     slum.sum %in% c(1:2) ~ "1-2",
                                     slum.sum %in% c(3:4) ~ "3-4"))


# CATEGORICAL FACTOR FOR JOINT/WOMEN DECISION-MAKING INDEX
IR <- IR %>%
  dplyr::mutate(jdwd.index.cat = case_when(
    marr.cohab == 0 ~ "Not partnered",
    jdwd.index == 0 ~ "None",
    jdwd.index %in% c(1:3) ~ "1-3",
    jdwd.index %in% c(4:6) ~ "4-6"
  ))
