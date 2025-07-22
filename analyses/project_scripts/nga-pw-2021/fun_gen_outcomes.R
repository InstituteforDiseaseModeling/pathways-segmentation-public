

################################################################################
# GENERATE OUTCOME VARIABLES
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
# 0 | DATA PREPROCESSING
# 1 | HEALTH SEEKING BEHAVIOR
# 2 | MATERNAL AND REPRODUCTIVE HEALTH OUTCOMES
# 3 | HER HEALTH
# 4 | CHILD HEALTH
# 4 | CLEAN UP


###################################
# DEFINE FUNCTION

gen_outcome_variables <- function(NN){


  ######################################################################
  ######################################################################
  # 0 | DATA PREPROCESSING

  NN <- NN %>%
    mutate(
      C21_S_T_C21_700 = str_replace_all(C21_S_T_C21_700, regex("\\b.*faith home.*\\b|\\b.*faith house.*\\b|\\b.*mission.*\\b|\\b.*mission house.*\\b|\\b.*mission jouse.*\\b", ignore_case = TRUE), "church"),
      C21_S_T_C21_700 = str_replace_all(C21_S_T_C21_700, regex("\\b.*home hospital.*\\b", ignore_case = TRUE), "doctor home"),
      C21_S_T_C21_701 = str_replace_all(C21_S_T_C21_701, regex("\\b.*faith home.*\\b|\\b.*faith house.*\\b|\\b.*mission.*\\b|\\b.*mission house.*\\b|\\b.*mission jouse.*\\b", ignore_case = TRUE), "church"),
      C21_S_T_C21_702 = str_replace_all(C21_S_T_C21_702, regex("\\b.*faith home.*\\b|\\b.*faith house.*\\b|\\b.*mission.*\\b|\\b.*mission house.*\\b|\\b.*mission jouse.*\\b", ignore_case = TRUE), "church"),
      C21_S_T_C21_800 = str_replace_all(C21_S_T_C21_800, regex("\\b.*faith home.*\\b|\\b.*faith house.*\\b|\\b.*mission.*\\b|\\b.*mission house.*\\b|\\b.*mission jouse.*\\b", ignore_case = TRUE), "church"),
      C21_S_T_C21_801 = str_replace_all(C21_S_T_C21_801, regex("\\b.*faith home.*\\b|\\b.*faith house.*\\b|\\b.*mission.*\\b|\\b.*mission house.*\\b|\\b.*mission jouse.*\\b", ignore_case = TRUE), "church"),
      C21_S_T_C21_801 = str_replace_all(C21_S_T_C21_801, regex("\\b.*doctor house.*\\b", ignore_case = TRUE), "doctor home"),
      C21_S_T_C21_802 = str_replace_all(C21_S_T_C21_802, regex("\\b.*faith home.*\\b|\\b.*faith house.*\\b|\\b.*mission.*\\b|\\b.*mission house.*\\b|\\b.*missionary  house.*\\b", ignore_case = TRUE), "church"),
      C21_S_T_C21_803 = str_replace_all(C21_S_T_C21_803, regex("\\b.*mission.*\\b|\\b.*mission house*\\b", ignore_case = TRUE), "church"))

  convert_to_numeric <- function(text) {
    # Check if the text is NA
    if (is.na(text)) {
      return(NA_integer_)  # Return NA preserved as integer type
    }

    # Normalize the text
    text <- tolower(text)

    # Return numeric codes based on keyword presence
    if (grepl("home|house|midwife|mid wife", text)) {
      return(1)
    } else if (grepl("church|chur\\)|traditional hospital", text)) {
      return(3)
    } else if (grepl("clinic|nurse|auxiliary", text)) {
      return(2)
    } else {
      return(NA_integer_)  # Return NA if none of the conditions are met
    }
  }

  NN <- NN %>%
    mutate(
      T_C21_700 = ifelse(is.na(C21_S_T_C21_700), T_C21_700, sapply(C21_S_T_C21_700, convert_to_numeric)),
      T_C21_701 = ifelse(is.na(C21_S_T_C21_701), T_C21_701, sapply(C21_S_T_C21_701, convert_to_numeric)),
      T_C21_702 = ifelse(is.na(C21_S_T_C21_702), T_C21_702, sapply(C21_S_T_C21_702, convert_to_numeric)),
      T_C21_800 = ifelse(is.na(C21_S_T_C21_800), T_C21_800, sapply(C21_S_T_C21_800, convert_to_numeric)),
      T_C21_801 = ifelse(is.na(C21_S_T_C21_801), T_C21_801, sapply(C21_S_T_C21_801, convert_to_numeric)),
      T_C21_802 = ifelse(is.na(C21_S_T_C21_802), T_C21_802, sapply(C21_S_T_C21_802, convert_to_numeric)),
      T_C21_803 = ifelse(is.na(C21_S_T_C21_803), T_C21_803, sapply(C21_S_T_C21_803, convert_to_numeric)))

  ######################################################################
  ######################################################################
  # 1 | HEALTH SEEKING BEHAVIOR

  #### 1.1. TOTAL NUMBER OF ANC VISITS DURING LAST PREGNANCY

  NN$C17 <- ifelse(NN$C17 == 97, 5, NN$C17)
  NN <- NN %>%
    mutate(anc.total = case_when(
      C17 == 1 ~ "0",
      C17 == 2 ~ "1",
      C17 == 3 ~ "2",
      C17 == 4 ~ "3",
      C17 == 5 ~ "4+",
      TRUE ~ NA))

  NN$anc.4plus <- ifelse(NN$anc.total == "4+", 1, 0)

  #### 1.2. FIRST TIME OF ANC VISIT

  NN$anc.start <- NN$C18
  NN$anc.1sttri <- ifelse(NN$C18 < 4, 1, 0)

  #### 1.3. FP CURRENT MODERN CONTRACEPTIVE USE

  NN <- NN %>%
    mutate(C36_1 = case_when((C36_O1 == 1 | C36_O2 == 1 | C36_O3 == 1) ~ 1, TRUE ~ 0),
           C36_2 = case_when((C36_O1 == 2 | C36_O2 == 2 | C36_O3 == 2) ~ 1, TRUE ~ 0),
           C36_3 = case_when((C36_O1 == 3 | C36_O2 == 3 | C36_O3 == 3) ~ 1, TRUE ~ 0),
           C36_4 = case_when((C36_O1 == 4 | C36_O2 == 4 | C36_O3 == 4) ~ 1, TRUE ~ 0),
           C36_5 = case_when((C36_O1 == 5 | C36_O2 == 5 | C36_O3 == 5) ~ 1, TRUE ~ 0),
           C36_6 = case_when((C36_O1 == 6 | C36_O2 == 6 | C36_O3 == 6) ~ 1, TRUE ~ 0),
           C36_7 = case_when((C36_O1 == 7 | C36_O2 == 7 | C36_O3 == 7) ~ 1, TRUE ~ 0),
           C36_8 = case_when((C36_O1 == 8 | C36_O2 == 8 | C36_O3 == 8) ~ 1, TRUE ~ 0),
           C36_9 = case_when((C36_O1 == 9 | C36_O2 == 9 | C36_O3 == 9) ~ 1, TRUE ~ 0),
           C36_10 = case_when((C36_O1 == 10 | C36_O2 == 10 | C36_O3 == 10) ~ 1, TRUE ~ 0),
           C36_11 = case_when((C36_O1 == 11 | C36_O2 == 11 | C36_O3 == 11) ~ 1, TRUE ~ 0),
           C36_12 = case_when((C36_O1 == 12 | C36_O2 == 12 | C36_O3 == 12) ~ 1, TRUE ~ 0),
           C36_97 = case_when((C36_O1 == 97 | C36_O2 == 97 | C36_O3 == 97) ~ 1, TRUE ~ 0),
           C36_99 = case_when((C36_O1 == 99 | C36_O2 == 99 | C36_O3 == 99) ~ 1, TRUE ~ 0))

  NN <- NN %>%
    mutate(fp.current.modern = ifelse((C36_1 == 1 |C36_2 == 1 |C36_3 == 1 |C36_4 == 1 | C36_5 == 1 | C36_6 == 1 | C36_7 == 1 |C36_9 == 1 | C36_11 == 1 ), 1, 0))

  #### 1.4. FP CURRENT: ANY CONTRACEPTIVE USE

  NN <- NN %>%
    mutate(fp.current.any = case_when((C36_1 == 1 |C36_2 == 1 |C36_3 == 1 |C36_4 == 1 | C36_5 == 1 | C36_6 == 1 | C36_7 == 1 |C36_9 == 1 | C36_11 == 1 | C36_8 == 1 |C36_10 == 1 |C36_12 == 1 ) ~ 1, TRUE ~ 0))

  #### 1.5. FP CURRENT: MODERN CONTRACEPTIVE USE

  NN <- NN %>%
    mutate(C37_1 = case_when((C37_O1 == 1 | C37_O2 == 1 | C37_O3 == 1 | C37_O4 == 1) ~ 1, TRUE ~ 0),
           C37_2 = case_when((C37_O1 == 2 | C37_O2 == 2 | C37_O3 == 2 | C37_O4 == 2) ~ 1, TRUE ~ 0),
           C37_3 = case_when((C37_O1 == 3 | C37_O2 == 3 | C37_O3 == 3 | C37_O4 == 3) ~ 1, TRUE ~ 0),
           C37_4 = case_when((C37_O1 == 4 | C37_O2 == 4 | C37_O3 == 4 | C37_O4 == 4) ~ 1, TRUE ~ 0),
           C37_5 = case_when((C37_O1 == 5 | C37_O2 == 5 | C37_O3 == 5 | C37_O4 == 5) ~ 1, TRUE ~ 0),
           C37_6 = case_when((C37_O1 == 6 | C37_O2 == 6 | C37_O3 == 6 | C37_O4 == 6) ~ 1, TRUE ~ 0),
           C37_7 = case_when((C37_O1 == 7 | C37_O2 == 7 | C37_O3 == 7 | C37_O4 == 7) ~ 1, TRUE ~ 0),
           C37_8 = case_when((C37_O1 == 8 | C37_O2 == 8 | C37_O3 == 8 | C37_O4 == 8) ~ 1, TRUE ~ 0),
           C37_9 = case_when((C37_O1 == 9 | C37_O2 == 9 | C37_O3 == 9 | C37_O4 == 9) ~ 1, TRUE ~ 0),
           C37_10 = case_when((C37_O1 == 10 | C37_O2 == 10 | C37_O3 == 10 | C37_O4 == 10) ~ 1, TRUE ~ 0),
           C37_11 = case_when((C37_O1 == 11 | C37_O2 == 11 | C37_O3 == 11 | C37_O4 == 11) ~ 1, TRUE ~ 0),
           C37_12 = case_when((C37_O1 == 12 | C37_O2 == 12 | C37_O3 == 12 | C37_O4 == 12) ~ 1, TRUE ~ 0),
           C37_99 = case_when((C37_O1 == 99 | C37_O2 == 99 | C37_O3 == 99 | C37_O4 == 99) ~ 1, TRUE ~ 0))

  NN <- NN %>%
    mutate(fp.previous.modern = ifelse((C37_1 == 1 |C37_2 == 1 |C37_3 == 1 |C37_4 == 1 | C37_5 == 1 | C37_6 == 1 | C37_7 == 1 |C37_9 == 1 | C37_11 == 1 ), 1, 0))

  #### 1.6. FP PREVIOUS: ANY CONTRACEPTIVE USE

  NN <- NN %>%
    mutate(fp.previous.any = case_when((C37_1 == 1 |C37_2 == 1 |C37_3 == 1 |C37_4 == 1 | C37_5 == 1 | C37_6 == 1 | C37_7 == 1 |C37_9 == 1 | C37_11 == 1 | C37_8 == 1 |C37_10 == 1 |C37_12 == 1 ) ~ 1, TRUE ~ 0))

  #### 1.7. DELIVERY AT HOME (EVER HAS)

  NN <- NN %>%
    mutate(deliver.home.son = ifelse((NN$T_C21_700==1 | NN$T_C21_701==1 | NN$T_C21_702==1 | NN$T_C21_703==1| NN$T_C21_704==1 | NN$T_C21_705==1 | NN$T_C21_706==1 | NN$T_C21_707==1| NN$T_C21_708==1 | NN$T_C21_709==1), 1, 0))
  table(NN$deliver.home.son)

  NN <- NN %>%
    mutate(deliver.home.dau = ifelse((NN$T_C21_800 ==1 | NN$T_C21_801==1 | NN$T_C21_802==1 | NN$T_C21_803==1 | NN$T_C21_804==1 | NN$T_C21_805==1 | NN$T_C21_806==1 | NN$T_C21_807==1 | NN$T_C21_808==1 | NN$T_C21_809== 1), 1, 0))
  table(NN$deliver.home.dau)

  NN <- NN %>%
    mutate(deliver.home = ifelse((deliver.home.son == 1 | deliver.home.dau == 1) , 1, 0))
  table(NN$deliver.home)

  NN <- NN %>%
    mutate(deliver.home.new = ifelse((NN$T_C21_700==1 | NN$T_C21_701==1 | NN$T_C21_702==1 | NN$T_C21_800 ==1 | NN$T_C21_801==1 | NN$T_C21_802==1 ), 1, 0))

  NN <- NN %>%
    mutate(hb.son1 = ifelse(T_C21_700==1, 1 ,0),
           hb.son2 = ifelse(T_C21_701==1, 1 ,0),
           hb.son3 = ifelse(T_C21_702==1, 1 ,0),
           hb.son4 = ifelse(T_C21_703==1, 1 ,0),
           hb.son5 = ifelse(T_C21_704==1, 1 ,0),
           hb.son6 = ifelse(T_C21_705==1, 1 ,0),
           hb.son7 = ifelse(T_C21_706==1, 1 ,0),
           hb.son8 = ifelse(T_C21_707==1, 1 ,0),
           hb.son9 = ifelse(T_C21_708==1, 1 ,0),
           hb.son10 = ifelse(T_C21_709==1, 1 ,0),
           hb.dau1 = ifelse(T_C21_800==1, 1 ,0),
           hb.dau2 = ifelse(T_C21_801==1, 1 ,0),
           hb.dau3 = ifelse(T_C21_802==1, 1 ,0),
           hb.dau4 = ifelse(T_C21_803==1, 1 ,0),
           hb.dau5 = ifelse(T_C21_804==1, 1 ,0),
           hb.dau6 = ifelse(T_C21_805==1, 1 ,0),
           hb.dau7 = ifelse(T_C21_806==1, 1 ,0),
           hb.dau8 = ifelse(T_C21_807==1, 1 ,0),
           hb.dau9 = ifelse(T_C21_808==1, 1 ,0),
           hb.dau10 = ifelse(T_C21_809==1, 1 ,0))

  NN <- NN %>%
    mutate(
      hb.son = ifelse(
        rowSums(!is.na(select(., starts_with("hb.son")))) == 0,
        NA,  # set to NA if all `hb.sonX` are NA
        rowSums(select(., starts_with("hb.son")), na.rm = TRUE)))

  table(NN$hb.son)

  NN <- NN %>%
    mutate(
      hb.dau = ifelse(
        rowSums(!is.na(select(., starts_with("hb.dau")))) == 0,
        NA,
        rowSums(select(., starts_with("hb.dau")), na.rm = TRUE)))

  NN <- NN %>%
    mutate(hb.son.yn = ifelse(hb.son > 0, 1, 0))

  NN <- NN %>%
    mutate(hb.dau.yn = ifelse(hb.dau > 0, 1, 0))

  NN <- NN %>%
    mutate(hb.sum.yn = ifelse((hb.son.yn==1 | hb.dau.yn==1), 1, 0))

  #### 1.8. DELIVERY AT HOME (# OF HOME BIRTH)

  NN <- NN %>%
    mutate(hb.sum = ifelse(is.na(hb.son) & is.na(hb.dau), NA,
                           coalesce(hb.son, 0) + coalesce(hb.dau, 0)))
  NN <- NN %>%
    mutate(hb.sum.3plus = ifelse(hb.sum > 3, 1, 0))

  ######################################################################
  ######################################################################
  # 2 | MATERNAL AND REPRODUCTIVE HEALTH OUTCOMES

  #### 2.1. NUMBER OF PREGNANCY

  NN$preg.num <- NN$T_C13_1 + NN$T_C13_2 + NN$T_C13_3 + NN$T_C13_4

  #### 2.2. CURRENT PREGNANCY STATUS

  NN$preg.current <- ifelse((NN$C35==3),1,0)

  #### 2.3. FERTILITY: NUM OF LIVE BIRTH

  NN$preg.live <- NN$T_C13_1

  #### 2.4. FERTILITY PREGNANCY LOSS (INCLUDING STILLBIRTH, ABORTION AND MISCARAGE)

  NN$preg.loss.num <- NN$T_C13_2 + NN$T_C13_3 + NN$T_C13_4

  NN$preg.loss <- ifelse((NN$preg.loss.num==0),0,1)

  #### 2.5. LAST PREGRANCY WEATHER IT WAS RISKY

  NN$preg.risk.diff <- ifelse(NN$T_C19_1 == 1, 1, 0)
  NN$preg.risk.deliver <- ifelse(NN$T_C19_2 == 1, 1, 0)
  NN$preg.risk.surgery <- ifelse(NN$T_C19_3 == 1, 1, 0)
  NN$preg.risk.anc <- ifelse(NN$T_C19_4 == 1, 1, 0)

  NN <- NN %>%
    mutate(preg.risk = ifelse(NN$preg.risk.diff==1 |NN$preg.risk.deliver ==1| NN$preg.risk.surgery ==1| NN$preg.risk.anc == 1, 1, 0))

  ######################################################################
  ######################################################################
  # 3 | HER HEALTH

  #### 3.1. WOMAN'S ILLNESS IN THE LAST 3 MONTHS

  NN <- NN %>%
    mutate(her.ill.1 = ifelse(T_C27_1==1, 1, 0),
           her.ill.2 = ifelse(T_C27_2==1, 1, 0),
           her.ill.3 = ifelse(T_C27_3==1, 1, 0),
           her.ill.4 = ifelse(T_C27_4==1, 1, 0),
           her.ill.5 = ifelse(T_C27_5==1, 1, 0),
           her.ill.6 = ifelse(T_C27_6==1, 1, 0),
           her.ill.7 = ifelse(T_C27_7==1, 1, 0),
           her.ill.8 = ifelse(!is.na(T_C27_98), 1, 0))


  NN <- NN %>%
    mutate(her.ill.sum = her.ill.1 + her.ill.2 + her.ill.3 + her.ill.4 + her.ill.5 + her.ill.6 + her.ill.7 + her.ill.8)

  NN <- NN %>%
    mutate(her.ill.4plus = ifelse(her.ill.sum>4, 1, 0))

  NN <- NN %>%
    mutate(her.ill.3mo = ifelse((her.ill.1==1 | her.ill.2==1 | her.ill.3==1 | her.ill.4==1 | her.ill.5==1 | her.ill.6==1 | her.ill.7==1 | !is.na(her.ill.8)), 1, 0))

  #### 3.2. HER WASTING

  daughters <- c("I_19_IO30_l", "I_20_IO30_l", "I_21_IO30_l", "I_22_IO30_l", "I_23_IO30_l", "I_24_IO30_l", "I_25_IO30_l", "I_26_IO30_l", "I_27_IO30_l", "I_28_IO30_l")
  daughters_final <- c("IO30_8_1", "IO30_8_2", "IO30_8_3", "IO30_8_4", "IO30_8_5", "IO30_8_6", "IO30_8_7", "IO30_8_8", "IO30_8_9", "IO30_8_10")

  sons <- c("I_9_IO30_l", "I_10_IO30_l", "I_11_IO30_l", "I_12_IO30_l", "I_13_IO30_l", "I_14_IO30_l", "I_15_IO30_l", "I_16_IO30_l","I_17_IO30_l","I_18_IO30_l")
  sons_final <- c("IO30_7_1", "IO30_7_2", "IO30_7_3", "IO30_7_4", "IO30_7_5", "IO30_7_6", "IO30_7_7", "IO30_7_8", "IO30_7_9", "IO30_7_10")

  self <- c("I_1_IO30_l")
  self_final <- c("IO30_29")

  husband <- c("I_2_IO30_l")
  husband_final <- c("IO30_1")

  partner <- c("I_3_IO30_l")
  partner_final <- c("IO30_2")

  names(daughters) <- daughters_final
  names(sons) <- sons_final
  names(self) <- self_final
  names(husband) <- husband_final
  names(partner) <- partner_final

  NN <- NN %>%
    rename(!!!daughters) %>%
    rename(!!!sons) %>%
    rename(!!!self) %>%
    rename(!!!husband) %>%
    rename(!!!partner)

  daughters <- c("I_19_IO31_l", "I_20_IO31_l", "I_21_IO31_l", "I_22_IO31_l", "I_23_IO31_l", "I_24_IO31_l", "I_25_IO31_l", "I_26_IO31_l", "I_27_IO31_l", "I_28_IO31_l")
  daughters_final <- c("IO31_8_1", "IO31_8_2", "IO31_8_3", "IO31_8_4", "IO31_8_5", "IO31_8_6", "IO31_8_7", "IO31_8_8", "IO31_8_9", "IO31_8_10")

  sons <- c("I_9_IO31_l", "I_10_IO31_l", "I_11_IO31_l", "I_12_IO31_l", "I_13_IO31_l", "I_14_IO31_l", "I_15_IO31_l", "I_16_IO31_l", "I_17_IO31_l", "I_18_IO31_l")
  sons_final <- c("IO31_7_1", "IO31_7_2", "IO31_7_3", "IO31_7_4", "IO31_7_5", "IO31_7_6", "IO31_7_7", "IO31_7_8", "IO31_7_9", "IO31_7_10")

  self <- c("I_1_IO31_l")
  self_final <- c("IO31_29")

  names(daughters) <- daughters_final
  names(sons) <- sons_final
  names(self) <- self_final
  NN <- NN %>%
    rename(!!!daughters) %>%
    rename(!!!sons) %>%
    rename(!!!self)

  NN$her.wasting <- ifelse((NN$IO30_29<=23),1,0)

  #### 3.3. STRESS AND DEPRESSION

  NN$T_B08_1 <- 6- NN$T_B08_1

  NN <- NN %>%
    mutate(depress.mean = rowMeans(across(starts_with('T_B08')), na.rm = TRUE))

  NN$depress <-  ifelse((NN$depress.mean <3),1,0)

  ######################################################################
  ######################################################################
  # 4 | CHILD HEALTH

  #### 4.1. BREASTFEEDING FOR THE LAST CHILD

  NN$T_C20_3[NN$T_C20_3 > 19] <- NA #recode 20, 21, 22, 24 to missing
  NN$breastfed <- ifelse((NN$T_C20_1 >1 | NN$T_C20_2 >1 | NN$T_C20_3 >1 ),1,0)

  NN$breastfed.6mo <- ifelse((NN$T_C20_1 >24 | NN$T_C20_2 >6 | NN$T_C20_3 >1 ),1,0)

  #### 4.2. CHILD WASTING

  wasting_count <- function(row) {
    # Count the number of elements <=11.5
    result <- sum(row <=12.5, na.rm = T)
    return(result)
  }
  NN$kid.wasting <-  apply(dplyr::select(NN,
                                         starts_with("IO30_7") | starts_with("IO30_8")),
                           1, wasting_count)
  table(NN$kid.wasting)
  NN$kid.wasting[NN$kid.wasting==2 |NN$kid.wasting==3] <- 1
  table(NN$kid.wasting)


  #### 4.3. CHILD STUNTING

  IO31 <- NN %>% select(starts_with("IO31"))
  anthro <- NN %>% select(caseid, starts_with("IO31_7"), starts_with("IO31_8"), starts_with("T_S03a_7"), starts_with("T_S03a_8"))
  #Stunting was defined as height-for-age z-scores less than -2 SD (-3 SD for severe stunting).
  # https://cran.r-project.org/web/packages/zscorer/readme/README.html
  #convert age to days
  anthro$age.son1 <- anthro$T_S03a_700 * 365.25
  anthro$age.son2 <- anthro$T_S03a_701 * 365.25
  anthro$age.son3 <- anthro$T_S03a_702 * 365.25
  anthro$age.son4 <- anthro$T_S03a_703 * 365.25
  anthro$age.son5 <- anthro$T_S03a_704 * 365.25
  anthro$age.son6 <- anthro$T_S03a_705 * 365.25
  anthro$age.son7 <- anthro$T_S03a_706 * 365.25
  anthro$age.son8 <- anthro$T_S03a_707 * 365.25
  anthro$age.son9 <- anthro$T_S03a_708 * 365.25
  anthro$age.son10 <- anthro$T_S03a_709 * 365.25
  anthro$age.dau1 <- anthro$T_S03a_800 * 365.25
  anthro$age.dau2 <- anthro$T_S03a_801 * 365.25
  anthro$age.dau3 <- anthro$T_S03a_802 * 365.25
  anthro$age.dau4 <- anthro$T_S03a_803 * 365.25
  anthro$age.dau5 <- anthro$T_S03a_804 * 365.25
  anthro$age.dau6 <- anthro$T_S03a_805 * 365.25
  anthro$age.dau7 <- anthro$T_S03a_806 * 365.25
  anthro$age.dau8 <- anthro$T_S03a_807 * 365.25
  anthro$age.dau9 <- anthro$T_S03a_808 * 365.25
  anthro$age.dau10 <- anthro$T_S03a_809 * 365.25

  # height-for-age z-score: for each child, keep complete cases with height and age info
  son1 <- anthro %>% select(caseid, IO31_7_1, age.son1) %>% filter(!is.na(IO31_7_1) & !is.na(age.son1))
  son2 <- anthro %>% select(caseid, IO31_7_2, age.son2) %>% filter(!is.na(IO31_7_2) & !is.na(age.son2))
  son3 <- anthro %>% select(caseid, IO31_7_3, age.son3) %>% filter(!is.na(IO31_7_3) & !is.na(age.son3))
  son4 <- anthro %>% select(caseid, IO31_7_4, age.son4) %>% filter(!is.na(IO31_7_4) & !is.na(age.son4))
  son5 <- anthro %>% select(caseid, IO31_7_5, age.son5) %>% filter(!is.na(IO31_7_5) & !is.na(age.son5))
  son6 <- anthro %>% select(caseid, IO31_7_6, age.son6) %>% filter(!is.na(IO31_7_6) & !is.na(age.son6))
  son7 <- anthro %>% select(caseid, IO31_7_7, age.son7) %>% filter(!is.na(IO31_7_7) & !is.na(age.son7))
  son8 <- anthro %>% select(caseid, IO31_7_8, age.son8) %>% filter(!is.na(IO31_7_8) & !is.na(age.son8))
  son9 <- anthro %>% select(caseid, IO31_7_9, age.son9) %>% filter(!is.na(IO31_7_9) & !is.na(age.son9))
  son10 <- anthro %>% select(caseid, IO31_7_10, age.son10) %>% filter(!is.na(IO31_7_10) & !is.na(age.son10))
  dau1 <- anthro %>% select(caseid, IO31_8_1, age.dau1) %>% filter(!is.na(IO31_8_1) & !is.na(age.dau1))
  dau2 <- anthro %>% select(caseid, IO31_8_2, age.dau2) %>% filter(!is.na(IO31_8_2) & !is.na(age.dau2))
  dau3 <- anthro %>% select(caseid, IO31_8_3, age.dau3) %>% filter(!is.na(IO31_8_3) & !is.na(age.dau3))
  dau4 <- anthro %>% select(caseid, IO31_8_4, age.dau4) %>% filter(!is.na(IO31_8_4) & !is.na(age.dau4))
  dau5 <- anthro %>% select(caseid, IO31_8_5, age.dau5) %>% filter(!is.na(IO31_8_5) & !is.na(age.dau5))
  dau6 <- anthro %>% select(caseid, IO31_8_6, age.dau6) %>% filter(!is.na(IO31_8_6) & !is.na(age.dau6))
  dau7 <- anthro %>% select(caseid, IO31_8_7, age.dau7) %>% filter(!is.na(IO31_8_7) & !is.na(age.dau7))
  dau8 <- anthro %>% select(caseid, IO31_8_8, age.dau8) %>% filter(!is.na(IO31_8_8) & !is.na(age.dau8))
  dau9 <- anthro %>% select(caseid, IO31_8_9, age.dau9) %>% filter(!is.na(IO31_8_9) & !is.na(age.dau9))
  dau10 <- anthro %>% select(caseid, IO31_8_10, age.dau10) %>% filter(!is.na(IO31_8_10) & !is.na(age.dau10))

  son <- mapply(c, son1, son2, son3, son4, son5, son6, son7, son8, son9, son10)
  son <- as.data.frame(son)
  son$sex <- 1

  dau <- mapply(c, dau1, dau2, dau3, dau4, dau5, dau6, dau7, dau8, dau9, dau10)
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

  lfa <- lfa %>% select(caseid, stunt)
  hfa <- hfa %>% select(caseid, stunt)
  stunt <- mapply(c, lfa, hfa)
  stunt <- as.data.frame(stunt)
  length(unique(stunt$caseid))
  class(stunt$stunt)
  stunt$stunt <- as.numeric(stunt$stunt)

  stunt <- stunt %>%
    group_by(caseid) %>%
    mutate(stunt_sum = sum(stunt))
  stunt <- stunt %>% select(caseid, stunt_sum)
  stunt <- unique( stunt[ , 1:2 ] )
  NN <- left_join(NN, stunt, by="caseid")
  NN$kid.stunt <- ifelse((NN$stunt_sum>0),1,0)
  table(NN$kid.stunt)

  #### 4.4. CHILD MORTALITY

  NN$death.u5 <- ifelse((NN$C13a==1),1,0)

  NN$death.u5.son <- ifelse((NN$T_C13b_1>0),1,0)

  NN$death.u5.dau <- ifelse((NN$T_C13b_2>0),1,0)

  #### 4.5. CHILD ILLNESS IN THE LAST 3 MONTHS

  NN <- NN %>%
    mutate(kid.ill.1 = ifelse(T_C24_1==1, 1, 0),
           kid.ill.2 = ifelse(T_C24_2==1, 1, 0),
           kid.ill.3 = ifelse(T_C24_3==1, 1, 0),
           kid.ill.4 = ifelse(T_C24_4==1, 1, 0),
           kid.ill.5 = ifelse(T_C24_5==1, 1, 0),
           kid.ill.6 = ifelse(T_C24_6==1, 1, 0),
           kid.ill.7 = ifelse(T_C24_7==1, 1, 0),
           kid.ill.8 = ifelse(!is.na(T_C24_98), 1, 0))

  NN <- NN %>%
    mutate(kid.ill.sum = kid.ill.1 + kid.ill.2 + kid.ill.3 + kid.ill.4 + kid.ill.5 + kid.ill.6 + kid.ill.7 + kid.ill.8)

  NN <- NN %>%
    mutate(kid.ill.3plus = ifelse(kid.ill.sum>3, 1, 0))

  NN <- NN %>%
    mutate(kid.ill.3mo = ifelse((kid.ill.1==1 | kid.ill.2==1 | kid.ill.3==1 | kid.ill.4==1 | kid.ill.5==1 | kid.ill.6==1 | kid.ill.7==1 | !is.na(kid.ill.8)), 1, 0))

  #### 4.6. CHILD HOSPITALIZED

  hospital_count <- function(row) {
    result <- sum(row == 1, na.rm = T)
    return(result)
  }

  NN$son.hosp_sum <-  apply(dplyr::select(NN, starts_with("T_C26_7")), 1, hospital_count)
  NN$dau.hosp_sum <-  apply(dplyr::select(NN, starts_with("T_C26_8")), 1, hospital_count)

  NN$kid.hosp <- ifelse((NN$son.hosp_sum >0 | NN$dau.hosp_sum >0),1,0)


  ##############################################################
  ##############################################################
  # 5 | CLEAN UP

  # CALCULATE WT FOR INDIVIDUAL

  NN$wt <- NN$base.wt

  # CREATE SEGMENTATION STRATA

  NN <- NN %>%
    mutate(strata = URBAN_RURA)

  return(NN)
}
