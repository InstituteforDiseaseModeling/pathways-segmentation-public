
################################################################################
# GENERATE VULNERABILITY FACTORS
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

# HH <- HH1
# IR <- IR1
# BR <- BR1
# MR <- MR1


###################################
# TABLE OF CONTENTS
# 1 | HH FILE
# 2 | IR FILE
# 3 | BR FILE
# 5 | MR FILE
# 6 | CLEAN UP


###################################
# DEFINE FUNCTION

# IR <- IR1
# BR <- BR1
# HH <- HH1
# MR <- MR1


gen_vulnerability_factors <- function(IR=NULL, BR=NULL, HH=NULL, MR=NULL){


  ######################################################################
  ######################################################################
  # 1 | HH FILE

  # PLACE OF RESIDENCE
  HH <- HH %>% dplyr::mutate(hh.urban = case_when(
    hv025=="rural" ~ "rural",
    hv025== "urban" ~ "urban"))


  # ELECTRICITY
  HH <- HH %>% dplyr::mutate(hh.electricity = case_when(
    hv206 == "yes" ~ 1,
    hv206 == "no" ~ 0))

  # INTERNET
  HH <- HH %>% dplyr::mutate(hh.internet = case_when(
    sh121o == "yes" ~ 1,
    sh121o == "no" ~ 0))

  # HOUSEHOLD BANK ACCOUNT
  HH <- HH %>% dplyr::mutate(hh.bank.acct = case_when(
    hv247 == "yes" ~ 1,
    hv247 == "no" ~ 0))


  ## HOUSEHOLD ASSETS
  # RADIO
  HH <- HH %>% dplyr::mutate(hh.radio = case_when(
    hv207 == "yes" ~ 1,
    hv207 == "no" ~ 0))


  # TELEVISION
  HH <- HH %>% dplyr::mutate(hh.tv = case_when(
    hv208 == "yes" ~ 1,
    hv208 == "no" ~ 0))


  # REFRIGIRATOR
  HH <- HH %>% dplyr::mutate(hh.refrig = case_when(
    hv209 == "yes" ~ 1,
    hv209 == "no" ~ 0))


  # BICYCLE
  HH <- HH %>% dplyr::mutate(hh.bike = case_when(
    hv210 == "yes" ~ 1,
    hv210 == "no" ~ 0))


  # MOTORCYCLE
  HH <- HH %>% dplyr::mutate(hh.motor = case_when(
    hv211 == "yes" ~ 1,
    hv211 == "no" ~ 0))


  # CAR
  HH <- HH %>% dplyr::mutate(hh.car = case_when(
    hv212 == "yes" ~ 1,
    hv212 == "no" ~ 0))


  # MOBILE PHONE
  HH <- HH %>% dplyr::mutate(hh.mobile = case_when(
    hv243a == "yes" | hv221 == "yes" ~ 1,
    hv243a == "no" & hv211 == "no" ~ 0))


  # ANIMAL-DRAWN CART
  HH <- HH %>% dplyr::mutate(hh.cart = case_when(
    hv243c == "yes" ~ 1,
    hv243c == "no" ~ 0))


  # BOAT WITH A MOTOR
  HH <- HH %>% dplyr::mutate(hh.motorboat = case_when(
    hv243d == "yes" ~ 1,
    hv243d == "no" ~ 0))


  # BINARY FACTOR FOR MOTOR TRANSPORT
  HH$hh.motortransport.yn <- ifelse(HH$hh.motor==1 | HH$hh.car==1,1,0)


  # COMPUTER
  HH <- HH %>% dplyr::mutate(hh.computer = case_when(
    hv243e == "yes" ~ 1,
    hv243e == "no" ~ 0))


  # STOVE
  HH <- HH %>% dplyr::mutate(hh.stove = case_when(
    sh121j == "yes" ~ 1,
    sh121j == "no" ~ 0))


  # VIDEO/DVD
  HH <- HH %>% dplyr::mutate(hh.video = case_when(
    sh121l == "yes" ~ 1,
    sh121l == "no" ~ 0))


  # OWN LAND USABLE FOR AGRICULTURE
  HH <- HH %>% dplyr::mutate(hh.land = case_when(
    hv244 == "yes" ~ 1,
    hv244 == "no" ~ 0))


  # HECTARES FOR AGRICULTURAL LAND
  # HH <- HH %>% dplyr::mutate(hh.land.ha = case_when(
  #   hv244 == "no" ~ "0",
  #   hv245 %in% c(1:5) ~ "0.1-0.5 ha",
  #   hv245 %in% c(6:10) ~ "0.6-1.0 ha",
  #   hv245 %in% c(10:950) ~ "More than 1.0 ha",
  #   hv245 %in% c(998, 999) ~ "Unknown"))


  # LIVESTOCK, HERDS, OR ANIMALS
  HH <- HH %>% dplyr::mutate(hh.animal = case_when(
    hv246 == "yes" ~ 1,
    hv246 == "no" ~ 0))


  # ANY COWS, BULLS
  HH <- HH %>% dplyr::mutate(hh.cows = case_when(
    hv246b %in% c(1:98, "95 or more", "unknown") ~ 1, ## Assume they have animals (but don't know how many) if they said 98
    hv246b %in% c(0, "none") ~ 0))


  # ANY HORSES, DONKEYS, MULES
  HH <- HH %>% dplyr::mutate(hh.horses = case_when(
    hv246c %in% c(1:98, "95 or more", "unknown") ~ 1, ## Assume they have animals (but don't know how many) if they said 99
    hv246c %in% c(0, "none") ~ 0))


  # ANY GOATS
  HH <- HH %>% dplyr::mutate(hh.goats = case_when(
    hv246d %in% c(1:98, "95 or more", "unknown") ~ 1, ## Assume they have animals (but don't know how many) if they said 99
    hv246d %in% c(0, "none") ~ 0))


  # ANY SHEEP
  HH <- HH %>% dplyr::mutate(hh.sheep = case_when(
    hv246e %in% c(1:98, "95 or more", "unknown") ~ 1, ## Assume they have animals (but don't know how many) if they said 99
    hv246e %in% c(0, "none") ~ 0))


  # ANY CHICKENS/POULTRY
  HH <- HH %>% dplyr::mutate(hh.chickens = case_when(
    hv246f %in% c(1:98, "95 or more", "unknown") ~ 1, ## Assume they have animals (but don't know how many) if they said 99
    hv246f %in% c(0, "none") ~ 0))


  # HOUSEHOLD COMBINED WEALTH INDEX
  # HH$wealth.index <- factor(HH$hv270)


  # URBAN/RURAL WEALTH INDEX
  HH$wealth.index.ur <- factor(HH$hv270a)


    # OWNERSHIP
  # HOUSE OWNERSHIP STATUS
  HH$hh.house.ownership <- HH$sh101a
  HH$hh.house.ownership <- case_when(
    HH$hh.house.ownership %in% c('owner','co-owner') ~ 'owner/ co-owner',
    HH$hh.house.ownership %in% c('only renting','co-renting','tenant-buyer','sub-renting') ~ 'renting/ co-renting/ tenant-buyer',
    HH$hh.house.ownership %in% c('housed by employer','housed for free by a relative or a friend') ~ 'housed by employer/ family/ friend')


  ## FINANCIAL & INSURANCE
  # A HOUSEHOLD MEMBER IS A MEMBER OF A SAVINGS CLUB
  HH <- HH %>% dplyr::mutate(hh.savingsclub = case_when(
    sh123a == "yes" ~ 1,
    sh123a %in% c("no", "dk") ~ 0))


  # MEMBER OF HOUSEHOLD SEND FUNDS TO SENEGAL OR INTERNATIONAL
  HH <- HH %>% dplyr::mutate(hh.send.funds = case_when(
    sh123b == "senegal" ~ "Send to Senegal",
    sh123b == "international" ~ "Send International",
    sh123b == "no" | sh123b == "dont know" ~ "Do not send/ Don't know"))

  HH <- HH %>% dplyr::mutate(hh.send.funds.cat = case_when(
    sh123b %in% c("senegal", "international") ~ "Send funds",
    sh123b == "no" | sh123b == "dont know" ~ "Do not send/ Don't know"))


  # MEMBER OF HOUSEHOLD RECEIVE FUND TO SENEGAL OR INTERNATIONAL
  HH <- HH %>% dplyr::mutate(hh.recv.funds = case_when(
    sh123c == "senegal" ~ "Received from Senegal",
    sh123c == "international" ~ "Received from International",
    sh123c == "no" | sh123c == "dont know" ~ "Did not receive/ Don't know"))

  HH <- HH %>% dplyr::mutate(hh.recv.funds.cat = case_when(
    sh123c %in% c("senegal","international") ~ "Received funds",
    sh123c == "no" | sh123c == "dont know" ~ "Did not receive/ Don't know"))


  # HOUSEHOLD MEMBER RECEIVED SUPPORT FROM bourse de securité familiale
  HH <- HH %>% dplyr::mutate(hh.bourse = case_when(
    sh152a == "yes" ~ "Received support",
    sh152a == "no"  | sh152a == "don't know" ~ "No support",
    sh151a == "no" ~ "No support"))


  # TIMES HOUSEHOLD MEMBER RECEIVED SUPPORT FROM bourse de securité familiale
  # HH$hh.recv.support.bourse <- as.numeric(HH$sh153c)
  HH <- HH %>%
    dplyr::mutate(hh.recv.support.bourse = case_when(sh153c %in% c(998, "don't know") ~ NA,
                                                     TRUE ~ as.numeric(sh153c)))


  HH <- HH %>%
    dplyr::mutate(hh.bourse.cnt = case_when(hh.recv.support.bourse < 200 ~ hh.recv.support.bourse-100,
                                            hh.recv.support.bourse >= 200 & hh.recv.support.bourse < 300 ~ hh.recv.support.bourse-200+12,
                                            is.na(sh152a) | sh152a == "no" ~ 0))


  # BOURSE CNT YES/NO
  HH$hh.bourse.moh.yn <- ifelse(HH$hh.bourse.cnt > 0, 1, 0)


  # HOUSEHOLD MEMBER RECEIVED SUPPORT FROM autre transfert de l'Etat
  HH <- HH %>% dplyr::mutate(hh.autre = case_when(
    sh152c == "yes" ~ "Received support",
    sh152c == "no"  | sh152c == "don't know" ~ "No support",
    is.na(sh151c) | sh151c == "no" ~ "No support"))


  ## HEALTH INSURANCE PLAN FOR HOUSEHOLD MEMBER: mutuelle de sant com
  HH_ins <- HH %>%
    dplyr::select(hv001, hv002, starts_with("sh20fay_")) %>%
    reshape2::melt(id.vars=c("hv001", "hv002")) %>%
    dplyr::filter(!is.na(value)) %>%
    dplyr::mutate(no.insurance = ifelse(value=="no", 1, 0)) %>%
    group_by(hv001, hv002) %>%
    dplyr::summarize(no.insurance.cnt = sum(no.insurance),
                     hh.cnt = n())
  HH_ins$pct.no.ins <- 100*(HH_ins$no.insurance.cnt/HH_ins$hh.cnt)
  HH_ins$no.insurance.yn<-ifelse(HH_ins$no.insurance.cnt > 0, 1, 0)

  HH <- HH %>%
    base::merge(HH_ins, by=c("hv001", "hv002"), all.x=TRUE) %>%
    dplyr::mutate(hh.pct.no.insurance = pct.no.ins,
           hh.no.insurance.yn = no.insurance.yn) %>%
    dplyr::select(-c(pct.no.ins, no.insurance.yn))


  ## HOUSEHOLD MEMBERS
  # NUMBER OF ADULTS (15+) IN THE HOUSEHOLD
  # NUMBER OF CHILDREN (<15) IN THE HOUSEHOLD
  # First impute unknown ages to the mean -- NOT imputing mean
  HH_mem <- HH %>%
    dplyr::select(hv001, hv002, starts_with("hv105_")) %>%
    reshape2::melt(id.vars=c("hv001", "hv002")) %>%
    dplyr::mutate(age = case_when(value %in% c(98, "don't know") ~ NA,
                                  value == "95+" ~ 95,
                                  is.na(value) ~ NA,
                                  TRUE ~ as.numeric(value)),
                  age_15up = ifelse(age >= 15, 1, 0),
                  age_under15 = ifelse(age < 15, 1, 0)) %>%
    group_by(hv001, hv002) %>%
    dplyr::summarize(num.15up = sum(age_15up, na.rm = TRUE),
                     num.under15 = sum(age_under15, na.rm = TRUE),
                     highestyearsedinHH.yrs = max(age, na.rm = TRUE))

  HH <- HH %>%
    base::merge(HH_mem, by=c("hv001", "hv002"), all.x=TRUE)


  # CATEGORICAL FACTOR FOR HOUSEHOLD MEMBERS 15+
  HH <- HH %>% dplyr::mutate(num.15up.cat = case_when(
    num.15up %in% c(0, 1) ~ "0-1",
    num.15up %in% c(2) ~ "2",
    num.15up %in% c(3:4) ~ "3-4",
    num.15up >=5 ~ "5 and up"))
  HH$num.15up.cat <- factor(HH$num.15up.cat,levels=c("5 and up","0-1","2","3-4"))


  # CATEGORICAL FACTOR FOR HOUSEHOLD MEMBERS UNDER 15
  HH <- HH %>% dplyr::mutate(num.under15.cat = case_when(
    num.under15 %in% c(0) ~ "0",
    num.under15 %in% c(1:2) ~ "1-2",
    num.under15 %in% c(3:4) ~ "3-4",
    num.under15 >=5 ~ "5 and up"))
  HH$num.under15.cat <- factor(HH$num.under15.cat,levels=c("5 and up","0","1-2","3-4"))


  # RATIO OF CHILDREN UNDER 5 TO WOMEN 15-49
  HH$hh.kidwom.rat<-ifelse(HH$hv010 == 0, 0, (HH$hv014/HH$hv010))


  # CATEGORICAL FACTOR FOR CHILDREN/WOMEN RATIO
  HH$hh.kidwom.rat.cat<-ifelse(HH$hh.kidwom.rat==0,0,
                               ifelse(HH$hh.kidwom.rat>0 & HH$hh.kidwom.rat<1,1,
                                      ifelse(HH$hh.kidwom.rat>=1 & HH$hh.kidwom.rat<2,2,
                                             ifelse(HH$hh.kidwom.rat>=2 & HH$hh.kidwom.rat<3,3,4))))


  ## LARGER ENVIRONMENT
  # AIR QUALITY IN THE HOUSE / VENTILATION
  HH <- HH %>% dplyr::mutate(hh.where.cook = case_when(
    hv226 == "no food cooked in house" ~ "No food cooked in house",
    hv241 == "outdoors" ~ "Food cooked outdoors",
    hv241 == "in a separate building" ~ "Food cooked in a separate building",
    hv241 == "other" ~ "Other",
    hv241 == "in the house" & hv242 == "yes" ~ "Food cooked inside in a separate kitchen",
    hv241 == "in the house" & (is.na(hv242) | hv242 == "no") ~ "Food cooked inside"))


  # WHERE IS FOOD COOKED IN THE HOUSE
  HH <- HH %>% dplyr::mutate(hh.where.cook.moh.cat = case_when(
    hv226 == "no food cooked in house" ~ "Other",
    hv241 == "outdoors" ~ "Food cooked outdoors",
    hv241 == "in a separate building" ~ "Food cooked outdoors",
    hv241 == "other" ~ "Other",
    hv241 == "in the house" & hv242 == "yes" ~ "Food cooked inside",
    hv241 == "in the house" & (is.na(hv242) | hv242 == "no") ~ "Food cooked inside"))


  # LOCATION OF COOKING INSIDE HOUSE
  HH$hh.cook.inside.yn<-ifelse(HH$hv241 == "in the house", 1, 0)


  # CLEAN COOKING FUEL
  HH$hh.clean.fuel<-ifelse(HH$hv226 %in% c("electricity","lpg","natural gas","biogas"), 1, 0)


  # NUMBER OF HOUSEHOLD ROOMS FOR SLEEPING
  HH$hh.rooms.num <- HH$hv216


  # NUMBER OF HOUSEHOLD ROOMS CATEGORY
  HH$hh.rooms.num.moh.cat <- ifelse(HH$hv216 < 6, "<6", "6+")


  # CATEGORICAL FACTOR OF NUMBER OF HOUSEHOLD ROOMS FOR SLEEPING
  # HH <- HH %>% dplyr::mutate(hh.rooms.cat = case_when(
  #   hv216 == 1 ~ "1",
  #   hv216 == 2 ~ "2",
  #   hv216 == 3 ~ "3",
  #   hv216 >= 4 & !is.na(hv216) ~ "4+"))


  # BINARY FACTOR FOR TOILET TYPE: LATRINE
  HH$latrine <- ifelse(HH$hv205 %in% c("pit latrine without slab/ open pit", "no facility/bush/field",
                                       "bucket toilet", "hanging toilet/latrine", "other"), 1,0)


  # TOILET FACILITY DOES NOT EXIST
  HH$no.latrine <- ifelse(HH$hv205 %in% c("no facility/bush/field", "no facility"), 0,1)


  # LOCATION OF TOILET FACILITY
  HH <- HH %>% dplyr::mutate(latrine.loc = case_when(
    hv205 =="no facility/bush/field" ~ "No facility",
    hv238a == "in own dwelling" ~ "In own dwelling",
    hv238a == "in own yard/plot" ~ "In own yard/plot",
    hv238a == "elsewhere" ~"Elsewhere"))


  # HOUSEHOLD HAS SHARED TOILET
  HH$hh.shared.latrine <- ifelse(HH$hv225 == "yes",1,0)


  # NUMBER OF HOUSEHOLDS SHARING TOILET
  HH <- HH %>% dplyr::mutate(hh.num.sharelatrine = case_when(
    as.numeric(hv238) > 1 & as.numeric(hv238) < 10 ~ as.numeric(hv238),
    as.numeric(hv238) == 95 | hv238 == "10 or more households" ~ 10,
    hv225 == "no" ~ 1,
    hv225 == "don't know" ~ 1))

  # WHO DEFNITION IMPROVED NOT SHARED LATRINE
  HH$hh.noimp.latrine<-ifelse(HH$latrine==1 | (HH$latrine==0 & HH$hh.shared.latrine==1),1,0)


  # SOURCE OF DRINKING WATER
  HH$water <- ifelse(HH$hv201 %in% c("unprotected well", "unprotected spring", "river/dam/lake/ponds/ stream/canal/irrigation channel",
                                     "rainwater", "other"), 1,0)


  # SOURCE OF NON-DRINKING WATER
  # HV202 only asked of people using bottled water for drinking


  # WATER SOURCE LOCATION
  HH <- HH %>%
    dplyr::mutate(hh.water.loc = case_when(hv201 %in% c("public tap/standpipe", "tube well or borehole", "protected well", "unprotected well", "unprotected spring",
                 "protected spring", "river/dam/lake/ponds/ stream/canal/irrigation channel", "rainwater", "tanker truck", "cart with small tank", "bottled water", "other") ~ hv235,
                 hv201 == "piped into dwelling" ~ "in own dwelling",
                 hv201 == "piped to yard/plot" ~ "in own yard/plot",
                 hv201 == "piped to neighbor" ~ "elsewhere"))


  # PIPED WATER TYPE
  HH$hh.water.notpiped <- ifelse(HH$hv201== "piped into dwelling" | HH$hv201== "piped to yard/plot" | HH$hv201 == "piped to neighbor" |
                                   HH$hv201== "public tap/standpipe", 0,1)


  # WATER TREATMENT: ANYTHING DONE TO MAKE WATER SAFE TO DRINK
  HH$hh.nowatpur <- ifelse(HH$hv237 == "yes",0,1)


  # DURATION TO GET WATER
  HH <- HH %>%
    dplyr::mutate(hh.wat.time = case_when(hv204 %in% c(1:10) ~ "1-10 minutes",
                                          hv204 %in% c(11:20) ~ "11-20 minutes",
                                          hv204 %in% c(21:30) ~ "21-30 minutes",
                                          hv204 %in% c(31:990) ~ ">30 minutes",
                                          hv204 %in% c(0, 996, "on premises") ~ "Water on premises",
                                          hv204 %in% c(998, "don't know") ~ NA))


  # WHO Definition IMPROVED WATER
  HH$hh.noimp.water <- ifelse(HH$water==1 | (HH$water==0 & HH$hh.wat.time == ">30 minutes"), 1, 0)


  # WATER INTERRUPTED
  HH <- HH %>% dplyr::mutate(hh.wat.interrupt = case_when(
    hv201a %in% c("no, not interrupted for a full day", "don't know") ~ "No",
    hv201a %in% c("yes, interrupted for a full day or more") ~ "Yes",
    is.na(hv201a) & hv201 %in% c("protected well", "unprotected well", "protected spring", "unprotected spring",
                                 "river/dam/lake/ponds/ stream/canal/irrigation channel", "rainwater",
                                 "tanker truck", "cart with small tank", "bottled water", "other") ~ "Water not piped"))


  # NATURAL/RUDIMENTARY FLOOR MATERIAL
  HH$hh.noimp.floor <- ifelse(HH$hv213 == "earth, sand" | HH$hv213== "dung" | HH$hv213== "wood planks" | HH$hv213== "palm, bamboo",1,0)


  # NATURAL/RUDIMENTARY WALL MATERIAL
  HH$hh.noimp.wall <- ifelse(HH$hv214 < 30 & HH$hv214 == 96, 1, 0)


  # URBAN SLUM - UN DEFINITION
  HH$hh.noimp.housing<-ifelse(HH$hh.noimp.floor == 1 & HH$hh.noimp.wall == 1,1,0)
  HH<-HH %>%
    rowwise() %>%
    dplyr::mutate(slum.sum = sum(hh.noimp.water, hh.noimp.latrine, hh.noimp.housing, na.rm=T))
  HH<-HH %>% dplyr::mutate(hh.urban.slum.un = case_when(
    hv025=="rural" ~ "rural",
    (hv025 == "urban" & slum.sum > 0) ~ "urban slum",
    (hv025 == "urban" & slum.sum == 0) ~ "urban non-slum"))


  # URBAN SLUM 2 STATUS (ZULU ET AL 2002 DEFINITION)
  HH$hh.urban.slum.zulu <- ifelse(HH$hh.urban == 1 & HH$hh.electricity == 0 & HH$latrine == 1 & HH$hh.water.notpiped == 0, 1, 0)


  # CATEGORICAL FACTOR FOR SANITATION
  HH <- HH %>% dplyr::mutate(hh.sanitation = case_when(
    (hv230b == "water is available" & hv232 == "yes") ~ "Water and soap observed",
    (hv230b == "water is available" & hv232 == "no") ~ "Water but no soap observed",
    (hv230b == "water not available" & hv232 == "yes")  ~ "Soap but no water observed",
    (hv230b == "water not available" & hv232 == "no") ~ "No water or soap observed",
    hv230a == "not observed: not in dwelling" ~ "No washing station",
    hv230a %in% c("not observed: no permission to see", "not observed: other reason") ~ "Washing place not observed"))

  HH <- HH %>% dplyr::mutate(hh.sanitation.cat = case_when(
    (hv230b == "water is available" & hv232 == "yes") ~ "Water and soap observed",
    (hv230b == "water is available" & hv232 == "no") | (hv230b == "water not available" & hv232 == "yes") | (hv230b == "water not available" & hv232 == "no") ~ "No water and/or soap observed",
    hv230a == "not observed: not in dwelling" | hv230a %in% c("not observed: no permission to see", "not observed: other reason") ~ "No washing station or place observed"))


  # HAND WASHING STATION OBSERVED
  HH$hh.sanitation.yn<-ifelse(HH$hh.sanitation=="No washing station" | HH$hh.sanitation=="No water or soap observed", 0, 1)


  # DIFFICULTY WALKING OR CLIMBING STAIRS
  HH_dis <- HH %>%
    dplyr::select(hv001, hv002, starts_with("sh20gi_")) %>%
    reshape2::melt(id.vars=c("hv001", "hv002")) %>%
    dplyr::filter(!is.na(value)) %>%
    dplyr::mutate(disability = ifelse(value %in% c("aucune difficulte pour marcher ou grimper", "quelques difficultes"), 0, 1)) %>%
    group_by(hv001, hv002) %>%
    dplyr::summarize(disability.cnt = sum(disability), hh.cnt = n())
  HH_dis$pct.disability <- 100*(HH_dis$disability.cnt/HH_dis$hh.cnt)
  HH_dis$disability.yn<-ifelse(HH_dis$disability.cnt > 0, 1, 0)
  HH_dis <- HH_dis %>%
    dplyr::select(hv001, hv002, pct.disability, disability.yn)
  HH <- HH %>%
    base::merge(HH_dis, by=c("hv001", "hv002"), all.x=TRUE)


  # NUMBER OF HOUSEHOLD MEMBERS
  HH$hh.members <- ifelse(HH$hv012 == 0, HH$hv013, HH$hv012)


  # SLEEPING ROOMS PER PERSON
  HH$hh.memsleep <- ifelse(HH$hh.rooms.num == 0, HH$hh.members, HH$hh.members/HH$hh.rooms.num)


  # MALARIA ZONE
  HH <- HH %>%
    dplyr::mutate(malaria.zone = case_when(grepl("0-2.0", shmzone) ~ "0-2.0",
                                    grepl("2.1 - 6.0", shmzone) ~ "2.1-6.0",
                                    grepl("6.1 - 30", shmzone) ~ "6.1-30"))


  # BINARY FACTOR FOR HIGHEST LEVEL OF EDUCATION IN HOUSEHOLD
  HH$highestyearsedinHH.7plus <- ifelse(HH$highestyearsedinHH.yrs >= 7, '7+', '0-7')


  # WEATLH SUM INDEX
  HH <- HH %>%
    dplyr::mutate(wealth.sum = rowSums(across(c(hh.electricity, hh.tv, hh.refrig, hh.bank.acct, hh.computer, hh.car, hh.noimp.latrine))))


  ######################################################################
  ######################################################################
  # 2 | IR FILE


  # CURRENT PARTNERSHIP STATUS
  IR$marr.cohab <- ifelse(IR$v501 == "married" | IR$v501 == "living with partner", 1, 0)

  # MARITAL CAT
  IR<-IR %>% dplyr::mutate(marital.cat = case_when(
    v501 == "never in union" ~ "never in union",
    v501 == "married" | v501 == "living with partner" ~ "married",
    v501 == "widowed" | v501 == "divorced" | v501 == "separated" ~ "widowed/ divorced/ separate"))

  # TYPE OF MARRIAGE, AMONG MARRIED WOMEN
  # IR$polygamy <- ifelse(IR$v506 %in% c(1:20), 1, 0)

  IR$polygamy <- ifelse(IR$v506 %in% c(1:20), "yes", "no")
  IR$polygamy[IR$marr.cohab==0] <- 'not partnered'
  IR$polygamy[IR$v506 %in% c(98, "don't know")] <- NA

  # # WIFE ORDER AMONG WOMEN IN POLYGAMOUS UNION
  # IR$wife.order <- IR$v506
  # IR$wife.order[IR$v506 == 98] <- NA
  # IR$wife.order[IR$marr.cohab==0] <- 'not partnered'
  # IR$wife.order[IR$v505==0 & !is.na(IR$v505)] <- 'monomgaous union'

  # WIFE ORDER AMONG WOMEN IN POLYGAMOUS UNION - MOH
  IR$wife.order <- IR$v506
  IR$wife.order[IR$v506 == 98] <- NA
  IR$wife.order[IR$v506 == 1] <- "1"
  IR$wife.order[IR$v506 == 2] <- "2"
  IR$wife.order[IR$v506 >= 3] <- "3+"
  IR$wife.order[IR$marr.cohab==0] <- 'not currently married'
  # IR$wife.order[IR$marr.cohab==1 & (IR$v505==0 | !is.na(IR$v505))] <- 'married with no other wives'
  IR$wife.order[IR$marr.cohab==1 & (IR$v505==0 | !is.na(IR$v505)) & is.na(IR$v506)] <- 'married with no other wives'


  # RESPONDENT IS FIRST WIFE
  IR <- IR %>%
    dplyr::mutate(first.wife = case_when(v506 == 1 ~ "First wife",
                                         v506 %in% c(2:20) ~ "Second or more wife",
                                         v506 %in% c(98, "don't know") ~ "Not known",
                                         !(v501 %in% c("married", "living with partner")) ~ "Not in union",
                                         v505 %in% c(0, "no other wives") ~ "Monomgaous union"))
  IR$first.wife[IR$first.wife=='Not in union'] <- 'not partnered'
  IR$first.wife[IR$first.wife=='Not known'] <- NA


  # ABILITY TO CONCEIVE
  IR$infecund.meno <- ifelse(IR$v625 == "infecund, menopausal", 1,
                             ifelse(!(is.na(IR$v625)), 0, NA))


  # HUSBAND/PARTNER OPPOSES FP USE
  IR <- IR %>% dplyr::mutate(fp.partner.oppose = case_when(
    v3a08j == "yes" ~ "Partner opposes",
    v3a08j == "no" ~ "Partner does not oppose",
    is.na(v3a08j) ~ "No identified need for FP"))


  ## BARRIERS TO HC
  # GETTING PERMISSION
  IR$med.permis <- IR$v467b
  IR <- IR %>% dplyr::mutate(med.permis = case_when(
    v467b =="not a big problem" | v467b =="no problem" ~0,
    v467b == "big problem" ~1))


  # COST FOR NECESSARY TREATMENT
  IR$med.cost <- IR$v467c
  IR <- IR %>% dplyr::mutate(med.cost = case_when(
    v467c =="not a big problem" | v467c =="no problem" ~0,
    v467c == "big problem" ~1))


  # DISTANCE TO HEALTH FACILITY
  IR$med.dist <- IR$v467d
  IR <- IR %>% dplyr::mutate(med.dist = case_when(
    v467d =="not a big problem" | v467d =="no problem" ~0,
    v467d == "big problem" ~1))


  # COST FOR NECESSARY TREATMENT | DISTANCE TO HEALTH FACILITY
  IR <- IR %>%
    dplyr::mutate(med.cost.dist.moh = case_when((v467b =="not a big problem" | v467b =="no problem") &
                                         (v467c =="not a big problem" | v467c =="no problem") ~ 0,
                                         (v467c == "big problem" & v467d == "big problem") ~ 1))



  # DOESN'T WANT TO GO ALONE
  IR$med.alone <- IR$v467f
  IR <- IR %>% dplyr::mutate(med.alone = case_when(
    v467f =="not a big problem"| v467f =="no problem" ~0,
    v467f == "big problem" ~1))


  # CREATE INDEX FROM BARRIERS TO HC VARIABLES
  # MUTATED ABOVE SO THAT 1 IS A BIG PROBLEM, THE REST IS ZERO
  IR<-IR %>%
    rowwise() %>%
    dplyr::mutate(med.index = sum(med.permis, med.cost, med.dist, med.alone, na.rm=TRUE))
  IR$med.index <- ifelse(IR$med.index == 0,"0",
                         ifelse(IR$med.index == 1, "1",
                                ifelse(IR$med.index == 2, "2", "3+")))

  # VISITED BY A HEALTHWORKER
  IR$hw.visit.12mo<-ifelse(IR$v393=="yes",1,0)


  # DID HEALTHWORKER TALK ABOUT FAMILY PLANNING
  IR <- IR %>% dplyr::mutate(hw.visit.fp = case_when(
    v393a == "yes" ~ "HW discussed FP",
    v393a == "no" ~ "HW did not discuss FP",
    is.na(v393a) ~ "No HW visit"))


  ## EMPLOYMENT FACTORS
  # RESPONDENT'S OCCUPATION
  IR$occupation <- factor(IR$v717)
  IR$occupation.cat <- case_when(
    IR$occupation %in% c('professional/technical/ managerial','clerical') ~ 'professional/mangerial/ clerical',
    IR$occupation %in% c('skilled manual','unskilled manual') ~ 'manual',
    IR$occupation %in% c('services','household and domestic') ~ 'service/domestic',
    IR$occupation %in% c('agricultural - self employed','agricultural - employee') ~ 'agricultural',
    IR$occupation %in% c('sales') ~ 'sales',
    IR$occupation %in% c('other') ~ 'other',
    IR$occupation %in% c('not working') ~ 'not working')


  # RESPONDENT WORKS FOR FAMILY, OTHERS, SELF-EMPLOYED
  IR <- IR %>% dplyr::mutate(occ.type = case_when(
    v719 == "for family member" ~ "Family",
    v719 == "for someone else" ~ "Someone else",
    v719 == "self-employed" ~ "Self",
    is.na(v719) ~ "Did not work"))


  # RESPONDENT CURRENTLY WORKS
  IR <- IR %>% dplyr::mutate(working = case_when(
    (v714 == "yes" & (v714a != "yes" | is.na(v714a))) ~ "Currently working",
    (v714a == "yes") ~ "On leave/absent",
    (v731 == "no") ~ "No work in the past 12 months",
    (v731 == "in the past year") ~ "Worked in the past year"))


  # BINARY FACTOR FOR EMPLOYED CURRENTLY
  IR$workingnow.yn <- ifelse(IR$working=="Currently working",1,0)


  # WORK SEASONALITY
  IR <- IR %>% dplyr::mutate(work.seasonal = case_when(
    v731 == "no" ~ "No work in past 12 months",
    v732 == "occasional" ~ "Occassional",
    v732 == "seasonal" ~ "Seasonal",
    v732 == "all year" ~ "All year"))


  # HUSBAND/PARTNER WORKING
  IR <- IR %>% dplyr::mutate(part.working = case_when(
    !(v501 %in% c("married","living with partner"))  ~ "no partner",
    v704a == "didn't work last 12 months" ~ "Didn't work last 12 months",
    v704a == "worked last 7 days" ~ "Worked last 7 days",
    v704a == "worked last 12 months" ~ "Worked last 12 months",
    v704a == "don't know" ~ "Don't know"))
  IR$part.working[IR$part.working=="Don't know"] <- NA
  IR$part.working <- factor(IR$part.working,levels=c('Worked last 7 days',"Worked last 12 months","Didn't work last 12 months","no partner"))

  # BINARY FACTOR FOR HUSBAND/PARTNER EMPLOYED CURRENTLY
  IR <- IR %>% dplyr::mutate(part.workingnow.yn = case_when(
    part.working == "no partner" ~ "no partner",
    part.working == "Worked last 7 days" ~ "Yes",
    part.working %in% c("didn't work last 12 months", "worked last 12 months", "don't know") ~ "No"))


  # HUSBAND/PARTNER'S OCCUPATION
  IR$partner.occupation <- IR$v704
  IR$partner.occupation.cat <- case_when(
    IR$partner.occupation %in% c('professional/technical/ managerial','clerical') ~ 'professional/mangerial/ clerical',
    IR$partner.occupation %in% c('skilled manual','unskilled manual') ~ 'manual',
    IR$partner.occupation %in% c('services','household and domestic') ~ 'service/domestic',
    IR$partner.occupation %in% c('agricultural - self employed','agricultural - employee') ~ 'agricultural',
    IR$partner.occupation %in% c('sales') ~ 'sales',
    IR$partner.occupation %in% c('other',"don't know") ~ 'other/dk',
    IR$partner.occupation %in% c('not working') ~ 'not working')


  # RESPONDENT EARNS MORE THAN HUSBAND/PARTNER
  IR <- IR %>% dplyr::mutate(earnings.rel.partner = case_when(
    v746 == "more than him" ~ "more than him",
    v746 == "less than him" ~ "less than him",
    is.na(v746) & working == "Currently working" ~ "less than him",
    v746 == "about the same" ~ "about the same",
    v746 == "husband/partner doesn't bring in money" & working == "Currently working" ~ "more than him",
    v746 == "husband/partner doesn't bring in money" & working != "Currently working" ~ "husband no earnings",
    v746 == "don't know" ~ "don't know"))


  # OWNS A HOUSE ALONE OR JOINTLY
  IR$jnt.house.ownership <- IR$v745a


  # OWNS LAND ALONE OR JOINTLY
  IR$jnt.land.ownership <- IR$v745b


  # NUMBER OF CHILDREN IN THE HOUSEHOLD (UNDER 5)
  IR$num.under5 <- IR$v137
  IR <- IR %>% dplyr::mutate(num.under5.cat = case_when(
    v137 == 0 ~ "0",
    v137 == 1 ~ "1",
    v137 ==2 ~ "2",
    (v137 >=3 & !is.na(v137)) ~ "3+"))
  IR$num.under52 <- factor(IR$num.under5.cat,levels=c("3+","0","1","2"))

  # NUMBER OF CHILDREN LIVING
  IR$num.child.alive <- IR$v218


  # CATEGORICAL FACTOR FOR NUMBER OF LIVING CHILDREN
  IR <- IR %>% dplyr::mutate(num.child.alive.cat = case_when(
    v218 %in% c(0) ~ "0",
    v218 %in% c(1:2) ~ "1-2",
    v218 %in% c(3:5) ~ "3-5",
    (v218 >=6 & !is.na(v218)) ~ "6+"))
  IR$num.child.alive.cat <- factor(IR$num.child.alive.cat,levels=c("6+","0","1-2","3-5"))


  # # CATEGORICAL FACTOR FOR NUMBER OF LIVING CHILDREN - MOH
  # IR <- IR %>% dplyr::mutate(num.child.alive.moh.cat = case_when(
  #   v218 < 3 ~ "<3",
  #   v218 %in% C(3:5) ~ "3-5",
  #   v218 > 5 ~ "6+"))


  # CHILDREN THAT HAVE DIED
  IR$num.child.die <- IR$v206 + IR$v207


  # CATEGORICAL FACTOR FOR NUMBER OF CHILDREN THAT HAVE DIED
  IR <- IR %>% dplyr::mutate(num.child.die.cat = case_when(
    num.child.die %in% c(0) ~ "0",
    num.child.die %in% c(1) ~ "1",
    num.child.die >=2 ~ "2 or more"))


  # CATEGORICAL FACTOR FOR NUMBER OF CHILDREN THAT HAVE DIED - MOH
  IR$num.child.die.moh.yn <- ifelse(IR$v206 + IR$v207 > 0 , 1, 0)



  # NUMBER OF BIOLOGICAL CHILDREN IN THE HOUSEHOLD
  # IR <- IR %>%
  #   rowwise() %>%
  #   dplyr::mutate(num.biochild.house = sum(v202, v203, na.rm=TRUE))


  # CATEGORICAL FACTOR FOR NUMBER OF BIOLOGICAL CHILDREN IN HOUSEHOLD
  # IR <- IR %>% dplyr::mutate(num.biochild.house.cat = case_when(
  #   num.biochild.house %in% c(0) ~ "0",
  #   num.biochild.house %in% c(1) ~ "1",
  #   num.biochild.house %in% c(2,3) ~ "2-3",
  #   num.biochild.house >=4 ~ "4 or more"))


  # PARTNER DOES NOT LIVE IN THE HOUSEHOLD (AMONG MARRIED OR IN UNION)
  IR <- IR %>% dplyr::mutate(partner.absent = case_when(
    v504 == "living with her" ~ "No",
    v504 == "staying elsewhere" ~ "Yes",
    !(v501 %in% c("married", "living with partner")) ~ "not partnered"))


  # BINARY FACTOR FOR PARTNER IS ABSENT FROM HOUSEHOLD
  IR$partner.absent.yn <- ifelse(IR$partner.absent == "Yes", 1, 0)


  # SEX OF HEAD OF HOUSEHOLD
  IR$head.sex <- IR$v151
  #IR$head.sex.male.num <- ifelse(IR$head.sex == "male", 1, 0)


  # CURRENT PARTNERSHIP STATUS
  IR$yrs.curr.pship <- ifelse(IR$v501 %in% c("married", "living with partner"), IR$v512, NA)
  IR$yrs.curr.pship[IR$marr.cohab==0] <- 'not partnered'


  # # DURATION OF MARRIAGE, AMONG WOMEN STILL WITH FIRST PARTNER
  IR <- IR %>% dplyr::mutate(yrs.curr.pship.cat = case_when(
    v501 %in% c("never in union") ~ "Never in union",
    (v501 %in% c("widowed", "divorced", "no longer living together/ separated") | v503 %in% c("more than once")) ~ "No longer with first partner",
    yrs.curr.pship %in% c(0:4) ~ "Less than 5 years",
    yrs.curr.pship %in% c(5:9) ~ "5 to 9 years",
    yrs.curr.pship %in% c(10:39) ~ "10 or more years"))
  IR$yrs.curr.pship.cat <- factor(IR$yrs.curr.pship.cat, levels = c("Never in union", "Less than 5 years", "More than 5 years", "No longer with first partner"))


  ## VIOLENCE IN HOUSEHOLD
  # DOMESTIC VIOLENCE - PHYSICAL
  IR <- IR %>% dplyr::mutate(dv.physical = case_when(
    (d106 == "no" & d107 == "no") ~ "No",
    (d106 == "yes" | d107 == "yes") ~ "Yes"))

  # DOMESTIC VIOLENCE - EMOTIONAL
  IR <- IR %>% dplyr::mutate(dv.emotional = case_when(
    d104 == "yes" ~ "Yes",
    d104 == "no" ~ "No"))

  # DOMESTIC VIOLENCE - SEXUAL
  IR <- IR %>% dplyr::mutate(dv.sexual = case_when(
    d108 == "yes" ~ "Yes",
    d108 == "no" ~ "No"))

  # HUSBAND/PARTNER JEALOUS IF RESPONDENT TALKS WITH OTHER MEN
  IR <- IR %>% dplyr::mutate(dv.jealous.othermen = case_when(
    d101a == "yes" ~ "Yes",
    d101a == "no" ~ "No"))

  # HUSBAND/PARTNER DOES NOT PERMIT RESPONDENT TO MEET FEMALE FRIENDS
  IR <- IR %>% dplyr::mutate(dv.nofriends = case_when(
    d101c == "yes" ~ "Yes",
    d101c == "no" ~ "No"))

  # HUSBAND/PARTNER TRIES TO LIMIT RESPONDENT'S CONTACT WITH FAMILY
  IR <- IR %>% dplyr::mutate(dv.nofamily.contact = case_when(
    d101d == "yes" ~ "Yes",
    d101d == "no" ~ "No"))

  # EVER BEEN HUMILIATED BY HUSBAND/PARTNER
  IR$dv.humiliated <- as.character(IR$d103a)

  # EVER BEEN THREATENED WITH HARM BY HUSBAND/PARTNER
  IR$dv.threatened <- as.character(IR$d103b)

  # EVER BEEN INSULTED OR MADE TO FEEL BAD BY HUSBAND/PARTNER
  IR$dv.insulted <- as.character(IR$d103c)

  # EVER BEEN PUSHED, SHOOK, OR HAD SOMETHING THROWN BY HUSBAND/PARTNER
  IR$dv.pushed <- as.character(IR$d105a)

  # EVER BEEN SLAPPED BY HUSBAND/PARTNER
  IR$dv.slapped <- as.character(IR$d105b)

  # EVER BEEN KICKED OR DRAGGED BY HUSBAND/PARTNER
  IR$dv.kicked <- as.character(IR$d105d)

  # EVER BEEN STRANGLED OR BURNT BY HUSBAND/PARTNER
  IR$dv.strangled <- as.character(IR$d105e)

  # EVER BEEN ATTACKED WITH KNIFE/GUN OR OTHER WEAPON BY HUSBAND/PARTNER
  IR$dv.weapon <- as.character(IR$d105f)

  # EVER CS PHYSICAL VIOLENCE BY HUSBAND/PARTNER
  # IR$dv.cs.physical <- as.character(IR$d105g)

  # EVER BEEN PHYSICALLY FORCED INTO UNWANTED SEX BY HUSBAND/PARTNER
  IR$dv.forcedsex1 <- as.character(IR$d105h)

  # EVER BEEN FORCED INTO OTHER UNWANTED SEXUAL ACTS BY HUSBAND/PARTNER
  IR$dv.forcedsex2 <- as.character(IR$d105i)

  # EVER HAD ARM TWISTED OR HAIR PULLED BY HUSBAND/PARTNER
  IR$dv.armtwist <- as.character(IR$d105j)

  # Ever been physically forced to perform sexual acts respondent didn't want to
  IR$dv.forcedsex3 <- as.character(IR$d105k)

  # HUSBAND/PARTNER: PERSON WHO HURT RESPONDENT DURING A PREGNANCY
  IR <- IR %>% dplyr::mutate(dv.hurtpreg.husband = case_when(
    d118a == "yes" ~ "Yes",
    d118a == "no" ~ "No"))

  # FORMER PARTNER: PERSON WHO HURT RESPONDENT DURING A PREGNANCY
  IR <- IR %>% dplyr::mutate(dv.hurtpreg.formerp = case_when(
    d118j == "yes" ~ "Yes",
    d118j == "no" ~ "No"))

  # PREVIOUS HUSBAND: EVER HIT, SLAP, KICK, OR PHYSICALLY HURT RESPONDENT
  IR$dv.prevparter.hurt <- as.character(IR$d130a)

  # PREVIOUS HUSBAND: PHYSICALLY FORCED TO HAVE SEX OR PERFORM SEXUAL ACTS
  IR$dv.prevparter.forcedsex <- as.character(IR$d130b)

  # FRIEND: PERSON RESPONDENT WENT TO SEEK HELP
  IR <- IR %>% dplyr::mutate(dv.friend.help = case_when(
    d119xd == "yes" ~ "Yes",
    d119xd == "no" ~ "No"))
  IR$dv.friend.help[IR$d128 == 'no'] <- 'Did not seek any help'

  # OWN FAMILY: PERSON RESPONDENT WENT TO SEEK HELP
  IR <- IR %>% dplyr::mutate(dv.family.help = case_when(
    d119h == "yes" ~ "Yes",
    d119h == "no" ~ "No"))
  IR$dv.family.help[IR$d128 == 'no'] <- 'Did not seek any help'

  # HUSBAND/PARTNER FAMILY: PERSON RESPONDENT WENT TO SEEK HELP
  IR <- IR %>% dplyr::mutate(dv.husband.help = case_when(
    d119i == "yes" ~ "Yes",
    d119i == "no" ~ "No"))
  IR$dv.husband.help[IR$d128 == 'no'] <- 'Did not seek any help'

  # CURRENT/FORMER HUSBAND/PARTNER: PERSON RESPONDENT WENT TO SEEK HELP
  IR <- IR %>% dplyr::mutate(dv.formerp.help = case_when(
    d119j == "yes" ~ "Yes",
    d119j == "no" ~ "No"))
  IR$dv.formerp.help[IR$d128 == 'no'] <- 'Did not seek any help'

  # CURRENT/FORMER BOYFRIEND: PERSON RESPONDENT WENT TO SEEK HELP
  IR <- IR %>% dplyr::mutate(dv.formerb.help = case_when(
    d119k == "yes" ~ "Yes",
    d119k == "no" ~ "No"))
  IR$dv.formerb.help[IR$d128 == 'no'] <- 'Did not seek any help'

  # NEIGHBOR: PERSON RESPONDENT WENT TO SEEK HELP
  IR <- IR %>% dplyr::mutate(dv.neighbor.help = case_when(
    d119u == "yes" ~ "Yes",
    d119u == "no" ~ "No"))
  IR$dv.neighbor.help[IR$d128 == 'no'] <- 'Did not seek any help'

  # OTHER: PERSON RESPONDENT WENT TO SEEK HELP
  IR <- IR %>% dplyr::mutate(dv.other.help = case_when(
    d119x == "yes" ~ "Yes",
    d119x == "no" ~ "No"))
  IR$dv.other.help[IR$d128 == 'no'] <- 'Did not seek any help'

  # SOCIAL SERVICE ORGANIZATION: PERSON RESPONDENT WENT TO SEEK HELP
  IR <- IR %>% dplyr::mutate(dv.sso.help = case_when(
    d119xb == "yes" ~ "Yes",
    d119xb == "no" ~ "No"))
  IR$dv.sso.help[IR$d128 == 'no'] <- 'Did not seek any help'

  # RELIGIOUS LEADER: PERSON RESPONDENT WENT TO SEEK HELP
  IR <- IR %>% dplyr::mutate(dv.religious.help = case_when(
    d119xf == "yes" ~ "Yes",
    d119xf == "no" ~ "No"))
  IR$dv.religious.help[IR$d128 == 'no'] <- 'Did not seek any help'

  # EVER TOLD ANYONE ELSE ABOUT VIOLENCE
  IR$dv.anyone.help <- IR$d128


  ## DECISION-MAKING

  # JOINT DECISION: FINANCIAL
  IR$desc.ownincome <- as.character(IR$v739)
  IR$desc.ownincome[(IR$v741 %in% c("not paid", "in-kind only") | IR$v731 == "no")] <- "Not paid in cash or not working"
  IR$desc.ownincome[IR$marr.cohab==0] <- 'not partnered'

  # IR$jd.ownincome <- ifelse(IR$v739== "respondent and husband/partner",1,0)


  # OWN DECISION: FINANCIAL
  # IR$wd.ownincome <- ifelse(IR$v739== "respondent alone",1,0)


  # BINARY FACTOR FOR EITHER JOINT OR OWN DECISION: FINANCIAL
  # IR$jdwd.ownincome <- ifelse(IR$v739== "respondent alone" | IR$v739=="respondent and husband/partner",1,0)


  # JOINT DECISION: LARGE HOUSEHOLD PURCHASES
  IR$desc.lrgpur <- as.character(IR$v743b)
  IR$desc.lrgpur[IR$marr.cohab==0] <- 'not partnered'

  #IR$jd.lrgpur <- ifelse(IR$v743b== "respondent and husband/partner",1,0)


  # # OWN DECISION: LARGE HOUSEHOLD PURCHASES
  # IR$wd.lrgpur <- ifelse(IR$v743b== "respondent alone",1,0)


  # # BINARY FACTOR FOR EITHER JOINT OR OWN DECISION: LARGE HOUSEHOLD PURCHASES
  # IR$jdwd.lrgpur <- ifelse(IR$v743b== "respondent alone" | IR$v743b=="respondent and husband/partner",1,0)


  # JOINT DECISION: HUSBAND'S INCOME
  IR$desc.money <- as.character(IR$v743d)
  IR$desc.money[IR$marr.cohab==0] <- 'not partnered'


  # OWN DECISION: HUSBAND'S INCOME
  # IR$wd.money <- ifelse(IR$v743f== "respondent alone",1,0)


  # BINARY FACTOR FOR EITHER JOINT OR OWN DECISION: HUSBAND'S INCOME
  # IR$jdwd.money <- ifelse(IR$v743f== "respondent alone" | IR$v743f=="respondent and husband/partner",1,0)


  # JOINT DECISION: HEALTH
  IR$desc.hlth <- as.character(IR$v743a)
  IR$desc.hlth[IR$marr.cohab==0] <- 'not partnered'


  # OWN DECISION: HEALTH
  # IR$wd.hlth <- ifelse(IR$v743a== "respondent alone",1,0)


  # BINARY FACTOR FOR EITHER JOINT OR OWN DECISION: HEALTH
  # IR$jdwd.hlth <- ifelse(IR$v743a== "respondent alone" | IR$v743a=="respondent and husband/partner",1,0)


  # JOINT DECISION: FAMILY PLANNING
  IR <- IR %>% dplyr::mutate(desc.fp = case_when(
    (v632 %in% c("joint decision") | v632a %in% c("joint decision")) ~ "respondent and husband/ partner",
    (v632 %in% c("mainly respondent") | v632a %in% c("mainly respondent")) ~ "respondent alone",
    (v632 %in% c("mainly husband, partner") | v632a %in% c("mainly husband, partner")) ~ "husband/ partner alone",
    (v632 %in% c("other") | v632a %in% c("other")) ~ "other",
    (marr.cohab %in% 0) ~ "not partnered"))
  IR$desc.fp[IR$v213 %in% 'yes'] <- 'currently pregnant'


  # OWN DECISION: FAMILY PLANNING
  # IR <- IR %>% dplyr::mutate(wd.fp = case_when(
  #   (v632 %in% c("mainly respondent") | v632a %in% c("mainly respondent")) ~ "Yes",((!is.na(v632) & !(v632 == "mainly respondent")) | (!is.na(v632a) & !(v632a == "mainly respondent"))) ~ "No"))
  # IR$wd.fp <- ifelse(IR$wd.fp=="Yes",1,0)


  # CATEGORICAL FACTOR FOR OWN DECISION: FAMILY PLANNING
  # IR <- IR %>% dplyr::mutate(wd.fp.cat = case_when(
  #   (v632 %in% c("mainly respondent") | v632a %in% c("mainly respondent")) ~ "Yes",
  #   ((!is.na(v632) & !(v632 == "mainly respondent")) | (!is.na(v632a) & !(v632a == "mainly respondent"))) ~ "No",
  #   !(v501 %in% c("married", "living with partner")) ~ "Not in union",
  #   v213 == 1 ~ "Pregnant"))


  # BINARY FACTOR FOR EITHER JOINT OR OWN DECISION: FAMILY PLANNING
  # IR <- IR %>% dplyr::mutate(jdwd.fp = case_when(
  #   v632 %in% c("joint decision") | v632a %in% c("joint decision") | v632 %in% c("mainly respondent") | v632a %in% c("mainly respondent") ~ "Yes",
  #   v632 %in% c("mainly husband, partner") | v632 %in% c("other") ~ "No",
  #   v632a %in% c("mainly husband, partner") | v632a %in% c("other") ~ "No"))
  # IR$jdwd.fp <- ifelse(IR$jdwd.fp=='Yes',1,0)


  # JOINT DECISION: EVERYDAY DECISIONS
  IR$desc.visit <- as.character(IR$v743d)
  IR$desc.visit[IR$marr.cohab==0] <- 'not partnered'


  # OWN DECISION: EVERYDAY DECISIONS
  # IR$wd.visit <- ifelse(IR$v743d== "respondent alone",1,0)


  # BINARY FACTOR FOR EITHER JOINT OR OWN DECISION: EVERYDAY DECISIONS
  # IR$jdwd.visit <- ifelse(IR$v743d== "respondent alone" | IR$v743d=="respondent and husband/partner",1,0)


  # JOINT DECISON-MAKING INDEX
  IR$jd.index <- IR$desc.ownincome %in% 'respondent and husband/partner' +
    IR$desc.lrgpur %in% 'respondent and husband/partner' +
    IR$desc.money %in% 'respondent and husband/partner' +
    IR$desc.hlth %in% 'respondent and husband/partner' +
    IR$desc.fp %in% 'respondent and husband/partner' +
    IR$desc.visit %in% 'respondent and husband/partner'
  IR$jd.index[IR$marr.cohab==0] <- 'not partnered'
  IR$jd.index <- ifelse(IR$jd.index == 0,'0',
                        ifelse(IR$jd.index %in% c(1,2,3), '1-3',
                               ifelse(IR$jd.index %in% c(4,5,6), '4+', 'not partnered')))

  # WOMEN'S DECISION MAKING INDEX
  IR$wd.index <- IR$desc.ownincome %in% 'respondent alone' +
    IR$desc.lrgpur %in% 'respondent alone' +
    IR$desc.money %in% 'respondent alone' +
    IR$desc.hlth %in% 'respondent alone' +
    IR$desc.fp %in% 'respondent alone' +
    IR$desc.visit %in% 'respondent alone'
  IR$wd.index[IR$marr.cohab==0] <- 'not partnered'
  IR$wd.index <- ifelse(IR$wd.index == 0,'0',
                        ifelse(IR$wd.index %in% c(1,2,3), '1-3',
                               ifelse(IR$wd.index %in% c(4,5,6), '4+', 'not partnered')))

  # CATEGORICAL FACTOR FOR JOINT DECISION-MAKING INDEX
  # IR <- IR %>% dplyr::mutate(jd.index.cat = case_when(
  #   marr.cohab == 0 ~ "Not partnered",
  #   jd.index == 0 ~ "None",
  #   jd.index %in% c(1:4) ~ "One to four",
  #   jd.index %in% c(5:6) ~ "Five to six"
  # ))


  # CATEGORICAL FACTOR FOR WOMEN'S DECISION-MAKING INDEX
  # IR <- IR %>% dplyr::mutate(wd.index.cat = case_when(
  #   marr.cohab == 0 ~ "Not partnered",
  #   wd.index == 0 ~ "None",
  #   wd.index %in% c(1:4) ~ "One to four",
  #   wd.index %in% c(5:6) ~ "Five to six"
  # ))


  # CATEGORICAL FACTOR FOR JOINT/WOMEN DECISION-MAKING INDEX
  # IR <- IR %>% dplyr::mutate(jdwd.index.cat = case_when(
  #   marr.cohab == 0 ~ "Not partnered",
  #   jdwd.index == 0 ~ "None",
  #   jdwd.index %in% c(1:4) ~ "One to four",
  #   jdwd.index %in% c(5:6) ~ "Five to six"
  # ))


  ## PARTNER CHARACTERISTICS
  # PARTNER'S AGE
  IR$partner.age <- IR$v730
  IR$partner.age[IR$marr.cohab==0] <- 'not partnered'


  # CATEGORICAL FACTOR FOR PARTNER'S AGE
  IR <- IR %>% dplyr::mutate(partner.age.cat = case_when(
    v730 %in% c(15:29) ~ "under 30",
    v730 %in% c(30:59) ~ "30-59",
    v730 %in% c(60:96) ~ "60+",
    v730 == 98 ~ "Don't know",
    v730 == 99 ~ "Missing",
    !(v501 %in% c("married", "living with partner")) ~ "not partnered"))
  IR$partner.age.cat <- factor(IR$partner.age.cat,levels=c("30-59","under 30","60+","not partnered"))

  # DIFFERENCE BETWEEN WOMAN AND PARTNER'S AGE
  IR$age.diff <- IR$v730 - IR$v012
  IR$age.diff[is.na(IR$v730) | IR$v730 > 96] <- NA
  IR$age.diff[IR$marr.cohab==0] <- 'not partnered'


  # CATEGORICAL FACTOR FOR DIFFERENCE BETWEEN WOMAN AND PARTNER'S AGE
  # IR <- IR %>% dplyr::mutate(age.diff.cat = case_when(
  #   age.diff < (-2) ~ "Partner 2 or more years younger",
  #   age.diff %in% c(-1:1) ~ "Partner within 1 year",
  #   age.diff %in% c(2:5) ~ "Partner 2-5 years older",
  #   age.diff %in% c(6:10) ~ "Partner 6-10 years older",
  #   age.diff >10 & !is.na(age.diff) ~ "Partner more than 10 years older",
  #   !(v501 %in% c("married", "living with partner")) ~ "Not in union"))
  # IR$age.diff.cat <- factor(IR$age.diff.cat, levels = c("Partner 2 or more years younger",
  #                                                       "Partner within 1 year", "Partner 2-5 years older", "Partner 6-10 years older", "Partner more than 10 years older",
  #                                                       "Not in union"))


  # CATEGORICAL FACTOR FOR PARTNER'S EDUCATION
  # IR$husb.ed.grp <- ifelse(IR$no.partner == 1,"Not in union",IR$husb.ed.grp)
  # IR$husb.ed.grp <- factor(IR$husb.ed.grp, levels = c("Not in union", "no education", "incomplete primary",
  #                                                     "complete primary", "incomplete secondary", "complete secondary",
  #                                                     "higher", "don't know"))


  # WOMAN HAD PREVIOUS PARTNERSHIPS
  IR <- IR %>% dplyr::mutate(prev.pship = case_when(
    v503 %in% c("once") ~ "No",
    v503 %in% c("more than once") ~ "Yes",
    v501 %in% c("never in union") ~ "Never in union"))


  # PARTNER HELPS WITH HOUSEHOLD CHORES
  # if (all(c('s924a', 's924b') %in% names(IR))){
  #   IR <- IR %>% dplyr::mutate(partner.chores = case_when(
  #     s924a == "no" ~ "No",
  #     (s924a == "yes" & s924b == "almost every day") ~ "Almost every day",
  #     (s924a == "yes" & s924b == "at least once a week") ~ "At least once a week",
  #     (s924a == "yes" & s924b == "rarely") ~ "Rarely",
  #     s924a == "not living husband/partner" ~ "Not living with husband/partner",
  #     !(v501 %in% c("married", "living with partner")) ~ "Not in union"))
  #
  #   IR$part.nochores <- ifelse(IR$partner.chores == "No", 1, 0)
  # } else {print("s924a & s924b do not exist in HH")}


  # RESPONDENT HAS TELEPHONE
  IR<-IR %>% dplyr::mutate(has.mobile = case_when(
    v169a=="no" ~ 0,
    v169a== "yes" ~ 1))


  # USE MOBILE PHONE FOR FINANCIAL TRANSACTIONS
  IR <- IR %>% dplyr::mutate(mobile.financial = case_when(
    is.na(v169b) ~ "no",
    v169b == "no" ~ "no",
    v169b == "yes" ~ "yes"))


  # HAS BANK ACCOUNT
  IR$has.bank <- IR$v170


  # USE OF INTERNET
  IR<-IR %>% dplyr::mutate(internet.use = case_when(
    v171a %in% c("never","yes, before last 12 months") ~ 0,
    v171a== "yes, last 12 months" ~ 1))


  ## CHILD HEALTH
  # DIARRHEA: RECIEVED MEDICAL TREAEMENT
  df1 <- IR %>%
    dplyr::select(caseid, starts_with("H11_")) %>%
    reshape2::melt(id.vars=c("caseid"), variable.name = "had_diarrhea", value.name = "had_diarrhea_resp") %>%
    dplyr::mutate(had_diarrhea = ifelse(had_diarrhea_resp == "yes, last two weeks", 1, 0)) %>%
    dplyr::filter(!is.na(had_diarrhea)) %>%
    group_by(caseid) %>%
    dplyr::summarize(had_diarrhea = max(had_diarrhea, na.rm = TRUE))

  df2 <- IR %>%
    dplyr::select(caseid, starts_with("H12y_")) %>%
    reshape2::melt(id.vars=c("caseid"), variable.name = "was_treated", value.name = "was_treated_resp") %>%
    dplyr::mutate(was_treated = ifelse(was_treated_resp == "no: received treatment", 1, 0)) %>%
    dplyr::filter(!is.na(was_treated)) %>%
    group_by(caseid) %>%
    dplyr::summarize(was_treated = max(was_treated, na.rm = TRUE))

  df3 <- df1 %>%
    base::merge(df2, by=c("caseid"), all.x=TRUE) %>%
    dplyr::mutate(diarrhea.medtreat = case_when(had_diarrhea==1 & was_treated==1 ~ "yes, child was treated",
                                         had_diarrhea==1 & was_treated==0 ~ "no, child wasn't treated",
                                         had_diarrhea==0 & is.na(was_treated) ~ "child didn't have diarrhea")) %>%
    dplyr::select(caseid, diarrhea.medtreat)

  IR <- IR %>%
    base::merge(df3, by="caseid", all.x=TRUE)

  # DIARRHEA: CS PUBLIC SECTOR
  # Only 4 "yes" responses
  #IR$diarrhea.treat.pub <- ifelse(IR$h12e_1=="yes" | IR$h12e_2=="yes" | IR$h12e_3=="yes",1,0)


  # FEVER/COUGH: CS PUBLIC SECTOR
  # Only 3 "yes" responses
  #IR$fever.treat.pub <- ifelse(IR$h32e_1=="yes" | IR$h32e_2=="yes" | IR$h32e_3=="yes",1,0)


  # SOURCE FOR IRON TABLETS: OTHER SOURCE - COMMUNITY WORKER
  # No "yes" responses
  # IR$irontablet.source.cw <- IR$s420aq_1


  # SOURCES OF INFORMATION ABOUT IRON ADVANTAGES - COMMUNITY WORKER
  # Only 2 "yes" responses
  # IR$ironinfo.source.cw <- IR$s421bx_1


  # GAVE CHILD FORTIFIED BABY FOOD (CERELAC, ETC)
  IR<-IR %>% dplyr::mutate(bf.fortified.food = case_when(
    v412a %in% c("no","don't know") ~ 0,
    v412a== "yes" ~ 1))


  # GAVE CHILD SWEET SNACKS
  IR<-IR %>% dplyr::mutate(bf.sweet.snacks = case_when(
    v414a %in% c("no","don't know") ~ 0,
    v414a== "yes" ~ 1))


  # GAVE CHILD MEAT (BEEF, PORK, LAMB, CHICKEN, ETC)
  IR<-IR %>% dplyr::mutate(bf.meat = case_when(
    v414h %in% c("no","don't know") ~ 0,
    v414h== "yes" ~ 1))


  # GAVE CHILD FOOD MADE FROM BEANS, PEAS, LENTILS
  IR<-IR %>% dplyr::mutate(bf.beans = case_when(
    v414o %in% c("no","don't know") ~ 0,
    v414o== "yes" ~ 1))


  # GAVE CHILD OTHER SOLID-SEMISOLID FOOD
  IR<-IR %>% dplyr::mutate(bf.other.solid = case_when(
    v414s %in% c("no","don't know") ~ 0,
    v414s== "yes" ~ 1))


  ## WOMAN CHARACTERISTICS
  # RELIGION
  IR$religion <- as.character(IR$v130)
  IR$religion[IR$religion=="chistiane"] <- 'christian'


  # MUSLIM RELIGION
  IR$muslim<-ifelse(IR$v130 %in% c("muslin", "muslim"), 1, 0)


  ## EDUCATION FACTORS
  # HIGHEST LEVEL OF EDUCATION
  IR$ed.level <- IR$v106


  # EDUCATION LEVEL MOH CAT 1
  IR <- IR %>%
    dplyr::mutate(ed.level.moh.cat1 = case_when(v106 %in% c("primary", "others", "higher", "secondary") ~ "education",
                                         v106 %in% c("no education") ~ "no education"))


  # EDUCATION LEVEL MOH CAT 2
  IR <- IR %>%
    dplyr::mutate(ed.level.moh.cat2 = case_when(v106 %in% c("higher", "secondary") ~ "higher education",
                                         v106 %in% c("no education", "primary", "others") ~ "some education"))


  # BINARY FACTOR FOR WOMAN'S EDUCATION
  IR$anyed.yn <- ifelse(IR$v149 == "no education", 0, 1)


  # HUSBAND/PARTNER'S EDUCATION LEVEL
  IR$partner.ed.level <- as.character(IR$v701)
  IR$partner.ed.level[IR$marr.cohab==0] <- 'not partnered'


  IR <- IR %>% dplyr::mutate(partner.anyed.yn = case_when(
    partner.ed.level == "no education" ~ "no",
    partner.ed.level %in% c("higher","primary","secondary") ~ "yes",
    partner.ed.level == "don't know" ~ "don't know",
    partner.ed.level == "not partnered" ~ "not partnered"))


  # HUSBAND/PARTNER'S EDUCATION LEVEL - MOH CAT 1
  IR <- IR %>%
    dplyr::mutate(partner.ed.level.moh.cat1 = case_when(v701 %in% c("primary", "others", "higher", "secondary") ~ "education",
                                                 v701 %in% c("no education") ~ "no education"))

  # HUSBAND/PARTNER'S EDUCATION LEVEL - MOH CAT 2
  IR <- IR %>%
    dplyr::mutate(partner.ed.level.moh.cat2 = case_when(v701 %in% c("higher", "secondary") ~ "higher education",
                                                 v701 %in% c("no education", "primary", "others") ~ "some education"))


  ## PROGRAM LANGUAGE
  IR <- IR %>% dplyr::mutate(pl.cat = case_when(
    (s111ba == "yes") ~ "Arabic",
    (s111bb == "yes") ~ "Wolof",
    (s111bc == "yes") ~ "Poular",
    (s111bd == "yes") ~ "Serer",
    (s111be == "yes") ~ "Diola",
    (s111bf == "yes") ~ "Mandinque",
    (s111bg == "yes") ~ "Soninke",
    (s111bx == "yes") ~ "other"))
  IR$pl.cat[(IR$s111ba == "no" & IR$s111bb == "no" & IR$s111bc =="no" & IR$s111bd =="no" &
               IR$s111be =="no" & IR$s111bf =="no" & IR$s111bg =="no" & IR$s111bx =="no")] <- "'No' to asked languages"


  ## PARTNERSHIP

  # EVER-MARRIED SAMPLE
  IR$ever.married <- IR$v020


  # AGE AT FIRST MARRIAGE/COHABITATION
  # IR$age.1stcohab <- IR$v511
  # IR$age.1stcohab[IR$v501] <- 'never partnered'

  IR <- IR %>%
    dplyr::mutate(age.1stcohab = case_when(v501 == "never partnered" ~ "never partnered",
                                           TRUE ~ as.character(v511)))


  #CATEGORICAL FACTOR FOR AGE AT FIRST MARRIAGE/COHABITATION
  IR<- IR %>% dplyr::mutate(age.1stcohab.cat = case_when(
    v501 == "never in union" ~ "never",
    (v511 > 0 & v511 < 15) ~ "5-14",
    (v511 >= 15 & v511 < 20) ~ "15-19",
    (v511 >= 20) ~ "20+"))


  ## REPRODUCTIVE HISTORY
  # AGE AT FIRST SEX (IMPUTED)
  IR$age.1stsex <- ifelse(IR$v531 == 97,NA,
                          ifelse(IR$v531 == 98, NA,
                                 ifelse(IR$v531 == 0, NA, as.numeric(IR$v531))))


  # CATEGORICAL FACTOR FOR AGE AT FIRST SEX
  IR<- IR %>%
    dplyr::mutate(age.1stsex.cat = case_when(v531 %in% c(0, "not had sex") ~ "never",
                                             (age.1stsex > 0 & age.1stsex < 15) ~ "5-14",
                                             (age.1stsex >= 15 & age.1stsex < 20) ~ "15-19",
                                             (age.1stsex >= 20 & age.1stsex < 50) ~ "20+",
                                             v531 %in% c(97, 98, "inconsistent", "don't know") ~ "Inconsistent or don't know"))


  # AGE AT FIRST BIRTH
  IR$age.1stbrth <- IR$v212


  # AGE AT FIRST BIRTH CATEGORY MOH GROUP A
  IR <- IR %>%
    dplyr::mutate(age.1stbrth.moh.cat1 = case_when(v212 < 20 ~ "<20",
                                            v212 >= 20 & v212 < 30 ~ "20-29",
                                            v212 >= 30  ~ "30+"))

  # AGE AT FIRST BIRTH CATEGORY MOH GROUP B
  IR <- IR %>%
    dplyr::mutate(age.1stbrth.moh.cat2 = case_when(v212 < 15 ~ "<15",
                                            v212 >= 15 & v212 < 20 ~ "15-19",
                                            v212 >= 20 & v212 < 25 ~ "20-24",
                                            v212 >= 25 & v212 < 40 ~ "25-39",
                                            v212 >= 40 ~ "40+"))


  # # CATEGORICAL FACTOR FOR AGE AT FIRST BIRTH
  # IR <- IR %>% dplyr::mutate(age.1stbrth.cat = case_when(
  #   is.na(v212) ~ "no births",
  #   (v212 > 0 & v212 < 15) ~ "5-14",
  #   (v212 >= 15 & v212 < 20) ~ "15-19",
  #   (v212 >= 20 & v212 < 50) ~ "20+"))


  # NUMBER OF PREGNANCIES
  IR$num.preg <- IR$v201
  IR$num.preg <- ifelse(IR$v228 == "yes" & IR$v234 == "yes", IR$v201 + 2,
                        ifelse(IR$v228 == "yes" & IR$v234 == "no", IR$v201 + 1,
                               IR$v201))


  # CATEGORICAL FACTOR FOR NUMBER OF PREGNANCIES
  IR$num.preg.cat <- case_when(IR$num.preg == 0 ~ "None",
                               IR$num.preg >0 & IR$num.preg < 4 ~ "1-3",
                               IR$num.preg >= 4 & IR$num.preg < 7 ~ "4-6",
                               IR$num.preg >= 7 ~ "7+",
                               is.na(IR$num.preg) ~ NA)


  # CATEGORICAL FACTOR FOR NUMBER OF PREGNANCIES - MOH
  IR <- IR %>%
    dplyr::mutate(num.preg.moh.cat = case_when(num.preg == 0 ~ "0",
                                               num.preg == 1 ~ "1",
                                               num.preg > 1 ~ "2+"))


  # TOTAL LIFETIME NUMBER OF SEX PARTNERS
  # IR$total.sex.partners <- IR$v836
  # IR$total.sex.partners[IR$total.sex.partners %in% c('95', '98', "don't know")] <- NA
  # IR$total.sex.partners <- case_when(
  #   IR$total.sex.partners == 1 ~ "1",
  #   IR$total.sex.partners == 2 ~ "2",
  #   IR$total.sex.partners >2  ~ "3+")

  IR <- IR %>%
    dplyr::mutate(total.sex.partners = case_when(v836 %in% c("98", "don't know") ~ NA,
                                                 v836 == "95+" ~ 95,
                                                 TRUE ~ as.numeric(v836))) %>%
    dplyr::mutate(total.sex.partners = case_when(total.sex.partners == 1 ~ "1",
                                                 total.sex.partners == 2 ~ "2",
                                                 total.sex.partners > 2 ~ "3+"))



  # WHO CHECKED RESPONDENT HEALTH AFTER DISCHARGE
  IR$discharge.checkedhealth <- as.character(IR$m68_1)
  IR$discharge.checkedhealth[IR$v201==0] <- 'no births'
  IR$discharge.checkedhealth <- case_when(
    IR$discharge.checkedhealth %in% c("midwife",'auxiliary midwife (matrone)') ~ 'midwife/aux midwife',
    IR$discharge.checkedhealth %in% c("doctor",'nurse') ~ 'doctor/nurse',
    IR$discharge.checkedhealth %in% c("other",'traditional birth attendant') ~ 'other/TBA',
    IR$discharge.checkedhealth == 'no births' ~ 'no births',
    is.na(IR$discharge.checkedhealth) ~ 'no one')


  # MOH CAT DISCHARGE CHECKED HEALTH
  IR <- IR %>%
    dplyr::mutate(discharge.checkedhealth.moh.cat = case_when(discharge.checkedhealth %in% c("other/TBA", "no one") ~ "TBA / no one",
                                                   discharge.checkedhealth %in% c("midwife/aux midwife", "doctor/nurse") ~ "midwife / doctor / nurse",
                                                   discharge.checkedhealth == "no births" ~ "no births"))



  # # MALE CHILD PREFERENCE
  # IR <- IR %>%
  #   dplyr::mutate(v627=as.numeric(v627),
  #                 v628=as.numeric(v628))


  # DISAGREEMENT ON CHILD PREFERENCE
  IR <- IR %>%
    dplyr::mutate(child.pref.discrep = case_when(v627 %in% c(96, "other") ~ NA,
                                                 v628 %in% c(96, "other") ~ NA,
                                                 v627 %in% c(0:30) & v628 %in% c(0:30) ~ as.numeric(v627) - as.numeric(v628)))

  IR <- IR %>%
    dplyr::mutate(male.child.pref = case_when(child.pref.discrep > 0 ~ 1,
                                              child.pref.discrep <= 0 ~ 0,
                                              v627 %in% c(96, "other") & v628 %in% c(96, "other") ~ 0))


  # FERTILITY PREFERENCE
  IR$fertility.pref <- as.character(IR$v602)
  IR$fertility.pref[IR$fertility.pref %in% c('sterilized (respondent or partner)','declared infecund')] <- 'sterilized/infecund'


  # PREFERRED WAITING TIME FOR BIRTH OF A/ANOTHER CHILD (GROUPED)
  IR$fertility.pref.temp <- as.character(IR$v604)
  IR$fertility.pref.temp[IR$fertility.pref.temp=='non-numeric'] <- NA
  IR$fertility.pref.temp[IR$v602!='have another'] <- 'No more'
  IR$fertility.pref.cat <- case_when(
    IR$fertility.pref.temp %in% c("<12 months", "1 year") ~ "<2 years",
    IR$fertility.pref.temp %in% c("2 years", "3 years","4 years") ~ "2-4 years",
    IR$fertility.pref.temp %in% c("5 years", "6+ years") ~ "5+ years",
    IR$fertility.pref.temp %in% c("No more") ~ "No more",
    IR$fertility.pref.temp %in% c("don't know") ~ "don't know")


  # IDEAL NUMBER OF CHILDREN CATEGORY
  IR <- IR %>%
    dplyr::mutate(ideal.n.child.moh.cat = case_when(v614 < 3 ~ "<3",
                                              v614 >= 3 & v614 <=5 ~ "3-5",
                                              v614 > 5 ~ "6+"))

  IR$ideal.n.child.cat <- case_when(
    IR$ideal.n.child %in% c('0',"1","2","3","4") ~ '0-4',
    IR$ideal.n.child %in% c('5',"6","More than 6") ~ '5+')
  IR$ideal.n.child.cat <- factor(IR$ideal.n.child.cat,levels=c("5+","0-4"))

  # HUSBAND'S DESIRE FOR CHILRDEN
  IR$partner.desire.child <- as.character(IR$v621)
  IR$partner.desire.child[IR$marr.cohab==0] <- 'not partnered'


  # NA-REDUCE RISK OF GETTING HIV: HAVE 1 SEX PARTNER
  # IR$hiv.risk.1partner <- IR$v754dp


  # CONDOM USED DURING LAST SEX WITH MOST RECENT PARTNER (WOMEN)
  IR$condom.last.sex <- IR$v761


  # CONDOM USED DURING LAST SEX WITH 2ND TO MOST RECENT PARTNER (WOMEN)
  IR$condom.2nd.last.sex <- IR$v761b


  # CONDOM USED DURING LAST SEX WITH 3RD TO MOST RECENT PARTNER (WOMEN)
  IR$condom.3rd.last.sex <- IR$v761c


  # SOURCE OF CONDOMS USED FOR LAST SEX
  IR$condom.source <- as.character(IR$v762)
  IR$condom.source[IR$condom.last.sex=='no'] <- 'condom not used'


  # NUMBER OF SEX PARTNERS, EXCLUDING SPOUSE, IN THE LAST 12 MONTHS
  IR$n.sex.excl.partner.12m <- IR$v766a


  # NUMBER OF SEX PARTNERS, INCLUDING SPOUSE, IN LAST 12 MONTHS
  IR$n.sex.incl.partner.12m <- IR$v766b


  # LAST 12 MONTHS HAD SEX IN RETURN FOR GIFTS, CASH, OTHER
  IR$sex.gifts.12m <- as.character(IR$v791a)
  IR$sex.gifts.12m[IR$n.sex.incl.partner.12m==0] <- 'no sex partners in past 12mon'


  # SOURCE OF FAMILY PLANNING FOR NON USERS: GOVERNMENTS RURAL MATERNITY
  IR <- IR %>% dplyr::mutate(source.fp.gov = case_when(
    v3a00e == "yes" ~ 1,
    v3a00e == "no" ~ 0))


  # # FP REASON FOR NOT USING
  # IR <- IR %>% dplyr::mutate(no.fp.reason = case_when(
  #   (v3a08a == "yes") ~ "not married",
  #   (v3a08e == "yes" | v3a08d == "yes") ~ "meno/infecund/ hysterectomy",
  #   (v3a08l == "yes") ~ "religious prohibition",
  #   (v3a08q == "yes") ~ "lack of access",
  #   (v3a08f == "yes" | v3a08g == "yes" | v3a08h == "yes" | v3a08p == "yes" |
  #      v3a08s == "yes" | v3a08t == "yes" | v3a08x == "yes") ~ "other",
  #   (v3a08u == "yes" | v3a08v == "yes") ~ "any or pref method unavailable",
  #   (v3a08j == "yes" | v3a08i=="yes") ~ "respondent or partner opposed",
  #   (v3a08m == "yes" | v3a08n == "yes") ~ "knows no method/source",
  #   (v3a08b == "yes" | v3a08c == "yes") ~ "not having/infrequent sex",
  #   (v3a08r == "yes") ~ "costs too much",
  #   (v3a08z == "yes: don't know") ~ "don't know"))

  #
  # IR <- IR1
  IR.fp <- IR %>%
    dplyr::select(caseid, starts_with("v3a08")) %>%
    reshape2::melt(id.vars=c("caseid")) %>%
    group_by(caseid) %>%
    dplyr::mutate(fp.all.na = ifelse(all(is.na(value)), 1, 0)) %>%
    reshape2::dcast(caseid + fp.all.na ~ variable) %>%
    dplyr::select(caseid, fp.all.na)

  IR <- IR %>%
    base::merge(IR.fp, by=c("caseid"))

  IR <- IR %>% dplyr::mutate(no.fp.access = case_when(
    (v3a08q == "yes" | v3a08r == "yes") ~ "yes",
    (v3a08q == "no" & v3a08r == "no" & fp.all.na==1) ~ "no",
    v361 != "currently using" ~ "not currently using",
    v361 == "currently using" ~ "currently using"))

  IR <- IR %>% dplyr::mutate(no.fp.oppose = case_when(
    (v3a08i == "yes" | v3a08j == "yes" | v3a08k == "yes" | v3a08l == "yes") ~ "yes",
    (v3a08i == "no" & v3a08j == "no" & v3a08k == "no" & v3a08l == "no" & fp.all.na==1) ~ "no",
    v361 != "currently using" ~ "not currently using",
    v361 == "currently using" ~ "currently using"))

  IR <- IR %>% dplyr::mutate(no.fp.noneed = case_when(
    (v3a08b == "yes" | v3a08d == "yes" | v3a08e == "yes" | v3a08f == "yes" | v3a08g == "yes") ~ "yes",
    (v3a08b == "no" & v3a08d == "no" & v3a08e == "no" & v3a08f == "no" & v3a08g == "no" & fp.all.na==1) ~ "no",
    v361 != "currently using" ~ "not currently using",
    v361 == "currently using" ~ "currently using"))

  IR <- IR %>% dplyr::mutate(no.fp.supply = case_when(
    (v3a08u == "yes" | v3a08v == "yes") ~ "yes",
    (v3a08u == "no" & v3a08v == "no" & fp.all.na==1) ~ "no",
    v361 != "currently using" ~ "not currently using",
    v361 == "currently using" ~ "currently using"))

  # test <- IR %>% dplyr::select(caseid, fp.all.na, no.fp.access, no.fp.oppose, no.fp.noneed, no.fp.supply)


  ## ATTITUDES ABOUT DOMESTIC VIOLENCE

  # BEATING JUSTIFIED IF WIFE GOES OUT WITHOUT TELLING HUSBAND
  IR <- IR %>% dplyr::mutate(dv.out = case_when(
    v744a =="no" ~0,
    v744a == "yes" ~1))


  # BEATING JUSITIFIED IF WIFE NEGLECTS CHILDREN
  IR <- IR %>% dplyr::mutate(dv.negkid = case_when(
    v744b =="no" ~0,
    v744b == "yes" ~1))


  # BEATING JUSTIFIED IF WIFE ARGUES WITH HUSBAND
  IR <- IR %>% dplyr::mutate(dv.argue = case_when(
    v744c =="no" ~0,
    v744c == "yes" ~1))


  # BEATING JUSTIFIED IF WIFE REFUSES TO HAVE SEX WITH HUSBAND
  IR <- IR %>% dplyr::mutate(dv.nosex = case_when(
    v744d =="no" ~0,
    v744d == "yes" ~1))


  # BEATING JUSTIFIED IF WIFE BURNS FOOD
  IR <- IR %>% dplyr::mutate(dv.burnfd = case_when(
    v744e =="no" ~0,
    v744e == "yes" ~1))


  # INDEX OF WOMAN'S ATTITUDES ABOUT DOMESTIC VIOLENCE
  IR <- IR  %>%
    rowwise() %>%
    dplyr::mutate(dv.nacnt = sum(is.na(dv.out),is.na(dv.negkid),is.na(dv.argue),is.na(dv.nosex),is.na(dv.burnfd)))

  IR <- IR %>%
    rowwise() %>%
    dplyr::mutate(dv.sum = sum(dv.out, dv.negkid, dv.argue, dv.nosex, dv.burnfd, na.rm=TRUE))

  IR$dv.index <-ifelse(IR$dv.nacnt == 5, NA, IR$dv.sum)
  IR$dv.index <-as.character(IR$dv.index)


  ## FEMALE GENITAL MUTILATION

  # FEMALE CIRCUMCISION
  IR <- IR %>% dplyr::mutate(female.circumcision = case_when(
    g101 == "no" ~0,
    g102 =="no" ~0,
    g102 == "yes" ~1))


  # BINARY FACTOR FOR FEMALE CIRCUMCISION
  #IR$female.circumcision.yn <- ifelse(IR$g102=="yes", 1, 0)


  # EVER HEARD OF GENITAL CUTTING (PROBED) (WOMEN) or female circ
  IR$know.genitalcut <- ifelse(IR$g101=='yes' | IR$g100=='yes', 1, 0)


  # FLESH REMOVED FROM GENITAL AREA
  IR$genitalflesh.removed <- as.character(IR$g103)
  IR$genitalflesh.removed <- ifelse(IR$g102=='no','never circumcised',IR$genitalflesh.removed)


  # GENITAL AREA JUST NICKED WITHOUT REMOVING ANY FLESH
  IR$genital.nicked <- as.character(IR$g104)
  IR$genital.nicked <- ifelse(IR$g102=='no','never circumcised',IR$genital.nicked)


  # GENITAL AREA SEWN UP
  IR$genital.sewn <- IR$g105
  IR$genital.sewn <- ifelse(IR$g105=='no','never circumcised',IR$genital.nicked)


  ## MEDIA EXPOSURE
  # NEWS: READS
  IR$freq.newsp <- as.character(IR$v157)
  # IR<-IR %>% dplyr::mutate(freq.newsp = case_when(
  #   (v157== "not at all" | is.na(v157)) ~ 0,
  #   v157=="at least once a week" ~2,
  #   v157=="less than once a week" ~1))
  # IR$freq.newsp <- factor(IR$freq.newsp)


  # BINARY FACTOR FOR NEWS: READS
  IR$newsp.yn <- ifelse((IR$v157== "not at all" | is.na(IR$v157)),0,1)


  # NEWS: RADIO
  IR$freq.rad <- as.character(IR$v158)
  # IR<-IR %>% dplyr::mutate(freq.rad = case_when(
  #   (v158== "not at all" | is.na(v158)) ~ 0,
  #   v158=="at least once a week" ~2,
  #   v158=="less than once a week" ~1))
  # IR$freq.rad <- factor(IR$freq.rad)


  # BINARY FACTOR FOR NEWS: RADIO
  IR$rad.yn<-ifelse((IR$v158== "not at all" | is.na(IR$v158)),0,1)


  # NEWS: TV
  IR$freq.tv <- as.character(IR$v159)
  # IR<-IR %>% dplyr::mutate(freq.tv = case_when(
  #   (v159=="not at all" | is.na(v159)) ~ 0,
  #   v159 == "at least once a week" ~2,
  #   v159 == "less than once a week" ~1))
  # IR$freq.tv <- factor(IR$freq.tv)


  # BINARY FACTOR FOR NEWS: TV
  IR$tv.yn<-ifelse((IR$v159=="not at all" | is.na(IR$v159)),0,1)


  # BINARY FACTOR FOR NEWS: ANY
  IR$any.media.yn <- ifelse((IR$newsp.yn == 1 | IR$rad.yn == 1 | IR$tv.yn == 1), 1, 0)


  # BIRTH WAS REGISTERED/DECLARED
  IR_brthreg <- IR %>%
    dplyr::select(caseid, starts_with("s428a_"), starts_with("s428b_")) %>%
    reshape2::melt(id.vars=c("caseid")) %>%
    dplyr::filter(!is.na(value)) %>%
    dplyr::mutate(birth.reg = ifelse(value %in% c("yes, seen","yes, not seen", "yes"), 1, 0)) %>%
    group_by(caseid) %>%
    dplyr::summarize(birth.reg.cnt = sum(birth.reg))
  IR_brthreg$birth.reg.yn<-ifelse(IR_brthreg$birth.reg.cnt > 0, 1, 0)


  IR <- IR %>%
    base::merge(IR_brthreg, by=c("caseid"), all.x=TRUE)
  IR$birth.reg.cnt[is.na(IR$birth.reg.cnt)] <- "no child <5"
  IR$birth.reg.yn[is.na(IR$birth.reg.yn)] <- "no child <5"


  #KNOW HOW TO REGISTER BIRTH
  IR_brthknow <- IR %>%
    dplyr::select(caseid, starts_with("s428c_")) %>%
    reshape2::melt(id.vars=c("caseid")) %>%
    dplyr::filter(!is.na(value)) %>%
    dplyr::mutate(birth.know = ifelse(value %in% c("no"), 1, 0)) %>%
    group_by(caseid) %>%
    dplyr::summarize(birth.know.cnt = sum(birth.know))
  IR_brthknow$birth.know.yn<-ifelse(IR_brthknow$birth.know.cnt > 0, 1, 0)

  IR <- IR %>%
    base::merge(IR_brthknow, by=c("caseid"), all.x=TRUE)

  IR <- IR %>% dplyr::mutate(birth.knowreg = case_when(
    birth.reg.yn == 1  ~ "yes",
    birth.reg.yn == 0 & birth.know.yn == 1 ~ "yes",
    birth.reg.yn == 0 & birth.know.yn == 0 ~ "no",
    birth.reg.yn == "no child <5" ~ "no child <5"))


  # REDEFINE 1/0 AS YES/NO
  IR <- IR %>%
    dplyr::mutate(birth.reg.yn = case_when(birth.reg.yn == 1 ~ "yes",
                                    birth.reg.yn == 0 ~ "no",
                                    TRUE ~ birth.reg.yn))


  ######################################################################
  ######################################################################
  # 3 | BR FILES

  # CHILD UNDER 15 LIVING ELSEWHERE (NOT IN HOUSEHOLD)
  BR$lives.away <- ifelse((BR$b9 %in% c("lives elsewhere", "someone else", "other relative", "father") & (BR$b8 %in% 1:14)), 1, 0)


  # NUMBER OF CHILDREN UNDER 15 LIVING ELSEWHERE
  BR <- BR %>%
    group_by(caseid) %>%
    dplyr::mutate(num.kids.away = sum(lives.away, na.rm = TRUE))


  # CATEGORICAL VARIABLE FOR NUMBER OF CHILDREN UNDER 15 LIVING ELSEWHERE
  # BR <- BR %>% dplyr::mutate(num.kids.away.cat = case_when(
  #   num.kids.away %in% c(0) ~ "0",
  #   num.kids.away %in% c(1) ~ "1",
  #   num.kids.away %in% c(2) ~ "2",
  #   num.kids.away >=3 ~ "3 or more"))


  # IN THE LAST 12 MONTHS GIVEN: LOCAL NAME FOR MULTIPLE MICRONUTRIENT POWDERS
  BR<-BR %>% dplyr::mutate(micronutrient.12m = case_when(
    h80a %in% c("no","don't know") ~ 0,
    h80a== "yes" ~ 1))


  # NUMBER OF TIMES ATE SOLID, SEMI-SOLID OR SOFT FOOD
  # BR$num.solidfood <- BR$m39

  BR <- BR %>%
    dplyr::mutate(num.solidfood = case_when(m39 %in% c(8, "don't know") ~ NA,
                                            m39 == "none" ~ 0,
                                            m39 == "7+" ~ 7,
                                            TRUE ~ as.numeric(m39)))


  ######################################################################
  ######################################################################
  # 4 | MR FILE

  # NUMBER OF WIVES/PARTNERS
  # MR$m.num.partners <- MR$mv035

  MR <- MR %>%
    dplyr::mutate(m.num.partners = case_when(mv035 == "no wives/partners" ~ 0,
                                             TRUE ~ as.numeric(mv035)))


  # AGE OF MALE RESPONDENT AT 1ST BIRTH
  MR$m.age.1stbirth <- MR$mv212


  # RECENT SEXUAL ACTIVITY
  MR$m.recent.sex <- MR$mv536


  # DESIRE FOR MORE CHILDREN
  MR$m.more.children <- MR$mv605
  MR<-MR %>% dplyr::mutate(m.recent.sex = case_when(
    mv605=="wants within 2 years" ~ "wants within 2 years",
    mv605 == "wants after 2+ years" ~ "wants in 2+ years",
    mv605 %in% c("wants, unsure timing", "undecided") ~ "unsure timing",
    mv605 %in% c("sterilized (respondent or partner(s))",
                 "declared infecund (respondent or partner(s))") ~ "infecund",
    mv605 == "wants no more" ~ "wants no more",
    mv605 == "never had sex" ~ "never had sex"))


  # PERSON WHO USUALLY DECIDES HOW TO SPEND RESPONDENT'S EARNINGS
  MR$m.men.dec.earnings <- ifelse(MR$mv739=="respondent alone",1,0)


  # TYPE OF EARNINGS FROM RESPONDENT'S WORK
  MR<-MR %>% dplyr::mutate(m.earnings.type = case_when(
    mv741 == "not paid" ~ "not paid",
    mv741 %in% c("cash only", "cash and in-kind") ~ "cash",
    mv741 == "in-kind only" ~ "in kind"))


  # PERSON WHO USUALLY MAKES DECISIONS ABOUT: RESPONDENT'S HEALTH CARE
  MR$m.men.dec.healthcare <- ifelse(MR$mv743a=="respondent alone",1,0)


  # PERSON WHO USUALLY MAKES DECISIONS ABOUT: LARGE HOUSEHOLD PURCHASES
  MR$m.men.dec.purchases <- ifelse(MR$mv743b=="respondent alone",1,0)


  # PAID FOR SEX IN LAST 12 MONTHS
  # Only 5 "yes" responses
  #MR$m.paid.sex.12m <- MR$mv793


  # CONDOM USED LAST TIME PAID FOR SEX IN LAST 12 MONTHS
  # Only 5 "yes" responses
  #MR$m.condom.last.paid.sex <- MR$mv793a


  # CONDOM USED EVERY TIME PAID FOR SEX IN LAST 12 MONTHS
  # Only 5 "yes" responses
  #MR$m.condom.every.paid.sex <- MR$mv793b


  # SEE OR READ FP INFORMATION FROM POSTERS OR PANELS
  MR$m.fp.posters <- MR$sm302e


  ######################################################################
  ######################################################################
  # 5 | CLEAN-UP

  # RENAME JOINING VARIABLES FROM HOUSEHOLD FILE
  HH$v001 <- HH$hv001
  HH$v002 <- HH$hv002


  # CALCULATE WT FOR INDIVIDUAL
  IR$wt <- IR$v005/1000000


  # PREP IR
  IR.seg <- IR %>%
    dplyr::filter(v501 != "never in union")


  # PREP HH
  HH.seg <- HH


  # PREP BR AND COLLAPSE TO ONE LINE PER WOMAN
  # BR.seg1 <- BR %>%
  #   dplyr::select(caseid, kids.under5, kids.under5.cat, kids.under15, kids.under15.cat, num.kids.away) %>%
  #   group_by(caseid) %>%
  #     dplyr::filter(row_number() == 1)


  BR.seg <- BR %>%
    group_by(survey, caseid) %>%
    dplyr::summarize(lives.away.cnt = ifelse(all(is.na(lives.away)), NA, sum(lives.away, na.rm=TRUE)),
                     micronutrient.12m.cnt = ifelse(all(is.na(micronutrient.12m)), NA, sum(micronutrient.12m, na.rm=TRUE)),
                     num.solidfood.cnt = ifelse(all(is.na(num.solidfood)), NA, sum(num.solidfood, na.rm=TRUE))) %>%
    dplyr::mutate(lives.away = ifelse(lives.away.cnt > 0, 1, 0),
           micronutrient.12m = ifelse(micronutrient.12m.cnt > 0, 1, 0),
           num.solidfood = ifelse(num.solidfood.cnt > 0, 1, 0)) %>%
    dplyr::select(survey, caseid, lives.away, micronutrient.12m, num.solidfood)


  # BR.seg <- BR.seg1 %>%
  #   base::merge(BR.seg2, by=c("caseid")) %>%
  #   group_by(caseid) %>%
  #   dplyr::filter(row_number() == 1)


  ##################################
  # PREP MR
  MR.seg <- MR %>%
    dplyr::select(survey, mcaseid, mv001, mv002, mv012, mv034_1, mv034_2, mv034_3, mv034_4, m.num.partners, m.age.1stbirth, m.recent.sex, m.more.children,
                  m.recent.sex, m.men.dec.earnings, m.men.dec.purchases, m.fp.posters)


  # HUSBANDS OF WOMEN IN IR
  MR.seg.ir <- MR.seg %>%
    dplyr::select(survey, mcaseid, mv001, mv002, mv034_1, mv034_2, mv034_3, mv034_4) %>%
    reshape2::melt(id.vars=c("survey", "mcaseid", "mv001", "mv002"), value.name = "v003") %>%
    dplyr::filter(!is.na(v003)) %>%
    base::merge(subset(MR.seg, select = -c(mv001, mv002, mv034_1, mv034_2, mv034_3, mv034_4)), by=c("survey", "mcaseid")) %>%
    group_by(mv001, mv002, v003) %>%
    arrange(variable) %>%
    dplyr::filter(row_number() == 1) %>%
    dplyr::select(-c(variable))


  # OLDEST MALE IN HH
  MR.seg.hh <- MR.seg %>%
    dplyr::filter(!mcaseid %in% MR.seg.ir$mcaseid) %>%
    subset(select = -c(mv034_1, mv034_2, mv034_3, mv034_4)) %>%
    group_by(mv001, mv002) %>%
    dplyr::mutate(oldest = max(mv012, na.rm=TRUE)) %>%
    dplyr::filter(mv012 == oldest) %>%
    dplyr::filter(row_number() == 1) %>%
    dplyr::select(-oldest)
  ##################################


  # MERGE TOGETHER WITH IR = MR
  vulnerability1 <- IR.seg %>%
    base::merge(BR.seg, by=c("survey", "caseid"), all.x=TRUE) %>%
    base::merge(HH.seg, by=c("survey", "v001", "v002"), all.x=TRUE) %>%
    base::merge(MR.seg.ir,
                by.x=c("survey", "v001", "v002", "v003"),
                by.y=c("survey", "mv001", "mv002", "v003"))


  # MERGE TOGETHER WITH HH = MR
  vulnerability2 <- IR.seg %>%
    base::merge(BR.seg, by=c("survey", "caseid"), all.x=TRUE) %>%
    base::merge(HH.seg, by=c("survey", "v001", "v002"), all.x=TRUE) %>%
    base::merge(MR.seg.hh,
                by.x=c("survey", "v001", "v002"),
                by.y=c("survey", "mv001", "mv002"),
                all.x=TRUE) %>%
    dplyr::filter(!caseid %in% vulnerability1$caseid) %>%
    dplyr::mutate(v003 = NA)

  vulnerability <- rbind(vulnerability1, vulnerability2)


  # CREATE SEGMENTATION STRATA
  vulnerability <- vulnerability %>%
    dplyr::mutate(strata = hv025)

  return(vulnerability)

}
