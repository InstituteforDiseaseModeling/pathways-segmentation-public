


################################################################################
# GENERATE VULNERABILITY FACTORS | DHS
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
# 1 | HH FILE
# 2 | IR FILE
# 3 | BR FILE
# 5 | MR FILE (OPTIONAL)
# 6 | CLEAN UP


###################################
# DEFINE FUNCTION


gen_vulnerability_factors_dhs <- function(IR=NULL, BR=NULL, HH=NULL, MR=NULL, dhs=7){


  ######################################################################
  # FILTER TO BIRTHS WITHIN THE LAST 10 YEARS
  # ALSO INCLUDES PREGNANCIES THAT TERMINATED
  BR <- BR %>%
    dplyr::filter(b3 >= (v008 - 120))

  IR <- IR %>%
    dplyr::filter(caseid %in% BR$caseid)


  ######################################################################
  ######################################################################
  # 1 | HH FILE

  # PLACE OF RESIDENCE
  HH <- HH %>% dplyr::mutate(hh.urban = case_when(
    hv025=="rural" ~ 0,
    hv025== "urban" ~ 1))


  # ELECTRICITY
  HH <- HH %>% dplyr::mutate(hh.electricity = case_when(
    hv206 == "yes" ~ 1,
    hv206 == "no" ~ 0))


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
  HH$hh.motortransport.yn <- ifelse(HH$hh.motor==1 | HH$hh.car==1,  1, 0)


  # COMPUTER
  HH <- HH %>% dplyr::mutate(hh.computer = case_when(
    hv243e == "yes" ~ 1,
    hv243e == "no" ~ 0))


  # OWN LAND USABLE FOR AGRICULTURE
  HH <- HH %>% dplyr::mutate(hh.land = case_when(
    hv244 == "yes" ~ 1,
    hv244 == "no" ~ 0))


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
                     num.under15 = sum(age_under15, na.rm = TRUE))

  HH <- HH %>%
    base::merge(HH_mem, by=c("hv001", "hv002"), all.x=TRUE)


  # CATEGORICAL FACTOR FOR HOUSEHOLD MEMBERS 15+
  HH <- HH %>%
    dplyr::mutate(num.15up.cat = case_when(
      num.15up >= 5 ~ "5+",
      num.15up < 5 ~ "<5"
  ))


  # Recode num.15up as binary
  HH$num.15up.4plus <- ifelse(HH$num.15up >= 4, 1, 0)


  # CATEGORICAL FACTOR FOR HOUSEHOLD MEMBERS UNDER 15
  HH <- HH %>% dplyr::mutate(num.under15.cat = case_when(
    num.under15 %in% c(0) ~ "0",
    num.under15 %in% c(1:2) ~ "1-2",
    num.under15 %in% c(3:4) ~ "3-4",
    num.under15 >=5 ~ "5 and up"))
  HH$num.under15.cat <- factor(HH$num.under15.cat,levels=c("5 and up","0","1-2","3-4"))


  # Number of biological children in the household
  IR <- IR %>%
    rowwise() %>%
    mutate(num.kids.house = sum(v202, v203, na.rm=TRUE))


  IR <- IR %>% mutate(num.kids.house.cat = case_when(
    num.kids.house %in% c(0) ~ "0",
    num.kids.house %in% c(1) ~ "1",
    num.kids.house %in% c(2,3) ~ "2-3",
    num.kids.house >=4 ~ "4 or more"))


  IR$num.kids.house.4plus <- ifelse(IR$num.kids.house.cat=="4 or more", 1, 0)


  # RATIO OF CHILDREN UNDER 5 TO WOMEN 15-49
  HH$hh.kidwom.rat<-ifelse(HH$hv010 == 0, 0, (HH$hv014/HH$hv010))


  # CATEGORICAL FACTOR FOR CHILDREN/WOMEN RATIO
  HH <- HH %>%
    dplyr::mutate(hh.kidwom.rat.cat = case_when(hh.kidwom.rat == 0 ~ 0,
                                                hh.kidwom.rat > 0 & hh.kidwom.rat < 1 ~ 1,
                                                hh.kidwom.rat >= 1 & hh.kidwom.rat < 2 ~ 2,
                                                hh.kidwom.rat >= 2 & hh.kidwom.rat < 3 ~ 3,
                                                TRUE ~ 4))



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
  HH$hh.cook.inside.yn <- ifelse(HH$hv241 == "in the house", 1, 0)


  # CLEAN COOKING FUEL
  HH$hh.clean.fuel <- ifelse(HH$hv226 %in% c("electricity","lpg","natural gas","biogas"), 1, 0)


  # NUMBER OF HOUSEHOLD ROOMS FOR SLEEPING
  HH$hh.rooms.num <- as.numeric(HH$hv216)


  # CATEGORICAL FACTOR OF NUMBER OF HOUSEHOLD ROOMS FOR SLEEPING
  HH <- HH %>% dplyr::mutate(hh.rooms.cat = case_when(
    hv216 == 1 ~ "1",
    hv216 == 2 ~ "2",
    hv216 == 3 ~ "3",
    hv216 >= 4 & !is.na(hv216) ~ "4+"))


  # BINARY FACTOR FOR TOILET TYPE: LATRINE
  HH$latrine <- ifelse(HH$hv205 %in% c("pit latrine without slab/ open pit", "no facility/bush/field",
                                       "bucket toilet", "hanging toilet/latrine", "other"), 1, 0)


  # TOILET FACILITY DOES NOT EXIST
  HH$no.latrine <- ifelse(HH$hv205 %in% c("no facility/bush/field", "no facility"), 1, 0)


  # LOCATION OF TOILET FACILITY
  HH <- HH %>% dplyr::mutate(latrine.loc = case_when(
    hv205 =="no facility/bush/field" ~ "No facility",
    hv238a == "in own dwelling" ~ "In own dwelling",
    hv238a == "in own yard/plot" ~ "In own yard/plot",
    hv238a == "elsewhere" ~ "Elsewhere"))


  # HOUSEHOLD HAS SHARED TOILET
  HH$hh.shared.latrine.yn <- ifelse(HH$hv225 == "yes", 1, 0)


  # NUMBER OF HOUSEHOLDS SHARING TOILET
  HH <- HH %>% dplyr::mutate(hh.num.sharelatrine = case_when(
    as.numeric(hv238) > 1 & as.numeric(hv238) < 10 ~ as.numeric(hv238),
    as.numeric(hv238) == 95 | hv238 == "10 or more households" ~ 10,
    hv225 == "no" ~ 1,
    hv225 == "don't know" ~ 1))


  # WHO DEFNITION IMPROVED NOT SHARED LATRINE
  HH$hh.noimp.latrine<-ifelse(HH$latrine==1 | (HH$latrine==1 & HH$hh.shared.latrine==1), 1, 0)


  # SOURCE OF DRINKING WATER
  HH$water <- ifelse(HH$hv201 %in% c("unprotected well", "unprotected spring", "river/dam/lake/ponds/ stream/canal/irrigation channel",
                                     "rainwater", "other"), 1, 0)


  # WATER SOURCE LOCATION
  HH <- HH %>%
    dplyr::mutate(hh.water.loc = case_when(hv201 %in% c("public tap/standpipe", "tube well or borehole", "protected well", "unprotected well", "unprotected spring",
                                                        "protected spring", "river/dam/lake/ponds/ stream/canal/irrigation channel", "rainwater", "tanker truck", "cart with small tank", "bottled water", "other") ~ hv235,
                                           hv201 == "piped into dwelling" ~ "in own dwelling",
                                           hv201 == "piped to yard/plot" ~ "in own yard/plot",
                                           hv201 == "piped to neighbor" ~ "elsewhere"))


  # PIPED WATER TYPE
  HH$hh.water.notpiped <- ifelse(HH$hv201== "piped into dwelling" | HH$hv201== "piped to yard/plot" | HH$hv201 == "piped to neighbor" |
                                 HH$hv201== "public tap/standpipe", 0, 1)


  # WATER TREATMENT: ANYTHING DONE TO MAKE WATER SAFE TO DRINK
  HH$hh.nowatpur <- ifelse(HH$hv237 == "yes", 0, 1)


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
  HH$hh.noimp.floor <- ifelse(HH$hv213 == "earth, sand" | HH$hv213== "dung" | HH$hv213== "wood planks" | HH$hv213== "palm, bamboo", 1, 0)


  ###################################
  # Urban slum - UN definition
  HH$floor <- ifelse(HH$hv213 == "earth, sand" | HH$hv213== "dung" | HH$hv213== "wood planks" | HH$hv213== "palm, bamboo", 1, 0)

  HH$wall <- ifelse(HH$hv214 == "no walls" | HH$hv214== "cane / palm / trunks" | HH$hv214== "dung / mud / sod" | HH$hv214== "grass" |
                    HH$hv214 == "bamboo with mud" | HH$hv214 == "stone with mud" | HH$hv214 == "uncovered adobe" |
                    HH$hv214 == "cardboard" | HH$hv214 == "reused wood" | HH$hv214 == "iron sheets", 1, 0)

  HH$members<-ifelse(HH$hv012 == 0, HH$hv013, HH$hv012)

  HH$memsleep <- ifelse(HH$hh.rooms.num == 0, HH$members, HH$members/HH$hh.rooms.num)

  HH$living <- ifelse(HH$memsleep > 3, 1, 0)

  HH <- HH %>%
    rowwise() %>%
    dplyr::mutate(slum.sum = sum(floor, water, latrine, living, na.rm=T))

  HH<-HH %>% mutate(slum1 = case_when(
    hv025=="rural" ~ "rural",
    (hv025 == "urban" & slum.sum >= 2) ~ "urban slum",
    (hv025 == "urban" & slum.sum < 2) ~ "urban non-slum"))

  # Urban slum 2 - Zulu et al, 2002
  HH$piped <- ifelse(HH$hv201 == "piped water" | HH$hv201== "piped into dwelling" | HH$hv201== "piped to yard/plot" | HH$hv201== "public tap/standpipe", 0, 1)
  HH$slum2 <- ifelse(HH$hh.urban == 1 & HH$hh.electricity == 0 & HH$latrine == 1 & HH$piped == 1, 1, 0)


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


  # NUMBER OF HOUSEHOLD MEMBERS
  HH$hh.members <- ifelse(HH$hv012 == 0, HH$hv013, HH$hv012)


  # SLEEPING ROOMS PER PERSON
  HH$hh.memsleep <- ifelse(HH$hh.rooms.num == 0, HH$hh.members, HH$hh.members/HH$hh.rooms.num)


  # WEATLH SUM INDEX
  HH <- HH %>%
    dplyr::mutate(wealth.sum = rowSums(across(c(hh.electricity, hh.tv, hh.refrig, hh.bank.acct, hh.computer, hh.car, hh.noimp.latrine))))


  # USE U/R WEALTH INDEX SINCE TO ALIGN WITH SEGMENTATION STRATIFICATION
  # # HOUSEHOLD COMBINED WEALTH INDEX
  # HH$wealth.index <- factor(HH$hv270)


  # URBAN/RURAL WEALTH INDEX
  HH$wealth.index.ur <- factor(HH$hv270a)


  # Binary urban/rural wealth index -
  HH$wealth.index.poor <- ifelse(HH$wealth.index.ur=="poorest", 1, 0)


  # Ordinal urban/rural wealth index
  #1 is poorest
  HH$wealth.index.ur.ord <- as.integer(HH$wealth.index.ur)


  ######################################################################
  ######################################################################
  # 2 | IR FILE


  # CURRENT PARTNERSHIP STATUS
  # CODE AS 1/0 TO EASILY HANDLE SKIP PATTERNS ACROSS DATASET
  IR$marr.cohab <- ifelse(IR$v501 == "married" | IR$v501 == "living with partner", 1, 0)


  # MARITAL CAT
  IR<-IR %>% dplyr::mutate(marital.cat = case_when(
    v501 == "never in union" ~ "never in union",
    v501 == "married" | v501 == "living with partner" ~ "married",
    v501 == "widowed" | v501 == "divorced" | v501 == "separated" ~ "widowed/ divorced/ separate"))


  # TYPE OF MARRIAGE, AMONG MARRIED WOMEN
  IR <- IR %>%
    dplyr::mutate(polygamy = case_when(v506 %in% c(1:20) ~ "Yes",
                                       marr.cohab == 0 ~ "not partnered",
                                       v506 %in% c(98, "don't know") ~ NA,
                                       TRUE ~ "No"))


  # WIFE ORDER AMONG WOMEN IN POLYGAMOUS UNION
  IR <- IR %>%
    dplyr::mutate(wife.order = case_when(v506 == 98 ~ NA,
                                         v506 == 1 ~ "1",
                                         v506 == 2 ~ "2",
                                         v506 >= 3 ~ "3+",
                                         marr.cohab == 0 ~ "not partnered",
                                         TRUE ~ as.character(v506)))


  # RESPONDENT IS FIRST WIFE
  IR <- IR %>%
    dplyr::mutate(first.wife = case_when(v506 == 1 ~ "First wife",
                                         v506 %in% c(2:20) ~ "Second or more wife",
                                         v506 %in% c(98, "don't know") ~ "Not known",
                                         !(v501 %in% c("married", "living with partner")) ~ "not partnered",
                                         v505 %in% c(0, "no other wives") ~ "Monomgaous union"))


  # ABILITY TO CONCEIVE
  IR <- IR %>%
    dplyr::mutate(infecund.meno = case_when(v625 == "infecund, menopausal" ~ 1,
                                            !is.na(v625) ~ 0))


  # HUSBAND/PARTNER OPPOSES FP USE
  IR <- IR %>%
    dplyr::mutate(fp.partner.oppose = case_when(v3a08j == "yes" ~ "Partner opposes",
                                                v3a08j == "no" ~ "Partner does not oppose",
                                                is.na(v3a08j) ~ "No identified need for FP"))


  ## BARRIERS TO HC
  # GETTING PERMISSION
  IR <- IR %>%
    dplyr::mutate(med.permis = case_when(v467b =="not a big problem" | v467b =="no problem" ~ 0,
                                         v467b == "big problem" ~ 1))


  # COST FOR NECESSARY TREATMENT
  IR <- IR %>%
    dplyr::mutate(med.cost = case_when(v467c =="not a big problem" | v467c =="no problem" ~ 0,
                                       v467c == "big problem" ~ 1))


  # DISTANCE TO HEALTH FACILITY
  IR <- IR %>%
    dplyr::mutate(med.dist = case_when(v467d =="not a big problem" | v467d =="no problem" ~ 0,
                                       v467d == "big problem" ~ 1))


  # TRANSPORTATION TO THE HEALTH FACILITY
  IR <- IR %>%
    dplyr::mutate(med.transp = case_when(v467e =="not a big problem" | v467e =="not a problem" ~ 0,
                                         v467e == "big problem" ~ 1))


  # DOESN'T WANT TO GO ALONE
  IR <- IR %>%
    dplyr::mutate(med.alone = case_when(v467f =="not a big problem"| v467f =="no problem" ~ 0,
                                        v467f == "big problem" ~ 1))


  # CONCERN THERE MAY NOT BE A FEMALE HEALTH PROVIDER
  IR <- IR %>%
    dplyr::mutate(med.fempro = case_when(v467g =="not a big problem" | v467g =="not a problem"  ~ 0,
                                         v467g == "big problem" ~ 1))


  # CONCERN THERE MAY NOT BE A PROVIDER
  IR <- IR %>%
    dplyr::mutate(med.nopro = case_when(v467h =="not a big problem"| v467g =="not a problem" ~ 0,
                                        v467h == "big problem" ~ 1))


  # CONCERN THERE MAY NOT BE MEDICATIONS
  IR <- IR %>%
    dplyr::mutate(med.nomeds = case_when(v467i =="not a big problem" | v467i =="not a problem" ~ 0,
                                         v467i == "big problem" ~ 1))


  # CREATE INDEX FROM BARRIERS TO HC VARIABLES
  IR<-IR %>%
    rowwise() %>%
    dplyr::mutate(med.sum = sum(med.permis, med.cost, med.dist, med.transp, med.alone, med.fempro, med.nopro, med.nomeds, na.rm=TRUE))

  IR$med.index <- IR$med.sum
  IR$med.index <- as.numeric(IR$med.index)

  # BINARY INDICATOR FROM INDEX
  IR$med.index.3plus <- ifelse(IR$med.index >= 3, 1, 0)


  # VISITED BY A HEALTHWORKER
  IR$hw.visit.12mo <- ifelse(IR$v393=="yes", 1, 0)


  # DID HEALTHWORKER TALK ABOUT FAMILY PLANNING
  IR <- IR %>%
    dplyr::mutate(hw.visit.fp = case_when(v393a == "yes" ~ "HW discussed FP",
                                          v393a == "no" ~ "HW did not discuss FP",
                                          is.na(v393a) ~ "No HW visit"))


  ## EMPLOYMENT FACTORS
  # RESPONDENT'S OCCUPATION
  IR$occupation <- IR$v717

  IR <- IR %>%
    dplyr::mutate(occupation.cat = case_when(
      occupation %in% c('professional/technical/ managerial','clerical') ~ 'professional/mangerial/ clerical',
      occupation %in% c('skilled manual','unskilled manual') ~ 'manual',
      occupation %in% c('services','household and domestic') ~ 'service/domestic',
      occupation %in% c('agricultural - self employed','agricultural - employee') ~ 'agricultural',
      occupation %in% c('sales') ~ 'sales',
      occupation %in% c('other') ~ 'other',
      occupation %in% c('not working') ~ 'not working'))


  # RESPONDENT WORKS FOR FAMILY, OTHERS, SELF-EMPLOYED
  IR <- IR %>% dplyr::mutate(occ.type = case_when(
    v719 == "for family member" ~ "Family",
    v719 == "for someone else" ~ "Someone else",
    v719 == "self-employed" ~ "Self",
    is.na(v719) ~ "Did not work"))


  # RESPONDENT CURRENTLY WORKS
  IR <- IR %>% mutate(working = case_when(
    (v714 == "yes" & (v714a != "yes" | is.na(v714a))) ~ "Currently working",
    (v714a == "yes") ~ "On leave/absent",
    (v731 == "no") ~ "No work in the past 12 months",
    (v731 == "in the past year") ~ "Worked in the past year"))

  IR$working.yn <- ifelse(IR$working=="Currently working" | IR$working=="Worked in the past year" | IR$working=="On leave/absent", "Yes", "No")

  IR$workingnow.yn <- ifelse(IR$working=="Currently working", 1, 0)


  # WORK SEASONALITY
  IR <- IR %>% dplyr::mutate(work.seasonal = case_when(
    v731 == "no" ~ "No work in past 12 months",
    v732 == "occasional" ~ "Occassional",
    v732 == "seasonal" ~ "Seasonal",
    v732 == "all year" ~ "All year"))


  # WORK SEASONALITY | BINARY
  IR <- IR %>% dplyr::mutate(work.seasonal2 = case_when(
    work.seasonal %in% c("Occasional", "No work in past 12 months", "All year") ~ "Not Seasonal",
    work.seasonal == "Seasonal" ~ "Seasonal"
  ))


  ###################################
  # HUSBAND/PARTNER WORKING 1
  IR <- IR %>% dplyr::mutate(part.working1 = case_when(
    v704a == "didn't work last 12 months" ~ "Didn't work last 12 months",
    v704a == "worked last 7 days" ~ "Worked last 7 days",
    v704a == "worked last 12 months" ~ "Worked last 12 months",
    v704a == "don't know" ~ "Don't know"))


  ###################################
  # HUSBAND/PARTNER WORKING 2
  # ACCOUNT FOR PARTNER STATUS SURVEY SKIP PATTERN
  IR <- IR %>% dplyr::mutate(part.working = case_when(
    !(v501 %in% c("married","living with partner"))  ~ "no partner",
    v704a == "didn't work last 12 months" ~ "Didn't work last 12 months",
    v704a == "worked last 7 days" ~ "Worked last 7 days",
    v704a == "worked last 12 months" ~ "Worked last 12 months",
    v704a == "don't know" ~ "Don't know"))


  IR <- IR %>% mutate(partner.working = case_when(
    is.na(v704a) ~ "No current partner",
    v704a == "didn't work last 12 months" ~ "Didn't work last 12 months",
    v704a == "worked last 7 days" ~ "Worked last 7 days",
    v704a == "worked last 12 months" ~ "Worked last 12 months",
    v704a == "don't know" ~ "Don't know"))


  IR$partner.working.yn <- ifelse(IR$partner.working %in% c("Worked last 7 days", "Worked last 12 months"), 1, 0)

  IR$partner.workingnow.yn <- ifelse(IR$partner.working %in% c("Worked last 7 days"), 1, 0)


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


  # NUMBER OF CHILDREN LIVING
  IR$num.child.alive <- as.numeric(IR$v218)


  # CATEGORICAL FACTOR FOR NUMBER OF LIVING CHILDREN
  IR <- IR %>% dplyr::mutate(num.child.alive.cat = case_when(
    v218 %in% c(0) ~ "0",
    v218 %in% c(1:2) ~ "1-2",
    v218 %in% c(3:5) ~ "3-5",
    (v218 >=6 & !is.na(v218)) ~ "6+"))


  # CHILDREN THAT HAVE DIED
  IR$num.child.die <- IR$v206 + IR$v207


  # CATEGORICAL FACTOR FOR NUMBER OF CHILDREN THAT HAVE DIED
  IR <- IR %>% dplyr::mutate(num.child.die.cat = case_when(
    num.child.die %in% c(0) ~ "0",
    num.child.die %in% c(1) ~ "1",
    num.child.die >=2 ~ "2 or more"))


  # NUMBER OF BIOLOGICAL CHILDREN IN THE HOUSEHOLD
  IR <- IR %>%
    rowwise() %>%
    dplyr::mutate(num.biochild.house = sum(v202, v203, na.rm=TRUE))


  # CATEGORICAL FACTOR FOR NUMBER OF BIOLOGICAL CHILDREN IN HOUSEHOLD
  IR <- IR %>% dplyr::mutate(num.biochild.house.cat = case_when(
    num.biochild.house %in% c(0) ~ "0",
    num.biochild.house %in% c(1) ~ "1",
    num.biochild.house %in% c(2,3) ~ "2-3",
    num.biochild.house >=4 ~ "4 or more"))


  # PARTNER DOES NOT LIVE IN THE HOUSEHOLD (AMONG MARRIED OR IN UNION)
  IR <- IR %>% dplyr::mutate(partner.absent = case_when(
    v504 == "living with her" ~ "No",
    v504 == "staying elsewhere" ~ "Yes",
    !(v501 %in% c("married", "living with partner")) ~ "not partnered"))


  # BINARY FACTOR FOR PARTNER IS ABSENT FROM HOUSEHOLD
  IR$partner.absent.yn <- ifelse(IR$partner.absent == "Yes", 1, 0)


  # SEX OF HEAD OF HOUSEHOLD
  IR$head.sex <- IR$v151


  if (all(c("d106", "d107") %in% names(IR))){
    ## VIOLENCE IN HOUSEHOLD
    # DOMESTIC VIOLENCE - PHYSICAL
    IR <- IR %>%
      dplyr::mutate(dv.physical = case_when(
      (d106 == "no" & d107 == "no") ~ 0,
      (d106 == "yes" | d107 == "yes") ~ 1))
  }


  if (all(c("d104") %in% names(IR))){
    # DOMESTIC VIOLENCE - EMOTIONAL
    IR <- IR %>%
      dplyr::mutate(dv.emotional = case_when(
      d104 == "yes" ~ 1,
      d104 == "no" ~ 0))
  }


  if (all(c("d108") %in% names(IR))){
    # DOMESTIC VIOLENCE - SEXUAL
    IR <- IR %>%
      dplyr::mutate(dv.sexual = case_when(
      d108 == "yes" ~ 1,
      d108 == "no" ~ 0))
  }


  if (all(c("d101a") %in% names(IR))){
    # HUSBAND/PARTNER JEALOUS IF RESPONDENT TALKS WITH OTHER MEN
    IR <- IR %>% dplyr::mutate(dv.jealous.othermen = case_when(
      d101a == "yes" ~ 1,
      d101a == "no" ~ 0))
  }


  if (all(c("d101c") %in% names(IR))){
    # HUSBAND/PARTNER DOES NOT PERMIT RESPONDENT TO MEET FEMALE FRIENDS
    IR <- IR %>% dplyr::mutate(dv.nofriends = case_when(
      d101c == "yes" ~ 1,
      d101c == "no" ~ 0))
  }


  if (all(c("d101d") %in% names(IR))){
    # HUSBAND/PARTNER TRIES TO LIMIT RESPONDENT'S CONTACT WITH FAMILY
    IR <- IR %>%
      dplyr::mutate(dv.nofamily.contact = case_when(
      d101d == "yes" ~ 1,
      d101d == "no" ~ 0))
  }


  if (all(c("d103a") %in% names(IR))){
    # EVER BEEN HUMILIATED BY HUSBAND/PARTNER
    IR$dv.humiliated <- as.character(IR$d103a)
  }


  if (all(c("d103b") %in% names(IR))){
    # EVER BEEN THREATENED WITH HARM BY HUSBAND/PARTNER
    IR$dv.threatened <- as.character(IR$d103b)
  }


  if (all(c("d103c") %in% names(IR))){
    # EVER BEEN INSULTED OR MADE TO FEEL BAD BY HUSBAND/PARTNER
    IR$dv.insulted <- as.character(IR$d103c)
  }


  if (all(c("d105a") %in% names(IR))){
    # EVER BEEN PUSHED, SHOOK, OR HAD SOMETHING THROWN BY HUSBAND/PARTNER
    IR$dv.pushed <- as.character(IR$d105a)
  }


  if (all(c("d105b") %in% names(IR))){
    # EVER BEEN SLAPPED BY HUSBAND/PARTNER
    IR$dv.slapped <- as.character(IR$d105b)
  }


  if (all(c("d105d") %in% names(IR))){
    # EVER BEEN KICKED OR DRAGGED BY HUSBAND/PARTNER
    IR$dv.kicked <- as.character(IR$d105d)
  }


  if (all(c("d105e") %in% names(IR))){
    # EVER BEEN STRANGLED OR BURNT BY HUSBAND/PARTNER
    IR$dv.strangled <- as.character(IR$d105e)
  }


  if (all(c("d105f") %in% names(IR))){
    # EVER BEEN ATTACKED WITH KNIFE/GUN OR OTHER WEAPON BY HUSBAND/PARTNER
    IR$dv.weapon <- as.character(IR$d105f)
  }


  if (all(c("d105h") %in% names(IR))){
    # EVER BEEN PHYSICALLY FORCED INTO UNWANTED SEX BY HUSBAND/PARTNER
    IR$dv.forcedsex1 <- as.character(IR$d105h)
  }


  if (all(c("d105i") %in% names(IR))){
    # EVER BEEN FORCED INTO OTHER UNWANTED SEXUAL ACTS BY HUSBAND/PARTNER
    IR$dv.forcedsex2 <- as.character(IR$d105i)
  }


  if (all(c("d105j") %in% names(IR))){
    # EVER HAD ARM TWISTED OR HAIR PULLED BY HUSBAND/PARTNER
    IR$dv.armtwist <- as.character(IR$d105j)
  }


  if (all(c("d105k") %in% names(IR))){
    # Ever been physically forced to perform sexual acts respondent didn't want to
    IR$dv.forcedsex3 <- as.character(IR$d105k)
  }


  if (all(c("d118a") %in% names(IR))){
  # HUSBAND/PARTNER: PERSON WHO HURT RESPONDENT DURING A PREGNANCY
  IR <- IR %>% dplyr::mutate(dv.hurtpreg.husband = case_when(
    d118a == "yes" ~ 1,
    d118a == "no" ~ 0))
  }


  if (all(c("d118j") %in% names(IR))){
  # FORMER PARTNER: PERSON WHO HURT RESPONDENT DURING A PREGNANCY
  IR <- IR %>% dplyr::mutate(dv.hurtpreg.formerp = case_when(
    d118j == "yes" ~ 1,
    d118j == "no" ~ 0))
  }


  if (all(c("d130a") %in% names(IR))){
    # PREVIOUS HUSBAND: EVER HIT, SLAP, KICK, OR PHYSICALLY HURT RESPONDENT
    IR$dv.prevparter.hurt <- as.character(IR$d130a)
  }


  if (all(c("d130b") %in% names(IR))){
    # PREVIOUS HUSBAND: PHYSICALLY FORCED TO HAVE SEX OR PERFORM SEXUAL ACTS
    IR$dv.prevparter.forcedsex <- as.character(IR$d130b)
  }


  if (all(c("d119xd", "d128") %in% names(IR))){
    # FRIEND: PERSON RESPONDENT WENT TO SEEK HELP
    IR <- IR %>% dplyr::mutate(dv.friend.help = case_when(
      d119xd == "yes" ~ "Yes",
      d119xd == "no" ~ "No",
      d128 == "no" ~ "Did not seek any help"))
  }


  if (all(c("d119h", "d128") %in% names(IR))){
    # OWN FAMILY: PERSON RESPONDENT WENT TO SEEK HELP
    IR <- IR %>% dplyr::mutate(dv.family.help = case_when(
      d119h == "yes" ~ "Yes",
      d119h == "no" ~ "No",
      d128 == "no" ~ "Did not seek any help"))
  }


  if (all(c("d119i", "d128") %in% names(IR))){
    # HUSBAND/PARTNER FAMILY: PERSON RESPONDENT WENT TO SEEK HELP
    IR <- IR %>% dplyr::mutate(dv.husband.help = case_when(
      d119i == "yes" ~ "Yes",
      d119i == "no" ~ "No",
      d128 == "no" ~ "Did not seek any help"))
  }


  if (all(c("d119j", "d128") %in% names(IR))){
    # CURRENT/FORMER HUSBAND/PARTNER: PERSON RESPONDENT WENT TO SEEK HELP
    IR <- IR %>% dplyr::mutate(dv.formerp.help = case_when(
      d119j == "yes" ~ "Yes",
      d119j == "no" ~ "No",
      d128 == "no" ~ "Did not seek any help"))
  }


  if (all(c("d119u", "d128") %in% names(IR))){
    # NEIGHBOR: PERSON RESPONDENT WENT TO SEEK HELP
    IR <- IR %>% dplyr::mutate(dv.neighbor.help = case_when(
      d119u == "yes" ~ "Yes",
      d119u == "no" ~ "No",
      d128 == "no" ~ "Did not seek any help"))
  }


  if (all(c("d119x", "d128") %in% names(IR))){
    # OTHER: PERSON RESPONDENT WENT TO SEEK HELP
    IR <- IR %>% dplyr::mutate(dv.other.help = case_when(
      d119x == "yes" ~ "Yes",
      d119x == "no" ~ "No",
      d128 == "no" ~ "Did not seek any help"))
  }


  if (all(c("d119xb", "d128") %in% names(IR))){
    # SOCIAL SERVICE ORGANIZATION: PERSON RESPONDENT WENT TO SEEK HELP
    IR <- IR %>% dplyr::mutate(dv.sso.help = case_when(
      d119xb == "yes" ~ "Yes",
      d119xb == "no" ~ "No",
      d128 == "no" ~ "Did not seek any help"))
  }


  if (all(c("d119xf", "d128") %in% names(IR))){
    # RELIGIOUS LEADER: PERSON RESPONDENT WENT TO SEEK HELP
    IR <- IR %>% dplyr::mutate(dv.religious.help = case_when(
      d119xf == "yes" ~ "Yes",
      d119xf == "no" ~ "No",
      d128 == "no" ~ "Did not seek any help"))
  }


  if (all(c("d128") %in% names(IR))){
    # EVER TOLD ANYONE ELSE ABOUT VIOLENCE
    IR <- IR %>%
      dplyr::mutate(dv.anyone.help = case_when(
        d128 == "yes" ~ "Yes",
        d128 == "no" ~ "No"))
  }


  ## DECISION-MAKING

  # JOINT DECISION: FINANCIAL
  IR <- IR %>%
    dplyr::mutate(desc.ownincome = case_when(
      marr.cohab == 0 ~ "not partnered",
      (v741 %in% c("not paid", "in-kind only") | v731 == "no") ~ "Not paid in cash or not working",
      !is.na(v739) ~ as.character(v739),
      TRUE ~ NA_character_
    ))

  IR$jd.ownincome <- ifelse(IR$v739== "respondent and husband/partner", 1, 0)

  # OWN DECISION: FINANCIAL
  IR$wd.ownincome <- ifelse(IR$v739== "respondent alone", 1, 0)

  # BINARY FACTOR FOR EITHER JOINT OR OWN DECISION: FINANCIAL
  IR$jdwd.ownincome <- ifelse(IR$v739== "respondent alone" | IR$v739=="respondent and husband/partner", 1, 0)


  # JOINT DECISION: LARGE HOUSEHOLD PURCHASES
  IR <- IR %>%
    dplyr::mutate(desc.lrgpur = case_when(marr.cohab == 0 ~ "not partnered",
                                          TRUE ~ as.character(v743b)))

  IR$jd.lrgpur <- ifelse(IR$v743b== "respondent and husband/partner", 1, 0)

  # OWN DECISION: LARGE HOUSEHOLD PURCHASES
  IR$wd.lrgpur <- ifelse(IR$v743b== "respondent alone", 1, 0)

  # # BINARY FACTOR FOR EITHER JOINT OR OWN DECISION: LARGE HOUSEHOLD PURCHASES
  IR$jdwd.lrgpur <- ifelse(IR$v743b== "respondent alone" | IR$v743b=="respondent and husband/partner", 1, 0)


  # JOINT DECISION: HUSBAND'S INCOME
  IR <- IR %>%
    dplyr::mutate(desc.money = case_when(marr.cohab == 0 ~ "not partnered",
                                         TRUE ~ as.character(v743d)))

  # OWN DECISION: HUSBAND'S INCOME
  IR$wd.money <- ifelse(IR$v743f== "respondent alone", 1, 0)

  # BINARY FACTOR FOR EITHER JOINT OR OWN DECISION: HUSBAND'S INCOME
  IR$jdwd.money <- ifelse(IR$v743f== "respondent alone" | IR$v743f=="respondent and husband/partner", 1, 0)


  # JOINT DECISION: HEALTH
  IR <- IR %>%
    dplyr::mutate(desc.hlth = case_when(marr.cohab == 0 ~ "not partnered",
                                        TRUE ~ as.character(v743a)))

  # OWN DECISION: HEALTH
  IR$wd.hlth <- ifelse(IR$v743a== "respondent alone", 1, 0)

  # BINARY FACTOR FOR EITHER JOINT OR OWN DECISION: HEALTH
  IR$jdwd.hlth <- ifelse(IR$v743a== "respondent alone" | IR$v743a=="respondent and husband/partner", 1, 0)


  # JOINT DECISION: FAMILY PLANNING
  IR <- IR %>%
    dplyr::mutate(desc.fp = case_when(
    v213 == "yes" ~ "currently pregnant",
    (v632 %in% c("joint decision") | v632a %in% c("joint decision")) ~ "respondent and husband/ partner",
    (v632 %in% c("mainly respondent") | v632a %in% c("mainly respondent")) ~ "respondent alone",
    (v632 %in% c("mainly husband, partner") | v632a %in% c("mainly husband, partner")) ~ "husband/ partner alone",
    (v632 %in% c("other") | v632a %in% c("other")) ~ "other",
    (marr.cohab %in% 0) ~ "not partnered"))

  # OWN DECISION: FAMILY PLANNING
  IR <- IR %>%
    dplyr::mutate(wd.fp = case_when(
    (v632 %in% c("mainly respondent") | v632a %in% c("mainly respondent")) ~ "Yes",
    ((!is.na(v632) & !(v632 == "mainly respondent")) | (!is.na(v632a) & !(v632a == "mainly respondent"))) ~ "No"))
  IR$wd.fp <- ifelse(IR$wd.fp=="Yes", 1, 0)


  # CATEGORICAL FACTOR FOR OWN DECISION: FAMILY PLANNING
  IR <- IR %>% dplyr::mutate(wd.fp.cat = case_when(
    (v632 %in% c("mainly respondent") | v632a %in% c("mainly respondent")) ~ "Yes",
    ((!is.na(v632) & !(v632 == "mainly respondent")) | (!is.na(v632a) & !(v632a == "mainly respondent"))) ~ "No",
    !(v501 %in% c("married", "living with partner")) ~ "Not in union",
    v213 == 1 ~ "Pregnant"))


  # BINARY FACTOR FOR EITHER JOINT OR OWN DECISION: FAMILY PLANNING
  IR <- IR %>% dplyr::mutate(jdwd.fp = case_when(
    v632 %in% c("joint decision") | v632a %in% c("joint decision") | v632 %in% c("mainly respondent") | v632a %in% c("mainly respondent") ~ "Yes",
    v632 %in% c("mainly husband, partner") | v632 %in% c("other") ~ "No",
    v632a %in% c("mainly husband, partner") | v632a %in% c("other") ~ "No"))
  IR$jdwd.fp <- ifelse(IR$jdwd.fp=='Yes', 1, 0)


  # JOINT DECISION: EVERYDAY DECISIONS
  IR <- IR %>%
    dplyr::mutate(desc.visit = case_when(marr.cohab == 0 ~ "not partnered",
                                         TRUE ~ as.character(v743d)))

  # OWN DECISION: EVERYDAY DECISIONS
  IR$wd.visit <- ifelse(IR$v743d== "respondent alone", 1, 0)

  # BINARY FACTOR FOR EITHER JOINT OR OWN DECISION: EVERYDAY DECISIONS
  IR$jdwd.visit <- ifelse(IR$v743d== "respondent alone" | IR$v743d=="respondent and husband/partner", 1, 0)


  # JOINT DECISON-MAKING INDEX
  IR$jd.index <- IR$desc.ownincome %in% 'respondent and husband/partner' +
    IR$desc.lrgpur %in% 'respondent and husband/partner' +
    IR$desc.money %in% 'respondent and husband/partner' +
    IR$desc.hlth %in% 'respondent and husband/partner' +
    IR$desc.fp %in% 'respondent and husband/partner' +
    IR$desc.visit %in% 'respondent and husband/partner'
  IR$jd.index <- ifelse(IR$marr.cohab==0, 'not partnered', IR$jd.index)


  # WOMEN'S DECISION MAKING INDEX
  IR$wd.index <- IR$desc.ownincome %in% 'respondent alone' +
    IR$desc.lrgpur %in% 'respondent alone' +
    IR$desc.money %in% 'respondent alone' +
    IR$desc.hlth %in% 'respondent alone' +
    IR$desc.fp %in% 'respondent alone' +
    IR$desc.visit %in% 'respondent alone'
  IR$wd.index <- ifelse(IR$marr.cohab==0, 'not partnered', IR$wd.index)


  # JOINT OR WOMAN DECISION MAKING INDEX
  IR <- IR %>%
    rowwise() %>%
    dplyr::mutate(jdwd.sum = sum(jdwd.hlth, jdwd.fp, jdwd.lrgpur, jdwd.visit, jdwd.money, jdwd.ownincome, na.rm=TRUE))


  IR$jdwd.index <- as.numeric(IR$jdwd.sum)


  IR$jdwd.index.5plus <- ifelse(IR$jdwd.index >= 5, 1, 0)


  # CATEGORICAL FACTOR FOR JOINT/WOMEN DECISION-MAKING INDEX
  IR <- IR %>%
    dplyr::mutate(jdwd.index.cat = case_when(
    marr.cohab == 0 ~ "Not partnered",
    jdwd.index == 0 ~ "None",
    jdwd.index %in% c(1:4) ~ "1-4",
    jdwd.index %in% c(5:6) ~ "5-6"
  ))


  # CATEGORICAL FACTOR FOR JOINT DECISION-MAKING INDEX
  IR <- IR %>%
    dplyr::mutate(jd.index.cat = case_when(
    jd.index == "not partnered" ~ "Not partnered",
    jd.index == 0 ~ "None",
    jd.index %in% c(1:3) ~ "1-3",
    jd.index %in% c(4:6) ~ "4-6"
  ))


  # CATEGORICAL FACTOR FOR WOMEN'S DECISION-MAKING INDEX
  IR <- IR %>% dplyr::mutate(wd.index.cat = case_when(
    marr.cohab == 0 ~ "Not partnered",
    wd.index == 0 ~ "None",
    wd.index %in% c(1:4) ~ "1-4",
    wd.index %in% c(5:6) ~ "5-6"
  ))


  ## PARTNER CHARACTERISTICS
  # PARTNER'S AGE
  IR <- IR %>%
    dplyr::mutate(partner.age = as.numeric(v730))


  # CATEGORICAL FACTOR FOR PARTNER'S AGE
  IR <- IR %>% dplyr::mutate(partner.age.cat = case_when(
    v730 %in% c(15:29) ~ "under 30",
    v730 %in% c(30:59) ~ "30-59",
    v730 %in% c(60:96) ~ "60+",
    v730 == 98 ~ "Don't know",
    v730 == 99 ~ "Missing",
    !(v501 %in% c("married", "living with partner")) ~ "not partnered"))


  # DIFFERENCE BETWEEN WOMAN AND PARTNER'S AGE
  IR <- IR %>%
    dplyr::mutate(age.diff = case_when(is.na(v730) | v730 > 96 ~ NA,
                                       TRUE ~ v730 - v012))


  # WOMAN HAD PREVIOUS PARTNERSHIPS
  IR <- IR %>% dplyr::mutate(prev.pship = case_when(
    v503 %in% c("once") ~ "No",
    v503 %in% c("more than once") ~ "Yes",
    v501 %in% c("never in union") ~ "Never in union"))


#Partnership by category AG 2/21/23
IR$pship.status <- (IR$v501)
IR<- IR %>% mutate(pship.cat = case_when
                   (
                     pship.status == "never in union" ~ "never",
                     pship.status == "widowed" ~ "no partner now",
                     pship.status == "divorced" ~ "no partner now",
                     pship.status == "no longer living together/separated" ~ "no partner now",
                     pship.status == "married" ~ "married/cohab",
                     pship.status == "living with partner" ~ "married/cohab"
                   )
)


  # RESPONDENT HAS TELEPHONE
  IR<-IR %>%
    dplyr::mutate(has.mobile = case_when(
      v169a=="no" ~ 0,
      v169a== "yes" ~ 1))


  # USE MOBILE PHONE FOR FINANCIAL TRANSACTIONS
  IR <- IR %>%
    dplyr::mutate(mobile.financial = case_when(
      v169b == "no" ~ 0,
      v169b == "yes" ~ 1))


  # HAS BANK ACCOUNT
  IR <- IR %>%
    dplyr::mutate(has.bank = case_when(
      v170 == "no" ~ 0,
      v170 == "yes" ~ 1))


  # USE OF INTERNET
  IR<-IR %>% dplyr::mutate(internet.use = case_when(
    v171a %in% c("never", "yes, before last 12 months") ~ 0,
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


  # GAVE CHILD FORTIFIED BABY FOOD (CERELAC, ETC)
  IR<-IR %>% dplyr::mutate(bf.fortified.food = case_when(
    v412a %in% c("no","don't know") ~ 0,
    v412a== "yes" ~ 1))


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
  IR <- IR %>%
    dplyr::mutate(religion = case_when(v130 == "chistiane" ~ "christian",
                                       v130 == "muslin" ~ "muslim",
                                       TRUE ~ as.character(v130)))


  # MUSLIM RELIGION
  IR$muslim <- ifelse(IR$religion %in% c("muslim", "islam"), 1, 0)


  #re-categorizing traditional, other and protestant together given extremely small sample sizes
  IR$religionrecode.cat <- IR$religion
  IR<- IR %>% mutate(religionrecode.cat =case_when(
    (religion=="traditional") ~ "other",
    (religion=="catholic") ~ "other",
    (religion=="other") ~ "other",
    (religion=="protestant") ~ "protestant",
    (religion=="orthodox") ~ "orthodox",
    (religion=="muslin") ~ "muslim"))

  IR$orthodox <- ifelse(IR$v130 =="orthodox", 1, 0)


  ## EDUCATION FACTORS
  # HIGHEST LEVEL OF EDUCATION
  IR$ed.level <- IR$v106


  # BINARY FACTOR FOR WOMAN'S EDUCATION
  IR$anyed.yn <- ifelse(IR$v149 == "no education", 0, 1)


  # HUSBAND/PARTNER'S EDUCATION LEVEL
  IR <- IR %>%
    dplyr::mutate(partner.ed.level = case_when(
      marr.cohab == 0 ~ "not partnered",
      TRUE ~ as.character(v701)))


  ###################################
  IR <- IR %>%
    dplyr::mutate(partner.ed.cat = case_when(
      partner.ed.level == "no education" ~ "no",
      partner.ed.level == "higher" ~ "higher",
      partner.ed.level == "primary" ~ "primary",
      partner.ed.level == "secondary" ~ "secondary",
      partner.ed.level == "don't know" ~ "don't know",
      partner.ed.level == "not partnered" ~ "not partnered"))


  ###################################
  IR <- IR %>%
    dplyr::mutate(partner.anyed.yn = case_when(
      partner.ed.level == "no education" ~ "no",
      partner.ed.level %in% c("higher","primary","secondary") ~ "yes",
      partner.ed.level == "don't know" ~ "don't know",
      partner.ed.level == "not partnered" ~ "not partnered"))


  # HUSBAND/PARTNER'S EDUCATION LEVEL - CAT 1
  IR <- IR %>%
    dplyr::mutate(partner.ed.level.cat1 = case_when(v701 %in% c("primary", "others", "higher", "secondary") ~ "education",
                                                    v701 %in% c("no education") ~ "no education"))


  # HUSBAND/PARTNER'S EDUCATION LEVEL - CAT 2
  IR <- IR %>%
    dplyr::mutate(partner.ed.level.cat2 = case_when(v701 %in% c("higher", "secondary") ~ "higher education",
                                                    v701 %in% c("no education", "primary", "others") ~ "some education"))


  ## PARTNERSHIP
  IR <- IR %>%
    dplyr::mutate(age.1stcohab = as.numeric(v511))


  #CATEGORICAL FACTOR FOR AGE AT FIRST MARRIAGE/COHABITATION
  IR<- IR %>%
    dplyr::mutate(age.1stcohab.cat = case_when(
      v501 == "never in union" ~ "never",
      (v511 > 0 & v511 < 15) ~ "5-14",
      (v511 >= 15 & v511 < 20) ~ "15-19",
      (v511 >= 20) ~ "20+"))


  ## REPRODUCTIVE HISTORY
  # AGE AT FIRST SEX (IMPUTED)
  IR <- IR %>%
    dplyr::mutate(age.1stsex = case_when(v531 %in% c(97, 98, 0) ~ NA,
                                         TRUE ~ as.numeric(v531)))


  ###################################
  # CATEGORICAL FACTOR FOR AGE AT FIRST SEX
  # ACCOUNTING FOR SKIP PATTERNS WHICH CREATE NA
  IR<- IR %>%
    dplyr::mutate(age.1stsex.cat = case_when(v531 %in% c(0, "not had sex") ~ "never",
                                             (age.1stsex > 0 & age.1stsex < 15) ~ "5-14",
                                             (age.1stsex >= 15 & age.1stsex < 20) ~ "15-19",
                                             (age.1stsex >= 20 & age.1stsex < 50) ~ "20+",
                                             v531 %in% c(97, 98, "inconsistent", "don't know") ~ "Inconsistent or don't know"))


  IR$early.sex.15 <- ifelse(IR$v531 >0 & IR$v531 <15, 1, 0)


  # AGE AT FIRST BIRTH
  IR$age.1stbrth <- as.numeric(IR$v212)


  # AGE AT FIRST BIRTH CATEGORY
  IR <- IR %>%
    dplyr::mutate(age.1stbrth.cat = case_when(age.1stbrth >= 19 ~ "19+",
                                              age.1stbrth < 19 ~ "<19"))


  # NUMBER OF PREGNANCIES
  IR <- IR %>%
    dplyr::mutate(num.preg = case_when(v228 == "yes" & v234 == "yes" ~ v201 + 2,
                                       v228 == "yes" & v234 == "no" ~ v201 + 1,
                                       TRUE ~ v201))


  # CATEGORICAL FACTOR FOR NUMBER OF PREGNANCIES
  IR$num.preg.cat <- case_when(IR$num.preg == 0 ~ "None",
                               IR$num.preg >0 & IR$num.preg < 4 ~ "1-3",
                               IR$num.preg >= 4 & IR$num.preg < 7 ~ "4-6",
                               IR$num.preg >= 7 ~ "7+",
                               is.na(IR$num.preg) ~ NA)


  IR <- IR %>%
    dplyr::mutate(total.sex.partners = case_when(v836 %in% c("98", "don't know") ~ NA,
                                                 v836 == "95+" ~ 95,
                                                 TRUE ~ as.numeric(v836)))


  IR <- IR %>%
    dplyr::mutate(total.sex.partners.cat = case_when(total.sex.partners == 1 ~ "1",
                                                     total.sex.partners == 2 ~ "2",
                                                     total.sex.partners > 2 ~ "3+"))


  # WHO CHECKED RESPONDENT HEALTH AFTER DISCHARGE
  IR <- IR %>%
    dplyr::mutate(discharge.checkedhealth = case_when(v201 == 0 ~ "no births",
                                                      m68_1 %in% c("midwife",'auxiliary midwife (matrone)') ~ 'midwife/aux midwife',
                                                      m68_1 %in% c("doctor",'nurse') ~ 'doctor/nurse',
                                                      m68_1 %in% c("other",'traditional birth attendant') ~ 'other/TBA',
                                                      m68_1 == 'no births' ~ 'no births',
                                                      is.na(IR$m68_1) ~ 'no one'))


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
  IR <- IR %>%
    dplyr::mutate(fertility.pref = case_when(v602 %in% c('sterilized (respondent or partner)','declared infecund') ~ "sterilized/infecund",
                                             TRUE ~ v602))


  # PREFERRED WAITING TIME FOR BIRTH OF A/ANOTHER CHILD (GROUPED)
  IR <- IR %>%
    dplyr::mutate(fertility.pref.cat = case_when(v604 == "non-numeric" ~ NA,
                                                 v602 != "have another" ~ "No more",
                                                 v604 %in% c("<12 months", "1 year") ~ "<2 years",
                                                 v604 %in% c("2 years", "3 years","4 years") ~ "2-4 years",
                                                 v604 %in% c("5 years", "6+ years") ~ "5+ years",
                                                 v604 %in% c("don't know") ~ "don't know"))


  # IDEAL NUMBER OF CHILDREN CATEGORY
  IR <- IR %>%
    dplyr::mutate(ideal.n.child.moh.cat = case_when(v614 < 3 ~ "<3",
                                                    v614 >= 3 & v614 <=5 ~ "3-5",
                                                    v614 > 5 ~ "6+"))


  IR <- IR %>%
    dplyr::mutate(ideal.n.child.cat = case_when(v614 %in% c(0:4) ~ '0-4',
                                                v614 > 5 ~ '5+'))



  # HUSBAND'S DESIRE FOR CHILRDEN
  IR <- IR %>%
    dplyr::mutate(partner.desire.child = case_when(marr.cohab == 0 ~ "not partnered",
                                                   TRUE ~ as.character(v621)))


  # CONDOM USED DURING LAST SEX WITH MOST RECENT PARTNER (WOMEN)
  # IR$condom.last.sex <- IR$v761
  IR <- IR %>%
    dplyr::mutate(condom.last.sex = case_when(v761 == "no" ~ 0,
                                              v761 == "yes" ~ 1,
                                              v761 == "don't know" ~ NA))


  # CONDOM USED DURING LAST SEX WITH 2ND TO MOST RECENT PARTNER (WOMEN)
  # IR$condom.2nd.last.sex <- IR$v761b
  IR <- IR %>%
    dplyr::mutate(condom.2nd.last.sex = case_when(v761b == "no" ~ 0,
                                                  v761b == "yes" ~ 1,
                                                  v761b == "don't know" ~ NA))


  # CONDOM USED DURING LAST SEX WITH 3RD TO MOST RECENT PARTNER (WOMEN)
  # IR$condom.3rd.last.sex <- IR$v761c
  IR <- IR %>%
    dplyr::mutate(condom.3rd.last.sex = case_when(v761c == "no" ~ 0,
                                                  v761c == "yes" ~ 1,
                                                  v761c == "don't know" ~ NA))


  # SOURCE OF CONDOMS USED FOR LAST SEX
  IR <- IR %>%
    dplyr::mutate(condom.source = case_when(condom.last.sex == 0 ~ "condom not used",
                                            TRUE ~ as.character(v762)))


  # NUMBER OF SEX PARTNERS, INCLUDING SPOUSE, IN LAST 12 MONTHS
  IR <- IR %>%
    dplyr::mutate(n.sex.incl.partner.12m = case_when(v766b %in% c(95, 98) ~ NA,
                                                     TRUE ~ as.numeric(v766b)))


  # LAST 12 MONTHS HAD SEX IN RETURN FOR GIFTS, CASH, OTHER
  IR <- IR %>%
    dplyr::mutate(sex.gifts.12m = case_when(n.sex.incl.partner.12m == 0 ~ "no sex partners in past 12m",
                                            TRUE ~ as.character(v791a)))


  # SOURCE OF FAMILY PLANNING FOR NON USERS: GOVERNMENTS RURAL MATERNITY
  IR <- IR %>% dplyr::mutate(source.fp.gov = case_when(
    v3a00e == "yes" ~ 1,
    v3a00e == "no" ~ 0))


  # FP MEASURES
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


  ## ATTITUDES ABOUT DOMESTIC VIOLENCE

  # BEATING JUSTIFIED IF WIFE GOES OUT WITHOUT TELLING HUSBAND
  IR <- IR %>% dplyr::mutate(dv.out = case_when(
    v744a =="no" ~ 0,
    v744a == "yes" ~ 1))


  # BEATING JUSITIFIED IF WIFE NEGLECTS CHILDREN
  IR <- IR %>% dplyr::mutate(dv.negkid = case_when(
    v744b =="no" ~ 0,
    v744b == "yes" ~ 1))


  # BEATING JUSTIFIED IF WIFE ARGUES WITH HUSBAND
  IR <- IR %>% dplyr::mutate(dv.argue = case_when(
    v744c =="no" ~ 0,
    v744c == "yes" ~ 1))


  # BEATING JUSTIFIED IF WIFE REFUSES TO HAVE SEX WITH HUSBAND
  IR <- IR %>% dplyr::mutate(dv.nosex = case_when(
    v744d =="no" ~ 0,
    v744d == "yes" ~ 1))


  # BEATING JUSTIFIED IF WIFE BURNS FOOD
  IR <- IR %>% dplyr::mutate(dv.burnfd = case_when(
    v744e =="no" ~ 0,
    v744e == "yes" ~ 1))


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

  if (all(c("g102") %in% names(IR))){
    # FEMALE CIRCUMCISION
    IR <- IR %>% dplyr::mutate(female.circumcision = case_when(
      g101 == "no" ~ 0,
      g102 =="no" ~ 0,
      g102 == "yes" ~ 1))
  }


  if (all(c("g101", "g100") %in% names(IR))){
    # EVER HEARD OF GENITAL CUTTING (PROBED) (WOMEN) OR FEMALE CIRC
    IR$know.genitalcut <- ifelse(IR$g101=='yes' | IR$g100=='yes', 1, 0)
  }


  if (all(c("g102", "g103") %in% names(IR))){
    # FLESH REMOVED FROM GENITAL AREA
    IR <- IR %>%
      dplyr::mutate(genitalflesh.removed = case_when(g102 == "no" ~ "never circumcised",
                                                     TRUE ~ as.character(g103)))
  }


  if (all(c("g102", "g104") %in% names(IR))){
    # GENITAL AREA JUST NICKED WITHOUT REMOVING ANY FLESH
    IR <- IR %>%
      dplyr::mutate(genital.nicked = case_when(g102 == "no" ~ "never circumcised",
                                               TRUE ~ as.character(g104)))
  }


  if (all(c("g102", "g105") %in% names(IR))){
    # GENITAL AREA SEWN UP
    IR <- IR %>%
      dplyr::mutate(genital.sewn = case_when(g102 == "no" ~ "never circumcised",
                                             TRUE ~ as.character(g105)))
  }


  ## MEDIA EXPOSURE
  # NEWS: READS
  IR$freq.newsp <- as.character(IR$v157)

  # BINARY FACTOR FOR NEWS: READS
  IR$newsp.yn <- ifelse((IR$v157== "not at all" | is.na(IR$v157)), 0, 1)


  # NEWS: RADIO
  IR$freq.rad <- as.character(IR$v158)

  # BINARY FACTOR FOR NEWS: RADIO
  IR$rad.yn<-ifelse((IR$v158== "not at all" | is.na(IR$v158)), 0, 1)


  # NEWS: TV
  IR$freq.tv <- as.character(IR$v159)

  # BINARY FACTOR FOR NEWS: TV
  IR$tv.yn<-ifelse((IR$v159=="not at all" | is.na(IR$v159)), 0, 1)


  # BINARY FACTOR FOR NEWS: ANY
  IR$any.media.yn <- ifelse((IR$newsp.yn == 1 | IR$rad.yn == 1 | IR$tv.yn == 1), 1, 0)


  ######################################################################
  ######################################################################
  # 3 | BR FILES

  # CHILD UNDER 15 LIVING ELSEWHERE (NOT IN HOUSEHOLD)
  BR$lives.away <- ifelse((BR$b9 %in% c("lives elsewhere", "someone else", "other relative", "father") & (BR$b8 %in% 1:14)), 1, 0)


  # NUMBER OF CHILDREN UNDER 15 LIVING ELSEWHERE
  BR <- BR %>%
    group_by(caseid) %>%
    dplyr::mutate(num.kids.away = sum(lives.away, na.rm = TRUE))


  # IN THE LAST 12 MONTHS GIVEN: LOCAL NAME FOR MULTIPLE MICRONUTRIENT POWDERS
  BR<-BR %>%
    dplyr::mutate(micronutrient.12m = case_when(
      h80a %in% c("no","don't know") ~ 0,
      h80a== "yes" ~ 1))


  # NUMBER OF TIMES ATE SOLID, SEMI-SOLID OR SOFT FOOD
  BR <- BR %>%
    dplyr::mutate(num.solidfood = case_when(m39 %in% c(8, "don't know") ~ NA,
                                            m39 == "none" ~ 0,
                                            m39 == "7+" ~ 7,
                                            TRUE ~ as.numeric(m39)))


  ######################################################################
  ######################################################################
  # 4 | MR FILE

  # # NUMBER OF WIVES/PARTNERS
  # MR <- MR %>%
  #   dplyr::mutate(m.num.partners = case_when(mv035 == "no wives/partners" ~ 0,
  #                                            TRUE ~ as.numeric(mv035)))
  #
  #
  # # AGE OF MALE RESPONDENT AT 1ST BIRTH
  # MR$m.age.1stbirth <- MR$mv212
  #
  #
  # # RECENT SEXUAL ACTIVITY
  # MR$m.recent.sex <- MR$mv536
  #
  #
  # # DESIRE FOR MORE CHILDREN
  # MR$m.more.children <- MR$mv605
  # MR<-MR %>% dplyr::mutate(m.recent.sex = case_when(
  #   mv605 == "wants within 2 years" ~ "wants within 2 years",
  #   mv605 == "wants after 2+ years" ~ "wants in 2+ years",
  #   mv605 %in% c("wants, unsure timing", "undecided") ~ "unsure timing",
  #   mv605 %in% c("sterilized (respondent or partner(s))",
  #                "declared infecund (respondent or partner(s))") ~ "infecund",
  #   mv605 == "wants no more" ~ "wants no more",
  #   mv605 == "never had sex" ~ "never had sex"))
  #
  #
  # # PERSON WHO USUALLY DECIDES HOW TO SPEND RESPONDENT'S EARNINGS
  # MR$m.men.dec.earnings <- ifelse(MR$mv739=="respondent alone", "Yes", "No")
  #
  #
  # # TYPE OF EARNINGS FROM RESPONDENT'S WORK
  # MR<-MR %>% dplyr::mutate(m.earnings.type = case_when(
  #   mv741 == "not paid" ~ "not paid",
  #   mv741 %in% c("cash only", "cash and in-kind") ~ "cash",
  #   mv741 == "in-kind only" ~ "in kind"))
  #
  #
  # # PERSON WHO USUALLY MAKES DECISIONS ABOUT: RESPONDENT'S HEALTH CARE
  # MR$m.men.dec.healthcare <- ifelse(MR$mv743a=="respondent alone", "Yes", "No")
  #
  #
  # # PERSON WHO USUALLY MAKES DECISIONS ABOUT: LARGE HOUSEHOLD PURCHASES
  # MR$m.men.dec.purchases <- ifelse(MR$mv743b=="respondent alone", "Yes", "No")
  #
  #
  # # SEE OR READ FP INFORMATION FROM POSTERS OR PANELS
  # MR$m.fp.posters <- MR$sm302e


  ######################################################################
  ######################################################################
  # 5 | CLEAN-UP

  # RENAME JOINING VARIABLES FROM HOUSEHOLD FILE
  HH$v001 <- HH$hv001
  HH$v002 <- HH$hv002


  # CALCULATE WT FOR INDIVIDUAL
  IR$wt <- IR$v005/1000000


  # IR
  IR.seg <- IR


  # HH
  HH.seg <- HH


  BR.seg <- BR %>%
    group_by(survey, caseid) %>%
    dplyr::summarize(lives.away.cnt = ifelse(all(is.na(lives.away)), NA, sum(lives.away, na.rm=TRUE)),
                     micronutrient.12m.cnt = ifelse(all(is.na(micronutrient.12m)), NA, sum(micronutrient.12m, na.rm=TRUE)),
                     num.solidfood.cnt = ifelse(all(is.na(num.solidfood)), NA, sum(num.solidfood, na.rm=TRUE))) %>%
    dplyr::mutate(lives.away = ifelse(lives.away.cnt > 0, 1, 0),
                  micronutrient.12m = ifelse(micronutrient.12m.cnt > 0, 1, 0),
                  num.solidfood = ifelse(num.solidfood.cnt > 0, 1, 0)) %>%
    dplyr::select(survey, caseid, lives.away, micronutrient.12m, num.solidfood)


  ##################################
  # PREP MR
  # MR.seg <- MR %>%
  #   dplyr::select(survey, mcaseid, mv001, mv002, mv012, mv034_1, mv034_2, mv034_3, mv034_4, m.num.partners, m.age.1stbirth, m.recent.sex, m.more.children,
  #                 m.recent.sex, m.men.dec.earnings, m.men.dec.purchases, m.fp.posters)


  # # HUSBANDS OF WOMEN IN IR
  # MR.seg.ir <- MR.seg %>%
  #   dplyr::select(survey, mcaseid, mv001, mv002, mv034_1, mv034_2, mv034_3, mv034_4) %>%
  #   reshape2::melt(id.vars=c("survey", "mcaseid", "mv001", "mv002"), value.name = "v003") %>%
  #   dplyr::filter(!is.na(v003)) %>%
  #   base::merge(subset(MR.seg, select = -c(mv001, mv002, mv034_1, mv034_2, mv034_3, mv034_4)), by=c("survey", "mcaseid")) %>%
  #   group_by(mv001, mv002, v003) %>%
  #   arrange(variable) %>%
  #   dplyr::filter(row_number() == 1) %>%
  #   dplyr::select(-c(variable))


  # # OLDEST MALE IN HH
  # MR.seg.hh <- MR.seg %>%
  #   dplyr::filter(!mcaseid %in% MR.seg.ir$mcaseid) %>%
  #   subset(select = -c(mv034_1, mv034_2, mv034_3, mv034_4)) %>%
  #   group_by(mv001, mv002) %>%
  #   dplyr::mutate(oldest = max(mv012, na.rm=TRUE)) %>%
  #   dplyr::filter(mv012 == oldest) %>%
  #   dplyr::filter(row_number() == 1) %>%
  #   dplyr::select(-oldest)
  ##################################


  # MERGE TOGETHER WITH IR = MR
  vulnerability1 <- IR.seg %>%
    base::merge(BR.seg, by=c("survey", "caseid")) %>%
    base::merge(HH.seg, by=c("survey", "v001", "v002"), all.x=TRUE) #%>%
    # base::merge(MR.seg.ir,
    #             by.x=c("survey", "v001", "v002", "v003"),
    #             by.y=c("survey", "mv001", "mv002", "v003"))


  # # MERGE TOGETHER WITH HH = MR
  # vulnerability2 <- IR.seg %>%
  #   base::merge(BR.seg, by=c("survey", "caseid"), all.x=TRUE) %>%
  #   base::merge(HH.seg, by=c("survey", "v001", "v002"), all.x=TRUE) %>%
  #   base::merge(MR.seg.hh,
  #               by.x=c("survey", "v001", "v002"),
  #               by.y=c("survey", "mv001", "mv002"),
  #               all.x=TRUE) %>%
  #   dplyr::filter(!caseid %in% vulnerability1$caseid) %>%
  #   dplyr::mutate(v003 = NA)

  # vulnerability <- rbind(vulnerability1, vulnerability2)


  # CREATE SEGMENTATION STRATA
  vulnerability <- vulnerability1 %>%
    dplyr::mutate(strata = hv025)

  return(vulnerability)

}
