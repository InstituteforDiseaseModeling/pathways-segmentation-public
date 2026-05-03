
################################################################################
# GENERATE VULNERABILITY FACTORS | DHS
################################################################################

###################################
# RUN SETUP

###################################
# TABLE OF CONTENTS
# 1 | HH FILE
# 2 | IR FILE
# 3 | BR FILE
# 5 | MR FILE
# 6 | CLEAN UP


gen_vulnerability_factors_dhs <- function(IR=NULL, BR=NULL, HH=NULL, KR=NULL, MR = NULL, PR = NULL, dhs=8){


  ######################################################################
  # FILTER TO BIRTHS WITHIN THE LAST 10 YEARS
  # ALSO INCLUDES PREGNANCIES THAT TERMINATED
  BR <- BR %>%
    dplyr::filter(b3 >= (v008 - 120))

  IR <- IR %>%
    dplyr::filter(caseid %in% BR$caseid)
  # dim(IR) #15220  6402
  # summary(IR$v012) #age range still 15-49

  # table(PR$hv102)
  # table(HH$hv102_02)

  PR <- PR %>%
    dplyr::filter(hv103=="yes",
                  hv105>=5) #De facto population age 5 or above (align with DHS report sample for disability)
  # dim(PR)


  ######################################################################
  ######################################################################
  # 1 | HH FILE ----

  # In this survey no questions were asked on hh.internet or hh.stove

  HH$hh.cupboard = case_when(HH$sh132k == "no" ~ 0, HH$sh132k == "yes" ~ 1)

  ## LIVING CONDITIONS ----
  ### SOURCE OF DRINKING WATER ----
  HH$water <- ifelse(HH$hv201 %in% c("unprotected well", "unprotected spring", "river/dam/lake/ponds/stream/canal/irrigation channel",
                                     "rainwater", "other"), 1, 0)
  # table(HH$hv201, useNA = "always") #water refers to open unprotected water

  ### DRINKING WATER NOT SUFFICIENT  ----
  # table(HH$hv201b, useNA = "always")
  HH <- HH %>%
    dplyr::mutate(hh.wat.insuff = case_when(
      hv201b=="yes" ~ "Not sufficient",
      hv201b=="no" ~ "Sufficient",
      hv201b=="don't know" ~ NA))
  # table(HH$hh.wat.insuff, useNA = "always")

  ### NATURAL/RUDIMENTARY FLOOR MATERIAL  ----
  HH$hh.noimp.floor <- ifelse(HH$hv213 == "earth/sand" | HH$hv213== "dung" | HH$hv213== "wood planks" | HH$hv213== "palm/bamboo",1,0)
  # table(HH$hv213, useNA = "always")

  ### NATURAL/RUDIMENTARY WALL MATERIAL ----
  HH$hh.noimp.wall <- ifelse(HH$hv214 %in% c("no walls", "cane/palm/trunks", "dirt", "mud", "bamboo with mud", "stone with mud", "uncovered adobe", "plywood", "cardboard", "reused wood"), 1, 0)
  # table(HH$hv214, useNA = "always")

  ### URBAN SLUM - UN DEFINITION (ADDED BY SONDER ATTN BROKEN) ----

  HH$hh.noimp.housing<-ifelse(HH$hh.noimp.floor == 1 & HH$hh.noimp.wall == 1,1,0)
  # table(HH$hh.noimp.housing, useNA = "always")

  ## PLACE OF RESIDENCE ----
  HH <- HH %>% dplyr::mutate(hh.urban = case_when(
    hv025=="rural" ~ "No",
    hv025== "urban" ~ "Yes"))
  # table(HH$hh.urban, useNA = "always")

  ## HOUSEHOLD ECONOMICS ----
  ### HH Items ----
  # ELECTRICITY
  HH <- HH %>% dplyr::mutate(hh.electricity = case_when(
    hv206 == "yes" ~ 1,
    hv206 == "no" ~ 0))
  # table(HH$hh.electricity, useNA = "always")

  # HOUSEHOLD BANK ACCOUNT
  HH <- HH %>% dplyr::mutate(hh.bank.acct = case_when(
    hv247 == "yes" ~ 1,
    hv247 == "no" ~ 0))
  # table(HH$hh.bank.acct, useNA = "always")

  ### HOUSEHOLD ASSETS ----
  # RADIO
  HH <- HH %>% dplyr::mutate(hh.radio = case_when(
    hv207 == "yes" ~ 1,
    hv207 == "no" ~ 0))
  # table(HH$hh.radio, useNA = "always")

  # TELEVISION
  HH <- HH %>% dplyr::mutate(hh.tv = case_when(
    hv208 == "yes" ~ 1,
    hv208 == "no" ~ 0))
  # table(HH$hh.tv, useNA = "always")

  # REFRIGIRATOR
  HH <- HH %>% dplyr::mutate(hh.refrig = case_when(
    hv209 == "yes" ~ 1,
    hv209 == "no" ~ 0))
  # table(HH$hh.refrig, useNA = "always")

  # BICYCLE
  HH <- HH %>% dplyr::mutate(hh.bike = case_when(
    hv210 == "yes" ~ 1,
    hv210 == "no" ~ 0))
  # table(HH$hh.bike, useNA = "always")

  # MOTORCYCLE
  HH <- HH %>% dplyr::mutate(hh.motor = case_when(
    hv211 == "yes" ~ 1,
    hv211 == "no" ~ 0))
  # table(HH$hh.motor, useNA = "always")

  # CAR
  HH <- HH %>% dplyr::mutate(hh.car = case_when(
    hv212 == "yes" ~ 1,
    hv212 == "no" ~ 0))
  # table(HH$hh.car, useNA = "always")

  # MOBILE PHONE
  #hv243a: has mobile phone
  #hv221: has telephone (landline)
  HH <- HH %>% dplyr::mutate(hh.mobile = case_when(
    hv243a == "yes" ~ 1,
    hv243a == "no" ~ 0))
  # table(HH$hv243a, useNA = "always")

  # ANIMAL-DRAWN CART
  HH <- HH %>% dplyr::mutate(hh.cart = case_when(
    hv243c == "yes" ~ 1,
    hv243c == "no" ~ 0))
  # table(HH$hh.cart, useNA = "always")

  # BOAT WITH A MOTOR
  HH <- HH %>% dplyr::mutate(hh.motorboat = case_when(
    hv243d == "yes" ~ 1,
    hv243d == "no" ~ 0))
  # table(HH$hh.motorboat, useNA = "always")

  # BINARY FACTOR FOR MOTOR TRANSPORT
  HH$hh.motortransport.yn <- ifelse(HH$hh.motor==1 | HH$hh.car==1,  "Yes", "No")
  # table(HH$hh.motortransport.yn, useNA = "always")

  # COMPUTER
  HH <- HH %>% dplyr::mutate(hh.computer = case_when(
    hv243e == "yes" ~ 1,
    hv243e == "no" ~ 0))
  # table(HH$hh.computer, useNA = "always")

  # OWN LAND USABLE FOR AGRICULTURE
  HH <- HH %>% dplyr::mutate(hh.land = case_when(
    hv244 == "yes" ~ 1,
    hv244 == "no" ~ 0))
  # table(HH$hh.land, useNA = "always")

  # LIVESTOCK, HERDS, OR ANIMALS
  HH <- HH %>% dplyr::mutate(hh.animal = case_when(
    hv246 == "yes" ~ "Yes",
    hv246 == "no" ~ "No"))
  # table(HH$hh.animal, useNA = "always")

  # ANY COWS, BULLS
  HH <- HH %>% dplyr::mutate(hh.cows = case_when(
    hv246b %in% c(1:98, "95 or more", "unknown") ~ 1, ## Assume they have animals (but don't know how many) if they said 98
    hv246b %in% c(0, "none") ~ 0))
  # table(HH$hh.cows, useNA = "always")

  # ANY HORSES, DONKEYS, MULES
  HH <- HH %>% dplyr::mutate(hh.horses = case_when(
    hv246c %in% c(1:98, "95 or more", "unknown") ~ 1, ## Assume they have animals (but don't know how many) if they said 99
    hv246c %in% c(0, "none") ~ 0))
  # table(HH$hh.horses, useNA = "always")

  # ANY GOATS
  HH <- HH %>% dplyr::mutate(hh.goats = case_when(
    hv246d %in% c(1:98, "95 or more", "unknown") ~ 1, ## Assume they have animals (but don't know how many) if they said 99
    hv246d %in% c(0, "none") ~ 0))
  # table(HH$hh.goats, useNA = "always")

  # ANY SHEEP
  HH <- HH %>% dplyr::mutate(hh.sheep = case_when(
    hv246e %in% c(1:98, "95 or more", "unknown") ~ 1, ## Assume they have animals (but don't know how many) if they said 99
    hv246e %in% c(0, "none") ~ 0))
  # table(HH$hh.sheep, useNA = "always")

  # ANY CHICKENS/POULTRY
  HH <- HH %>% dplyr::mutate(hh.chickens = case_when(
    hv246f %in% c(1:98, "95 or more", "unknown") ~ 1, ## Assume they have animals (but don't know how many) if they said 99
    hv246f %in% c(0, "none") ~ 0))
  # table(HH$hh.chickens, useNA = "always")

  ### NON PRODUCTIVE VEHICLES INDEX  ----
  #* Non-productive, vehicles (e.g., bicycle, motorcycle, car)
  HH <- HH %>%
    dplyr::mutate(vehic.sum = if(all(is.na(c(hh.bike, hh.motor, hh.car)))) NA else hh.bike + hh.motor + hh.car, na.rm = TRUE)
  ## For index (range from 0 to 3), create 3 levels: 0, 1, 2 (having 2+ pieces)
  HH <- HH %>% dplyr::mutate(vehic.index = case_when(
    vehic.sum == 1 ~ 1,
    vehic.sum %in% c(2,3) ~ 2,
    vehic.sum == 0 ~ 0)) %>%
    dplyr::mutate(vehic.index = factor(vehic.index))
  # table(HH$vehic.index, useNA = "always")
  HH$vehic.index.1plus <- ifelse(HH$vehic.sum >=1, 1, 0)
  HH$vehic.index.2plus <- ifelse(HH$vehic.sum >=2, 1, 0)

  ### NON PRODUCTIVE HOUSE AMENITIES  ----
  # *(e.g. electricity, radio, TV, refrigerator, non-mobile telephone, solar panel, sofa, microwave oven, bed, dvd player)
  HH <- HH %>%
    mutate(hh.cupboard = case_when(sh132k == "no" ~ 0, hv221 == "yes" ~ 1),
           hh.sofa = case_when(sh132j == "no" ~ 0, sh132j == "yes" ~ 1),
           hh.air = case_when(sh132l == "no" ~ 0, sh132l == "yes" ~ 1),
           hh.iron = case_when(sh132m == "no" ~ 0, sh132m == "yes" ~ 1),
           hh.generator = case_when(sh132n == "no" ~ 0, sh132n == "yes" ~ 1),
           hh.fan = case_when(sh132o == "no" ~ 0, sh132o == "yes" ~ 1)) %>%
    mutate(house.sum = if(all(is.na(c(hh.electricity, hh.radio, hh.tv, hh.refrig, hh.cupboard, hh.sofa, hh.air, hh.iron, hh.generator, hh.fan)))) NA else hh.electricity + hh.radio + hh.tv + hh.refrig + hh.cupboard + hh.sofa + hh.air + hh.iron + hh.generator + hh.fan, na.rm = TRUE)
  # table(HH$house.sum, useNA = "always") #range from 0 to 10
  ## For index, create 3 levels: [0:2] ~ 0, [3:5] ~ 1, [6:8] ~ 2
  HH <- HH %>% mutate(house.index = case_when(
    house.sum %in% c(3,4,5) ~ 1,
    house.sum %in% c(6,7,8,9,10) ~ 2,
    house.sum %in% c(0,1,2) ~ 0)) %>%
    mutate(house.index = factor(house.index))
  # table(HH$house.index, useNA = "always")

  ### URBAN/RURAL WEALTH INDEX ----
  HH$wealth.index.ur <- factor(HH$hv270a)
  # table(HH$wealth.index.ur, useNA = "always")
  #### CATEGORICAL VARIABLE FOR WEALTH INDEX ----
  HH <- HH %>%
    dplyr::mutate(wealth.index.ur.cat = case_when(wealth.index.ur %in% c("poorest", "poorer") ~ "Poor",
                                                  wealth.index.ur %in% c("middle", "richer", "richest") ~ "Not Poor"))


  ## HOUSEHOLD RELATIONSHIPS ----
  ### HIGHEST EDU OF HH MEMBER  ----

  HH <- HH %>%
    mutate(across(starts_with("hv108_"), ~ ifelse(. == 98, NA, .))) %>%
    mutate(highestyearsedinHH.yrs = do.call(pmax, c(across(starts_with("hv108_")), na.rm = TRUE)))
  # table(HH$hv108_02, useNA = "always")
  # table(HH$highestyearsedinHH.yrs, useNA = "always")
  HH$highestyearsedinHH.9plus <- ifelse(HH$highestyearsedinHH.yrs>=9,'9+','0-8')
  HH$highestyearsedinHH.8plus <- ifelse(HH$highestyearsedinHH.yrs>=8,'8+','0-7')
  HH$highestyearsedinHH.7plus <- ifelse(HH$highestyearsedinHH.yrs>=7,'7+','0-6')

  ### HH MEMBER W/ DISABILITY ----
  #Functional domains: Seeing, hearing, communicating, remembering or concentrating, walking or climbing steps, and washing all over or dressing.
  #Sample: De facto household population age 5 or above

  #### Use PR ----
  #Seeing
  PR <- PR %>%
    mutate(dis.see = ifelse(hdis2 %in% c("no difficulty seeing", "don't know"), 0,
                            ifelse(is.na(hdis2), NA, 1)))
  #Hearing
  PR <- PR %>%
    mutate(dis.hear = ifelse(hdis4 %in% c("no difficulty hearing", "don't know"), 0,
                             ifelse(is.na(hdis4), NA, 1)))
  #Communicating
  PR <- PR %>%
    mutate(dis.comm = ifelse(hdis5 %in% c("no difficulty communicating", "don't know"), 0,
                             ifelse(is.na(hdis5), NA, 1)))
  #Remembering or concentrating
  PR <- PR %>%
    mutate(dis.remem = ifelse(hdis6 %in% c("no difficulty remembering/concentrating", "don't know"), 0,
                              ifelse(is.na(hdis6), NA, 1)))
  #Walking or climbing
  PR <- PR %>%
    mutate(dis.walk = ifelse(hdis7 %in% c("no difficulty walking or climbing", "don't know"), 0,
                             ifelse(is.na(hdis7), NA, 1)))
  #Washing all over or dressing
  PR <- PR %>%
    mutate(dis.wash = ifelse(hdis8 %in% c("no difficulty washing or dressing", "don't know"), 0,
                             ifelse(is.na(hdis8), NA, 1)))
  PR.disable <- PR %>%
    group_by(hv001, hv002) %>%
    summarise(any.dis.see = if(all(is.na(dis.see))) NA else max(dis.see, na.rm = TRUE),
              any.dis.hear = if(all(is.na(dis.hear))) NA else max(dis.hear, na.rm = TRUE),
              any.dis.comm = if(all(is.na(dis.comm))) NA else max(dis.comm, na.rm = TRUE),
              any.dis.remem = if(all(is.na(dis.remem))) NA else max(dis.remem, na.rm = TRUE),
              any.dis.walk = if(all(is.na(dis.walk))) NA else max(dis.walk, na.rm = TRUE),
              any.dis.wash = if(all(is.na(dis.wash))) NA else max(dis.wash, na.rm = TRUE))

  HH <- HH %>%
    left_join(PR.disable, by = c("hv001", "hv002"))

  HH <- HH %>%
    mutate(any.hh.dis = ifelse(any.dis.see==1|any.dis.hear==1|any.dis.comm==1|any.dis.remem==1|any.dis.walk==1|any.dis.wash==1, 1, 0))
  # table(HH$any.hh.dis, useNA = "always")

  ### Number of HH Members ----
  # First impute unknown ages to NA
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

  #### NUMBER OF ADULTS (15+) IN THE HOUSEHOLD ----
  # CATEGORICAL FACTOR FOR HOUSEHOLD MEMBERS 15+
  HH <- HH %>%
    dplyr::mutate(num.15up.cat = case_when(
      num.15up >= 5 ~ "5+",
      num.15up < 5 ~ "<5"))
  # table(HH$num.15up.cat, useNA = "always")

  # Recode num.15up as binary
  HH$num.15up.4plus <- ifelse(HH$num.15up >= 4, "Yes", "No")

  #### NUMBER OF CHILDREN (<15) IN THE HOUSEHOLD ----
  # CATEGORICAL FACTOR FOR HOUSEHOLD MEMBERS UNDER 15
  HH <- HH %>% dplyr::mutate(num.under15.cat = case_when(
    num.under15 %in% c(0) ~ "0",
    num.under15 %in% c(1:2) ~ "1-2",
    num.under15 %in% c(3:4) ~ "3-4",
    num.under15 >=5 ~ "5 and up"))
  HH$num.under15.cat <- factor(HH$num.under15.cat,levels=c("0","1-2","3-4","5 and up"))
  # table(HH$num.under15.cat, useNA = "always")

  # #### Number of biological children in the household (Crossout for duplication)
  IR <- IR %>%
    rowwise() %>%
    mutate(num.kids.house = sum(v202, v203, na.rm=TRUE))
  # table(IR$num.kids.house, useNA = "always")

  IR <- IR %>% mutate(num.kids.house.cat = case_when(
    num.kids.house %in% c(0) ~ "0",
    num.kids.house %in% c(1) ~ "1",
    num.kids.house %in% c(2,3) ~ "2-3",
    num.kids.house >=4 ~ "4 or more"))
  # table(IR$num.kids.house, useNA = "always") #may need to recode for better patterns, given the number of children in Nigeria

  IR$num.kids.house.4plus <- ifelse(IR$num.kids.house.cat=="4 or more", "Yes", "No")

  ### RATIO OF CHILDREN UNDER 5 TO WOMEN 15-49 ----
  HH$hh.kidwom.rat<-ifelse(HH$hv010 == 0, 0, (HH$hv014/HH$hv010))
  # CATEGORICAL FACTOR FOR CHILDREN/WOMEN RATIO
  HH <- HH %>%
    dplyr::mutate(hh.kidwom.rat.cat = case_when(
      hh.kidwom.rat==0 ~ "0",
      hh.kidwom.rat>0 & hh.kidwom.rat<=1 ~ "(0-1]",
      hh.kidwom.rat>1 & hh.kidwom.rat<=2 ~ "(1-2]",
      TRUE ~ "More than 2"))
  # table(HH$hh.kidwom.rat.cat, useNA = "always")
  HH$hh.kidwom.rat.cat <- factor(HH$hh.kidwom.rat.cat,levels=c("0","(0-1]","(1-2]","More than 2"))

  HH$hh.kidwom.rat.2plus<-ifelse(HH$hh.kidwom.rat>=2,1,0)
  # table(HH$hh.kidwom.rat.2plus, useNA = "always")

  ## LARGER ENVIRONMENT ----
  ### AIR QUALITY IN THE HOUSE / VENTILATION ----
  HH <- HH %>% dplyr::mutate(hh.where.cook = case_when(
    hv226 == "no food cooked in house" ~ "No food cooked in house",
    hv241 == "outdoors" ~ "Food cooked outdoors",
    hv241 == "in a separate building" ~ "Food cooked in a separate building",
    hv241 == "in the house" & hv242 == "yes" ~ "Food cooked inside in a separate kitchen",
    hv241 == "in the house" & (hv242 == "no") ~ "Food cooked inside",
    hv241 == "other" ~ NA))
  # table(HH$hh.where.cook, useNA = "always")

  # LOCATION OF COOKING INSIDE HOUSE
  HH$hh.cook.inside.yn <- ifelse(HH$hv241 == "in the house", "Yes", "No")
  # table(HH$hv241, useNA = "always")

  ### CLEAN COOKING FUEL ----
  HH$hh.clean.fuel <- ifelse(HH$hv226 %in% c("electricity","liquefied petroleum gas (lpg)/cooking gas","piped natural gas","biogas","solar energy"), "Yes", "No")
  # table(HH$hh.clean.fuel, useNA = "always")

  ### NUMBER OF HOUSEHOLD ROOMS FOR SLEEPING ----
  HH$hh.rooms.num <- HH$hv216
  # table(HH$hh.rooms.num, useNA = "always")

  # CATEGORICAL FACTOR OF NUMBER OF HOUSEHOLD ROOMS FOR SLEEPING
  HH <- HH %>% dplyr::mutate(hh.rooms.cat = case_when(
    hv216 == 1 ~ "1",
    hv216 == 2 ~ "2",
    hv216 == 3 ~ "3",
    hv216 >= 4 & !is.na(hv216) ~ "4+"))
  # table(HH$hh.rooms.cat, useNA = "always")


  ### TOILET TYPE  ----
  HH <- HH %>%
    mutate(hh.toilet.type = case_when(
      hv205 %in% c("flush to piped sewer system", "flush to septic tank", "flush to pit latrine", "flush to somewhere else", "flush, don't know where") ~ "flush toilet",
      hv205 %in% c("ventilated improved pit latrine (vip)", "pit latrine with slab", "pit latrine without slab/open pit") ~ "pit latrine",
      hv205 %in% c("no facility/bush/field", "composting toilet", "bucket toilet", "hanging toilet/latrine", "other") ~ "informal or no facility"))
  # table(HH$hh.toilet.type, useNA = "always")

  #### BINARY FOR TYPE: FLUSH ----
  HH <- HH %>%
    mutate(hh.toilet.flush = ifelse(hh.toilet.type == "flush toilet", 1, 0))
  # table(HH$hh.toilet.flush, useNA = "always")

  #### BINARY FOR TYPE: LATRINE ----
  HH$latrine <- ifelse(HH$hv205 %in% c("pit latrine without slab/open pit", "no facility/bush/field", "bucket toilet", "hanging toilet/latrine", "other"), 1, 0)

  # TOILET FACILITY DOES NOT EXIST
  HH$no.latrine <- ifelse(HH$hv205 %in% c("no facility/bush/field", "no facility"), "Yes", "No")
  # table(HH$no.latrine, useNA = "always")

  ### LOCATION OF TOILET FACILITY ----
  HH <- HH %>% dplyr::mutate(latrine.loc = case_when(
    hv205 == "no facility/bush/field" ~ "No facility",
    hv238a == "in own dwelling" ~ "In own yard/plot/dwelling",
    hv238a == "in own yard/plot" ~ "In own yard/plot/dwelling",
    hv238a == "elsewhere" ~ "Elsewhere"))

  ### HOUSEHOLD HAS SHARED TOILET ----
  HH <- HH %>%
    dplyr::mutate(hh.shared.latrine.yn = case_when(
      hv205=="no facility/bush/field" ~ "No facility",
      hv225== "yes" ~ "Yes",
      hv225== "no" ~ "No"))
  # table(HH$hh.shared.latrine, useNA = "always")

  #### NUMBER OF HOUSEHOLDS SHARING TOILET ----

  HH$hv238 <- as.numeric(HH$hv238)
  mean_hv238 <- HH %>%
    dplyr::filter(!hv238 %in% c(95, 98)) %>%
    summarise(m = mean(hv238, na.rm = TRUE)) %>%
    pull(m)
  HH <- HH %>%
    dplyr::mutate(
      hv238 = ifelse(hv238==95|hv238==98, mean_hv238, hv238),
      hh.num.sharelatrine = case_when(
        hv238>=2 & hv238<=4 ~ "2-4 HHs",
        hv238 >4 ~ "5 or more HHs",
        hv225=="no" ~ "No sharing",
        hv205=="no facility/bush/field" ~ "No facility"))
  # table(HH$hh.num.sharelatrine, useNA = "always")

  ### WHO DEFNITION NOT IMPROVED LATRINE ----
  HH$hh.noimp.latrine<-ifelse(HH$latrine=="Yes" | (HH$latrine=="No" & HH$hh.shared.latrine=="Yes"), 1, 0)

  ### SOURCE OF DRINKING WATER ----

  #### WATER SOURCE LOCATION ----
  HH <- HH %>%
    dplyr::mutate(hh.water.loc = case_when(
      hv201=="piped into dwelling" ~ "in own dwelling",
      hv201=="piped to yard/plot" ~ "in own yard/plot",
      hv201=="piped to neighbor" ~ "elsewhere",
      hv202%in% c("piped into dwelling", "piped to yard/plot", "piped to neighbor") ~ "elsewhere", #these use bottled water for drinking but water for other purposes comes from these pipes
      TRUE ~ hv235))
  # table(HH$hh.water.loc, useNA = "always")

  #### NO PIPED WATER FOR DRINKING ----
  HH$hh.water.notpiped <- ifelse(HH$hv201== "piped into dwelling" | HH$hv201== "piped to yard/plot" | HH$hv201 == "piped to neighbor" |
                                   HH$hv201== "public tap/standpipe", "No", "Yes")
  # table(HH$hh.water.notpiped, useNA = "always")

  #### WATER TREATMENT: DID NOT DO ANYTHING TO MAKE WATER SAFE TO DRINK ----
  HH$hh.nowatpur <- ifelse(HH$hv237 == "yes", "No", "Yes")
  # table(HH$hh.nowatpur, useNA = "always")

  # DURATION TO GET WATER
  HH <- HH %>%
    dplyr::mutate(hh.wat.time = case_when(hv204 %in% c(1:10) ~ "1-10 minutes",
                                          hv204 %in% c(11:20) ~ "11-20 minutes",
                                          hv204 %in% c(21:30) ~ "21-30 minutes",
                                          hv204 %in% c(31:990) ~ ">30 minutes",
                                          hv204 %in% c(0, 996, "on premises") ~ "Water on premises",
                                          hv204 %in% c(998, "don't know") ~ NA))
  # table(HH$hh.wat.time, useNA = "always")

  #### WHO Definition NOT IMPROVED WATER ----
  HH$hh.noimp.water <- ifelse(HH$water==1 | (HH$water==0 & HH$hh.wat.time == ">30 minutes"), "Yes", "No")
  # table(HH$hh.noimp.water, useNA = "always")

  # ROOF FINISHED
  # hv215: Main material of the roof. Country-specific code. 10=="natural", 12=="thatch/palm leaf",
  # 13=="sod"; 20=="rudimentary", 21=="rustic mat", 22=="palm/bamboo", 23=="wood planks"; 30=="finished",
  # 31=="roofing", 32=="asbestos", 33=="tile", 34=="concrete", 35=="metal tile", 36=="roofing shingles"; 96=="other"
  HH <- HH %>%
    mutate(hh.roof = case_when(
      hv215 %in% c("no roof", "thatch/palm leaf", "mud") ~ "natural or no roof",
      hv215 %in% c("rustic mat", "palm/bamboo", "wood planks", "cardboard", "other") ~ "rudimentary",
      hv215 %in% c("metal/zinc", "wood", "calamine/cement fiber", "ceramic tiles", "cement", "roofing shingles") ~ "finished"))
  # table(HH$hh.roof, useNA = "always")

  HH$hh.roof.finish <- ifelse(HH$hh.roof == "finished", 1, 0)
  # table(HH$hh.roof.finish, useNA = "always")


  ### URBAN SLUM (UN DEFINITION) ----
  HH$roof <- ifelse(HH$hh.roof == "finished", 0, 1)

  HH$floor <- ifelse(HH$hv213 == "earth/sand" | HH$hv213== "dung" | HH$hv213== "wood planks" | HH$hv213== "palm/bamboo", 1, 0)

  HH$wall <- ifelse(HH$hv214 == "no walls" | HH$hv214== "cane/palm/trunks" | HH$hv214== "dirt" | HH$hv214== "mud" |
                      HH$hv214 == "bamboo with mud" | HH$hv214 == "stone with mud" | HH$hv214 == "uncovered adobe" | HH$hv214 == "plywood" |
                      HH$hv214 == "cardboard" | HH$hv214 == "reused wood", "Yes", "No")

  HH$durable <- ifelse(HH$roof == 1 & HH$floor == 1 & HH$wall == "Yes", 1, 0)
  # table(HH$durable, useNA = "always")
  HH$hh.members <- ifelse(HH$hv012 == 0, HH$hv013, HH$hv012)
  HH$hh.memsleep <- ifelse(HH$hh.rooms.num == 0, HH$hh.members, HH$hh.members/HH$hh.rooms.num)

  HH$memsleep.4plus <- ifelse(HH$hh.memsleep >= 4, 1, 0)
  # table(HH$memsleep.4plus, useNA = "always")

  cols <- c("durable", "water", "latrine", "memsleep.4plus")
  HH <- HH %>%
    dplyr::mutate(slum.sum = case_when(
      if_all(all_of(cols), is.na) ~ NA_real_,
      TRUE ~ rowSums(across(all_of(cols)), na.rm = TRUE)))
  # table(HH$slum.sum, useNA = "always")

  # # CATEGORICAL VARIABLE FOR SLUM SUM
  HH <- HH %>%
    dplyr::mutate(slum.sum.cat = case_when(slum.sum == 0 ~ "None",
                                           slum.sum == 1 ~ "1",
                                           slum.sum >= 2 ~ "2+"))


  HH<-HH %>% mutate(slum1 = case_when(
    hv025=="rural" ~ "rural",
    (hv025 == "urban" & slum.sum >= 2) ~ "urban slum",
    (hv025 == "urban" & slum.sum < 2) ~ "urban non-slum"))

  # CONVERT BACK TO CHARACTER
  HH$slum.sum <- as.character(HH$slum.sum)

  # Urban slum 2 - Zulu et al, 2002

  HH$slum2 <- ifelse(HH$hh.urban == "Yes" & HH$hh.electricity == 0 & HH$latrine == 1 & HH$hh.water.notpiped == "Yes", "Yes", "No")

  ### SANITATION (HANDWASH) ----
  #### CATEGORICAL FACTOR FOR SANITATION ----
  HH <- HH %>% dplyr::mutate(hh.sanitation = case_when(
    (hv230b == "water is available" & hv232 == "yes") ~ "Water and soap observed",
    (hv230b == "water is available" & hv232 == "no") ~ "Water but no soap observed",
    (hv230b == "water not available" & hv232 == "yes")  ~ "Soap but no water observed",
    (hv230b == "water not available" & hv232 == "no") ~ "No water or soap observed",
    hv230a == "not observed: not in dwelling" ~ "No washing station",
    hv230a %in% c("not observed: no permission to see", "not observed: other reason") ~ "Washing place not observed"))
  # table(HH$hh.sanitation, useNA = "always")

  HH <- HH %>% dplyr::mutate(hh.sanitation.cat = case_when(
    (hv230b == "water is available" & hv232 == "yes") ~ "Water and soap observed",
    (hv230b == "water is available" & hv232 == "no") | (hv230b == "water not available" & hv232 == "yes") | (hv230b == "water not available" & hv232 == "no") ~ "No water and/or soap observed",
    hv230a == "not observed: not in dwelling" | hv230a %in% c("not observed: no permission to see", "not observed: other reason") ~ "No washing station or place observed"))
  # table(HH$hh.sanitation.cat, useNA = "always")

  #### HAND WASHING STATION OBSERVED ----
  HH$hh.sanitation.yn<-ifelse(HH$hh.sanitation=="No washing station" | HH$hh.sanitation=="No water or soap observed", "No", "Yes")
  # table(HH$hh.sanitation.yn, useNA = "always")

  ### WEATLH SUM INDEX ----
  cols <- c("hh.electricity", "hh.tv", "hh.refrig", "hh.bank.acct", "hh.computer", "hh.car", "hh.noimp.latrine")
  HH <- HH %>%
    dplyr::mutate(wealth.sum = case_when(
      if_all(all_of(cols), is.na) ~ NA_real_,
      TRUE ~ rowSums(across(all_of(cols)), na.rm = TRUE)))

  HH$wealth.sum.index <- as.character(HH$wealth.sum)

  # CATEGORICAL VARIABLE FOR WEALTH SUM INDEX
  HH <- HH %>%
    dplyr::mutate(wealth.sum.index.cat = case_when(
      wealth.sum.index %in% c(0) ~ "0",
      wealth.sum.index %in% c(1) ~ "1",
      wealth.sum.index %in% c(2:6) ~ "2-6"))



  ######################################################################
  ######################################################################
  # 2 | IR FILE ----


  # DEMOGRAPHICS

  IR <- IR %>%
    dplyr::mutate(
      age.yrs  = v012, # age in years
      age.5yrs = v013, # age in 5 year age groups
      parity   = v201 # parity - total number of children ever born
    )

  # DIGITAL CONNECTIVITY & VISITED HEALTH FACILITY LAST 12 MONTHSS &
  # HEALTH FACILITY FP

  ir.add <- IR %>%
    dplyr::select(
      caseid,
      v169c, v155, v171b, v394, v395,
      h12a_1, h12a_2, h12a_3, h12a_4, h12a_5, h12a_6
    ) %>%
    dplyr::mutate(
      care.diarrhea = dplyr::case_when( # care-seeking for diarrhea
        dplyr::if_all(c(h12a_1, h12a_2, h12a_3, h12a_4, h12a_5, h12a_6), is.na) ~ NA_character_,
        dplyr::if_any(c(h12a_1, h12a_2, h12a_3, h12a_4, h12a_5, h12a_6), ~ . == "yes") ~ "yes",
        TRUE ~ "no"
      )
    ) %>%
    dplyr::rename(
      smart.phone = v169c, # smartphone
      literacy    = v155, # literacy
      freq.net    = v171b, # internet use
      wom.hlthfac.12mo = v394, #visited health facility last 12 months
      hlthfac.fp.mes = v395 #Health facility discussed FP
    ) %>%
    dplyr::select(caseid, smart.phone, literacy, freq.net, care.diarrhea, wom.hlthfac.12mo, hlthfac.fp.mes )

  IR <- IR %>%
    dplyr::left_join(ir.add, by = "caseid")

  # CHILD GIVEN SWEET SNACKS
  IR <-IR %>% dplyr::mutate(bf.sweet.snacks = case_when(
    v414a %in% c("no","don't know") ~ 0,
    v414a== "yes" ~ 1))
  # table(IR$bf.sweet.snacks, useNA = "always") #there's a lot of NAs (> 50%)

  ## PARTNERSHIPS ----
  ### CURRENT PARTNERSHIP STATUS (BINARY) ----
  # CODE AS 1/0 TO EASILY HANDLE SKIP PATTERNS ACROSS DATASET
  IR$marr.cohab <- ifelse(IR$v501 == "married" | IR$v501 == "living with partner", 1, 0)

  ### NUMBER OF SEXUAL PARTNERS IN THE LAST YEAR  ----
  IR <- IR %>%
    mutate(sex.partner.12mo = case_when(
      v766b==0 ~ "No sexual partner",
      v766b==1 ~ "1 sexual partner",
      v766b==2 ~ "2 sexual partners",
      TRUE ~ "3 or more")) #althohugh v766b gives "98", v767c indicates that it should be 3+
  # table(IR$sex.partner.12mo, useNA = "always")

  ### DURATION OF CURRENT UNION  ----
  IR$yrs.curr.pship <- ifelse(IR$v501 %in% c("married", "living with partner"), IR$v512, NA)
  IR$yrs.curr.pship[IR$marr.cohab==0] <- 'not partnered'

  IR <- IR %>% dplyr::mutate(yrs.curr.pship.cat = case_when(
    v501 %in% c("never in union") ~ "Never in union",
    (v501 %in% c("widowed", "divorced", "no longer living together/separated") | v503 %in% c("more than once")) ~ "No longer with first partner",
    yrs.curr.pship %in% c(0:4) ~ "Less than 5 years",
    yrs.curr.pship %in% c(5:9) ~ "5 to 9 years",
    yrs.curr.pship %in% c(10:39) ~ "10 or more years"))
  IR$yrs.curr.pship.cat <- factor(IR$yrs.curr.pship.cat, levels = c("Never in union", "Less than 5 years", "5 to 9 years", "10 or more years", "No longer with first partner"))
  # table(IR$yrs.curr.pship.cat, useNA = "always")

  ### MARITAL CAT ----
  IR <- IR %>% dplyr::mutate(marital.cat = case_when(
    v501 %in% c("never in union") ~ "never in union",
    v501 %in% c("married", "living with partner") ~ "married",
    v501 %in% c("widowed", "divorced", "separated", "no longer living together/separated") ~ "widowed/ divorced/ separated"))
  # table(IR$marital.cat, useNA = "always")

  ### POLYGAMY ----
  IR <- IR %>%
    dplyr::mutate(polygamy = case_when(
      v505==0 ~ "No",
      v505 %in% c(1:20) ~ "Yes",
      marr.cohab == 0 ~ "not partnered",
      v505 %in% c(98, "don't know") ~ NA))
  # table(IR$polygamy, useNA = "always")

  ### WIFE ORDER AMONG WOMEN IN POLYGAMOUS UNION ----
  IR <- IR %>%
    dplyr::mutate(wife.order = case_when(v506 == 98 ~ NA,
                                         v506 == 1 ~ "1",
                                         v506 == 2 ~ "2",
                                         v506 >= 3 ~ "3+",
                                         marr.cohab == 0 ~ "not partnered",
                                         v505==0 ~ "monogamous union"))
  # table(IR$wife.order, useNA = "always")

  ### RESPONDENT IS FIRST WIFE ----
  IR <- IR %>%
    dplyr::mutate(first.wife = case_when(v506 == 1 ~ "First wife",
                                         v506 %in% c(2:20) ~ "Second or more wife",
                                         marr.cohab == 0 ~ "not partnered",
                                         v505 %in% c(0, "no other wives") ~ "Monogamous union"))
  # table(IR$first.wife, useNA = "always")


  ## ABILITY TO CONCEIVE ----
  IR <- IR %>%
    dplyr::mutate(infecund.meno = case_when(v625 == "infecund, menopausal" ~ "Yes",
                                            !is.na(v625) ~ "No"))
  # table(IR$infecund.meno, useNA = "always")

  # HUSBAND/PARTNER OPPOSES FP USE
  IR <- IR %>%
    dplyr::mutate(fp.partner.oppose = case_when(
      v3a08j == "yes" ~ "Partner opposes",
      v3a08j == "no" ~ "Partner does not oppose",
      v213 == "yes" ~ "Currently pregnant",
      v361 == "currently using" ~ "Currently using FP",
      is.na(v3a08j) ~ "No identified need for FP"))
  # table(IR$fp.partner.oppose, useNA = "always")

  ## BARRIERS TO HC ----
  ### GETTING PERMISSION ----
  IR <- IR %>%
    dplyr::mutate(med.permis = case_when(v467b =="not a big problem" | v467b =="no problem" ~ 0,
                                         v467b == "big problem" ~ 1))
  # table(IR$med.permis, useNA = "always")

  ### COST FOR NECESSARY TREATMENT ----
  IR <- IR %>%
    dplyr::mutate(med.cost = case_when(v467c =="not a big problem" | v467c =="no problem" ~ 0,
                                       v467c == "big problem" ~ 1))

  ### DISTANCE TO HEALTH FACILITY ----
  IR <- IR %>%
    dplyr::mutate(med.dist = case_when(v467d =="not a big problem" | v467d =="no problem" ~ 0,
                                       v467d == "big problem" ~ 1))

  ### DOESN'T WANT TO GO ALONE ----
  IR <- IR %>%
    dplyr::mutate(med.alone = case_when(v467f =="not a big problem"| v467f =="no problem" ~ 0,
                                        v467f == "big problem" ~ 1))

  ### CREATE INDEX FROM BARRIERS TO HC VARIABLES ----

  cols <- c("med.permis", "med.cost", "med.dist", "med.alone")
  IR <- IR %>%
    dplyr::mutate(med.sum = case_when(
      if_all(all_of(cols), is.na) ~ NA_real_,
      TRUE ~ rowSums(across(all_of(cols)), na.rm = TRUE)))

  IR$med.index <- as.character(IR$med.sum)

  #### MED INDEX CAT ----
  IR <- IR %>%
    dplyr::mutate(med.index.cat = case_when(med.index == 0 ~ "0",
                                            med.index == 1 ~ "1",
                                            med.index %in% c(2:4) ~ "2+"))
  # table(IR$med.index.cat, useNA = "always")

  #### BINARY INDICATOR FROM INDEX ----
  IR$med.index.3plus <- ifelse(IR$med.index >= 3, 1, 0)
  # table(IR$med.index.3plus, useNA = "always")


  ## VISITED BY A HEALTHWORKER ----
  IR$hw.visit.12mo <- ifelse(IR$v393=="yes", "Yes", "No")
  # table(IR$hw.visit.12mo, useNA = "always")

  ## DID HEALTHWORKER TALK ABOUT FAMILY PLANNING ----
  IR <- IR %>%
    dplyr::mutate(hw.visit.fp = case_when(v393a == "yes" ~ "HW discussed FP",
                                          v393a == "no" ~ "HW did not discuss FP",
                                          hw.visit.12mo == "No" ~ "No HW visit"))
  # table(IR$hw.visit.fp, useNA = "always")

  ## EMPLOYMENT FACTORS ----

  ### RESPONDENT'S OCCUPATION ----
  IR$occupation <- IR$v717

  IR <- IR %>%
    dplyr::mutate(occupation.cat = case_when(
      occupation %in% c('professional/technical/managerial','clerical') ~ 'professional/mangerial/clerical',
      occupation %in% c('skilled manual','unskilled manual') ~ 'manual',
      occupation %in% c('services','household and domestic', 'other', "don't know") ~ 'service/domestic/other',
      occupation %in% c('agricultural - self employed','agricultural - employee') ~ 'agricultural',
      occupation %in% c('sales') ~ 'sales',
      occupation %in% c('not working') ~ 'not working'))
  # table(IR$occupation.cat, useNA = "always")

  ### RESPONDENT WORKS FOR FAMILY, OTHERS, SELF-EMPLOYED ----
  IR <- IR %>% dplyr::mutate(occ.type = case_when(
    v719 == "for family member" ~ "Family",
    v719 == "for someone else" ~ "Someone else",
    v719 == "self-employed" ~ "Self",
    is.na(v719) ~ "Did not work"))
  # table(IR$occ.type, useNA = "always")

  ### RESPONDENT CURRENTLY WORKS ----
  IR <- IR %>% mutate(working = case_when(
    (v714 == "yes" & (v714a != "yes" | is.na(v714a))) ~ "Currently working",
    (v714a == "yes") ~ "Worked in the past year", #note that in "working", now there is no more "On leave/absent", it is treated the same as "worked in the past year".
    (v731 == "no") ~ "No work in the past 12 months",
    (v731 == "in the past year") ~ "Worked in the past year"))
  # table(IR$v731, useNA = "always")

  IR$working.yn <- ifelse(IR$working=="Currently working" | IR$working=="Worked in the past year" | IR$working=="On leave/absent", "Yes", "No")

  IR$workingnow.yn <- ifelse(IR$working=="Currently working", "Yes", "No")

  ### WORK SEASONALITY ----
  IR <- IR %>% dplyr::mutate(work.seasonal = case_when(
    v731 == "no" ~ "No work in past 12 months",
    v732 == "occasional" ~ "Occassional",
    v732 == "seasonal" ~ "Seasonal",
    v732 == "all year" ~ "All year"))
  # table(IR$work.seasonal, useNA = "always")

  # WORK SEASONALITY | BINARY
  IR <- IR %>% dplyr::mutate(work.seasonal2 = case_when(
    work.seasonal == "All year" ~ "All year",
    work.seasonal %in% c("Occassional", "Seasonal") ~ "Seasonal/Occassional",
    work.seasonal == "No work in past 12 months" ~ "No work in past 12 months"))
  # table(IR$work.seasonal2, useNA = "always")


  ## HUSBAND/PARTNER WORKING ----

  ### HIS WORK STATUS ----
  IR <- IR %>% mutate(partner.working = case_when(
    !(v501 %in% c("married", "living with partner")) ~ "not partnered",
    v704a == "didn't work last 12 months" ~ "Didn't work last 12 months",
    v704a == "worked last 7 days" ~ "Worked last 7 days",
    v704a == "worked last 12 months" ~ "Worked last 12 months",
    v704a == "don't know" ~ "Don't know"))
  # table(IR$partner.working, useNA = "always")

  IR$partner.working.yn <- ifelse(IR$partner.working %in% c("Worked last 7 days", "Worked last 12 months"), "Yes", "No")
  IR$partner.workingnow.yn <- ifelse(IR$partner.working %in% c("Worked last 7 days"), "Yes", "No")
  # table(IR$partner.working.yn, useNA = "always")
  # table(IR$partner.workingnow.yn, useNA = "always")

  ### HUSBAND/PARTNER'S OCCUPATION ----
  IR$partner.occupation <- IR$v704
  # table(IR$partner.occupation, useNA = "always")
  IR <- IR %>%
    dplyr::mutate(partner.occupation.cat = case_when(
      partner.occupation %in% c("professional, technical and related workers", "administrative and managerial workers", "office/administrative support workers") ~ "professional/mangerial/clerical",
      partner.occupation %in% c("installations, maintenance and repair workers","production, construction and extractions workers", "transportation and material moving workers") ~ "manual",
      partner.occupation %in% c("service workers") ~ "service",
      partner.occupation %in% c("agricultural, animal husbandry, forestry, fishermen/hunters") ~ "agricultural",
      partner.occupation %in% c("sales and related workers") ~ "sales",
      partner.occupation %in% c("other","don't know") ~ "other/dk",
      partner.occupation %in% c("not working and didn't work in last 12 months") ~ "not working",
      !(v501 %in% c("married", "living with partner")) ~ "not partnered"))
  # table(IR$partner.occupation.cat, useNA = "always")

  IR <- IR %>%
    dplyr::mutate(partner.occupation.cat.2 = case_when(
      partner.occupation %in% c("professional, technical and related workers", "administrative and managerial workers", "office/administrative support workers") ~ "professional/mangerial/clerical",
      partner.occupation %in% c("installations, maintenance and repair workers","production, construction and extractions workers", "transportation and material moving workers") ~ "manual",
      partner.occupation %in% c("service workers") ~ "service",
      partner.occupation %in% c("agricultural, animal husbandry, forestry, fishermen/hunters") ~ "agricultural",
      partner.occupation %in% c("sales and related workers") ~ "sales",
      partner.occupation %in% c("other","don't know") ~ "not partnered, not working, other/dk",
      partner.occupation %in% c("not working and didn't work in last 12 months") ~ "not partnered, not working, other/dk",
      !(v501 %in% c("married", "living with partner")) ~ "not partnered, not working, other/dk"))
  table(IR$partner.occupation.cat.2, useNA = "always")

  ## RESPONDENT EARNS MORE THAN HUSBAND/PARTNER ----
  IR <- IR %>% dplyr::mutate(earnings.rel.partner = case_when(
    !(v501 %in% c("married", "living with partner")) ~ "not partnered",
    working=="No work in the past 12 months" ~ "woman did not work",
    !(v741 %in% c("cash only", "cash and in-kind")) ~ "woman worked but not paid in cash",
    v746 == "husband/partner doesn't bring in money" & working == "Currently working" & v741 != "not paid" ~ "more than him",
    v746 == "husband/partner doesn't bring in money" | partner.working=="Didn't work last 12 months"  ~ "husband no earnings",
    TRUE ~ v746))
  # table(IR$earnings.rel.partner, useNA = "always")

  ## WOMAN'S CONTROL OVER RESOURCES ----
  ### OWNS A HOUSE ALONE OR JOINTLY ----
  IR <- IR %>% mutate(jnt.house.ownership = case_when(
    v745a == "jointly with someone else only" ~ "jointly with others",
    v745a == "both alone and jointly" ~ "jointly with others",
    v745a == "jointly with husband/partner and someone else" ~ "jointly with others",
    v745a == "does not own" ~ "does not own",
    v745a == "jointly with husband/partner only" ~ "jointly with husband/partner only",
    v745a == "alone only" ~ "alone only",))

  ### OWNS LAND ALONE OR JOINTLY ----
  IR <- IR %>% mutate(jnt.land.ownership = case_when(
    v745b == "jointly with someone else only" ~ "jointly with others",
    v745b == "both alone and jointly" ~ "jointly with others",
    v745b == "jointly with husband/partner and someone else" ~ "jointly with others",
    v745b == "does not own" ~ "does not own",
    v745b == "jointly with husband/partner only" ~ "jointly with husband/partner only",
    v745b == "alone only" ~ "alone only"))


  ## NUMBER OF CHILDREN IN THE HOUSEHOLD (UNDER 5) ----
  IR$num.under5 <- IR$v137
  IR <- IR %>% dplyr::mutate(num.under5.cat = case_when(
    v137 == 0 ~ "0",
    v137 == 1 ~ "1",
    v137 ==2 ~ "2",
    (v137 >=3 & !is.na(v137)) ~ "3+"))
  # table(IR$num.under5.cat, useNA = "always")

  ## NUMBER OF CHILDREN LIVING ----
  IR$num.child.alive <- IR$v218

  # CATEGORICAL FACTOR FOR NUMBER OF LIVING CHILDREN
  IR <- IR %>% dplyr::mutate(num.child.alive.cat = case_when(
    v218 %in% c(0) ~ "0",
    v218 %in% c(1:2) ~ "1-2",
    v218 %in% c(3:5) ~ "3-5",
    (v218 >=6 & !is.na(v218)) ~ "6+"))
  # table(IR$num.child.alive.cat, useNA = "always")

  IR <- IR %>% dplyr::mutate(num.child.alive.cat.2 = case_when(
    v218 %in% c(0:2) ~ "0-2",
    v218 %in% c(3:5) ~ "3-5",
    (v218 >=6 & !is.na(v218)) ~ "6+"))
  table(IR$num.child.alive.cat.2, useNA = "always")

  ## NUMBER OF BIOLOGICAL CHILDREN IN THE HOUSEHOLD ----
  IR <- IR %>%
    rowwise() %>%
    dplyr::mutate(num.biochild.house = sum(v202, v203, na.rm=TRUE))
  #num.kids.house

  # CATEGORICAL FACTOR FOR NUMBER OF BIOLOGICAL CHILDREN IN HOUSEHOLD
  IR <- IR %>% dplyr::mutate(num.biochild.house.cat = case_when(
    num.biochild.house %in% c(0) ~ "0",
    num.biochild.house %in% c(1) ~ "1",
    num.biochild.house %in% c(2,3) ~ "2-3",
    num.biochild.house >=4 ~ "4 or more"))

  ## PARTNER DOES NOT LIVE IN THE HOUSEHOLD (AMONG MARRIED OR IN UNION) ----
  IR <- IR %>% dplyr::mutate(partner.absent = case_when(
    v504 == "living with her" ~ "No",
    v504 == "staying elsewhere" ~ "Yes",
    !(v501 %in% c("married", "living with partner")) ~ "not partnered"))


  ## SEX OF HEAD OF HOUSEHOLD ----
  IR$head.sex <- IR$v151

  ## VIOLENCE IN HOUSEHOLD ----
  # DOMESTIC VIOLENCE - PHYSICAL
  IR <- IR %>%
    dplyr::mutate(dv.physical = case_when(
      (d106 == "no" & d107 == "no") ~ "No",
      (d106 == "yes" | d107 == "yes") ~ "Yes",
      !(v501 %in% c("married", "living with partner")) ~ "not partnered",
      is.na(d106) ~ "Did not answer the question"))
  # table(IR$dv.physical, useNA = "always")

  # DOMESTIC VIOLENCE - EMOTIONAL
  IR <- IR %>%
    dplyr::mutate(dv.emotional = case_when(
      d104 == "yes" ~ "Yes",
      d104 == "no" ~ "No",
      !(v501 %in% c("married", "living with partner")) ~ "not partnered",
      is.na(d104) ~ "Did not answer the question"))
  table(IR$dv.emotional, useNA = "always")

  # DOMESTIC VIOLENCE - SEXUAL
  IR <- IR %>%
    dplyr::mutate(dv.sexual = case_when(
      d108 == "yes" ~ "Yes",
      d108 == "no" ~ "No",
      !(v501 %in% c("married", "living with partner")) ~ "not partnered",
      is.na(d108) ~ "Did not answer the question"))
  # table(IR$dv.sexual, useNA = "always")


  # HUSBAND/PARTNER JEALOUS IF RESPONDENT TALKS WITH OTHER MEN

  IR <- IR %>%
    dplyr::mutate(dv.jealous.othermen = case_when(
      d101a %in% c("often", "sometimes", "yes, but not in the last 12 months", "yes, but frequency in last 12 months missing") ~ "Yes",
      d101a %in% c("never") ~ "No",
      d101a %in% c("don't know") ~ "Don't know",
      is.na(d101a) ~ "Did not answer the question"))
  # table(IR$dv.jealous.othermen, useNA = "always")

  # HUSBAND/PARTNER DOES NOT PERMIT RESPONDENT TO MEET FEMALE FRIENDS

  IR <- IR %>%
    dplyr::mutate(dv.nofriends = case_when(
      d101c %in% c("often", "sometimes", "yes, but not in the last 12 months", "yes, but frequency in last 12 months missing") ~ "Yes",
      d101c %in% c("never") ~ "No",
      d101c %in% c("don't know") ~ "Don't know",
      is.na(d101c) ~ "Did not answer the question"))
  # table(IR$dv.nofriends, useNA = "always")

  # HUSBAND/PARTNER TRIES TO LIMIT RESPONDENT'S CONTACT WITH FAMILY

  IR <- IR %>%
    dplyr::mutate(dv.nofamily.contact = case_when(
      d101d %in% c("often", "sometimes", "yes, but not in the last 12 months", "yes, but frequency in last 12 months missing") ~ "Yes",
      d101d %in% c("never") ~ "No",
      d101d %in% c("don't know") ~ "Don't know",
      is.na(d101c) ~ "Did not answer the question"))
  # table(IR$dv.nofamily.contact, useNA = "always")

  # EVER BEEN HUMILIATED BY HUSBAND/PARTNER

  IR <- IR %>%
    dplyr::mutate(dv.humiliated = case_when(
      d103a %in% c("often", "sometimes", "yes, but not in the last 12 months", "yes, but frequency in last 12 months missing") ~ "Yes",
      d103a %in% c("never") ~ "No",
      d103a %in% c("don't know") ~ "Don't know",
      is.na(d103a) ~ "Did not answer the question"))
  # table(IR$dv.humiliated, useNA = "always")

  # EVER BEEN THREATENED WITH HARM BY HUSBAND/PARTNER

  IR <- IR %>%
    dplyr::mutate(dv.threatened = case_when(
      d103b %in% c("often", "sometimes", "yes, but not in the last 12 months", "yes, but frequency in last 12 months missing") ~ "Yes",
      d103b %in% c("never") ~ "No",
      d103b %in% c("don't know") ~ "Don't know",
      is.na(d103a) ~ "Did not answer the question"))
  # table(IR$dv.threatened, useNA = "always")

  # EVER BEEN INSULTED OR MADE TO FEEL BAD BY HUSBAND/PARTNER

  IR <- IR %>%
    dplyr::mutate(dv.insulted = case_when(
      d103c %in% c("often", "sometimes", "yes, but not in the last 12 months", "yes, but frequency in last 12 months missing") ~ "Yes",
      d103c %in% c("never") ~ "No",
      d103c %in% c("don't know") ~ "Don't know",
      is.na(d103c) ~ "Did not answer the question"))
  # table(IR$dv.insulted, useNA = "always")

  # EVER BEEN PUSHED, SHOOK, OR HAD SOMETHING THROWN BY HUSBAND/PARTNER

  IR <- IR %>%
    dplyr::mutate(dv.pushed = case_when(
      d105a %in% c("often", "sometimes", "yes, but not in the last 12 months", "yes, but frequency in last 12 months missing") ~ "Yes",
      d105a %in% c("never") ~ "No",
      d105a %in% c("don't know") ~ "Don't know",
      is.na(d105a) ~ "Did not answer the question"))
  # table(IR$dv.pushed, useNA = "always")

  # EVER BEEN SLAPPED BY HUSBAND/PARTNER

  IR <- IR %>%
    dplyr::mutate(dv.slapped = case_when(
      d105b %in% c("often", "sometimes", "yes, but not in the last 12 months", "yes, but frequency in last 12 months missing") ~ "Yes",
      d105b %in% c("never") ~ "No",
      d105b %in% c("don't know") ~ "Don't know",
      is.na(d105b) ~ "Did not answer the question"))
  # table(IR$dv.slapped, useNA = "always")

  # EVER BEEN KICKED OR DRAGGED BY HUSBAND/PARTNER

  IR <- IR %>%
    dplyr::mutate(dv.kicked = case_when(
      d105d %in% c("often", "sometimes", "yes, but not in the last 12 months", "yes, but frequency in last 12 months missing") ~ "Yes",
      d105d %in% c("never") ~ "No",
      d105d %in% c("don't know") ~ "Don't know",
      is.na(d105d) ~ "Did not answer the question"))
  # table(IR$dv.kicked, useNA = "always")

  # EVER BEEN STRANGLED OR BURNT BY HUSBAND/PARTNER

  IR <- IR %>%
    dplyr::mutate(dv.strangled = case_when(
      d105e %in% c("often", "sometimes", "yes, but not in the last 12 months", "yes, but frequency in last 12 months missing") ~ "Yes",
      d105e %in% c("never") ~ "No",
      d105e %in% c("don't know") ~ "Don't know",
      is.na(d105e) ~ "Did not answer the question"))
  # table(IR$dv.strangled, useNA = "always")

  # EVER BEEN ATTACKED WITH KNIFE/GUN OR OTHER WEAPON BY HUSBAND/PARTNER

  IR <- IR %>%
    dplyr::mutate(dv.weapon = case_when(
      d105f %in% c("often", "sometimes", "yes, but not in the last 12 months", "yes, but frequency in last 12 months missing") ~ "Yes",
      d105f %in% c("never") ~ "No",
      d105f %in% c("don't know") ~ "Don't know",
      is.na(d105f) ~ "Did not answer the question"))
  # table(IR$dv.weapon, useNA = "always")

  # EVER BEEN PHYSICALLY FORCED INTO UNWANTED SEX BY HUSBAND/PARTNER

  IR <- IR %>%
    dplyr::mutate(dv.forcedsex1 = case_when(
      d105h %in% c("often", "sometimes", "yes, but not in the last 12 months", "yes, but frequency in last 12 months missing") ~ "Yes",
      d105h %in% c("never") ~ "No",
      d105h %in% c("don't know") ~ "Don't know",
      is.na(d105h) ~ "Did not answer the question"))
  # table(IR$dv.forcedsex1, useNA = "always")

  # EVER BEEN FORCED INTO OTHER UNWANTED SEXUAL ACTS BY HUSBAND/PARTNER

  IR <- IR %>%
    dplyr::mutate(dv.forcedsex2 = case_when(
      d105i %in% c("often", "sometimes", "yes, but not in the last 12 months", "yes, but frequency in last 12 months missing") ~ "Yes",
      d105i %in% c("never") ~ "No",
      d105i %in% c("don't know") ~ "Don't know",
      is.na(d105i) ~ "Did not answer the question"))
  # table(IR$dv.forcedsex2, useNA = "always")

  # Ever been physically forced to perform sexual acts respondent didn't want to

  IR <- IR %>%
    dplyr::mutate(dv.forcedsex3 = case_when(
      d105k %in% c("often", "sometimes", "yes, but not in the last 12 months", "yes, but frequency in last 12 months missing") ~ "Yes",
      d105k %in% c("never") ~ "No",
      d105k %in% c("don't know") ~ "Don't know",
      is.na(d105k) ~ "Did not answer the question"))
  # table(IR$dv.forcedsex3, useNA = "always")

  # FORCED SEX Y/N
  IR <- IR %>%
    dplyr::mutate(dv.forcedsex.yn = case_when(
      dv.forcedsex1 == "Yes" | dv.forcedsex2 == "Yes" | dv.forcedsex3 == "Yes" ~ "Yes",
      dv.forcedsex1 == "No" & dv.forcedsex2 == "No" & dv.forcedsex3 == "No" ~ "No",
      dv.forcedsex1 == "Don't know" & dv.forcedsex2 == "Don't know" & dv.forcedsex3 == "Don't know" ~ "Don't know",
      is.na(d105h) & is.na(d105i) & is.na(d105k) ~ "Did not answer the question"))
  # table(IR$dv.forcedsex.yn, useNA = "always")

  # EVER HAD ARM TWISTED OR HAIR PULLED BY HUSBAND/PARTNER

  IR <- IR %>%
    dplyr::mutate(dv.armtwist = case_when(
      d105j %in% c("often", "sometimes", "yes, but not in the last 12 months", "yes, but frequency in last 12 months missing") ~ "Yes",
      d105j %in% c("never") ~ "No",
      d105j %in% c("don't know") ~ "Don't know",
      is.na(d105j) ~ "Did not answer the question"))
  # table(IR$dv.armtwist, useNA = "always")

  # HURT DURING PREGNANCY
  # HUSBAND/PARTNER: PERSON WHO HURT RESPONDENT DURING A PREGNANCY
  IR <- IR %>%
    mutate(
      dv.physical.preg = case_when(
        v044 != "woman selected and interviewed" ~ "NA - not eligible for questionnaire",
        d118a == "no"  ~ "No",
        d118a == "yes" ~ "Yes",
        TRUE ~ NA_character_))
  # table(IR$dv.physical.preg, useNA = "ifany")


  # FORMER PARTNER: PERSON WHO HURT RESPONDENT DURING A PREGNANCY
  IR <- IR %>% dplyr::mutate(dv.hurtpreg.formerp = case_when(
    d118j == "yes" ~ "Yes",
    d118j == "no" ~ "No",
    v044 != "woman selected and interviewed" ~ "Did not answer the question"))
  # table(IR$dv.hurtpreg.formerp, useNA = "ifany")

  # PREVIOUS HUSBAND: EVER HIT, SLAP, KICK, OR PHYSICALLY HURT RESPONDENT

  IR <- IR %>%
    dplyr::mutate(dv.prevparter.hurt = case_when(
      d130a %in% c("0-11 months ago", "12+ months ago", "yes, but don't remember when", "yes, but frequency in last 12 months missing") ~ "Yes",
      d130a %in% c("never had another husband/male partner") ~ "Never had another partner",
      d130a %in% c("never") ~ "No",
      v044 != "woman selected and interviewed" ~ "Did not answer the question"))
  # table(IR$d130a, useNA = "ifany")

  # PREVIOUS HUSBAND: PHYSICALLY FORCED TO HAVE SEX OR PERFORM SEXUAL ACTS

  IR <- IR %>%
    dplyr::mutate(dv.prevparter.forcedsex = case_when(
      d130b %in% c("0-11 months ago", "12+ months ago", "yes, but don't remember when", "yes, but frequency in last 12 months missing") ~ "Yes",
      d130b %in% c("never had another husband/male partner") ~ "Never had another partner",
      d130b %in% c("never") ~ "No",
      v044 != "woman selected and interviewed" ~ "Did not answer the question"))
  # table(IR$dv.prevparter.forcedsex, useNA = "ifany")

  #SEEKING HELP: who have ever experienced physical or sexual violence by anyone
  # EVER TOLD ANYONE ELSE ABOUT VIOLENCE
  IR <- IR %>%
    dplyr::mutate(dv.anyone.help = case_when(
      d128 == "yes" ~ "Yes",
      d128 == "no" ~ "No",
      (dv.physical == "NO" | dv.sexual=="No" | dv.emotional =="No") ~ "Not experienced violence",
      v044 != "woman selected and interviewed" ~ "Did not answer the question"))
  # table(IR$dv.anyone.help, useNA = "always")


  # FRIEND: PERSON RESPONDENT WENT TO SEEK HELP
  IR <- IR %>%
    dplyr::mutate(dv.friend.help = case_when(
      d119xd == "yes" ~ "Yes",
      d119xd == "no" ~ "No",
      d128 == 'no' ~ "Did not seek any help"))

  # IR$dv.friend.help[IR$d128 == 'no'] <- 'Did not seek any help'


  # OWN FAMILY: PERSON RESPONDENT WENT TO SEEK HELP
  IR <- IR %>%
    dplyr::mutate(dv.family.help = case_when(
      d119h == "yes" ~ "Yes",
      d119h == "no" ~ "No",
      d128 == "no" ~ "Did not seek any help"))


  # HUSBAND/PARTNER FAMILY: PERSON RESPONDENT WENT TO SEEK HELP
  IR <- IR %>%
    dplyr::mutate(dv.husband.help = case_when(
      d119i == "yes" ~ "Yes",
      d119i == "no" ~ "No",
      d128 == "no" ~ "Did not seek any help"))


  # CURRENT/FORMER HUSBAND/PARTNER: PERSON RESPONDENT WENT TO SEEK HELP
  IR <- IR %>%
    dplyr::mutate(dv.formerp.help = case_when(
      d119j == "yes" ~ "Yes",
      d119j == "no" ~ "No",
      d128 == "no" ~ "Did not seek any help"))


  # CURRENT/FORMER BOYFRIEND: PERSON RESPONDENT WENT TO SEEK HELP
  IR <- IR %>%
    dplyr::mutate(dv.formerb.help = case_when(
      d119k == "yes" ~ "Yes",
      d119k == "no" ~ "No",
      d128 == "no" ~ "Did not seek any help"))


  # NEIGHBOR: PERSON RESPONDENT WENT TO SEEK HELP
  IR <- IR %>%
    dplyr::mutate(dv.neighbor.help = case_when(
      d119u == "yes" ~ "Yes",
      d119u == "no" ~ "No",
      d128 == "no" ~ "Did not seek any help"))


  # OTHER: PERSON RESPONDENT WENT TO SEEK HELP
  IR <- IR %>%
    dplyr::mutate(dv.other.help = case_when(
      d119x == "yes" ~ "Yes",
      d119x == "no" ~ "No",
      d128 == "no" ~ "Did not seek any help"))


  # SOCIAL SERVICE ORGANIZATION: PERSON RESPONDENT WENT TO SEEK HELP
  IR <- IR %>%
    dplyr::mutate(dv.sso.help = case_when(
      d119xb == "yes" ~ "Yes",
      d119xb == "no" ~ "No",
      d128 == "no" ~ "Did not seek any help"))
  # IR$dv.sso.help[IR$d128 == 'no'] <- 'Did not seek any help'


  # RELIGIOUS LEADER: PERSON RESPONDENT WENT TO SEEK HELP
  IR <- IR %>%
    dplyr::mutate(dv.religious.help = case_when(
      d119xf == "yes" ~ "Yes",
      d119xf == "no" ~ "No",
      d128 == "no" ~ "Did not seek any help"))



  ## DECISION-MAKING ----
  ### FINANCIAL ----
  IR <- IR %>%
    dplyr::mutate(desc.ownincome = case_when(
      !is.na(v739) ~ as.character(v739),
      (v741 %in% c("not paid", "in-kind only") | v731 == "no") ~ "Not paid in cash or not working",
      marr.cohab == 0 ~ "not partnered"))
  # table(IR$desc.ownincome, useNA = "always")

  #JOINT DECISION

  IR <- IR %>% mutate(jd.ownincome = case_when(
    v739 == "respondent and husband/partner" ~ "Yes",
    (!is.na(v739) & !(v739 == "respondent and husband/partner")) ~ "No",
    !(v501 %in% c("married", "living with partner")) ~ "Not partnered",
    (v501 %in% c("married", "living with partner") & (v741 %in% c("not paid", "in-kind only") | v731 == "no")) ~ "Not paid in cash or not working"))
  # table(IR$jd.ownincome, useNA = "always")

  # OWN DECISION: FINANCIAL

  IR <- IR %>% mutate(wd.ownincome = case_when(
    v739 == "respondent alone" ~ "Yes",
    (!is.na(v739) & !(v739 == "respondent alone")) ~ "No",
    !(v501 %in% c("married", "living with partner")) ~ "Not partnered",
    (v501 %in% c("married", "living with partner") & (v741 %in% c("not paid", "in-kind only") | v731 == "no")) ~ "Not paid in cash or not working"))
  # table(IR$wd.ownincome, useNA = "always")

  # BINARY FACTOR FOR EITHER JOINT OR OWN DECISION: FINANCIAL

  IR <- IR %>%
    mutate(jdwd.ownincome = case_when(
      v739 %in% c("respondent alone", "respondent and husband/partner") ~ "Yes",
      (!is.na(v739) & !(v739 %in%c("respondent alone","respondent and husband/partner"))) ~ "No",
      !(v501 %in% c("married", "living with partner")) ~ "Not partnered",
      (v501 %in% c("married", "living with partner") & (v741 %in% c("not paid", "in-kind only") | v731 == "no")) ~ "Not paid in cash or not working"))
  # table(IR$jdwd.ownincome, useNA = "always")

  ### LARGE HOUSEHOLD PURCHASES ----
  IR <- IR %>%
    dplyr::mutate(desc.lrgpur = case_when(
      !is.na(v743b) ~ as.character(v743b),
      marr.cohab == 0 ~ "not partnered"))

  #JOINT DEICSION

  IR <- IR %>% mutate(jd.lrgpur = case_when(
    v743b == "respondent and husband/partner" ~ "Yes",
    (!is.na(v743b) & !(v743b == "respondent and husband/partner")) ~ "No",
    !(v501 %in% c("married", "living with partner")) ~ "Not partnered"))
  # table(IR$jd.lrgpur, useNA = "always")

  # OWN DECISION: LARGE HOUSEHOLD PURCHASES

  IR <- IR %>% mutate(wd.lrgpur = case_when(
    v743b == "respondent alone" ~ "Yes",
    (!is.na(v743b) & !(v743b == "respondent alone")) ~ "No",
    !(v501 %in% c("married", "living with partner")) ~ "Not partnered"))
  # table(IR$wd.lrgpur, useNA = "always")

  # # BINARY FACTOR FOR EITHER JOINT OR OWN DECISION: LARGE HOUSEHOLD PURCHASES

  IR <- IR %>%
    mutate(jdwd.lrgpur = case_when(
      v743b %in% c("respondent alone", "respondent and husband/partner") ~ "Yes",
      (!is.na(v743b) & !(v743b %in%c("respondent alone","respondent and husband/partner"))) ~ "No",
      !(v501 %in% c("married", "living with partner")) ~ "Not partnered"))
  # table(IR$jdwd.lrgpur, useNA = "always")

  ### RESPONDENT'S HEALTH ----
  IR <- IR %>%
    dplyr::mutate(desc.hlth = case_when(
      !is.na(v743a) ~ as.character(v743a),
      marr.cohab == 0 ~ "not partnered"))
  # table(IR$desc.hlth, useNA = "always")

  #JOINT DECISION

  IR <- IR %>% mutate(jd.hlth = case_when(
    v743a == "respondent and husband/partner" ~ "Yes",
    (!is.na(v743a) & !(v743a == "respondent and husband/partner")) ~ "No",
    !(v501 %in% c("married", "living with partner")) ~ "Not partnered"))
  # table(IR$jd.hlth, useNA = "always")

  # OWN DECISION: HEALTH

  IR <- IR %>% mutate(wd.hlth = case_when(
    v743a == "respondent alone" ~ "Yes",
    (!is.na(v743a) & !(v743a == "respondent alone")) ~ "No",
    !(v501 %in% c("married", "living with partner")) ~ "Not partnered"))
  # table(IR$wd.hlth, useNA = "always")

  # BINARY FACTOR FOR EITHER JOINT OR OWN DECISION: HEALTH

  IR <- IR %>%
    mutate(jdwd.hlth = case_when(
      v743a %in% c("respondent alone", "respondent and husband/partner") ~ "Yes",
      (!is.na(v743a) & !(v743a %in%c("respondent alone","respondent and husband/partner"))) ~ "No",
      !(v501 %in% c("married", "living with partner")) ~ "Not partnered"))
  # table(IR$jdwd.hlth, useNA = "always")

  ### VISITS TO FAMILY ----
  IR <- IR %>%
    dplyr::mutate(desc.visit = case_when(
      !is.na(v743d) ~ as.character(v743d),
      marr.cohab == 0 ~ "not partnered"))

  #JOIN DECISION

  IR <- IR %>% mutate(jd.visit = case_when(
    v743d == "respondent and husband/partner" ~ "Yes",
    (!is.na(v743d) & !(v743d == "respondent and husband/partner")) ~ "No",
    !(v501 %in% c("married", "living with partner")) ~ "Not partnered"))
  # table(IR$jd.visit, useNA = "always")

  # OWN DECISION: EVERYDAY DECISIONS

  IR <- IR %>% mutate(wd.visit = case_when(
    v743d == "respondent alone" ~ "Yes",
    (!is.na(v743d) & !(v743d == "respondent alone")) ~ "No",
    !(v501 %in% c("married", "living with partner")) ~ "Not partnered"))


  # BINARY FACTOR FOR EITHER JOINT OR OWN DECISION: EVERYDAY DECISIONS

  IR <- IR %>%
    mutate(jdwd.visit = case_when(
      v743d %in% c("respondent alone", "respondent and husband/partner") ~ "Yes",
      (!is.na(v743d) & !(v743d %in%c("respondent alone","respondent and husband/partner"))) ~ "No",
      !(v501 %in% c("married", "living with partner")) ~ "Not partnered"))
  # table(IR$jdwd.visit, useNA = "always")

  ### HUSBAND'S INCOME ----
  IR <- IR %>%
    dplyr::mutate(desc.money = case_when(
      !is.na(v743f) ~ as.character(v743f),
      marr.cohab == 0 ~ "not partnered"))
  # table(IR$desc.money, useNA = "always")

  #JOIN DECISION

  IR <- IR %>% mutate(jd.money = case_when(
    !(v501 %in% c("married", "living with partner")) ~ "Not partnered",
    v743f == "husband/partner has no earnings" | partner.working == "Didn't work last 12 months" ~ "Partner no earning",
    v743f == "respondent and husband/partner" ~ "Yes",
    (!is.na(v743f) & !(v743f == "respondent and husband/partner")) ~ "No"))
  # table(IR$jd.money, useNA = "always")

  # OWN DECISION: HUSBAND'S INCOME

  IR <- IR %>% mutate(wd.money = case_when(
    !(v501 %in% c("married", "living with partner")) ~ "Not partnered",
    v743f == "husband/partner has no earnings" | partner.working == "Didn't work last 12 months" ~ "Partner no earning",
    v743f == "respondent alone" ~ "Yes",
    (!is.na(v743f) & !(v743f == "respondent alone")) ~ "No"))
  # table(IR$v632, useNA = "always")

  # BINARY FACTOR FOR EITHER JOINT OR OWN DECISION: HUSBAND'S INCOME

  IR <- IR %>%
    mutate(jdwd.money = case_when(
      !(v501 %in% c("married", "living with partner")) ~ "Not partnered",
      v743f == "husband/partner has no earnings" | partner.working == "Didn't work last 12 months" ~ "Partner no earning",
      v743f %in% c("respondent alone", "respondent and husband/partner") ~ "Yes",
      (!is.na(v743f) & !(v743f %in%c("respondent alone","respondent and husband/partner"))) ~ "No"))
  # table(IR$jdwd.money, useNA = "always")

  ### FAMILY PLANNING ----
  IR <- IR %>%
    dplyr::mutate(desc.fp = case_when(
      v632 %in% c("joint decision") | v632a %in% c("joint decision") ~ "respondent and husband/partner",
      v632 %in% c("respondent") | v632a %in% c("mainly respondent") ~ "respondent alone",
      v632 %in% c("husband/partner") | v632a %in% c("mainly husband, partner") ~ "husband/partner alone",
      v632 %in% c("other", "someone else") | v632a %in% c("other") ~ "other",
      marr.cohab == 0 ~ "not partnered"))

  # JOINT DECISION: FAMILY PLANNING

  IR <- IR %>% mutate(jd.fp = case_when(
    !(v501 %in% c("married", "living with partner")) ~ "Not partnered",
    (v632 == "joint decision") ~ "Yes",
    (!is.na(v632) & !(v632 == "joint decision")) ~ "No"))
  # table(IR$jd.fp, useNA = "always")

  # OWN DECISION: FAMILY PLANNING

  IR <- IR %>% mutate(wd.fp = case_when(
    !(v501 %in% c("married", "living with partner")) ~ "Not partnered",
    (v632 == "respondent") ~ "Yes",
    (!is.na(v632) & !(v632 == "respondent")) ~ "No"))
  # table(IR$wd.fp, useNA = "always")

  # BINARY FACTOR FOR EITHER JOINT OR OWN DECISION: FAMILY PLANNING

  IR <- IR %>%
    mutate(jdwd.fp = case_when(
      !(v501 %in% c("married", "living with partner")) ~ "Not partnered",
      v632 %in% c("respondent", "joint decision") ~ "Yes",
      (!is.na(v632) & !(v632 %in%c("respondent", "joint decision"))) ~ "No"))
  # table(IR$jdwd.fp, useNA = "always")

  ### INDEX ----
  # JOINT DDECISION-MAKING INDEX
  IR$jd.index <- IR$jd.ownincome %in% 'Yes' + IR$jd.lrgpur %in% 'Yes' + IR$jd.money %in% 'Yes' + IR$jd.hlth %in% 'Yes' +
    IR$jd.fp %in% 'Yes' + IR$jd.visit %in% 'Yes'
  IR$jd.index[IR$marr.cohab == 0] <- 'not partnered'
  # table(IR$jd.index, useNA = "always")

  # CATEGORICAL FACTOR FOR JOINT DECISION-MAKING INDEX
  IR <- IR %>%
    dplyr::mutate(jd.index.cat = case_when(
      jd.index == "not partnered" ~ "Not partnered",
      jd.index == 0 ~ "None",
      jd.index %in% c(1:2) ~ "1-2",
      jd.index %in% c(3:6) ~ "3-6"))
  # table(IR$jd.index.cat, useNA = "always")

  # WOMEN'S DECISION MAKING INDEX
  IR$wd.index <- IR$wd.ownincome %in% 'Yes' + IR$wd.lrgpur %in% 'Yes' + IR$wd.money %in% 'Yes' + IR$wd.hlth %in% 'Yes' +
    IR$wd.fp %in% 'Yes' + IR$wd.visit %in% 'Yes'
  IR$wd.index[IR$marr.cohab == 0] <- 'not partnered'
  # table(IR$wd.index, useNA = "always")

  # CATEGORICAL FACTOR FOR WOMEN'S DECISION-MAKING INDEX
  IR <- IR %>% dplyr::mutate(wd.index.cat = case_when(
    wd.index == "not partnered" ~ "Not partnered",
    wd.index == 0 ~ "None",
    wd.index %in% c(1) ~ "1",
    wd.index %in% c(2:6) ~ "2+"))
  # table(IR$wd.index.cat, useNA = "always")


  # JOINT OR WOMAN DECISION MAKING INDEX
  IR$jdwd.index <- IR$jdwd.ownincome %in% 'Yes' + IR$jdwd.lrgpur %in% 'Yes' + IR$jdwd.money %in% 'Yes' + IR$jdwd.hlth %in% 'Yes' +
    IR$jdwd.fp %in% 'Yes' + IR$jdwd.visit %in% 'Yes'
  IR$jdwd.index[IR$marr.cohab == 0] <- 'not partnered'
  # table(IR$jdwd.index, useNA = "always")

  # CATEGORICAL FACTOR FOR JOINT/WOMEN DECISION-MAKING INDEX
  IR <- IR %>%
    dplyr::mutate(jdwd.index.cat = case_when(
      jdwd.index == "not partnered" ~ "Not partnered",
      jdwd.index == 0 ~ "None",
      jdwd.index %in% c(1:2) ~ "1-2",
      jdwd.index %in% c(3:6) ~ "3-6"))



  ## PARTNER CHARACTERISTICS ----
  ### PARTNER'S AGE ----
  IR <- IR %>%
    dplyr::mutate(partner.age = case_when(marr.cohab == 0 ~ "not partnered",
                                          TRUE ~ as.character(v730)))
  # CATEGORICAL FACTOR FOR PARTNER'S AGE
  IR <- IR %>% dplyr::mutate(partner.age.cat = case_when(
    v730 %in% c(15:29) ~ "under 30",
    v730 %in% c(30:59) ~ "30-59",
    v730 %in% c(60:96) ~ "60+",
    v730 == 98 ~ "Don't know",
    v730 == 99 ~ "Missing",
    !(v501 %in% c("married", "living with partner")) ~ "not partnered"))
  # table(IR$partner.age, useNA = "always")


  ### DIFFERENCE BETWEEN WOMAN AND PARTNER'S AGE ----
  IR <- IR %>%
    dplyr::mutate(age.diff = case_when(is.na(v730) | v730 > 96 ~ NA,
                                       TRUE ~ v730 - v012))


  ## WOMAN HAD PREVIOUS PARTNERSHIPS ----
  IR <- IR %>% dplyr::mutate(prev.pship = case_when(
    v503 %in% c("once") ~ "No",
    v503 %in% c("more than once") ~ "Yes",
    v501 %in% c("never in union") ~ "Never in union"))
  # table(IR$prev.pship, useNA = "always")

  #Partnership by category AG 2/21/23
  IR$pship.status <- (IR$v501)
  IR<- IR %>% mutate(pship.cat = case_when(
    pship.status == "never in union" ~ "never",
    pship.status == "widowed" ~ "no partner now",
    pship.status == "divorced" ~ "no partner now",
    pship.status == "no longer living together/separated" ~ "no partner now",
    pship.status == "married" ~ "married/cohab",
    pship.status == "living with partner" ~ "married/cohab"))
  # table(IR$pship.cat, useNA = "always")


  ## RESPONDENT HAS TELEPHONE ----
  IR<-IR %>%
    dplyr::mutate(has.mobile = case_when(
      v169a=="no" ~ "No",
      v169a== "yes" ~ "Yes"))
  # table(IR$has.mobile, useNA = "always")

  ## USE MOBILE PHONE FOR FINANCIAL TRANSACTIONS ----
  IR <- IR %>%
    dplyr::mutate(mobile.financial = case_when(
      v169b == "no" ~ "No",
      v169b == "yes" ~ "Yes"))
  # table(IR$mobile.financial, useNA = "always")

  ## HAS BANK ACCOUNT ---
  IR <- IR %>%
    dplyr::mutate(has.bank = case_when(
      v170 == "no" ~ "No",
      v170 == "yes" ~ "Yes"))

  ## USE OF INTERNET ----
  IR<-IR %>% dplyr::mutate(internet.use = case_when(
    v171a %in% c("never", "yes, before last 12 months") ~ "No",
    v171a== "yes, last 12 months" ~ "Yes"))
  # table(IR$v171a, useNA = "always")

  ## CHILD HEALTH ----
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


  # GAVE CHILD MEAT (BEEF, PORK, LAMB, CHICKEN, ETC)
  IR<-IR %>% dplyr::mutate(bf.meat = case_when(
    v414h %in% c("no","don't know") ~ "No",
    v414h== "yes" ~ "Yes",
    is.na(v414h) ~ "No living child <24months at home"))
  # table(IR$bf.meat, useNA = "always")

  # GAVE CHILD FOOD MADE FROM BEANS, PEAS, LENTILS
  IR<-IR %>% dplyr::mutate(bf.beans = case_when(
    v414o %in% c("no","don't know") ~ 'No',
    v414o== "yes" ~ "Yes",
    is.na(v414o) ~ "No living child <24months at home"))

  # GAVE CHILD OTHER SOLID-SEMISOLID FOOD
  IR<-IR %>% dplyr::mutate(bf.other.solid = case_when(
    v414s %in% c("no","don't know") ~ "No",
    v414s== "yes" ~ "Yes",
    is.na(v414s) ~ "No living child <24months at home"))


  ## WOMAN CHARACTERISTICS ----
  ### RELIGION ----
  IR <- IR %>%
    dplyr::mutate(religion = case_when(v130 == "christian" ~ "christian",
                                       v130 == "islam" ~ "muslim",
                                       TRUE ~ as.character(v130)))
  # MUSLIM RELIGION
  IR$muslim <- ifelse(IR$religion == "muslim", "Yes", "No")
  # table(IR$muslim, useNA = "always")

  # CATHOLIC RELIGION
  IR$catholic <- ifelse(IR$religion == "catholic", "Yes", "No")

  # CATHOLIC CHRISTIAN
  IR$other.christian <- ifelse(IR$religion == "other christian", "Yes", "No")


  ### EDUCATION FACTORS ----

  # HIGHEST LEVEL OF EDUCATION
  IR$ed.level <- IR$v106
  # table(IR$ed.level)

  # ADDING FOR VARIABLE FOR URBAN SETTING
  IR <- IR %>% mutate(ed.level.2 = case_when(
    ed.level == "higher" ~ "higher",
    ed.level == "secondary" ~ "secondary",
    ed.level == "no education" ~ "no education or primary",
    ed.level == "primary" ~ "no education or primary"
  ))

  table(IR$ed.level.2)

  # BINARY FACTOR FOR WOMAN'S EDUCATION
  IR$anyed.yn <- ifelse(IR$v149 == "no education", "No", "Yes")
  # table(IR$ed.level, useNA = "always")

  ## HUSBAND/PARTNER'S EDUCATION LEVEL ----
  IR <- IR %>%
    dplyr::mutate(partner.ed.level = case_when(
      marr.cohab == 0 ~ "not partnered",
      TRUE ~ as.character(v701)))
  # table(IR$partner.ed.level, useNA = "always")

  # HUSBAND/PARTNER'S EDUCATION LEVEL - CAT 1
  IR <- IR %>%
    dplyr::mutate(partner.ed.level.cat1 = case_when(
      v701 %in% c("primary", "others", "higher", "secondary") ~ "education",
      v701 %in% c("no education") ~ "no education",
      marr.cohab == 0 ~ "not partnered"))
  #table(IR$partner.ed.level.cat1, useNA = "always")


  # HUSBAND/PARTNER'S EDUCATION LEVEL - CAT 2
  IR <- IR %>%
    dplyr::mutate(partner.ed.level.cat2 = case_when(
      marr.cohab == 0 ~ "Not partnered",
      v701 %in% c("no education") ~ "no education",
      v701 %in% c("higher", "secondary") ~ "secondary or more",
      v701 %in% c("primary", "others") ~ "some education"))
  table(IR$partner.ed.level.cat2, useNA = "always")

  # HUSBAND/PARTNER'S EDUCATION LEVEL - CAT 3
  IR <- IR %>%
    dplyr::mutate(partner.ed.level.cat3 = case_when(
      marr.cohab == 0 ~ "Not partnered",
      v701 %in% c("higher", "secondary") ~ "secondary or more",
      v701 %in% c("primary", "others", "no education") ~ "some education or no education"))
  table(IR$partner.ed.level.cat3, useNA = "always")

  ## REPRODUCTIVE HISTORY ----
  ### AGE AT 1st COHAB ----
  IR <- IR %>%
    dplyr::mutate(age.1stcohab = case_when(v501 == "never in union" ~ "never partnered",
                                           TRUE ~ as.character(v511)))

  #CATEGORICAL FACTOR FOR AGE AT FIRST MARRIAGE/COHABITATION
  IR<- IR %>%
    dplyr::mutate(age.1stcohab.cat = case_when(
      v501 == "never in union" ~ "never",
      (v511 > 0 & v511 < 16) ~ "<16",
      (v511 >= 16 & v511 < 20) ~ "16-19",
      (v511 >= 20) ~ "20+"))
  # table(IR$age.1stcohab.cat, useNA = "always")

  ### AGE AT FIRST SEX (IMPUTED) ----
  IR <- IR %>%
    dplyr::mutate(age.1stsex = case_when(v531 %in% c(97, 98, 0) ~ NA,
                                         TRUE ~ as.numeric(v531)))

  # CATEGORICAL FACTOR FOR AGE AT FIRST SEX
  # PRESERVING NA AS NA
  IR<- IR %>%
    dplyr::mutate(age.1stsex.cat1 = case_when((age.1stsex > 0 & age.1stsex < 15) ~ "5-14",
                                              (age.1stsex >= 15 & age.1stsex < 20) ~ "15-19",
                                              (age.1stsex >= 20 & age.1stsex < 50) ~ "20+"))
  # table(IR$age.1stsex.cat1, useNA = "always")

  # CATEGORICAL FACTOR FOR AGE AT FIRST SEX
  # ACCOUNTING FOR SKIP PATTERNS WHICH CREATE NA
  IR<- IR %>%
    dplyr::mutate(age.1stsex.cat = case_when(v531 %in% c(0, "not had sex") ~ "never",
                                             age.1stsex < 16 ~ "<16",
                                             age.1stsex >= 16 & age.1stsex < 20 ~ "16-19",
                                             age.1stsex >= 20 & age.1stsex < 50 ~ "20+",
                                             v531 %in% c(97, 98, "inconsistent", "don't know") ~ NA))

  # table(IR$age.1stsex.cat, useNA = "always")

  IR$early.sex.15 <- ifelse(IR$v531 >0 & IR$v531 <15, "Yes", "No")
  # table(IR$early.sex.15, useNA = "always")


  ### AGE AT FIRST BIRTH ----
  IR$age.1stbrth <- IR$v212
  # table(IR$age.1stbrth, useNA = "always")

  # AGE AT FIRST BIRTH CATEGORY 1
  IR <- IR %>%
    dplyr::mutate(age.1stbrth.cat1 = case_when(age.1stbrth >= 19 ~ "19+",
                                               age.1stbrth < 19 ~ "<19"))
  # table(IR$age.1stbrth.cat1, useNA = "always")

  # AGE AT FIRST BIRTH CATEGORY 2
  IR <- IR %>%
    dplyr::mutate(age.1stbrth.cat2 = case_when(v212 < 20 ~ "<20",
                                               v212 >= 20 & v212 < 30 ~ "20-29",
                                               v212 >= 30  ~ "30+"))
  # table(IR$age.1stbrth.cat2, useNA = "always")

  # AGE AT FIRST BIRTH CATEGORY 3
  IR <- IR %>%
    dplyr::mutate(age.1stbrth.cat3 = case_when(v212 < 16 ~ "<16",
                                               v212 >= 16 & v212 < 20 ~ "16-19",
                                               v212 >= 20 & v212 < 25 ~ "20-24",
                                               v212 >= 25 ~ "25+"))
  # table(IR$age.1stbrth.cat3, useNA = "always")

  # AGE AT FIRST BIRTH CATEGORY 4
  IR <- IR %>%
    dplyr::mutate(age.1stbrth.cat4 = case_when(v212 < 15 ~ "<15",
                                               v212 >= 15 & v212 < 20 ~ "15-19",
                                               v212 >= 20 & v212 < 25 ~ "20-24",
                                               v212 >= 25 & v212 < 30 ~ "25-29",
                                               v212 >= 30 ~ "30+"))
  # table(IR$age.1stbrth.cat4, useNA = "always")

  ## TOTAL NUMBER OF SEX PARTNERS ----
  IR <- IR %>%
    dplyr::mutate(total.sex.partners = case_when(v836 %in% c("98", "don't know") ~ NA,
                                                 v836 == "95+" ~ 95,
                                                 TRUE ~ as.numeric(v836)))

  IR <- IR %>%
    dplyr::mutate(total.sex.partners.cat = case_when(total.sex.partners == 1 ~ "1",
                                                     total.sex.partners == 2 ~ "2",
                                                     total.sex.partners > 2 ~ "3+"))

  # table(IR$total.sex.partners.cat, useNA = "always")

  ## WHO CHECKED RESPONDENT HEALTH AFTER DISCHARGE ----
  IR <- IR %>%
    dplyr::mutate(discharge.checkedhealth = case_when(
      v201 == 0 ~ "no births",
      m68_1 %in% c("doctor","nurse/midwife", "community health extension worker") ~ "health personnel",
      m68_1 %in% c("traditional birth attendant","auxiliary midwife") ~ "midwife/aux midwife",
      m68_1 %in% c("community health influencers promoters and services (chips)/community health wor", "other") ~ "Other",
      is.na(IR$m68_1) ~ "No live birth <36 months"))
  # table(IR$m68_1, useNA = "always")

  ## FERTILITY PERFERENCE RELATED ----
  ### DISAGREEMENT ON CHILD PREFERENCE ----
  IR <- IR %>%
    dplyr::mutate(child.pref.discrep = case_when(v627 %in% c(96, "other") ~ NA,
                                                 v628 %in% c(96, "other") ~ NA,
                                                 v627 %in% c(0:30) & v628 %in% c(0:30) ~ as.numeric(v627) - as.numeric(v628)))
  ### MALE CHILD REFERENCE ----
  IR <- IR %>%
    dplyr::mutate(male.child.pref = case_when(child.pref.discrep > 0 ~ 1,
                                              child.pref.discrep <= 0 ~ 0,
                                              v627 %in% c(96, "other") & v628 %in% c(96, "other") ~ 0))

  ### FERTILITY PREFERENCE (SPACING) ----
  IR <- IR %>%
    dplyr::mutate(fertility.pref = case_when(v602 %in% c('sterilized (respondent or partner)','declared infecund') ~ "sterilized/infecund",
                                             TRUE ~ v602))


  # PREFERRED WAITING TIME FOR BIRTH OF A/ANOTHER CHILD (GROUPED)
  IR <- IR %>%
    dplyr::mutate(fertility.pref.cat = case_when(
      v604 == "non-numeric" ~ "No specific timing",
      v604 %in% c("<12 months", "1 year") ~ "<2 years",
      v604 %in% c("2 years", "3 years","4 years") ~ "2-4 years",
      v604 %in% c("5 years", "6+ years") ~ "5+ years",
      v604 %in% c("don't know") ~ "No specific timing",
      v602 != "have another" ~ "No more/undecided"))
  # table(IR$fertility.pref.cat, useNA = "always")

  # PREFERRED WAITING TIME FOR BIRTH OF A/ANOTHER CHILD (GROUPED)
  IR <- IR %>%
    dplyr::mutate(fertility.pref.cat2 = case_when(
      v604 == "non-numeric" ~ "No specific timing",
      v604 %in% c("<12 months", "1 year") ~ "<2 years",
      v604 %in% c("2 years", "3 years", "4 years",
                  "5 years", "6+ years") ~ "2+ years",
      v604 %in% c("don't know") ~ "No specific timing",
      v602 != "have another" ~ "No more/undecided"))
  # table(IR$fertility.pref.cat2, useNA = "always")

  ### IDEAL NUMBER OF CHILDREN CATEGORY ----



  IR <- IR %>%
    dplyr::mutate(ideal.n.child.cat = case_when(v614 %in% c(0:4) ~ '0-4',
                                                v614 >= 5 ~ '5+'))
  # table(IR$ideal.n.child.cat, useNA = "always")


  ### HUSBAND'S DESIRE FOR CHILRDEN ----
  IR <- IR %>%
    dplyr::mutate(partner.desire.child = case_when(marr.cohab == 0 ~ "not partnered",
                                                   TRUE ~ as.character(v621)))
  # table(IR$partner.desire.child, useNA = "always")

  ## CONDOM USED DURING LAST SEX WITH MOST RECENT PARTNER (WOMEN) ----
  # IR$condom.last.sex <- IR$v761
  IR <- IR %>%
    dplyr::mutate(condom.last.sex = case_when(
      v761 == "no" ~ "No",
      v761 == "yes" ~ "Yes",
      v761 == "don't know" ~ NA,
      v766b==0 ~ "No sex partner in last 12 months"))
  # table(IR$condom.last.sex, useNA = "always")

  ## CONDOM USED DURING LAST SEX WITH 2ND TO MOST RECENT PARTNER (WOMEN)
  # IR$condom.2nd.last.sex <- IR$v761b
  IR <- IR %>%
    dplyr::mutate(condom.2nd.last.sex = case_when(
      v761b == "no" ~ "No",
      v761b == "yes" ~ "Yes",
      v761b == "don't know" ~ NA,
      v766b==1 ~  "Only 1 sex partner",
      v766b==0 ~ "No sex partner in last 12 months"))
  # table(IR$v766b, useNA = "always")

  # CONDOM USED DURING LAST SEX WITH 3RD TO MOST RECENT PARTNER (WOMEN)
  # IR$condom.3rd.last.sex <- IR$v761c
  IR <- IR %>%
    dplyr::mutate(condom.3rd.last.sex = case_when(
      v761c == "no" ~ "No",
      v761c == "yes" ~ "Yes",
      v761c == "don't know" ~ NA,
      v766b==1 ~  "Only 1 sex partner",
      v766b==2 ~  "Only 2 sex partners",
      v766b==0 ~ "No sex partner in last 12 months"))
  # table(IR$condom.3rd.last.sex, useNA = "always")

  ## SOURCE OF CONDOMS USED FOR LAST SEX
  IR <- IR %>%
    dplyr::mutate(condom.source = case_when(condom.last.sex == "no" ~ "condom not used",
                                            TRUE ~ as.character(v762)))
  # table(IR$condom.source, useNA = "always")

  # NUMBER OF SEX PARTNERS, INCLUDING SPOUSE, IN LAST 12 MONTHS
  IR$n.sex.incl.partner.12m <- IR$v766b
  # table(IR$n.sex.incl.partner.12m, useNA = "always")

  ## REASONS NOT USING FP ----
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
    (v3a08q == "no" & v3a08r == "no" & fp.all.na==0) ~ "no",
    v213 == "yes" ~ "Currently pregnant",
    v361 == "currently using" ~ "Currently using FP",
    is.na(v3a08q) ~ "No identified need for FP"))

  IR <- IR %>% dplyr::mutate(no.fp.oppose = case_when(
    (v3a08i == "yes" | v3a08j == "yes" | v3a08k == "yes" | v3a08l == "yes") ~ "yes",
    (v3a08i == "no" & v3a08j == "no" & v3a08k == "no" & v3a08l == "no" & fp.all.na==1) ~ "no",
    v213 == "yes" ~ "Currently pregnant",
    v361 == "currently using" ~ "Currently using FP",
    is.na(v3a08j) ~ "No identified need for FP"))

  IR <- IR %>% dplyr::mutate(no.fp.noneed = case_when(
    (v3a08b == "yes" | v3a08d == "yes" | v3a08e == "yes" | v3a08f == "yes" | v3a08g == "yes") ~ "yes",
    (v3a08b == "no" & v3a08d == "no" & v3a08e == "no" & v3a08f == "no" & v3a08g == "no" & fp.all.na==1) ~ "no",
    v213 == "yes" ~ "Currently pregnant",
    v361 == "currently using" ~ "Currently using FP",
    is.na(v3a08b) ~ "No identified need for FP"))

  IR <- IR %>% dplyr::mutate(no.fp.supply = case_when(
    (v3a08u == "yes" | v3a08v == "yes") ~ "yes",
    (v3a08u == "no" & v3a08v == "no" & fp.all.na==1) ~ "no",
    v213 == "yes" ~ "Currently pregnant",
    v361 == "currently using" ~ "Currently using FP",
    is.na(v3a08u) ~ "No identified need for FP"))


  ## ATTITUDES ABOUT DOMESTIC VIOLENCE ----

  # BEATING JUSTIFIED IF WIFE GOES OUT WITHOUT TELLING HUSBAND
  IR <- IR %>% dplyr::mutate(dv.out = case_when(
    v744a %in% c("no", "don't know") ~ 0,
    v744a == "yes" ~ 1))

  # BEATING JUSITIFIED IF WIFE NEGLECTS CHILDREN
  IR <- IR %>% dplyr::mutate(dv.negkid = case_when(
    v744b %in% c("no", "don't know") ~ 0,
    v744b == "yes" ~ 1))

  # BEATING JUSTIFIED IF WIFE ARGUES WITH HUSBAND
  IR <- IR %>% dplyr::mutate(dv.argue = case_when(
    v744c %in% c("no", "don't know") ~ 0,
    v744c == "yes" ~ 1))

  # BEATING JUSTIFIED IF WIFE REFUSES TO HAVE SEX WITH HUSBAND
  IR <- IR %>% dplyr::mutate(dv.nosex = case_when(
    v744d %in% c("no", "don't know") ~ 0,
    v744d == "yes" ~ 1))

  # BEATING JUSTIFIED IF WIFE BURNS FOOD
  IR <- IR %>% dplyr::mutate(dv.burnfd = case_when(
    v744e %in% c("no", "don't know") ~ 0,
    v744e == "yes" ~ 1))


  # # INDEX OF WOMAN'S ATTITUDES ABOUT DOMESTIC VIOLENCE
  cols <- c("dv.out", "dv.negkid", "dv.argue", "dv.nosex", "dv.burnfd")
  IR <- IR %>%
    dplyr::mutate(dv.sum = case_when(
      if_all(all_of(cols), is.na) ~ NA_real_,
      TRUE ~ rowSums(across(all_of(cols)), na.rm = TRUE)))

  # IR$dv.index <-ifelse(IR$dv.nacnt == 5, NA, IR$dv.sum)
  IR$dv.index <-as.character(IR$dv.sum)

  IR <- IR %>%
    dplyr::mutate(dv.index.cat = case_when(
      dv.index == 0 ~ "None",
      dv.index %in% c(1:5) ~ "1+"))
  # dv.index %in% c(3:5) ~ "3-5"))
  # table(IR$dv.index, useNA = "always")


  ## FEMALE GENITAL MUTILATION ----

  # FEMALE CIRCUMCISION
  IR <- IR %>% dplyr::mutate(female.circumcision = case_when(
    g101 == "no" ~ "No",
    g102 =="no" ~ "No",
    g102 == "yes" ~ "Yes"))


  # EVER HEARD OF GENITAL CUTTING (PROBED) (WOMEN) or female circ
  IR$know.genitalcut <- ifelse(IR$g101=='yes' | IR$g100=='yes', "Yes", "No")

  # table(IR$know.genitalcut, useNA = "always")

  # FLESH REMOVED FROM GENITAL AREA
  IR <- IR %>%
    dplyr::mutate(genitalflesh.removed = case_when(female.circumcision == "No" ~ "never circumcised",
                                                   TRUE ~ as.character(g103)))
  # table(IR$genitalflesh.removed, useNA = "always")

  # GENITAL AREA JUST NICKED WITHOUT REMOVING ANY FLESH
  IR <- IR %>%
    dplyr::mutate(genital.nicked = case_when(female.circumcision == "No" ~ "never circumcised",
                                             TRUE ~ as.character(g104)))
  # GENITAL AREA SEWN UP
  IR <- IR %>%
    dplyr::mutate(genital.sewn = case_when(female.circumcision == "No" ~ "never circumcised",
                                           TRUE ~ as.character(g105)))
  # table(IR$genital.sewn, useNA = "always")

  ## MEDIA EXPOSURE ----
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
  IR$any.media.yn <- ifelse((IR$newsp.yn == 1 | IR$rad.yn == 1 | IR$tv.yn == 1), "Yes", "No")
  # table(IR$any.media.yn, useNA = "always")


  ## EXPOSURE TO FP MESSAGING ----
  #Any exposure to FP messaging from radio, TV, newspaper/magazine, or text message on mobile in the "last few months".
  # table(IR$v384g, useNA = "always")
  IR <- IR %>%
    mutate(
      fp.message.exp = case_when(
        if_any(starts_with("v384"), ~ .x == "yes") ~ "Yes",
        TRUE ~ "No"
      )
    )
  # table(IR$fp.message.exp, useNA = "ifany")

  ## HEALTH INSURANCE ----
  ### Y/N COVERAGE ----
  # table(IR$v481, useNA = "ifany")
  IR<-IR %>%
    dplyr::mutate(wm.hlth.insurance = case_when(
      v481=="no" ~ "No",
      v481== "yes" ~ "Yes"))
  # table(IR$wm.hlth.insurance, useNA = "ifany")

  ### HEALTH INSURANCE TYPE ----
  IR <- IR %>%
    mutate(
      wm.hlth.insurance.type = case_when(
        v481a == "yes" ~ "Mutual health org/community based insurance",
        v481b == "yes" ~ "Employer based",
        v481c == "yes" ~ "Social security",
        v481d == "yes" ~ "Privately purchased/commercial",
        v481e == "yes" | v481f == "yes" | v481g == "yes" | v481h == "yes" | v481x == "yes" ~ "Other",
        TRUE ~ "NA - no insurance"))
  # table(IR$wm.hlth.insurance.type, useNA = "ifany") #some categories are extremely small. Need to recode during exploratory.




  ## TRAVEL TIME TO NEAREST FACILITY ----
  IR <- IR %>% mutate(travtime.fac.cat = case_when(v483a <= 10 ~ "10 minutes or less",
                                                   v483a > 10 & v483a <= 30 ~ "11-30 minutes",
                                                   v483a > 30 & v483a <= 60 ~ "31-60 minutes",
                                                   v483a > 60 ~ "over an hour"))

  # table(IR$travtime.fac.cat, useNA = "ifany")
  # table(IR$v483a, useNA = "always")


  ## PREGNANCY WANTED ----
  #BASE: Women who gave birth to a child in the last three/five years (V417 > 0).
  IR <- IR %>%
    mutate(preg.wanted = case_when(
      v367 == "wanted then" | v367 == "wanted later" ~ "Yes",
      v367 == "wanted no more" ~ "No",
      v417 == 0 ~ "NA - no child last 3/5 yrs"))
  # table(IR$preg.wanted, useNA = "always")

  ## INTENTION TO USE CONTRACEPTION ----
  #BASE: All respondents not currently using contraception (V312 = 0).
  #Does she intend to use contraceptives in the future or not
  IR <- IR %>%
    mutate(
      intent.fp = case_when(
        v362 == "in next 12 months" | v362 == "use later" ~ "Yes",
        v362 == "unsure about timing" | v362 == "unsure about use" ~ "Unsure",
        v362 == "does not intend" ~ "No",
        TRUE ~ "NA - currently using FP")) #crossed out the line for "Never had sex" (in this sample there is 0) to avoid of error running EDA.
  # table(IR$v312, useNA = "ifany")
  # table(IR$intent.fp, useNA = "ifany")

  # 3 | BR FILES ----

  # CHILD UNDER 15 LIVING ELSEWHERE (NOT IN HOUSEHOLD)
  BR$lives.away <- ifelse((BR$b9 %in% c("lives elsewhere", "someone else", "other relative", "father") & (BR$b8 %in% 1:14)), 1, 0)


  # NUMBER OF CHILDREN UNDER 15 LIVING ELSEWHERE
  BR <- BR %>%
    group_by(caseid) %>%
    dplyr::mutate(num.kids.away = sum(lives.away, na.rm = TRUE))


  # 4 | MR FILE ----



  # 5 | CLEAN-UP ----

  # RENAME JOINING VARIABLES FROM HOUSEHOLD FILE
  HH$v001 <- HH$hv001
  HH$v002 <- HH$hv002


  # CALCULATE WT FOR INDIVIDUAL
  IR$wt <- IR$v005/1000000


  # IR
  IR.seg <- IR

  # HH
  HH.seg <- HH

  # BR
  BR.seg <- BR %>%
    group_by(survey, caseid) %>%
    dplyr::summarize(
      lives.away.cnt = ifelse(
        all(is.na(lives.away)),
        NA,
        sum(lives.away, na.rm = TRUE)
      ),
      .groups = "drop"
    ) %>%
    dplyr::mutate(
      lives.away = ifelse(lives.away.cnt > 0, 1, 0)
    ) %>%
    dplyr::select(survey, caseid, lives.away)


  vulnerability <- IR.seg %>%
    base::merge(BR.seg, by=c("survey", "caseid"), all.x=TRUE) %>%
    base::merge(HH.seg, by=c("survey", "v001", "v002"), all.x=TRUE)


  # CREATE SEGMENTATION STRATA
  vulnerability <- vulnerability %>%
    dplyr::mutate(strata = hv025)


  return(vulnerability)

}

