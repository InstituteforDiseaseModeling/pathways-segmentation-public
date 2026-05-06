################################################################################
# GENERATE VULNERABILITY FACTORS | DHS
################################################################################

# TABLE OF CONTENTS
# 1 | HH FILE
# 2 | IR FILE
# 3 | BR FILE
# 4 | MR FILE (OPTIONAL)
# 5 | CLEAN UP


gen_vulnerability_factors_dhs <- function(IR = NULL, BR = NULL, HH = NULL,
                                          MR = NULL, PR = NULL, dhs = 8) {

  library(dplyr)
  library(reshape2)
  library(stringr)
  library(data.table)

  message("[1/6] Starting vulnerability factor generation...")
  t_start <- proc.time()

  # Helper: safely access a column that may not exist in this survey
  col_or_na <- function(df, col) {
    if (col %in% names(df)) df[[col]] else NA
  }

  # Helper: apply case_when only if all referenced columns exist
  safe_mutate <- function(df, new_col, cols_needed, expr_fn) {
    missing <- setdiff(cols_needed, names(df))
    if (length(missing) > 0) {
      message("  Skipping '", new_col, "' — column(s) not in this survey: ",
              paste(missing, collapse = ", "))
      df[[new_col]] <- NA
      return(df)
    }
    expr_fn(df)
  }


  ######################################################################
  # FILTER TO BIRTHS WITHIN THE LAST 10 YEARS
  BR <- BR %>% dplyr::filter(b3 >= (v008 - 120))
  IR <- IR %>% dplyr::filter(caseid %in% BR$caseid)

  if (!is.null(PR)) {
    PR <- PR %>%
      dplyr::mutate(
        hv105_num = dplyr::case_when(
          hv105 == "95+"        ~ 95,
          hv105 == "don't know" ~ NA_real_,
          TRUE                  ~ suppressWarnings(as.numeric(as.character(hv105)))
        )
      ) %>%
      dplyr::filter(hv103 == "yes", !is.na(hv105_num), hv105_num >= 5) %>%
      dplyr::select(-hv105_num)
  }


  ######################################################################
  # COERCE FACTOR COLUMNS TO NUMERIC WHERE NEEDED
  # haven::read_dta() sometimes reads numeric DHS variables as labelled
  # factors. Any variable used with a numeric comparison operator (>, <,
  # >=, <=) must be numeric before it reaches those expressions.
  # Add columns here as needed when introducing new surveys.

  coerce_to_numeric <- function(df, cols) {
    for (col in intersect(cols, names(df))) {
      if (is.factor(df[[col]]) || haven::is.labelled(df[[col]])) {
        df[[col]] <- suppressWarnings(as.numeric(as.character(df[[col]])))
      }
    }
    df
  }

  IR <- coerce_to_numeric(IR, c(
    "v506",    # wife order — used with >= comparison
    "v730",    # partner age — used with range comparisons
    "v531",    # age at first sex — used with > < comparisons
    "v614",    # ideal number of children — used with >= comparison
    "v483a",   # travel time to facility — used with <= > comparisons
    "v512",    # years in current union
    "v511",    # age at first cohabitation
    "v212",    # age at first birth
    "v012",    # respondent age
    "v013",    # age in 5 year groups
    "v201",    # total children ever born
    "v218",    # number of living children
    "v137",    # children under 5 in household
    "v202",    # sons at home
    "v203"     # daughters at home
  ))

  HH <- coerce_to_numeric(HH, c(
    "hv010",   # number of women 15-49 in HH
    "hv012",   # number of household members
    "hv013",   # number of dejure members
    "hv014",   # number of children under 5
    "hv216",   # number of rooms for sleeping
    "hv204",   # minutes to water source
    "hv238"    # number of HHs sharing toilet
  ))

  # hv105_* age-of-member columns — coerce all present
  hv105_present <- names(HH)[grepl("^hv105_", names(HH))]
  HH <- coerce_to_numeric(HH, hv105_present)

  # hv108_* years of education columns — coerce all present
  hv108_present <- names(HH)[grepl("^hv108_", names(HH))]
  HH <- coerce_to_numeric(HH, hv108_present)


  ######################################################################
  # 1 | HH FILE ----
  message("[2/6] Processing HH file...")

  ## LIVING CONDITIONS ----

  HH$water <- ifelse(HH$hv201 %in% c(
    "unprotected well", "unprotected spring",
    "river/dam/lake/ponds/stream/canal/irrigation channel",
    "rainwater", "other"), 1, 0)

  HH <- safe_mutate(HH, "hh.wat.insuff", "hv201b", function(df) {
    df %>% dplyr::mutate(hh.wat.insuff = dplyr::case_when(
      hv201b == "yes"        ~ "Not sufficient",
      hv201b == "no"         ~ "Sufficient",
      hv201b == "don't know" ~ NA_character_))
  })

  HH$hh.noimp.floor <- ifelse(
    HH$hv213 %in% c("earth/sand", "dung", "wood planks", "palm/bamboo"), 1, 0)

  HH$hh.noimp.wall <- ifelse(HH$hv214 %in% c(
    "no walls", "cane/palm/trunks", "dirt", "mud", "bamboo with mud",
    "stone with mud", "uncovered adobe", "plywood", "cardboard", "reused wood"), 1, 0)

  HH$hh.noimp.housing <- ifelse(HH$hh.noimp.floor == 1 & HH$hh.noimp.wall == 1, 1, 0)

  ## PLACE OF RESIDENCE ----
  HH <- HH %>% dplyr::mutate(hh.urban = dplyr::case_when(
    hv025 == "rural" ~ "No",
    hv025 == "urban" ~ "Yes"))

  ## HOUSEHOLD ECONOMICS ----

  HH <- HH %>% dplyr::mutate(
    hh.electricity = dplyr::case_when(hv206 == "yes" ~ 1, hv206 == "no" ~ 0),
    hh.bank.acct   = dplyr::case_when(hv247 == "yes" ~ 1, hv247 == "no" ~ 0),
    hh.radio       = dplyr::case_when(hv207 == "yes" ~ 1, hv207 == "no" ~ 0),
    hh.tv          = dplyr::case_when(hv208 == "yes" ~ 1, hv208 == "no" ~ 0),
    hh.refrig      = dplyr::case_when(hv209 == "yes" ~ 1, hv209 == "no" ~ 0),
    hh.bike        = dplyr::case_when(hv210 == "yes" ~ 1, hv210 == "no" ~ 0),
    hh.motor       = dplyr::case_when(hv211 == "yes" ~ 1, hv211 == "no" ~ 0),
    hh.car         = dplyr::case_when(hv212 == "yes" ~ 1, hv212 == "no" ~ 0))

  HH <- safe_mutate(HH, "hh.mobile", "hv243a", function(df) {
    df %>% dplyr::mutate(hh.mobile = dplyr::case_when(
      hv243a == "yes" ~ 1, hv243a == "no" ~ 0))
  })

  HH <- safe_mutate(HH, "hh.cart", "hv243c", function(df) {
    df %>% dplyr::mutate(hh.cart = dplyr::case_when(
      hv243c == "yes" ~ 1, hv243c == "no" ~ 0))
  })

  HH <- safe_mutate(HH, "hh.motorboat", "hv243d", function(df) {
    df %>% dplyr::mutate(hh.motorboat = dplyr::case_when(
      hv243d == "yes" ~ 1, hv243d == "no" ~ 0))
  })

  HH$hh.motortransport.yn <- ifelse(HH$hh.motor == 1 | HH$hh.car == 1, "Yes", "No")

  HH <- safe_mutate(HH, "hh.computer", "hv243e", function(df) {
    df %>% dplyr::mutate(hh.computer = dplyr::case_when(
      hv243e == "yes" ~ 1, hv243e == "no" ~ 0))
  })

  HH <- safe_mutate(HH, "hh.land", "hv244", function(df) {
    df %>% dplyr::mutate(hh.land = dplyr::case_when(
      hv244 == "yes" ~ 1, hv244 == "no" ~ 0))
  })

  HH <- safe_mutate(HH, "hh.animal", "hv246", function(df) {
    df %>% dplyr::mutate(hh.animal = dplyr::case_when(
      hv246 == "yes" ~ "Yes", hv246 == "no" ~ "No"))
  })

  HH <- safe_mutate(HH, "hh.cows", "hv246b", function(df) {
    df %>% dplyr::mutate(hh.cows = dplyr::case_when(
      hv246b %in% c(1:98, "95 or more", "unknown") ~ 1,
      hv246b %in% c(0, "none") ~ 0))
  })

  HH <- safe_mutate(HH, "hh.horses", "hv246c", function(df) {
    df %>% dplyr::mutate(hh.horses = dplyr::case_when(
      hv246c %in% c(1:98, "95 or more", "unknown") ~ 1,
      hv246c %in% c(0, "none") ~ 0))
  })

  HH <- safe_mutate(HH, "hh.goats", "hv246d", function(df) {
    df %>% dplyr::mutate(hh.goats = dplyr::case_when(
      hv246d %in% c(1:98, "95 or more", "unknown") ~ 1,
      hv246d %in% c(0, "none") ~ 0))
  })

  HH <- safe_mutate(HH, "hh.sheep", "hv246e", function(df) {
    df %>% dplyr::mutate(hh.sheep = dplyr::case_when(
      hv246e %in% c(1:98, "95 or more", "unknown") ~ 1,
      hv246e %in% c(0, "none") ~ 0))
  })

  HH <- safe_mutate(HH, "hh.chickens", "hv246f", function(df) {
    df %>% dplyr::mutate(hh.chickens = dplyr::case_when(
      hv246f %in% c(1:98, "95 or more", "unknown") ~ 1,
      hv246f %in% c(0, "none") ~ 0))
  })

  ## VEHICLES INDEX ----
  HH <- HH %>%
    dplyr::mutate(
      vehic.sum = dplyr::case_when(
        dplyr::if_all(dplyr::all_of(c("hh.bike", "hh.motor", "hh.car")), is.na) ~ NA_real_,
        TRUE ~ rowSums(dplyr::across(dplyr::all_of(c("hh.bike", "hh.motor", "hh.car"))), na.rm = TRUE)),
      vehic.index = factor(dplyr::case_when(
        vehic.sum == 0        ~ 0,
        vehic.sum == 1        ~ 1,
        vehic.sum %in% c(2,3) ~ 2)))
  HH$vehic.index.1plus <- ifelse(HH$vehic.sum >= 1, 1, 0)
  HH$vehic.index.2plus <- ifelse(HH$vehic.sum >= 2, 1, 0)

  ## HOUSE AMENITIES INDEX ----
  # Country-specific sh132* columns: set to NA if absent in this survey
  HH$hh.cupboard  <- if ("sh132k" %in% names(HH)) dplyr::case_when(HH$sh132k == "no" ~ 0, HH$sh132k == "yes" ~ 1) else NA_real_
  HH$hh.sofa      <- if ("sh132j" %in% names(HH)) dplyr::case_when(HH$sh132j == "no" ~ 0, HH$sh132j == "yes" ~ 1) else NA_real_
  HH$hh.air       <- if ("sh132l" %in% names(HH)) dplyr::case_when(HH$sh132l == "no" ~ 0, HH$sh132l == "yes" ~ 1) else NA_real_
  HH$hh.iron      <- if ("sh132m" %in% names(HH)) dplyr::case_when(HH$sh132m == "no" ~ 0, HH$sh132m == "yes" ~ 1) else NA_real_
  HH$hh.generator <- if ("sh132n" %in% names(HH)) dplyr::case_when(HH$sh132n == "no" ~ 0, HH$sh132n == "yes" ~ 1) else NA_real_
  HH$hh.fan       <- if ("sh132o" %in% names(HH)) dplyr::case_when(HH$sh132o == "no" ~ 0, HH$sh132o == "yes" ~ 1) else NA_real_

  house_amenity_cols <- c("hh.electricity", "hh.radio", "hh.tv", "hh.refrig",
                          "hh.cupboard", "hh.sofa", "hh.air", "hh.iron",
                          "hh.generator", "hh.fan")

  HH <- HH %>%
    dplyr::mutate(
      house.sum = dplyr::case_when(
        dplyr::if_all(dplyr::all_of(house_amenity_cols), is.na) ~ NA_real_,
        TRUE ~ rowSums(dplyr::across(dplyr::all_of(house_amenity_cols)), na.rm = TRUE)),
      house.index = factor(dplyr::case_when(
        house.sum %in% c(0,1,2) ~ 0,
        house.sum %in% c(3,4,5) ~ 1,
        house.sum >= 6          ~ 2)))

  ## WEALTH INDEX ----
  HH <- safe_mutate(HH, "wealth.index.ur", "hv270a", function(df) {
    df %>% dplyr::mutate(
      wealth.index.ur     = factor(hv270a),
      wealth.index.ur.cat = dplyr::case_when(
        hv270a %in% c("poorest", "poorer")              ~ "Poor",
        hv270a %in% c("middle", "richer", "richest")    ~ "Not Poor"))
  })

  ## HOUSEHOLD RELATIONSHIPS ----

  # Highest education of any HH member — hv108_* columns
  hv108_cols <- names(HH)[grepl("^hv108_", names(HH))]
  if (length(hv108_cols) > 0) {
    HH <- HH %>%
      dplyr::mutate(dplyr::across(dplyr::all_of(hv108_cols), ~ ifelse(. == 98, NA, .))) %>%
      dplyr::mutate(highestyearsedinHH.yrs = do.call(
        pmax, c(dplyr::across(dplyr::all_of(hv108_cols)), na.rm = TRUE)))
    HH$highestyearsedinHH.9plus <- ifelse(HH$highestyearsedinHH.yrs >= 9, "9+", "0-8")
    HH$highestyearsedinHH.8plus <- ifelse(HH$highestyearsedinHH.yrs >= 8, "8+", "0-7")
    HH$highestyearsedinHH.7plus <- ifelse(HH$highestyearsedinHH.yrs >= 7, "7+", "0-6")
  } else {
    message("  Skipping highestyearsedinHH — no hv108_* columns found in this survey.")
    HH$highestyearsedinHH.yrs   <- NA
    HH$highestyearsedinHH.9plus <- NA
    HH$highestyearsedinHH.8plus <- NA
    HH$highestyearsedinHH.7plus <- NA
  }

  ## HH MEMBER WITH DISABILITY (via PR file) ----
  dis_cols <- c("hdis2", "hdis4", "hdis5", "hdis6", "hdis7", "hdis8")

  if (!is.null(PR) && all(dis_cols %in% names(PR))) {
    PR <- PR %>%
      dplyr::mutate(
        dis.see   = ifelse(hdis2 %in% c("no difficulty seeing",                    "don't know"), 0, ifelse(is.na(hdis2), NA, 1)),
        dis.hear  = ifelse(hdis4 %in% c("no difficulty hearing",                   "don't know"), 0, ifelse(is.na(hdis4), NA, 1)),
        dis.comm  = ifelse(hdis5 %in% c("no difficulty communicating",             "don't know"), 0, ifelse(is.na(hdis5), NA, 1)),
        dis.remem = ifelse(hdis6 %in% c("no difficulty remembering/concentrating", "don't know"), 0, ifelse(is.na(hdis6), NA, 1)),
        dis.walk  = ifelse(hdis7 %in% c("no difficulty walking or climbing",       "don't know"), 0, ifelse(is.na(hdis7), NA, 1)),
        dis.wash  = ifelse(hdis8 %in% c("no difficulty washing or dressing",       "don't know"), 0, ifelse(is.na(hdis8), NA, 1)))

    PR.disable <- PR %>%
      dplyr::group_by(hv001, hv002) %>%
      dplyr::summarise(
        any.dis.see   = if (all(is.na(dis.see)))   NA else max(dis.see,   na.rm = TRUE),
        any.dis.hear  = if (all(is.na(dis.hear)))  NA else max(dis.hear,  na.rm = TRUE),
        any.dis.comm  = if (all(is.na(dis.comm)))  NA else max(dis.comm,  na.rm = TRUE),
        any.dis.remem = if (all(is.na(dis.remem))) NA else max(dis.remem, na.rm = TRUE),
        any.dis.walk  = if (all(is.na(dis.walk)))  NA else max(dis.walk,  na.rm = TRUE),
        any.dis.wash  = if (all(is.na(dis.wash)))  NA else max(dis.wash,  na.rm = TRUE),
        .groups = "drop")

    HH <- HH %>% dplyr::left_join(PR.disable, by = c("hv001", "hv002"))
    HH <- HH %>% dplyr::mutate(any.hh.dis = ifelse(
      any.dis.see == 1 | any.dis.hear == 1 | any.dis.comm == 1 |
        any.dis.remem == 1 | any.dis.walk == 1 | any.dis.wash == 1, 1, 0))
  } else {
    message("  Skipping disability variables — PR file or hdis* columns not available.")
    HH$any.hh.dis <- NA
  }

  ## NUMBER OF HH MEMBERS ----
  hv105_cols <- names(HH)[grepl("^hv105_", names(HH))]
  if (length(hv105_cols) > 0) {
    HH_mem <- HH %>%
      dplyr::select(hv001, hv002, dplyr::all_of(hv105_cols)) %>%
      reshape2::melt(id.vars = c("hv001", "hv002")) %>%
      dplyr::mutate(
        age       = dplyr::case_when(
          value %in% c(98, "don't know") ~ NA_real_,
          value == "95+"                 ~ 95,
          is.na(value)                   ~ NA_real_,
          TRUE                           ~ as.numeric(value)),
        age_15up    = ifelse(age >= 15, 1, 0),
        age_under15 = ifelse(age < 15,  1, 0)) %>%
      dplyr::group_by(hv001, hv002) %>%
      dplyr::summarize(
        num.15up    = sum(age_15up,    na.rm = TRUE),
        num.under15 = sum(age_under15, na.rm = TRUE),
        .groups = "drop")

    HH <- HH %>% base::merge(HH_mem, by = c("hv001", "hv002"), all.x = TRUE)
  } else {
    message("  Skipping HH age composition — no hv105_* columns found in this survey.")
    HH$num.15up    <- NA
    HH$num.under15 <- NA
  }

  HH <- HH %>%
    dplyr::mutate(
      num.15up.cat   = dplyr::case_when(num.15up >= 5 ~ "5+", num.15up < 5 ~ "<5"),
      num.under15.cat = factor(dplyr::case_when(
        num.under15 %in% c(0)   ~ "0",
        num.under15 %in% c(1:2) ~ "1-2",
        num.under15 %in% c(3:4) ~ "3-4",
        num.under15 >= 5        ~ "5 and up"),
        levels = c("0", "1-2", "3-4", "5 and up")))
  HH$num.15up.4plus <- ifelse(HH$num.15up >= 4, "Yes", "No")

  IR <- IR %>%
    dplyr::mutate(
      num.kids.house = rowSums(dplyr::pick(v202, v203), na.rm = TRUE),
      num.kids.house.cat = dplyr::case_when(
        num.kids.house == 0  ~ "0",
        num.kids.house == 1  ~ "1",
        num.kids.house %in% c(2, 3) ~ "2-3",
        num.kids.house >= 4 ~ "4 or more"))
  IR$num.kids.house.4plus <- ifelse(IR$num.kids.house.cat == "4 or more", "Yes", "No")

  HH$hh.kidwom.rat <- ifelse(HH$hv010 == 0, 0, HH$hv014 / HH$hv010)
  HH <- HH %>%
    dplyr::mutate(
      hh.kidwom.rat.cat = factor(dplyr::case_when(
        hh.kidwom.rat == 0                        ~ "0",
        hh.kidwom.rat > 0 & hh.kidwom.rat <= 1   ~ "(0-1]",
        hh.kidwom.rat > 1 & hh.kidwom.rat <= 2   ~ "(1-2]",
        TRUE                                      ~ "More than 2"),
        levels = c("0", "(0-1]", "(1-2]", "More than 2")))
  HH$hh.kidwom.rat.2plus <- ifelse(HH$hh.kidwom.rat >= 2, 1, 0)

  ## LARGER ENVIRONMENT ----

  HH <- HH %>% dplyr::mutate(hh.where.cook = dplyr::case_when(
    hv226 == "no food cooked in house"   ~ "No food cooked in house",
    hv241 == "outdoors"                  ~ "Food cooked outdoors",
    hv241 == "in a separate building"    ~ "Food cooked in a separate building",
    hv241 == "in the house" & hv242 == "yes" ~ "Food cooked inside in a separate kitchen",
    hv241 == "in the house" & hv242 == "no"  ~ "Food cooked inside",
    hv241 == "other"                     ~ NA_character_))

  HH$hh.cook.inside.yn <- ifelse(HH$hv241 == "in the house", "Yes", "No")
  HH$hh.clean.fuel     <- ifelse(HH$hv226 %in% c(
    "electricity", "liquefied petroleum gas (lpg)/cooking gas",
    "piped natural gas", "biogas", "solar energy"), "Yes", "No")

  HH$hh.rooms.num <- HH$hv216
  HH <- HH %>% dplyr::mutate(hh.rooms.cat = dplyr::case_when(
    hv216 == 1                        ~ "1",
    hv216 == 2                        ~ "2",
    hv216 == 3                        ~ "3",
    hv216 >= 4 & !is.na(hv216)        ~ "4+"))

  HH <- HH %>% dplyr::mutate(hh.toilet.type = dplyr::case_when(
    hv205 %in% c("flush to piped sewer system", "flush to septic tank",
                 "flush to pit latrine", "flush to somewhere else",
                 "flush, don't know where")                         ~ "flush toilet",
    hv205 %in% c("ventilated improved pit latrine (vip)",
                 "pit latrine with slab",
                 "pit latrine without slab/open pit")               ~ "pit latrine",
    hv205 %in% c("no facility/bush/field", "composting toilet",
                 "bucket toilet", "hanging toilet/latrine", "other") ~ "informal or no facility"))

  HH <- HH %>% dplyr::mutate(hh.toilet.flush = ifelse(hh.toilet.type == "flush toilet", 1, 0))
  HH$latrine    <- ifelse(HH$hv205 %in% c(
    "pit latrine without slab/open pit", "no facility/bush/field",
    "bucket toilet", "hanging toilet/latrine", "other"), 1, 0)
  HH$no.latrine <- ifelse(HH$hv205 %in% c("no facility/bush/field", "no facility"), "Yes", "No")

  HH <- safe_mutate(HH, "latrine.loc", "hv238a", function(df) {
    df %>% dplyr::mutate(latrine.loc = dplyr::case_when(
      hv205 == "no facility/bush/field" ~ "No facility",
      hv238a == "in own dwelling"        ~ "In own yard/plot/dwelling",
      hv238a == "in own yard/plot"       ~ "In own yard/plot/dwelling",
      hv238a == "elsewhere"              ~ "Elsewhere"))
  })

  HH <- safe_mutate(HH, "hh.shared.latrine.yn", "hv225", function(df) {
    df %>% dplyr::mutate(hh.shared.latrine.yn = dplyr::case_when(
      hv205 == "no facility/bush/field" ~ "No facility",
      hv225 == "yes"                    ~ "Yes",
      hv225 == "no"                     ~ "No"))
  })

  HH <- safe_mutate(HH, "hh.num.sharelatrine", c("hv238", "hv225"), function(df) {
    df$hv238 <- as.numeric(df$hv238)
    mean_hv238 <- df %>%
      dplyr::filter(!hv238 %in% c(95, 98)) %>%
      dplyr::summarise(m = mean(hv238, na.rm = TRUE)) %>%
      dplyr::pull(m)
    df %>% dplyr::mutate(
      hv238 = ifelse(hv238 %in% c(95, 98), mean_hv238, hv238),
      hh.num.sharelatrine = dplyr::case_when(
        hv238 >= 2 & hv238 <= 4         ~ "2-4 HHs",
        hv238 > 4                        ~ "5 or more HHs",
        hv225 == "no"                    ~ "No sharing",
        hv205 == "no facility/bush/field" ~ "No facility"))
  })

  HH$hh.noimp.latrine <- ifelse(
    HH$latrine == "Yes" | (HH$latrine == "No" & HH$hh.shared.latrine == "Yes"), 1, 0)

  HH <- HH %>% dplyr::mutate(hh.water.loc = dplyr::case_when(
    hv201 == "piped into dwelling"                                     ~ "in own dwelling",
    hv201 == "piped to yard/plot"                                      ~ "in own yard/plot",
    hv201 == "piped to neighbor"                                       ~ "elsewhere",
    hv202 %in% c("piped into dwelling", "piped to yard/plot",
                 "piped to neighbor")                                  ~ "elsewhere",
    TRUE                                                               ~ col_or_na(HH, "hv235")))

  HH$hh.water.notpiped <- ifelse(HH$hv201 %in% c(
    "piped into dwelling", "piped to yard/plot",
    "piped to neighbor", "public tap/standpipe"), "No", "Yes")

  HH <- safe_mutate(HH, "hh.nowatpur", "hv237", function(df) {
    df %>% dplyr::mutate(hh.nowatpur = ifelse(hv237 == "yes", "No", "Yes"))
  })

  HH <- HH %>% dplyr::mutate(hh.wat.time = dplyr::case_when(
    hv204 %in% c(1:10)             ~ "1-10 minutes",
    hv204 %in% c(11:20)            ~ "11-20 minutes",
    hv204 %in% c(21:30)            ~ "21-30 minutes",
    hv204 %in% c(31:990)           ~ ">30 minutes",
    hv204 %in% c(0, 996, "on premises") ~ "Water on premises",
    hv204 %in% c(998, "don't know") ~ NA_character_))

  HH$hh.noimp.water <- ifelse(
    HH$water == 1 | (HH$water == 0 & HH$hh.wat.time == ">30 minutes"), "Yes", "No")

  HH <- HH %>% dplyr::mutate(hh.roof = dplyr::case_when(
    hv215 %in% c("no roof", "thatch/palm leaf", "mud")                           ~ "natural or no roof",
    hv215 %in% c("rustic mat", "palm/bamboo", "wood planks", "cardboard", "other") ~ "rudimentary",
    hv215 %in% c("metal/zinc", "wood", "calamine/cement fiber",
                 "ceramic tiles", "cement", "roofing shingles")                  ~ "finished"))
  HH$hh.roof.finish <- ifelse(HH$hh.roof == "finished", 1, 0)

  ## URBAN SLUM (UN DEFINITION) ----
  HH$roof          <- ifelse(HH$hh.roof == "finished", 0, 1)
  HH$floor         <- ifelse(HH$hv213 %in% c("earth/sand", "dung", "wood planks", "palm/bamboo"), 1, 0)
  HH$wall          <- ifelse(HH$hv214 %in% c(
    "no walls", "cane/palm/trunks", "dirt", "mud", "bamboo with mud",
    "stone with mud", "uncovered adobe", "plywood", "cardboard", "reused wood"), "Yes", "No")
  HH$durable       <- ifelse(HH$roof == 1 & HH$floor == 1 & HH$wall == "Yes", 1, 0)
  HH$hh.members    <- ifelse(HH$hv012 == 0, HH$hv013, HH$hv012)
  HH$hh.memsleep   <- ifelse(HH$hh.rooms.num == 0, HH$hh.members, HH$hh.members / HH$hh.rooms.num)
  HH$memsleep.4plus <- ifelse(HH$hh.memsleep >= 4, 1, 0)

  slum_cols <- c("durable", "water", "latrine", "memsleep.4plus")
  HH <- HH %>%
    dplyr::mutate(
      slum.sum = dplyr::case_when(
        dplyr::if_all(dplyr::all_of(slum_cols), is.na) ~ NA_real_,
        TRUE ~ rowSums(dplyr::across(dplyr::all_of(slum_cols)), na.rm = TRUE)),
      slum.sum.cat = dplyr::case_when(
        slum.sum == 0  ~ "None",
        slum.sum == 1  ~ "1",
        slum.sum >= 2  ~ "2+"),
      slum1 = dplyr::case_when(
        hv025 == "rural"                          ~ "rural",
        hv025 == "urban" & slum.sum >= 2          ~ "urban slum",
        hv025 == "urban" & slum.sum < 2           ~ "urban non-slum"))
  HH$slum.sum <- as.character(HH$slum.sum)
  HH$slum2    <- ifelse(
    HH$hh.urban == "Yes" & HH$hh.electricity == 0 &
      HH$latrine == 1 & HH$hh.water.notpiped == "Yes", "Yes", "No")

  ## SANITATION (HANDWASH) ----
  HH <- safe_mutate(HH, "hh.sanitation", c("hv230a", "hv230b", "hv232"), function(df) {
    df %>% dplyr::mutate(
      hh.sanitation = dplyr::case_when(
        hv230b == "water is available"    & hv232 == "yes" ~ "Water and soap observed",
        hv230b == "water is available"    & hv232 == "no"  ~ "Water but no soap observed",
        hv230b == "water not available"   & hv232 == "yes" ~ "Soap but no water observed",
        hv230b == "water not available"   & hv232 == "no"  ~ "No water or soap observed",
        hv230a == "not observed: not in dwelling"          ~ "No washing station",
        hv230a %in% c("not observed: no permission to see",
                      "not observed: other reason")        ~ "Washing place not observed"),
      hh.sanitation.cat = dplyr::case_when(
        hv230b == "water is available" & hv232 == "yes"               ~ "Water and soap observed",
        (hv230b == "water is available"  & hv232 == "no")  |
          (hv230b == "water not available" & hv232 == "yes") |
          (hv230b == "water not available" & hv232 == "no")           ~ "No water and/or soap observed",
        hv230a == "not observed: not in dwelling" |
          hv230a %in% c("not observed: no permission to see",
                        "not observed: other reason")                 ~ "No washing station or place observed"),
      hh.sanitation.yn = ifelse(
        hh.sanitation %in% c("No washing station", "No water or soap observed"), "No", "Yes"))
  })

  ## WEALTH SUM INDEX ----
  wealth_cols <- c("hh.electricity", "hh.tv", "hh.refrig",
                   "hh.bank.acct", "hh.computer", "hh.car", "hh.noimp.latrine")
  HH <- HH %>%
    dplyr::mutate(
      wealth.sum = dplyr::case_when(
        dplyr::if_all(dplyr::all_of(wealth_cols), is.na) ~ NA_real_,
        TRUE ~ rowSums(dplyr::across(dplyr::all_of(wealth_cols)), na.rm = TRUE)),
      wealth.sum.index = as.character(wealth.sum),
      wealth.sum.index.cat = dplyr::case_when(
        wealth.sum == 0           ~ "0",
        wealth.sum == 1           ~ "1",
        wealth.sum %in% c(2:6)   ~ "2-6"))


  ######################################################################
  # 2 | IR FILE ----
  message("[3/6] Processing IR file...")

  IR <- IR %>%
    dplyr::mutate(
      age.yrs  = v012,
      age.5yrs = v013,
      parity   = v201)

  ## DIGITAL CONNECTIVITY ----
  digital_cols <- c("v169c", "v155", "v171b", "v394", "v395",
                    "h12a_1", "h12a_2", "h12a_3", "h12a_4", "h12a_5", "h12a_6")
  present_digital <- intersect(digital_cols, names(IR))

  if (length(present_digital) > 0) {
    ir.add <- IR %>%
      dplyr::select(caseid, dplyr::any_of(digital_cols))

    h12a_present <- intersect(paste0("h12a_", 1:6), names(IR))
    if (length(h12a_present) > 0) {
      ir.add <- ir.add %>%
        dplyr::mutate(care.diarrhea = dplyr::case_when(
          dplyr::if_all(dplyr::all_of(h12a_present), is.na) ~ NA_character_,
          dplyr::if_any(dplyr::all_of(h12a_present), ~ . == "yes") ~ "yes",
          TRUE ~ "no"))
    } else {
      ir.add$care.diarrhea <- NA_character_
    }

    rename_map <- c(smart.phone = "v169c", literacy = "v155", freq.net = "v171b",
                    wom.hlthfac.12mo = "v394", hlthfac.fp.mes = "v395")
    rename_present <- rename_map[rename_map %in% names(ir.add)]
    if (length(rename_present) > 0) ir.add <- dplyr::rename(ir.add, !!!rename_present)

    keep_cols <- intersect(
      c("caseid", "smart.phone", "literacy", "freq.net",
        "care.diarrhea", "wom.hlthfac.12mo", "hlthfac.fp.mes"),
      names(ir.add))
    IR <- IR %>% dplyr::left_join(ir.add %>% dplyr::select(dplyr::all_of(keep_cols)),
                                  by = "caseid")
  } else {
    message("  Skipping digital connectivity variables — columns not in this survey.")
  }

  IR <- safe_mutate(IR, "bf.sweet.snacks", "v414a", function(df) {
    df %>% dplyr::mutate(bf.sweet.snacks = dplyr::case_when(
      v414a %in% c("no", "don't know") ~ 0,
      v414a == "yes"                   ~ 1))
  })

  ## PARTNERSHIPS ----
  IR$marr.cohab <- ifelse(IR$v501 %in% c("married", "living with partner"), 1, 0)

  IR <- IR %>% dplyr::mutate(sex.partner.12mo = dplyr::case_when(
    v766b == 0 ~ "No sexual partner",
    v766b == 1 ~ "1 sexual partner",
    v766b == 2 ~ "2 sexual partners",
    TRUE       ~ "3 or more"))

  IR$yrs.curr.pship <- ifelse(IR$v501 %in% c("married", "living with partner"), IR$v512, NA)
  IR$yrs.curr.pship[IR$marr.cohab == 0] <- "not partnered"

  IR <- IR %>% dplyr::mutate(
    yrs.curr.pship.cat = factor(dplyr::case_when(
      v501 %in% c("never in union")                                           ~ "Never in union",
      v501 %in% c("widowed", "divorced", "no longer living together/separated") |
        v503 %in% c("more than once")                                         ~ "No longer with first partner",
      yrs.curr.pship %in% c(0:4)                                              ~ "Less than 5 years",
      yrs.curr.pship %in% c(5:9)                                              ~ "5 to 9 years",
      yrs.curr.pship %in% c(10:39)                                            ~ "10 or more years"),
      levels = c("Never in union", "Less than 5 years", "5 to 9 years",
                 "10 or more years", "No longer with first partner")),
    marital.cat = dplyr::case_when(
      v501 %in% c("never in union")                                                  ~ "never in union",
      v501 %in% c("married", "living with partner")                                  ~ "married",
      v501 %in% c("widowed", "divorced", "separated",
                  "no longer living together/separated")                             ~ "widowed/ divorced/ separated"))

  IR <- safe_mutate(IR, "polygamy", c("v505"), function(df) {
    df %>% dplyr::mutate(polygamy = dplyr::case_when(
      v505 == 0              ~ "No",
      v505 %in% c(1:20)      ~ "Yes",
      marr.cohab == 0        ~ "not partnered",
      v505 %in% c(98, "don't know") ~ NA_character_))
  })

  IR <- safe_mutate(IR, "wife.order", c("v506", "v505"), function(df) {
    df %>% dplyr::mutate(wife.order = dplyr::case_when(
      v506 == 98        ~ NA_character_,
      v506 == 1         ~ "1",
      v506 == 2         ~ "2",
      v506 >= 3         ~ "3+",
      marr.cohab == 0   ~ "not partnered",
      v505 == 0         ~ "monogamous union"))
  })

  IR <- safe_mutate(IR, "first.wife", c("v506", "v505"), function(df) {
    df %>% dplyr::mutate(first.wife = dplyr::case_when(
      v506 %in% c(2:20)               ~ "Second or more wife",
      v506 == 1                        ~ "First wife",
      marr.cohab == 0                  ~ "not partnered",
      v505 %in% c(0, "no other wives") ~ "Monogamous union"))
  })

  IR <- safe_mutate(IR, "infecund.meno", "v625", function(df) {
    df %>% dplyr::mutate(infecund.meno = dplyr::case_when(
      v625 == "infecund, menopausal" ~ "Yes",
      !is.na(v625)                   ~ "No"))
  })

  IR <- safe_mutate(IR, "fp.partner.oppose", c("v3a08j", "v213", "v361"), function(df) {
    df %>% dplyr::mutate(fp.partner.oppose = dplyr::case_when(
      v3a08j == "yes"               ~ "Partner opposes",
      v3a08j == "no"                ~ "Partner does not oppose",
      v213 == "yes"                 ~ "Currently pregnant",
      v361 == "currently using"     ~ "Currently using FP",
      is.na(v3a08j)                 ~ "No identified need for FP"))
  })

  ## BARRIERS TO HC ----
  IR <- IR %>% dplyr::mutate(
    med.permis = dplyr::case_when(v467b %in% c("not a big problem", "no problem") ~ 0, v467b == "big problem" ~ 1),
    med.cost   = dplyr::case_when(v467c %in% c("not a big problem", "no problem") ~ 0, v467c == "big problem" ~ 1),
    med.dist   = dplyr::case_when(v467d %in% c("not a big problem", "no problem") ~ 0, v467d == "big problem" ~ 1),
    med.alone  = dplyr::case_when(v467f %in% c("not a big problem", "no problem") ~ 0, v467f == "big problem" ~ 1))

  med_cols <- c("med.permis", "med.cost", "med.dist", "med.alone")
  IR <- IR %>%
    dplyr::mutate(
      med.sum = dplyr::case_when(
        dplyr::if_all(dplyr::all_of(med_cols), is.na) ~ NA_real_,
        TRUE ~ rowSums(dplyr::across(dplyr::all_of(med_cols)), na.rm = TRUE)),
      med.index = as.character(med.sum),
      med.index.cat = dplyr::case_when(
        med.sum == 0            ~ "0",
        med.sum == 1            ~ "1",
        med.sum %in% c(2:4)    ~ "2+"))
  IR$med.index.3plus <- ifelse(IR$med.index >= 3, 1, 0)

  IR$hw.visit.12mo <- ifelse(IR$v393 == "yes", "Yes", "No")

  IR <- safe_mutate(IR, "hw.visit.fp", "v393a", function(df) {
    df %>% dplyr::mutate(hw.visit.fp = dplyr::case_when(
      v393a == "yes"       ~ "HW discussed FP",
      v393a == "no"        ~ "HW did not discuss FP",
      hw.visit.12mo == "No" ~ "No HW visit"))
  })

  ## EMPLOYMENT ----
  IR$occupation <- IR$v717
  IR <- IR %>% dplyr::mutate(
    occupation.cat = dplyr::case_when(
      occupation %in% c("professional/technical/managerial", "clerical")          ~ "professional/mangerial/clerical",
      occupation %in% c("skilled manual", "unskilled manual")                     ~ "manual",
      occupation %in% c("services", "household and domestic", "other", "don't know") ~ "service/domestic/other",
      occupation %in% c("agricultural - self employed", "agricultural - employee") ~ "agricultural",
      occupation == "sales"                                                         ~ "sales",
      occupation == "not working"                                                   ~ "not working"),
    occ.type = dplyr::case_when(
      v719 == "for family member"  ~ "Family",
      v719 == "for someone else"   ~ "Someone else",
      v719 == "self-employed"      ~ "Self",
      is.na(v719)                  ~ "Did not work"),
    working = dplyr::case_when(
      v714 == "yes" & (v714a != "yes" | is.na(v714a)) ~ "Currently working",
      v714a == "yes"                                    ~ "Worked in the past year",
      v731 == "no"                                      ~ "No work in the past 12 months",
      v731 == "in the past year"                        ~ "Worked in the past year"))

  IR$working.yn    <- ifelse(IR$working %in% c("Currently working", "Worked in the past year",
                                               "On leave/absent"), "Yes", "No")
  IR$workingnow.yn <- ifelse(IR$working == "Currently working", "Yes", "No")

  IR <- IR %>% dplyr::mutate(
    work.seasonal  = dplyr::case_when(
      v731 == "no"          ~ "No work in past 12 months",
      v732 == "occasional"  ~ "Occassional",
      v732 == "seasonal"    ~ "Seasonal",
      v732 == "all year"    ~ "All year"),
    work.seasonal2 = dplyr::case_when(
      work.seasonal == "All year"                   ~ "All year",
      work.seasonal %in% c("Occassional","Seasonal") ~ "Seasonal/Occassional",
      work.seasonal == "No work in past 12 months"  ~ "No work in past 12 months"))

  ## PARTNER EMPLOYMENT ----
  IR <- IR %>% dplyr::mutate(
    partner.working = dplyr::case_when(
      !(v501 %in% c("married", "living with partner"))  ~ "not partnered",
      v704a == "didn't work last 12 months"             ~ "Didn't work last 12 months",
      v704a == "worked last 7 days"                     ~ "Worked last 7 days",
      v704a == "worked last 12 months"                  ~ "Worked last 12 months",
      v704a == "don't know"                             ~ "Don't know"))
  IR$partner.working.yn    <- ifelse(IR$partner.working %in% c("Worked last 7 days","Worked last 12 months"), "Yes", "No")
  IR$partner.workingnow.yn <- ifelse(IR$partner.working == "Worked last 7 days", "Yes", "No")

  IR$partner.occupation <- IR$v704
  IR <- IR %>% dplyr::mutate(
    partner.occupation.cat = dplyr::case_when(
      partner.occupation %in% c("professional, technical and related workers",
                                "administrative and managerial workers",
                                "office/administrative support workers")           ~ "professional/mangerial/clerical",
      partner.occupation %in% c("installations, maintenance and repair workers",
                                "production, construction and extractions workers",
                                "transportation and material moving workers")      ~ "manual",
      partner.occupation == "service workers"                                      ~ "service",
      partner.occupation == "agricultural, animal husbandry, forestry, fishermen/hunters" ~ "agricultural",
      partner.occupation == "sales and related workers"                            ~ "sales",
      partner.occupation %in% c("other", "don't know")                            ~ "other/dk",
      partner.occupation == "not working and didn't work in last 12 months"       ~ "not working",
      !(v501 %in% c("married", "living with partner"))                            ~ "not partnered"),
    partner.occupation.cat.2 = dplyr::case_when(
      partner.occupation %in% c("professional, technical and related workers",
                                "administrative and managerial workers",
                                "office/administrative support workers")           ~ "professional/mangerial/clerical",
      partner.occupation %in% c("installations, maintenance and repair workers",
                                "production, construction and extractions workers",
                                "transportation and material moving workers")      ~ "manual",
      partner.occupation == "service workers"                                      ~ "service",
      partner.occupation == "agricultural, animal husbandry, forestry, fishermen/hunters" ~ "agricultural",
      partner.occupation == "sales and related workers"                            ~ "sales",
      partner.occupation %in% c("other", "don't know",
                                "not working and didn't work in last 12 months")  ~ "not partnered, not working, other/dk",
      !(v501 %in% c("married", "living with partner"))                            ~ "not partnered, not working, other/dk"))

  IR <- IR %>% dplyr::mutate(earnings.rel.partner = dplyr::case_when(
    !(v501 %in% c("married", "living with partner"))                              ~ "not partnered",
    working == "No work in the past 12 months"                                    ~ "woman did not work",
    !(v741 %in% c("cash only", "cash and in-kind"))                               ~ "woman worked but not paid in cash",
    v746 == "husband/partner doesn't bring in money" &
      working == "Currently working" & v741 != "not paid"                         ~ "more than him",
    v746 == "husband/partner doesn't bring in money" |
      partner.working == "Didn't work last 12 months"                             ~ "husband no earnings",
    TRUE                                                                           ~ as.character(v746)))

  ## OWNERSHIP ----
  IR <- IR %>% dplyr::mutate(
    jnt.house.ownership = dplyr::case_when(
      v745a %in% c("jointly with someone else only",
                   "both alone and jointly",
                   "jointly with husband/partner and someone else") ~ "jointly with others",
      v745a == "does not own"                                        ~ "does not own",
      v745a == "jointly with husband/partner only"                   ~ "jointly with husband/partner only",
      v745a == "alone only"                                          ~ "alone only"),
    jnt.land.ownership = dplyr::case_when(
      v745b %in% c("jointly with someone else only",
                   "both alone and jointly",
                   "jointly with husband/partner and someone else") ~ "jointly with others",
      v745b == "does not own"                                        ~ "does not own",
      v745b == "jointly with husband/partner only"                   ~ "jointly with husband/partner only",
      v745b == "alone only"                                          ~ "alone only"))

  ## CHILDREN ----
  IR$num.under5 <- IR$v137
  IR <- IR %>% dplyr::mutate(
    num.under5.cat = dplyr::case_when(
      v137 == 0              ~ "0",
      v137 == 1              ~ "1",
      v137 == 2              ~ "2",
      v137 >= 3 & !is.na(v137) ~ "3+"),
    num.child.alive = v218,
    num.child.alive.cat = dplyr::case_when(
      v218 %in% c(0)         ~ "0",
      v218 %in% c(1:2)       ~ "1-2",
      v218 %in% c(3:5)       ~ "3-5",
      v218 >= 6 & !is.na(v218) ~ "6+"),
    num.child.alive.cat.2 = dplyr::case_when(
      v218 %in% c(0:2)       ~ "0-2",
      v218 %in% c(3:5)       ~ "3-5",
      v218 >= 6 & !is.na(v218) ~ "6+"),
    num.biochild.house = rowSums(dplyr::pick(v202, v203), na.rm = TRUE),
    num.biochild.house.cat = dplyr::case_when(
      num.biochild.house == 0              ~ "0",
      num.biochild.house == 1              ~ "1",
      num.biochild.house %in% c(2, 3)     ~ "2-3",
      num.biochild.house >= 4              ~ "4 or more"),
    partner.absent = dplyr::case_when(
      v504 == "living with her"                                  ~ "No",
      v504 == "staying elsewhere"                                ~ "Yes",
      !(v501 %in% c("married", "living with partner"))           ~ "not partnered"))

  IR$head.sex <- IR$v151

  ## DOMESTIC VIOLENCE ----
  dv_cols <- c("d106", "d107", "d104", "d108")
  if (all(dv_cols %in% names(IR))) {
    IR <- IR %>% dplyr::mutate(
      dv.physical = dplyr::case_when(
        d106 == "no"  & d107 == "no"  ~ "No",
        d106 == "yes" | d107 == "yes" ~ "Yes",
        !(v501 %in% c("married","living with partner")) ~ "not partnered",
        is.na(d106)                   ~ "Did not answer the question"),
      dv.emotional = dplyr::case_when(
        d104 == "yes" ~ "Yes", d104 == "no" ~ "No",
        !(v501 %in% c("married","living with partner")) ~ "not partnered",
        is.na(d104)  ~ "Did not answer the question"),
      dv.sexual = dplyr::case_when(
        d108 == "yes" ~ "Yes", d108 == "no" ~ "No",
        !(v501 %in% c("married","living with partner")) ~ "not partnered",
        is.na(d108)  ~ "Did not answer the question"))
  } else {
    message("  Skipping domestic violence (dv.physical/emotional/sexual) — d10* columns not in this survey.")
    IR$dv.physical  <- NA
    IR$dv.emotional <- NA
    IR$dv.sexual    <- NA
  }

  dv_control_cols <- c("d101a", "d101c", "d101d", "d103a", "d103b", "d103c",
                       "d105a", "d105b", "d105d", "d105e", "d105f",
                       "d105h", "d105i", "d105k", "d105j")
  dv_present <- intersect(dv_control_cols, names(IR))

  make_dv_var <- function(df, new_col, col) {
    safe_mutate(df, new_col, col, function(d) {
      d[[new_col]] <- dplyr::case_when(
        d[[col]] %in% c("often", "sometimes",
                        "yes, but not in the last 12 months",
                        "yes, but frequency in last 12 months missing") ~ "Yes",
        d[[col]] %in% c("never")      ~ "No",
        d[[col]] %in% c("don't know") ~ "Don't know",
        is.na(d[[col]])               ~ "Did not answer the question")
      d
    })
  }

  IR <- make_dv_var(IR, "dv.jealous.othermen", "d101a")
  IR <- make_dv_var(IR, "dv.nofriends",        "d101c")
  IR <- make_dv_var(IR, "dv.nofamily.contact", "d101d")
  IR <- make_dv_var(IR, "dv.humiliated",       "d103a")
  IR <- make_dv_var(IR, "dv.threatened",       "d103b")
  IR <- make_dv_var(IR, "dv.insulted",         "d103c")
  IR <- make_dv_var(IR, "dv.pushed",           "d105a")
  IR <- make_dv_var(IR, "dv.slapped",          "d105b")
  IR <- make_dv_var(IR, "dv.kicked",           "d105d")
  IR <- make_dv_var(IR, "dv.strangled",        "d105e")
  IR <- make_dv_var(IR, "dv.weapon",           "d105f")
  IR <- make_dv_var(IR, "dv.forcedsex1",       "d105h")
  IR <- make_dv_var(IR, "dv.forcedsex2",       "d105i")
  IR <- make_dv_var(IR, "dv.forcedsex3",       "d105k")
  IR <- make_dv_var(IR, "dv.armtwist",         "d105j")

  if (all(c("dv.forcedsex1", "dv.forcedsex2", "dv.forcedsex3") %in% names(IR))) {
    IR <- IR %>% dplyr::mutate(dv.forcedsex.yn = dplyr::case_when(
      dv.forcedsex1 == "Yes" | dv.forcedsex2 == "Yes" | dv.forcedsex3 == "Yes" ~ "Yes",
      dv.forcedsex1 == "No"  & dv.forcedsex2 == "No"  & dv.forcedsex3 == "No"  ~ "No",
      dv.forcedsex1 == "Don't know" & dv.forcedsex2 == "Don't know" &
        dv.forcedsex3 == "Don't know"                                             ~ "Don't know",
      is.na(d105h) & is.na(d105i) & is.na(d105k)                                 ~ "Did not answer the question"))
  }

  IR <- safe_mutate(IR, "dv.physical.preg", c("v044", "d118a"), function(df) {
    df %>% dplyr::mutate(dv.physical.preg = dplyr::case_when(
      v044 != "woman selected and interviewed" ~ "NA - not eligible for questionnaire",
      d118a == "no"  ~ "No",
      d118a == "yes" ~ "Yes",
      TRUE           ~ NA_character_))
  })

  IR <- safe_mutate(IR, "dv.hurtpreg.formerp", c("d118j", "v044"), function(df) {
    df %>% dplyr::mutate(dv.hurtpreg.formerp = dplyr::case_when(
      d118j == "yes" ~ "Yes",
      d118j == "no"  ~ "No",
      v044 != "woman selected and interviewed" ~ "Did not answer the question"))
  })

  IR <- safe_mutate(IR, "dv.prevparter.hurt", c("d130a", "v044"), function(df) {
    df %>% dplyr::mutate(dv.prevparter.hurt = dplyr::case_when(
      d130a %in% c("0-11 months ago", "12+ months ago",
                   "yes, but don't remember when",
                   "yes, but frequency in last 12 months missing") ~ "Yes",
      d130a == "never had another husband/male partner"             ~ "Never had another partner",
      d130a == "never"                                              ~ "No",
      v044 != "woman selected and interviewed"                      ~ "Did not answer the question"))
  })

  IR <- safe_mutate(IR, "dv.prevparter.forcedsex", c("d130b", "v044"), function(df) {
    df %>% dplyr::mutate(dv.prevparter.forcedsex = dplyr::case_when(
      d130b %in% c("0-11 months ago", "12+ months ago",
                   "yes, but don't remember when",
                   "yes, but frequency in last 12 months missing") ~ "Yes",
      d130b == "never had another husband/male partner"             ~ "Never had another partner",
      d130b == "never"                                              ~ "No",
      v044 != "woman selected and interviewed"                      ~ "Did not answer the question"))
  })

  ## DV HELP-SEEKING ----
  IR <- safe_mutate(IR, "dv.anyone.help", c("d128", "v044"), function(df) {
    df %>% dplyr::mutate(dv.anyone.help = dplyr::case_when(
      d128 == "yes" ~ "Yes",
      d128 == "no"  ~ "No",
      dv.physical == "NO" | dv.sexual == "No" | dv.emotional == "No" ~ "Not experienced violence",
      v044 != "woman selected and interviewed" ~ "Did not answer the question"))
  })

  for (pair in list(
    c("dv.friend.help",    "d119xd"),
    c("dv.family.help",    "d119h"),
    c("dv.husband.help",   "d119i"),
    c("dv.formerp.help",   "d119j"),
    c("dv.formerb.help",   "d119k"),
    c("dv.neighbor.help",  "d119u"),
    c("dv.other.help",     "d119x"),
    c("dv.sso.help",       "d119xb"),
    c("dv.religious.help", "d119xf")
  )) {
    new_col <- pair[1]; src_col <- pair[2]
    IR <- safe_mutate(IR, new_col, c(src_col, "d128"), function(df) {
      df[[new_col]] <- dplyr::case_when(
        df[[src_col]] == "yes" ~ "Yes",
        df[[src_col]] == "no"  ~ "No",
        df[["d128"]]  == "no"  ~ "Did not seek any help")
      df
    })
  }

  ## DECISION-MAKING ----
  IR <- IR %>% dplyr::mutate(
    desc.ownincome = dplyr::case_when(
      !is.na(v739)                                                    ~ as.character(v739),
      v741 %in% c("not paid", "in-kind only") | v731 == "no"         ~ "Not paid in cash or not working",
      marr.cohab == 0                                                 ~ "not partnered"),
    jd.ownincome = dplyr::case_when(
      v739 == "respondent and husband/partner"                        ~ "Yes",
      !is.na(v739) & v739 != "respondent and husband/partner"         ~ "No",
      !(v501 %in% c("married","living with partner"))                 ~ "Not partnered",
      v501 %in% c("married","living with partner") &
        (v741 %in% c("not paid","in-kind only") | v731 == "no")       ~ "Not paid in cash or not working"),
    wd.ownincome = dplyr::case_when(
      v739 == "respondent alone"                                      ~ "Yes",
      !is.na(v739) & v739 != "respondent alone"                       ~ "No",
      !(v501 %in% c("married","living with partner"))                 ~ "Not partnered",
      v501 %in% c("married","living with partner") &
        (v741 %in% c("not paid","in-kind only") | v731 == "no")       ~ "Not paid in cash or not working"),
    jdwd.ownincome = dplyr::case_when(
      v739 %in% c("respondent alone","respondent and husband/partner") ~ "Yes",
      !is.na(v739) &
        !(v739 %in% c("respondent alone","respondent and husband/partner")) ~ "No",
      !(v501 %in% c("married","living with partner"))                  ~ "Not partnered",
      v501 %in% c("married","living with partner") &
        (v741 %in% c("not paid","in-kind only") | v731 == "no")        ~ "Not paid in cash or not working"))

  IR <- IR %>% dplyr::mutate(
    desc.lrgpur  = dplyr::case_when(!is.na(v743b) ~ as.character(v743b), marr.cohab == 0 ~ "not partnered"),
    jd.lrgpur    = dplyr::case_when(v743b == "respondent and husband/partner" ~ "Yes", !is.na(v743b) & v743b != "respondent and husband/partner" ~ "No", !(v501 %in% c("married","living with partner")) ~ "Not partnered"),
    wd.lrgpur    = dplyr::case_when(v743b == "respondent alone" ~ "Yes", !is.na(v743b) & v743b != "respondent alone" ~ "No", !(v501 %in% c("married","living with partner")) ~ "Not partnered"),
    jdwd.lrgpur  = dplyr::case_when(v743b %in% c("respondent alone","respondent and husband/partner") ~ "Yes", !is.na(v743b) & !(v743b %in% c("respondent alone","respondent and husband/partner")) ~ "No", !(v501 %in% c("married","living with partner")) ~ "Not partnered"),
    desc.hlth    = dplyr::case_when(!is.na(v743a) ~ as.character(v743a), marr.cohab == 0 ~ "not partnered"),
    jd.hlth      = dplyr::case_when(v743a == "respondent and husband/partner" ~ "Yes", !is.na(v743a) & v743a != "respondent and husband/partner" ~ "No", !(v501 %in% c("married","living with partner")) ~ "Not partnered"),
    wd.hlth      = dplyr::case_when(v743a == "respondent alone" ~ "Yes", !is.na(v743a) & v743a != "respondent alone" ~ "No", !(v501 %in% c("married","living with partner")) ~ "Not partnered"),
    jdwd.hlth    = dplyr::case_when(v743a %in% c("respondent alone","respondent and husband/partner") ~ "Yes", !is.na(v743a) & !(v743a %in% c("respondent alone","respondent and husband/partner")) ~ "No", !(v501 %in% c("married","living with partner")) ~ "Not partnered"),
    desc.visit   = dplyr::case_when(!is.na(v743d) ~ as.character(v743d), marr.cohab == 0 ~ "not partnered"),
    jd.visit     = dplyr::case_when(v743d == "respondent and husband/partner" ~ "Yes", !is.na(v743d) & v743d != "respondent and husband/partner" ~ "No", !(v501 %in% c("married","living with partner")) ~ "Not partnered"),
    wd.visit     = dplyr::case_when(v743d == "respondent alone" ~ "Yes", !is.na(v743d) & v743d != "respondent alone" ~ "No", !(v501 %in% c("married","living with partner")) ~ "Not partnered"),
    jdwd.visit   = dplyr::case_when(v743d %in% c("respondent alone","respondent and husband/partner") ~ "Yes", !is.na(v743d) & !(v743d %in% c("respondent alone","respondent and husband/partner")) ~ "No", !(v501 %in% c("married","living with partner")) ~ "Not partnered"))

  IR <- safe_mutate(IR, "desc.money", c("v743f"), function(df) {
    df %>% dplyr::mutate(
      desc.money  = dplyr::case_when(!is.na(v743f) ~ as.character(v743f), marr.cohab == 0 ~ "not partnered"),
      jd.money    = dplyr::case_when(!(v501 %in% c("married","living with partner")) ~ "Not partnered", v743f == "husband/partner has no earnings" | partner.working == "Didn't work last 12 months" ~ "Partner no earning", v743f == "respondent and husband/partner" ~ "Yes", !is.na(v743f) & v743f != "respondent and husband/partner" ~ "No"),
      wd.money    = dplyr::case_when(!(v501 %in% c("married","living with partner")) ~ "Not partnered", v743f == "husband/partner has no earnings" | partner.working == "Didn't work last 12 months" ~ "Partner no earning", v743f == "respondent alone" ~ "Yes", !is.na(v743f) & v743f != "respondent alone" ~ "No"),
      jdwd.money  = dplyr::case_when(!(v501 %in% c("married","living with partner")) ~ "Not partnered", v743f == "husband/partner has no earnings" | partner.working == "Didn't work last 12 months" ~ "Partner no earning", v743f %in% c("respondent alone","respondent and husband/partner") ~ "Yes", !is.na(v743f) & !(v743f %in% c("respondent alone","respondent and husband/partner")) ~ "No"))
  })

  IR <- safe_mutate(IR, "desc.fp", c("v632"), function(df) {
    v632a_present <- "v632a" %in% names(df)
    df %>% dplyr::mutate(
      desc.fp = dplyr::case_when(
        v632 == "joint decision" | (v632a_present & v632a == "joint decision")     ~ "respondent and husband/partner",
        v632 == "respondent"     | (v632a_present & v632a == "mainly respondent")  ~ "respondent alone",
        v632 == "husband/partner"| (v632a_present & v632a == "mainly husband, partner") ~ "husband/partner alone",
        v632 %in% c("other","someone else") | (v632a_present & v632a == "other")   ~ "other",
        marr.cohab == 0                                                             ~ "not partnered"),
      jd.fp   = dplyr::case_when(!(v501 %in% c("married","living with partner")) ~ "Not partnered", v632 == "joint decision" ~ "Yes", !is.na(v632) & v632 != "joint decision" ~ "No"),
      wd.fp   = dplyr::case_when(!(v501 %in% c("married","living with partner")) ~ "Not partnered", v632 == "respondent" ~ "Yes", !is.na(v632) & v632 != "respondent" ~ "No"),
      jdwd.fp = dplyr::case_when(!(v501 %in% c("married","living with partner")) ~ "Not partnered", v632 %in% c("respondent","joint decision") ~ "Yes", !is.na(v632) & !(v632 %in% c("respondent","joint decision")) ~ "No"))
  })

  ## DECISION-MAKING INDICES ----
  dm_jd   <- c("jd.ownincome","jd.lrgpur","jd.money","jd.hlth","jd.fp","jd.visit")
  dm_wd   <- c("wd.ownincome","wd.lrgpur","wd.money","wd.hlth","wd.fp","wd.visit")
  dm_jdwd <- c("jdwd.ownincome","jdwd.lrgpur","jdwd.money","jdwd.hlth","jdwd.fp","jdwd.visit")

  present_jd   <- intersect(dm_jd,   names(IR))
  present_wd   <- intersect(dm_wd,   names(IR))
  present_jdwd <- intersect(dm_jdwd, names(IR))

  IR$jd.index   <- rowSums(sapply(present_jd,   function(c) IR[[c]] %in% "Yes"), na.rm = TRUE)
  IR$wd.index   <- rowSums(sapply(present_wd,   function(c) IR[[c]] %in% "Yes"), na.rm = TRUE)
  IR$jdwd.index <- rowSums(sapply(present_jdwd, function(c) IR[[c]] %in% "Yes"), na.rm = TRUE)

  IR$jd.index[IR$marr.cohab   == 0] <- "not partnered"
  IR$wd.index[IR$marr.cohab   == 0] <- "not partnered"
  IR$jdwd.index[IR$marr.cohab == 0] <- "not partnered"

  IR <- IR %>% dplyr::mutate(
    jd.index.cat = dplyr::case_when(
      jd.index == "not partnered" ~ "Not partnered", jd.index == 0 ~ "None",
      jd.index %in% c(1:2) ~ "1-2", jd.index %in% c(3:6) ~ "3-6"),
    wd.index.cat = dplyr::case_when(
      wd.index == "not partnered" ~ "Not partnered", wd.index == 0 ~ "None",
      wd.index == 1 ~ "1", wd.index %in% c(2:6) ~ "2+"),
    jdwd.index.cat = dplyr::case_when(
      jdwd.index == "not partnered" ~ "Not partnered", jdwd.index == 0 ~ "None",
      jdwd.index %in% c(1:2) ~ "1-2", jdwd.index %in% c(3:6) ~ "3-6"))

  ## PARTNER CHARACTERISTICS ----
  IR <- IR %>% dplyr::mutate(
    partner.age     = dplyr::case_when(marr.cohab == 0 ~ "not partnered", TRUE ~ as.character(v730)),
    partner.age.cat = dplyr::case_when(
      v730 %in% c(15:29)                               ~ "under 30",
      v730 %in% c(30:59)                               ~ "30-59",
      v730 %in% c(60:96)                               ~ "60+",
      v730 == 98                                        ~ "Don't know",
      v730 == 99                                        ~ "Missing",
      !(v501 %in% c("married","living with partner"))   ~ "not partnered"),
    age.diff = dplyr::case_when(
      is.na(v730) | v730 > 96 ~ NA_real_,
      TRUE                     ~ as.numeric(v730) - v012),
    prev.pship = dplyr::case_when(
      v503 == "once"          ~ "No",
      v503 == "more than once" ~ "Yes",
      v501 == "never in union" ~ "Never in union"))

  IR$pship.status <- as.character(IR$v501)
  IR <- IR %>% dplyr::mutate(pship.cat = dplyr::case_when(
    pship.status == "never in union"                      ~ "never",
    pship.status %in% c("widowed","divorced",
                        "no longer living together/separated") ~ "no partner now",
    pship.status %in% c("married","living with partner")  ~ "married/cohab"))

  ## MOBILE / INTERNET / BANKING ----
  IR <- safe_mutate(IR, "has.mobile", "v169a", function(df) {
    df %>% dplyr::mutate(has.mobile = dplyr::case_when(v169a == "no" ~ "No", v169a == "yes" ~ "Yes"))
  })
  IR <- safe_mutate(IR, "mobile.financial", "v169b", function(df) {
    df %>% dplyr::mutate(mobile.financial = dplyr::case_when(v169b == "no" ~ "No", v169b == "yes" ~ "Yes"))
  })
  IR <- safe_mutate(IR, "has.bank", "v170", function(df) {
    df %>% dplyr::mutate(has.bank = dplyr::case_when(v170 == "no" ~ "No", v170 == "yes" ~ "Yes"))
  })
  IR <- safe_mutate(IR, "internet.use", "v171a", function(df) {
    df %>% dplyr::mutate(internet.use = dplyr::case_when(
      v171a %in% c("never","yes, before last 12 months") ~ "No",
      v171a == "yes, last 12 months"                     ~ "Yes"))
  })

  ## CHILD HEALTH ----
  h11_cols  <- names(IR)[grepl("^H11_",  names(IR))]
  h12y_cols <- names(IR)[grepl("^H12y_", names(IR))]

  if (length(h11_cols) > 0 && length(h12y_cols) > 0) {
    df1 <- IR %>%
      dplyr::select(caseid, dplyr::all_of(h11_cols)) %>%
      reshape2::melt(id.vars = "caseid", variable.name = "had_diarrhea",
                     value.name = "had_diarrhea_resp") %>%
      dplyr::mutate(had_diarrhea = ifelse(had_diarrhea_resp == "yes, last two weeks", 1, 0)) %>%
      dplyr::filter(!is.na(had_diarrhea)) %>%
      dplyr::group_by(caseid) %>%
      dplyr::summarize(had_diarrhea = max(had_diarrhea, na.rm = TRUE), .groups = "drop")

    df2 <- IR %>%
      dplyr::select(caseid, dplyr::all_of(h12y_cols)) %>%
      reshape2::melt(id.vars = "caseid", variable.name = "was_treated",
                     value.name = "was_treated_resp") %>%
      dplyr::mutate(was_treated = ifelse(was_treated_resp == "no: received treatment", 1, 0)) %>%
      dplyr::filter(!is.na(was_treated)) %>%
      dplyr::group_by(caseid) %>%
      dplyr::summarize(was_treated = max(was_treated, na.rm = TRUE), .groups = "drop")

    df3 <- df1 %>%
      base::merge(df2, by = "caseid", all.x = TRUE) %>%
      dplyr::mutate(diarrhea.medtreat = dplyr::case_when(
        had_diarrhea == 1 & was_treated == 1 ~ "yes, child was treated",
        had_diarrhea == 1 & was_treated == 0 ~ "no, child wasn't treated",
        had_diarrhea == 0 & is.na(was_treated) ~ "child didn't have diarrhea")) %>%
      dplyr::select(caseid, diarrhea.medtreat)

    IR <- IR %>% base::merge(df3, by = "caseid", all.x = TRUE)
  } else {
    message("  Skipping diarrhea treatment variable — H11_*/H12y_* columns not in this survey.")
    IR$diarrhea.medtreat <- NA
  }

  IR <- safe_mutate(IR, "bf.meat",       "v414h", function(df) df %>% dplyr::mutate(bf.meat       = dplyr::case_when(v414h %in% c("no","don't know") ~ "No", v414h == "yes" ~ "Yes", is.na(v414h) ~ "No living child <24months at home")))
  IR <- safe_mutate(IR, "bf.beans",      "v414o", function(df) df %>% dplyr::mutate(bf.beans      = dplyr::case_when(v414o %in% c("no","don't know") ~ "No", v414o == "yes" ~ "Yes", is.na(v414o) ~ "No living child <24months at home")))
  IR <- safe_mutate(IR, "bf.other.solid","v414s", function(df) df %>% dplyr::mutate(bf.other.solid = dplyr::case_when(v414s %in% c("no","don't know") ~ "No", v414s == "yes" ~ "Yes", is.na(v414s) ~ "No living child <24months at home")))

  ## WOMAN CHARACTERISTICS ----
  IR <- IR %>% dplyr::mutate(
    religion = dplyr::case_when(
      v130 == "christian" ~ "christian",
      v130 == "islam"     ~ "muslim",
      TRUE                ~ as.character(v130)),
    muslim         = ifelse(religion == "muslim",         "Yes", "No"),
    catholic       = ifelse(religion == "catholic",       "Yes", "No"),
    other.christian = ifelse(religion == "other christian","Yes", "No"),
    ed.level       = v106,
    ed.level.2     = dplyr::case_when(
      v106 == "higher"    ~ "higher",
      v106 == "secondary" ~ "secondary",
      v106 %in% c("no education","primary") ~ "no education or primary"),
    anyed.yn       = ifelse(v149 == "no education", "No", "Yes"))

  IR <- IR %>% dplyr::mutate(
    partner.ed.level = dplyr::case_when(marr.cohab == 0 ~ "not partnered", TRUE ~ as.character(v701)),
    partner.ed.level.cat1 = dplyr::case_when(
      v701 %in% c("primary","others","higher","secondary") ~ "education",
      v701 == "no education"                               ~ "no education",
      marr.cohab == 0                                      ~ "not partnered"),
    partner.ed.level.cat2 = dplyr::case_when(
      marr.cohab == 0                                      ~ "Not partnered",
      v701 == "no education"                               ~ "no education",
      v701 %in% c("higher","secondary")                    ~ "secondary or more",
      v701 %in% c("primary","others")                      ~ "some education"),
    partner.ed.level.cat3 = dplyr::case_when(
      marr.cohab == 0                                      ~ "Not partnered",
      v701 %in% c("higher","secondary")                    ~ "secondary or more",
      v701 %in% c("primary","others","no education")       ~ "some education or no education"))

  ## REPRODUCTIVE HISTORY ----
  IR <- IR %>% dplyr::mutate(
    age.1stcohab = dplyr::case_when(v501 == "never in union" ~ "never partnered", TRUE ~ as.character(v511)),
    age.1stcohab.cat = dplyr::case_when(
      v501 == "never in union"         ~ "never",
      v511 > 0 & v511 < 16            ~ "<16",
      v511 >= 16 & v511 < 20          ~ "16-19",
      v511 >= 20                       ~ "20+"),
    age.1stsex = dplyr::case_when(v531 %in% c(97, 98, 0) ~ NA_real_, TRUE ~ as.numeric(v531)),
    age.1stsex.cat1 = dplyr::case_when(
      age.1stsex > 0  & age.1stsex < 15 ~ "5-14",
      age.1stsex >= 15 & age.1stsex < 20 ~ "15-19",
      age.1stsex >= 20 & age.1stsex < 50 ~ "20+"),
    age.1stsex.cat = dplyr::case_when(
      v531 %in% c(0,"not had sex")     ~ "never",
      age.1stsex < 16                  ~ "<16",
      age.1stsex >= 16 & age.1stsex < 20 ~ "16-19",
      age.1stsex >= 20 & age.1stsex < 50 ~ "20+",
      v531 %in% c(97,98,"inconsistent","don't know") ~ NA_character_),
    early.sex.15 = ifelse(v531 > 0 & v531 < 15, "Yes", "No"),
    age.1stbrth  = v212,
    age.1stbrth.cat1 = dplyr::case_when(v212 >= 19 ~ "19+", v212 < 19 ~ "<19"),
    age.1stbrth.cat2 = dplyr::case_when(v212 < 20 ~ "<20", v212 >= 20 & v212 < 30 ~ "20-29", v212 >= 30 ~ "30+"),
    age.1stbrth.cat3 = dplyr::case_when(v212 < 16 ~ "<16", v212 >= 16 & v212 < 20 ~ "16-19", v212 >= 20 & v212 < 25 ~ "20-24", v212 >= 25 ~ "25+"),
    age.1stbrth.cat4 = dplyr::case_when(v212 < 15 ~ "<15", v212 >= 15 & v212 < 20 ~ "15-19", v212 >= 20 & v212 < 25 ~ "20-24", v212 >= 25 & v212 < 30 ~ "25-29", v212 >= 30 ~ "30+"))

  IR <- safe_mutate(IR, "total.sex.partners", "v836", function(df) {
    df %>% dplyr::mutate(
      total.sex.partners = dplyr::case_when(
        v836 %in% c("98","don't know") ~ NA_real_,
        v836 == "95+"                  ~ 95,
        TRUE                           ~ as.numeric(v836)),
      total.sex.partners.cat = dplyr::case_when(
        total.sex.partners == 1 ~ "1",
        total.sex.partners == 2 ~ "2",
        total.sex.partners > 2  ~ "3+"))
  })

  IR <- safe_mutate(IR, "discharge.checkedhealth", "m68_1", function(df) {
    df %>% dplyr::mutate(discharge.checkedhealth = dplyr::case_when(
      v201 == 0 ~ "no births",
      m68_1 %in% c("doctor","nurse/midwife","community health extension worker") ~ "health personnel",
      m68_1 %in% c("traditional birth attendant","auxiliary midwife")            ~ "midwife/aux midwife",
      m68_1 %in% c("community health influencers promoters and services (chips)/community health wor","other") ~ "Other",
      is.na(m68_1) ~ "No live birth <36 months"))
  })

  ## FERTILITY PREFERENCES ----
  IR <- safe_mutate(IR, "child.pref.discrep", c("v627","v628"), function(df) {
    df %>% dplyr::mutate(
      child.pref.discrep = dplyr::case_when(
        v627 %in% c(96,"other") ~ NA_real_,
        v628 %in% c(96,"other") ~ NA_real_,
        v627 %in% c(0:30) & v628 %in% c(0:30) ~ as.numeric(v627) - as.numeric(v628)),
      male.child.pref = dplyr::case_when(
        child.pref.discrep > 0 ~ 1,
        child.pref.discrep <= 0 ~ 0,
        v627 %in% c(96,"other") & v628 %in% c(96,"other") ~ 0))
  })

  IR <- IR %>% dplyr::mutate(
    fertility.pref = dplyr::case_when(
      v602 %in% c("sterilized (respondent or partner)","declared infecund") ~ "sterilized/infecund",
      TRUE ~ as.character(v602)),
    fertility.pref.cat = dplyr::case_when(
      v604 == "non-numeric"                         ~ "No specific timing",
      v604 %in% c("<12 months","1 year")            ~ "<2 years",
      v604 %in% c("2 years","3 years","4 years")    ~ "2-4 years",
      v604 %in% c("5 years","6+ years")             ~ "5+ years",
      v604 == "don't know"                          ~ "No specific timing",
      v602 != "have another"                        ~ "No more/undecided"),
    fertility.pref.cat2 = dplyr::case_when(
      v604 == "non-numeric"                                  ~ "No specific timing",
      v604 %in% c("<12 months","1 year")                    ~ "<2 years",
      v604 %in% c("2 years","3 years","4 years","5 years","6+ years") ~ "2+ years",
      v604 == "don't know"                                   ~ "No specific timing",
      v602 != "have another"                                 ~ "No more/undecided"),
    ideal.n.child.cat = dplyr::case_when(v614 %in% c(0:4) ~ "0-4", v614 >= 5 ~ "5+"),
    partner.desire.child = dplyr::case_when(
      marr.cohab == 0 ~ "not partnered", TRUE ~ as.character(v621)))

  ## CONDOM USE ----
  IR <- safe_mutate(IR, "condom.last.sex", c("v761","v766b"), function(df) {
    df %>% dplyr::mutate(condom.last.sex = dplyr::case_when(
      v761 == "no"         ~ "No",
      v761 == "yes"        ~ "Yes",
      v761 == "don't know" ~ NA_character_,
      v766b == 0           ~ "No sex partner in last 12 months"))
  })

  IR <- safe_mutate(IR, "condom.2nd.last.sex", c("v761b","v766b"), function(df) {
    df %>% dplyr::mutate(condom.2nd.last.sex = dplyr::case_when(
      v761b == "no"         ~ "No",
      v761b == "yes"        ~ "Yes",
      v761b == "don't know" ~ NA_character_,
      v766b == 1            ~ "Only 1 sex partner",
      v766b == 0            ~ "No sex partner in last 12 months"))
  })

  IR <- safe_mutate(IR, "condom.3rd.last.sex", c("v761c","v766b"), function(df) {
    df %>% dplyr::mutate(condom.3rd.last.sex = dplyr::case_when(
      v761c == "no"         ~ "No",
      v761c == "yes"        ~ "Yes",
      v761c == "don't know" ~ NA_character_,
      v766b == 1            ~ "Only 1 sex partner",
      v766b == 2            ~ "Only 2 sex partners",
      v766b == 0            ~ "No sex partner in last 12 months"))
  })

  IR <- safe_mutate(IR, "condom.source", c("v762","v761"), function(df) {
    df %>% dplyr::mutate(condom.source = dplyr::case_when(
      condom.last.sex == "no" ~ "condom not used",
      TRUE                    ~ as.character(v762)))
  })

  IR$n.sex.incl.partner.12m <- col_or_na(IR, "v766b")

  ## REASONS NOT USING FP ----
  v3a08_cols <- names(IR)[grepl("^v3a08", names(IR))]
  if (length(v3a08_cols) > 0) {
    IR.fp <- IR %>%
      dplyr::select(caseid, dplyr::all_of(v3a08_cols)) %>%
      reshape2::melt(id.vars = "caseid") %>%
      dplyr::group_by(caseid) %>%
      dplyr::mutate(fp.all.na = ifelse(all(is.na(value)), 1, 0)) %>%
      reshape2::dcast(caseid + fp.all.na ~ variable) %>%
      dplyr::select(caseid, fp.all.na)

    IR <- IR %>% base::merge(IR.fp, by = "caseid")

    IR <- safe_mutate(IR, "no.fp.access", c("v3a08q","v3a08r"), function(df) {
      df %>% dplyr::mutate(no.fp.access = dplyr::case_when(
        v3a08q == "yes" | v3a08r == "yes" ~ "yes",
        v3a08q == "no"  & v3a08r == "no" & fp.all.na == 0 ~ "no",
        v213 == "yes"                     ~ "Currently pregnant",
        v361 == "currently using"         ~ "Currently using FP",
        is.na(v3a08q)                     ~ "No identified need for FP"))
    })

    IR <- safe_mutate(IR, "no.fp.oppose", c("v3a08i","v3a08j","v3a08k","v3a08l"), function(df) {
      df %>% dplyr::mutate(no.fp.oppose = dplyr::case_when(
        v3a08i == "yes" | v3a08j == "yes" | v3a08k == "yes" | v3a08l == "yes" ~ "yes",
        v3a08i == "no"  & v3a08j == "no"  & v3a08k == "no"  & v3a08l == "no" & fp.all.na == 1 ~ "no",
        v213 == "yes"   ~ "Currently pregnant",
        v361 == "currently using" ~ "Currently using FP",
        is.na(v3a08j)   ~ "No identified need for FP"))
    })

    IR <- safe_mutate(IR, "no.fp.noneed", c("v3a08b","v3a08d","v3a08e","v3a08f","v3a08g"), function(df) {
      df %>% dplyr::mutate(no.fp.noneed = dplyr::case_when(
        v3a08b == "yes" | v3a08d == "yes" | v3a08e == "yes" | v3a08f == "yes" | v3a08g == "yes" ~ "yes",
        v3a08b == "no"  & v3a08d == "no"  & v3a08e == "no"  & v3a08f == "no"  & v3a08g == "no" & fp.all.na == 1 ~ "no",
        v213 == "yes"   ~ "Currently pregnant",
        v361 == "currently using" ~ "Currently using FP",
        is.na(v3a08b)   ~ "No identified need for FP"))
    })

    IR <- safe_mutate(IR, "no.fp.supply", c("v3a08u","v3a08v"), function(df) {
      df %>% dplyr::mutate(no.fp.supply = dplyr::case_when(
        v3a08u == "yes" | v3a08v == "yes" ~ "yes",
        v3a08u == "no"  & v3a08v == "no"  & fp.all.na == 1 ~ "no",
        v213 == "yes"   ~ "Currently pregnant",
        v361 == "currently using" ~ "Currently using FP",
        is.na(v3a08u)   ~ "No identified need for FP"))
    })
  } else {
    message("  Skipping FP reason variables — v3a08* columns not in this survey.")
    IR$fp.all.na    <- NA
    IR$no.fp.access <- NA
    IR$no.fp.oppose <- NA
    IR$no.fp.noneed <- NA
    IR$no.fp.supply <- NA
  }

  ## ATTITUDES ABOUT DOMESTIC VIOLENCE ----
  IR <- IR %>% dplyr::mutate(
    dv.out    = dplyr::case_when(v744a %in% c("no","don't know") ~ 0, v744a == "yes" ~ 1),
    dv.negkid = dplyr::case_when(v744b %in% c("no","don't know") ~ 0, v744b == "yes" ~ 1),
    dv.argue  = dplyr::case_when(v744c %in% c("no","don't know") ~ 0, v744c == "yes" ~ 1),
    dv.nosex  = dplyr::case_when(v744d %in% c("no","don't know") ~ 0, v744d == "yes" ~ 1),
    dv.burnfd = dplyr::case_when(v744e %in% c("no","don't know") ~ 0, v744e == "yes" ~ 1))

  dv_att_cols <- c("dv.out","dv.negkid","dv.argue","dv.nosex","dv.burnfd")
  IR <- IR %>%
    dplyr::mutate(
      dv.sum = dplyr::case_when(
        dplyr::if_all(dplyr::all_of(dv_att_cols), is.na) ~ NA_real_,
        TRUE ~ rowSums(dplyr::across(dplyr::all_of(dv_att_cols)), na.rm = TRUE)),
      dv.index     = as.character(dv.sum),
      dv.index.cat = dplyr::case_when(dv.index == 0 ~ "None", dv.index %in% c(1:5) ~ "1+"))

  ## FEMALE GENITAL MUTILATION ----
  fgm_cols <- c("g101","g102")
  if (all(fgm_cols %in% names(IR))) {
    IR <- IR %>% dplyr::mutate(
      female.circumcision = dplyr::case_when(
        g101 == "no" ~ "No", g102 == "no" ~ "No", g102 == "yes" ~ "Yes"),
      know.genitalcut = ifelse(g101 == "yes" | col_or_na(IR, "g100") == "yes", "Yes", "No"))
    IR <- safe_mutate(IR, "genitalflesh.removed", "g103", function(df) {
      df %>% dplyr::mutate(genitalflesh.removed = dplyr::case_when(
        female.circumcision == "No" ~ "never circumcised", TRUE ~ as.character(g103)))
    })
    IR <- safe_mutate(IR, "genital.nicked", "g104", function(df) {
      df %>% dplyr::mutate(genital.nicked = dplyr::case_when(
        female.circumcision == "No" ~ "never circumcised", TRUE ~ as.character(g104)))
    })
    IR <- safe_mutate(IR, "genital.sewn", "g105", function(df) {
      df %>% dplyr::mutate(genital.sewn = dplyr::case_when(
        female.circumcision == "No" ~ "never circumcised", TRUE ~ as.character(g105)))
    })
  } else {
    message("  Skipping FGM variables — g101/g102 columns not in this survey.")
    IR$female.circumcision  <- NA
    IR$know.genitalcut      <- NA
    IR$genitalflesh.removed <- NA
    IR$genital.nicked       <- NA
    IR$genital.sewn         <- NA
  }

  ## MEDIA EXPOSURE ----
  IR$freq.newsp <- as.character(col_or_na(IR, "v157"))
  IR$newsp.yn   <- ifelse(is.na(IR$freq.newsp) | IR$freq.newsp == "not at all", 0, 1)
  IR$freq.rad   <- as.character(col_or_na(IR, "v158"))
  IR$rad.yn     <- ifelse(is.na(IR$freq.rad)   | IR$freq.rad   == "not at all", 0, 1)
  IR$freq.tv    <- as.character(col_or_na(IR, "v159"))
  IR$tv.yn      <- ifelse(is.na(IR$freq.tv)    | IR$freq.tv    == "not at all", 0, 1)
  IR$any.media.yn <- ifelse(IR$newsp.yn == 1 | IR$rad.yn == 1 | IR$tv.yn == 1, "Yes", "No")

  v384_cols <- names(IR)[grepl("^v384", names(IR))]
  if (length(v384_cols) > 0) {
    IR <- IR %>% dplyr::mutate(fp.message.exp = dplyr::case_when(
      dplyr::if_any(dplyr::all_of(v384_cols), ~ .x == "yes") ~ "Yes",
      TRUE ~ "No"))
  } else {
    message("  Skipping fp.message.exp — v384* columns not in this survey.")
    IR$fp.message.exp <- NA
  }

  ## HEALTH INSURANCE ----
  IR <- safe_mutate(IR, "wm.hlth.insurance", "v481", function(df) {
    df %>% dplyr::mutate(wm.hlth.insurance = dplyr::case_when(
      v481 == "no"  ~ "No", v481 == "yes" ~ "Yes"))
  })

  v481_type_cols <- paste0("v481", c("a","b","c","d","e","f","g","h","x"))
  present_v481   <- intersect(v481_type_cols, names(IR))
  if (length(present_v481) > 0) {
    IR <- IR %>% dplyr::mutate(wm.hlth.insurance.type = dplyr::case_when(
      "v481a" %in% present_v481 & v481a == "yes" ~ "Mutual health org/community based insurance",
      "v481b" %in% present_v481 & v481b == "yes" ~ "Employer based",
      "v481c" %in% present_v481 & v481c == "yes" ~ "Social security",
      "v481d" %in% present_v481 & v481d == "yes" ~ "Privately purchased/commercial",
      TRUE                                         ~ "NA - no insurance"))
  } else {
    message("  Skipping wm.hlth.insurance.type — v481a/b/c/d columns not in this survey.")
    IR$wm.hlth.insurance.type <- NA
  }

  ## TRAVEL TIME TO FACILITY ----
  IR <- safe_mutate(IR, "travtime.fac.cat", "v483a", function(df) {
    df %>% dplyr::mutate(travtime.fac.cat = dplyr::case_when(
      v483a <= 10               ~ "10 minutes or less",
      v483a > 10 & v483a <= 30  ~ "11-30 minutes",
      v483a > 30 & v483a <= 60  ~ "31-60 minutes",
      v483a > 60                ~ "over an hour"))
  })

  ## PREGNANCY WANTED ----
  IR <- safe_mutate(IR, "preg.wanted", c("v367","v417"), function(df) {
    df %>% dplyr::mutate(preg.wanted = dplyr::case_when(
      v367 %in% c("wanted then","wanted later") ~ "Yes",
      v367 == "wanted no more"                  ~ "No",
      v417 == 0                                 ~ "NA - no child last 3/5 yrs"))
  })

  ## INTENTION TO USE CONTRACEPTION ----
  IR <- safe_mutate(IR, "intent.fp", "v362", function(df) {
    df %>% dplyr::mutate(intent.fp = dplyr::case_when(
      v362 %in% c("in next 12 months","use later")         ~ "Yes",
      v362 %in% c("unsure about timing","unsure about use") ~ "Unsure",
      v362 == "does not intend"                             ~ "No",
      TRUE                                                  ~ "NA - currently using FP"))
  })


  ######################################################################
  # 3 | BR FILE ----
  message("[4/6] Processing BR file...")

  BR$lives.away <- ifelse(
    BR$b9 %in% c("lives elsewhere","someone else","other relative","father") &
      BR$b8 %in% 1:14, 1, 0)

  BR <- BR %>%
    dplyr::group_by(caseid) %>%
    dplyr::mutate(num.kids.away = sum(lives.away, na.rm = TRUE)) %>%
    dplyr::ungroup()


  ######################################################################
  # 4 | MR FILE (OPTIONAL) ----
  message("[5/6] Processing MR file (if provided)...")


  ######################################################################
  # 5 | CLEAN UP ----
  message("[6/6] Merging files and cleaning up...")

  HH$v001 <- HH$hv001
  HH$v002 <- HH$hv002
  IR$wt   <- IR$v005 / 1000000

  BR.seg <- BR %>%
    dplyr::group_by(survey, caseid) %>%
    dplyr::summarize(
      lives.away.cnt = ifelse(all(is.na(lives.away)), NA, sum(lives.away, na.rm = TRUE)),
      .groups = "drop") %>%
    dplyr::mutate(lives.away = ifelse(lives.away.cnt > 0, 1, 0)) %>%
    dplyr::select(survey, caseid, lives.away)

  vulnerability <- IR %>%
    base::merge(BR.seg, by = c("survey","caseid"), all.x = TRUE) %>%
    base::merge(HH,     by = c("survey","v001","v002"), all.x = TRUE) %>%
    dplyr::mutate(strata = hv025)

  rm(IR, HH, BR)
  if (!is.null(MR)) rm(MR)
  if (!is.null(PR)) rm(PR)
  gc()

  t_end <- proc.time()
  message("Vulnerability factor generation complete in ",
          round((t_end - t_start)[["elapsed"]] / 60, 1), " minutes.")

  return(vulnerability)

}
