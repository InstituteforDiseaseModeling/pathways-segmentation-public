################################################################################
# GENERATE OUTCOME VARIABLES | DHS
################################################################################

# TABLE OF CONTENTS
# 1  | GENERAL
# 2  | ANC/PNC
# 3  | BMI
# 4  | BREASTFEEDING
# 5  | CHILD MORTALITY
# 6  | FAMILY PLANNING
# 7  | HOME BIRTHS
# 8  | IMMUNIZATION
# 9  | MALNOURISHMENT
# 10 | MENSTRUAL HEALTH
# 11 | CHILD HEALTH
# 12 | MATERNAL HEALTH
# 13 | SUMMARIZE


gen_outcome_variables_dhs <- function(IR = NULL, KR = NULL, BR = NULL, dhs = 8) {

  library(dplyr)
  library(data.table)
  library(reshape2)
  library(stringr)

  message("[1/14] Starting outcome variable generation...")
  t_start <- proc.time()

  # Convert to data.table for := syntax used in calendar string parsing
  setDT(IR)
  setDT(BR)
  setDT(KR)

  # Helper: safely access a column that may not exist in this survey
  col_or_na <- function(df, col) {
    if (col %in% names(df)) df[[col]] else NA
  }

  # Helper: apply mutate only if all referenced columns exist; otherwise set output to NA
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

  # Helper: coerce factor/labelled columns to numeric where comparisons are needed
  coerce_to_numeric <- function(df, cols) {
    for (col in intersect(cols, names(df))) {
      if (is.factor(df[[col]]) || (requireNamespace("haven", quietly = TRUE) && haven::is.labelled(df[[col]]))) {
        df[[col]] <- suppressWarnings(as.numeric(as.character(df[[col]])))
      }
    }
    df
  }

  # COERCE NUMERIC COLUMNS THAT ARE SOMETIMES READ AS FACTORS BY haven
  IR <- coerce_to_numeric(IR, c(
    "v008", "v011", "v012", "v013", "v018", "v137", "v201",
    "v445", "v313", "v626a",
    "b19_01", "m14_1", "m13_1", "m19_1"
  ))

  BR <- coerce_to_numeric(BR, c("v001", "v002", "v008", "b3", "b5", "b7", "b8", "b9"))
  KR <- coerce_to_numeric(KR, c("v008", "v137", "b3", "b19", "m39", "v469e", "v469f", "v469x"))


  ######################################################################
  # 1 | GENERAL ----
  message("[2/14] General setup (calendar string, child ages)...")

  IR[, str := substr(vcal_1, v018, v018 + 59)]
  IR[, STL := ifelse(grepl("TPPPPPP",  str), 1, NA)]
  IR[, LB  := ifelse(grepl("BPPPPPPP", str), 1, NA)]

  BR$hhid     <- paste(BR$v001, BR$v002)
  BR[, child.age := (v008 - b3)]

  KR[, child.age := (v008 - b3)]
  KR$hhid <- paste(KR$v001, KR$v002)

  KR <- KR %>% dplyr::filter(child.age < 61)


  ######################################################################
  # 2 | ANC/PNC ----
  message("[3/14] ANC/PNC variables...")

  IR <- IR %>%
    dplyr::group_by(survey) %>%
    dplyr::mutate(
      anc.total = dplyr::case_when(
        m14_1 == 98 | m14_1 == "don't know"         ~ NA_real_,
        m14_1 == "no antenatal visits" | m14_1 == 0  ~ 0,
        is.na(m14_1)                                 ~ NA_real_,
        TRUE                                         ~ as.numeric(m14_1)),
      anc.mean    = as.integer(mean(anc.total, na.rm = TRUE)),
      anc.total   = ifelse(m14_1 == 98 | m14_1 == "don't know", anc.mean, anc.total),
      anc.less4.last = ifelse(anc.total >= 4, 0, 1),
      anc.8plus.last = ifelse(anc.total >= 8, 0, 1)) %>%
    dplyr::ungroup()

  IR <- IR %>%
    dplyr::group_by(survey) %>%
    dplyr::mutate(
      anc.1stvisit = dplyr::case_when(
        m13_1 == 98 | m13_1 == "don't know" ~ NA_real_,
        is.na(m13_1)                         ~ NA_real_,
        TRUE                                 ~ as.numeric(m13_1)),
      anc.1st.mean   = as.integer(mean(anc.1stvisit, na.rm = TRUE)),
      anc.1stvisit   = ifelse(m13_1 == 98 | m13_1 == "don't know", anc.1st.mean, anc.1stvisit),
      no.anc.1st.tri = ifelse(anc.1stvisit < 4, 0, 1)) %>%
    dplyr::ungroup()

  BR <- BR %>%
    dplyr::mutate(unassisted.del = dplyr::case_when(
      is.na(m3a) & is.na(m3b) ~ NA_integer_,
      m3a == 1 | m3b == 1     ~ 0L,
      TRUE                     ~ 1L))

  IR <- IR %>%
    dplyr::mutate(
      anc.month = dplyr::case_when(
        anc.1stvisit < 4                          ~ "Month 1-3",
        anc.1stvisit > 3 & anc.1stvisit < 7      ~ "Month 4-6",
        anc.1stvisit > 6 & anc.1stvisit < 10     ~ "Month 7-9",
        m14_1 == 0                                ~ "No ANC"))
  IR$anc.month <- factor(IR$anc.month, levels = c("No ANC", "Month 1-3", "Month 4-6", "Month 7-9"))

  IR <- safe_mutate(IR, "wom.nohlthck", c("m62_1", "m66_1"), function(df) {
    temp1 <- ifelse(df$m66_1 == "yes" & !is.na(df$m66_1), 1, 0)
    temp2 <- ifelse(df$m62_1 == "yes" & !is.na(df$m62_1), 1, 0)
    df$wom.nohlthck <- ifelse(temp1 == 1 | temp2 == 1, 0, 1)
    df$wom.nohlthck <- ifelse(is.na(df$m62_1) & is.na(df$m66_1), NA, df$wom.nohlthck)
    df
  })

  IR <- safe_mutate(IR, "baby.nohlthck", c("m70_1", "m74_1"), function(df) {
    temp3 <- ifelse(df$m70_1 == "yes" & !is.na(df$m70_1), 1, 0)
    temp4 <- ifelse(df$m74_1 == "yes" & !is.na(df$m74_1), 1, 0)
    df$baby.nohlthck <- ifelse(temp3 == 1 | temp4 == 1, 0, 1)
    df$baby.nohlthck <- ifelse(is.na(df$m70_1) & is.na(df$m74_1), NA, df$baby.nohlthck)
    df
  })


  ######################################################################
  # 3 | BMI ----
  message("[4/14] BMI variables...")

  IR$bmi <- as.numeric(IR$v445) / 100
  IR$bmi <- ifelse(IR$bmi > 90, NA, IR$bmi)
  IR <- IR %>% dplyr::mutate(woman.underweight = dplyr::case_when(
    bmi < 18.5 ~ 1, is.na(bmi) ~ NA, TRUE ~ 0))


  ######################################################################
  # 4 | BREASTFEEDING ----
  message("[5/14] Breastfeeding variables...")

  IR <- IR %>% dplyr::mutate(
    no.breastfeed.last = dplyr::case_when(
      m4_1 == "never breastfed" ~ 1, is.na(m4_1) ~ NA, TRUE ~ 0))

  KR <- KR %>% dplyr::mutate(
    no.breastfeed.n = dplyr::case_when(
      m4 == "never breastfed" ~ 1, is.na(m4) ~ NA, TRUE ~ 0))

  IR <- safe_mutate(IR, "no.breastfeed2.last", c("b19_01", "b9_01"), function(df) {
    df %>% dplyr::mutate(
      no.breastfeed2.last = ifelse(b19_01 < 24 & b9_01 == "respondent", no.breastfeed.last, NA))
  })

  KR <- KR %>% dplyr::mutate(
    no.breastfeed2.n = ifelse(b19 < 24 & b9 == "respondent", no.breastfeed.n, NA))


  ######################################################################
  # 5 | CHILD MORTALITY ----
  message("[6/14] Child mortality variables...")

  BR$u1mort <- ifelse(BR$b7 < 12 & BR$b5 == "no", 1, 0)
  BR$u5mort <- ifelse(BR$b7 < 60 & BR$b5 == "no", 1, 0)

  ## STILLBIRTHS ----
  getStillbirths <- function(individ, id_vars, recode = 7) {
    setDT(individ)
    data.table::melt(individ,
                     measure.vars = patterns("^bord[_]", "^b0[_]", "^b3[_]", "^b4[_]", "^b5[_]",
                                             "^b6[_]", "^b7[_]", "^b8[_]", "^b11[_]", "^m14[_]",
                                             "^m15[_]", "^m17[_]"),
                     value.name   = c("bord", "b0", "dob", "sex", "alive", "deathAge",
                                      "deathAgeMo", "CurrAge", "b11", "m14", "m15", "m17"),
                     variable.name = "ReverseOrder",
                     id.vars = id_vars)
  }

  stl_var_list <- c("caseid", "v000", "v001", "v002", "v003", "v004", "v005",
                    "v006", "v007", "v008", "v008a", "v009", "v010", "v011",
                    "v012", "v013", "v014", "v015", "v016", "v017", "v018",
                    "v019", "v019a", "v020", "v021", "v022", "v023", "v024",
                    "v025", "v026", "v027", "v028", "v029", "v030", "v031",
                    "v032", "v034", "v040",
                    "sm508va_4", "sm508va_5", "sm508va_6", "str", "STL", "LB")

  some <- names(IR)[names(IR) %in% stl_var_list]
  stl  <- getStillbirths(IR, id_vars = some)

  stl[, id    := 1:.N]
  stl[, start := str_locate_all(str, "TPPPPPP")[[1]][1], by = "id"]
  stl[, MomAgeSTL := ceiling(v012 - (start / 12))]
  stl[, MomAge    := ceiling((dob - v011) / 12)]
  stl$period <- 60
  stl$tu     <- stl$v008
  stl$tl     <- stl$v008 - stl$period
  stl        <- stl[tl <= dob & dob < tu]
  stl[, index := paste(caseid, v000, v007)]
  stl[, diff  := MomAgeSTL - MomAge]

  sub <- stl[STL == 1]
  setkey(sub, index)
  sub$rep    <- NA
  sub$rep[1] <- 1
  for (i in 2:nrow(sub)) {
    sub$rep[i] <- ifelse(sub$index[i] == sub$index[i - 1], 0, 1)
  }
  setkey(sub, index)
  sub <- sub[rep == 1]
  sub[, `:=`(MomAge = MomAgeSTL, deathAge = NA, CurrAge = NA,
             deathAgeMo = NA, sex = NA, dob = NA, alive = "STL")]

  stl <- rbind(stl, sub[, -"rep"])
  setkey(stl, index)
  stl[alive == "STL", stl := 1]
  stl[is.na(stl),    stl := 0]

  stl.out <- stl %>%
    dplyr::group_by(caseid) %>%
    dplyr::summarize(stl.cnt = sum(stl, na.rm = TRUE), .groups = "drop") %>%
    dplyr::mutate(stl.yn = ifelse(stl.cnt > 0, 1, 0))


  ######################################################################
  # 6 | FAMILY PLANNING ----
  message("[7/14] Family planning variables...")

  IR <- IR %>% dplyr::mutate(
    nofp.mod.now = ifelse(v313 == "modern method", 0, 1),
    nofp.dis.5yr = dplyr::case_when(
      v359 %in% c("pill", "iud", "injections", "male condom", "implants/norplant",
                  "female condom", "emergency contraception", "other modern method",
                  "lactational amenorrhea (lam)", "standard days method (sdm)") ~ 0,
      is.na(v359) ~ NA_real_,
      TRUE ~ 1))
  IR$nofp.dis.5yr[is.na(IR$nofp.dis.5yr)] <- 1
  IR <- IR %>% dplyr::mutate(
    nofp.mod.ever = ifelse(nofp.mod.now == 0 | nofp.dis.5yr == 0, 0, 1))

  IR <- safe_mutate(IR, "fp_unmet_tot", "v626a", function(df) {
    df %>% dplyr::mutate(fp_unmet_tot = ifelse(v626a == 1 | v626a == 2, 1, 0))
  })


  ######################################################################
  # 7 | HOME BIRTHS ----
  message("[8/14] Home birth variables...")

  home_vals <- c("home", "parents' home", "her home", "other home")

  IR <- IR %>% dplyr::mutate(
    home.birth.last = dplyr::case_when(
      m15_1 %in% home_vals ~ 1, is.na(m15_1) ~ NA, TRUE ~ 0))

  for (i in 2:5) {
    col <- paste0("m15_", i)
    new <- paste0("home.birth.", i)
    IR <- safe_mutate(IR, new, col, function(df) {
      df[[new]] <- dplyr::case_when(
        df[[col]] %in% c("her home", "other home") ~ 1,
        is.na(df[[col]])                            ~ NA_real_,
        TRUE                                        ~ 0)
      df
    })
  }

  IR <- IR %>% dplyr::mutate(home.birth.ever = dplyr::case_when(
    home.birth.last == 1 | home.birth.2 == 1 | home.birth.3 == 1 |
      home.birth.4 == 1 | home.birth.5 == 1                          ~ 1,
    is.na(home.birth.last) & is.na(home.birth.2) & is.na(home.birth.3) &
      is.na(home.birth.4)  & is.na(home.birth.5)                     ~ NA,
    TRUE ~ 0))

  BR <- BR %>% dplyr::mutate(home.birth = dplyr::case_when(
    m15 %in% home_vals ~ 1, is.na(m15) ~ NA, TRUE ~ 0))


  ######################################################################
  # 8 | IMMUNIZATION ----
  message("[9/14] Immunization variables...")

  vax_levels <- c("vaccination date on card", "vaccination marked on card", "reported by mother")
  vax_no     <- c("no", "don't know")

  vax_col <- function(df, new_col, src_col) {
    safe_mutate(df, new_col, src_col, function(d) {
      d[[new_col]] <- dplyr::case_when(
        d[[src_col]] %in% vax_levels ~ 1,
        d[[src_col]] %in% vax_no    ~ 0)
      d
    })
  }

  IR <- safe_mutate(IR, "vac.doc.yn", "h1a_1", function(df) {
    df %>% dplyr::mutate(
      vac.doc    = h1a_1,
      vac.doc.yn = ifelse(vac.doc == "does not have either card or other document", 0, 1))
  })

  KR <- vax_col(KR, "dpt1", "h3")
  KR <- vax_col(KR, "dpt2", "h5")
  KR <- vax_col(KR, "dpt3", "h7")
  KR <- KR %>% dplyr::mutate(
    dptsum   = dpt1 + dpt2 + dpt3,
    dpt.full = dplyr::case_when(is.na(dptsum) ~ NA, dptsum == 3 ~ 0, TRUE ~ 1),
    dpt.all  = dplyr::case_when(is.na(dptsum) ~ NA, dptsum == 3 ~ 1, TRUE ~ 0),
    dpt.none = dplyr::case_when(is.na(dptsum) ~ NA, dptsum == 0 ~ 1, TRUE ~ 0),
    zero.dose = dplyr::case_when(is.na(dptsum) ~ NA, dptsum == 0 ~ 1, TRUE ~ 0))

  KR <- vax_col(KR, "polio1", "h4")
  KR <- vax_col(KR, "polio2", "h6")
  KR <- vax_col(KR, "polio3", "h8")
  KR <- KR %>% dplyr::mutate(
    poliosum   = polio1 + polio2 + polio3,
    polio.full = dplyr::case_when(is.na(poliosum) ~ NA, poliosum == 3 ~ 0, TRUE ~ 1),
    polio.all  = dplyr::case_when(is.na(poliosum) ~ NA, poliosum == 3 ~ 0, TRUE ~ 1),
    polio.none = dplyr::case_when(is.na(poliosum) ~ NA, poliosum == 0 ~ 1, TRUE ~ 0))

  KR <- vax_col(KR, "measles1", "h9")
  KR <- safe_mutate(KR, "measles2", "h9a", function(df) vax_col(df, "measles2", "h9a"))
  KR <- KR %>% dplyr::mutate(
    measlessum   = measles1 + measles2,
    measles.full = dplyr::case_when(is.na(measlessum) ~ NA, measlessum == 2 ~ 0, TRUE ~ 1),
    measles.all  = dplyr::case_when(is.na(measlessum) ~ NA, measlessum == 2 ~ 1, TRUE ~ 0),
    measles.none = dplyr::case_when(is.na(measlessum) ~ NA, measlessum == 0 ~ 1, TRUE ~ 0))

  KR <- vax_col(KR, "bcg1", "h2")
  KR <- KR %>% dplyr::mutate(
    bcg.all  = dplyr::case_when(is.na(bcg1) ~ NA, bcg1 == 1 ~ 1, TRUE ~ 0),
    bcg.none = dplyr::case_when(is.na(bcg1) ~ NA, bcg1 == 0 ~ 1, TRUE ~ 0))

  if ("h50" %in% names(KR)) {
    KR <- vax_col(KR, "hepb1", "h50")
    KR <- KR %>% dplyr::mutate(
      hepb.all  = dplyr::case_when(is.na(hepb1) ~ NA, hepb1 == 1 ~ 1, TRUE ~ 0),
      hepb.none = dplyr::case_when(is.na(hepb1) ~ NA, hepb1 == 0 ~ 1, TRUE ~ 0))
  } else {
    message("  Skipping hepb — h50 column not in this survey.")
    KR$hepb1     <- NA
    KR$hepb.all  <- NA
    KR$hepb.none <- NA
  }

  if (all(c("h51","h52","h53") %in% names(KR))) {
    KR <- vax_col(KR, "pentavalent1", "h51")
    KR <- vax_col(KR, "pentavalent2", "h52")
    KR <- vax_col(KR, "pentavalent3", "h53")
    KR <- KR %>% dplyr::mutate(
      pentavalentsum   = pentavalent1 + pentavalent2 + pentavalent3,
      pentavalent.all  = dplyr::case_when(is.na(pentavalentsum) ~ NA, pentavalentsum == 3 ~ 1, TRUE ~ 0),
      pentavalent.none = dplyr::case_when(is.na(pentavalentsum) ~ NA, pentavalentsum == 0 ~ 1, TRUE ~ 0))
  } else {
    message("  Skipping pentavalent — h51/h52/h53 columns not in this survey.")
    KR$pentavalent1    <- NA
    KR$pentavalent2    <- NA
    KR$pentavalent3    <- NA
    KR$pentavalentsum  <- NA
    KR$pentavalent.all  <- NA
    KR$pentavalent.none <- NA
  }

  if (all(c("h54","h55","h56") %in% names(KR))) {
    KR <- vax_col(KR, "pneumo1", "h54")
    KR <- vax_col(KR, "pneumo2", "h55")
    KR <- vax_col(KR, "pneumo3", "h56")
    KR <- KR %>% dplyr::mutate(
      pneumosum   = pneumo1 + pneumo2 + pneumo3,
      pneumo.all  = dplyr::case_when(is.na(pneumosum) ~ NA, pneumosum == 3 ~ 1, TRUE ~ 0),
      pneumo.none = dplyr::case_when(is.na(pneumosum) ~ NA, pneumosum == 0 ~ 1, TRUE ~ 0))
  } else {
    message("  Skipping pneumo — h54/h55/h56 columns not in this survey.")
    KR$pneumo1    <- NA
    KR$pneumo2    <- NA
    KR$pneumo3    <- NA
    KR$pneumosum  <- NA
    KR$pneumo.all  <- NA
    KR$pneumo.none <- NA
  }

  if (all(c("h57","h58","h59") %in% names(KR))) {
    KR <- vax_col(KR, "rota1", "h57")
    KR <- vax_col(KR, "rota2", "h58")
    KR <- vax_col(KR, "rota3", "h59")
    KR <- KR %>% dplyr::mutate(
      rotasum   = rota1 + rota2 + rota3,
      rota.all  = dplyr::case_when(is.na(rotasum) ~ NA, rotasum == 3 ~ 1, TRUE ~ 0),
      rota.none = dplyr::case_when(is.na(rotasum) ~ NA, rotasum == 0 ~ 1, TRUE ~ 0))
  } else {
    message("  Skipping rota — h57/h58/h59 columns not in this survey.")
    KR$rota1    <- NA
    KR$rota2    <- NA
    KR$rota3    <- NA
    KR$rotasum  <- NA
    KR$rota.all  <- NA
    KR$rota.none <- NA
  }

  if ("syf" %in% names(KR)) {
    KR <- vax_col(KR, "yellowfever1", "syf")
    KR <- KR %>% dplyr::mutate(
      yellowfever.all  = dplyr::case_when(is.na(yellowfever1) ~ NA, yellowfever1 == 1 ~ 1, TRUE ~ 0),
      yellowfever.none = dplyr::case_when(is.na(yellowfever1) ~ NA, yellowfever1 == 0 ~ 1, TRUE ~ 0))
  } else {
    message("  Skipping yellowfever — syf column not in this survey.")
    KR$yellowfever1   <- NA
    KR$yellowfever.all  <- NA
    KR$yellowfever.none <- NA
  }

  if ("smg" %in% names(KR)) {
    KR <- vax_col(KR, "meningitis1", "smg")
    KR <- KR %>% dplyr::mutate(
      meningitis.all  = dplyr::case_when(is.na(meningitis1) ~ NA, meningitis1 == 1 ~ 1, TRUE ~ 0),
      meningitis.none = dplyr::case_when(is.na(meningitis1) ~ NA, meningitis1 == 0 ~ 1, TRUE ~ 0))
  } else {
    message("  Skipping meningitis — smg column not in this survey.")
    KR$meningitis1    <- NA
    KR$meningitis.all  <- NA
    KR$meningitis.none <- NA
  }

  ## SUMMARY VAX MEASURES ----
  KR <- KR %>%
    dplyr::mutate(
      basic.antigen.full.12m = dplyr::case_when(
        is.na(h1) ~ NA_real_, b19 < 12 | b19 > 24 ~ NA_real_,
        b19 >= 12 & bcg.all == 1 & dpt.all == 1 & polio.all == 1 & measles1 == 1 ~ 1,
        b19 >= 12 & b19 <= 24 ~ 0),
      not.basic.antigen.full.12m = dplyr::case_when(
        is.na(h1) ~ NA_real_, b19 < 12 | b19 > 24 ~ NA_real_,
        b19 >= 12 & b19 <= 24 &
          (bcg.all == 0 | polio.all == 0 | measles1 == 0 | dpt.all == 0) ~ 1,
        b19 >= 12 & b19 <= 24 ~ 0),
      full.vax.schedule.12m = dplyr::case_when(
        is.na(h1) ~ NA_real_, b19 < 12 | b19 > 24 ~ NA_real_,
        b19 >= 12 & b19 <= 24 &
          bcg.all == 1 & hepb.all == 1 & pentavalent.all == 1 & polio.all == 1 &
          pneumo.all == 1 & measles1 == 1 & yellowfever.all == 1 & meningitis.all == 1 ~ 1,
        b19 >= 12 & b19 <= 24 ~ 0),
      not.full.vax.schedule.12m = dplyr::case_when(
        is.na(h1) ~ NA_real_, b19 < 12 | b19 > 24 ~ NA_real_,
        b19 >= 12 & b19 <= 24 &
          (bcg.all == 0 | hepb.all == 0 | pentavalent.all == 0 | polio.all == 0 |
             pneumo.all == 0 | measles1 == 0 | yellowfever.all == 0 | meningitis.all == 0) ~ 1,
        b19 >= 12 & b19 <= 24 ~ 0),
      full.vax.schedule.24m = dplyr::case_when(
        is.na(h1) ~ NA_real_, b19 < 24 | b19 > 36 ~ NA_real_,
        b19 >= 24 & bcg.all == 1 & hepb.all == 1 & pentavalent.all == 1 & polio.all == 1 &
          pneumo.all == 1 & measles.all == 1 & yellowfever.all == 1 & meningitis.all == 1 ~ 1,
        b19 >= 24 & b19 <= 36 ~ 0),
      not.full.vax.schedule.24m = dplyr::case_when(
        is.na(h1) ~ NA_real_, b19 < 24 | b19 > 36 ~ NA_real_,
        b19 >= 24 & (bcg.all == 0 | hepb.all == 0 | pentavalent.all == 0 | polio.all == 0 |
                       pneumo.all == 0 | measles.all == 0 | yellowfever.all == 0 |
                       meningitis.all == 0) ~ 1,
        b19 >= 24 & b19 <= 36 ~ 0),
      zero.dose.4vax.12m = dplyr::case_when(
        b19 < 12 | b19 > 24 ~ NA_real_,
        b19 >= 12 & bcg1 == 0 & dpt1 == 0 & dpt2 == 0 & dpt3 == 0 &
          polio1 == 0 & polio2 == 0 & polio3 == 0 & measles1 == 0 ~ 1,
        b19 >= 12 & b19 <= 24 ~ 0),
      zero.dose.operational = dplyr::case_when(
        b19 < 12 | b19 > 24 ~ NA_real_,
        b19 >= 12 & dpt1 == 0 ~ 1,
        b19 >= 12 & b19 <= 24 ~ 0))


  ######################################################################
  # 9 | MALNOURISHMENT ----
  message("[10/14] Malnourishment variables...")

  v469_cols <- intersect(c("v469e", "v469f", "v469x"), names(KR))
  if (length(v469_cols) == 3) {
    KR <- KR %>%
      dplyr::mutate(
        across(dplyr::all_of(v469_cols), ~ suppressWarnings(as.numeric(as.character(.)))) ) %>%
      dplyr::mutate(
        across(dplyr::all_of(v469_cols), ~ dplyr::na_if(., 8), .names = "{.col}_"),
        across(paste0(v469_cols, "_"), ~ dplyr::coalesce(., 0)),
        totmilkf = v469e_ + v469f_ + v469x_,
        nt_fed_milk = dplyr::case_when(
          (totmilkf >= 2 | m4 == "still breastfeeding") & b19 >= 6 & b19 <= 23 ~ 1,
          (totmilkf < 2  | m4 != "still breastfeeding") & b19 >= 6 & b19 <= 23 ~ 0,
          TRUE ~ NA_real_))
  } else {
    message("  Skipping totmilkf/nt_fed_milk — v469e/f/x columns not in this survey.")
    KR$totmilkf    <- NA_real_
    KR$nt_fed_milk <- NA_real_
  }

  food_cols <- paste0("v414", c("a","b","c","d","e","f","g","h","i","j","k","l","m","n","o","p","s","v"))
  present_food <- intersect(food_cols, names(KR))
  if (length(present_food) > 0) {
    KR <- KR %>%
      dplyr::mutate(
        food1      = dplyr::case_when(v414a == "yes" ~ 1, v414a != "yes" ~ 0),
        food2      = dplyr::case_when(v414b == "yes" ~ 1, v414b != "yes" ~ 0),
        food3      = dplyr::case_when(v414c == "yes" ~ 1, v414c != "yes" ~ 0),
        food4      = dplyr::case_when(v414d == "yes" ~ 1, v414d != "yes" ~ 0),
        nt_formula = dplyr::case_when(v411a == "yes" ~ 1, v411a != "yes" ~ 0),
        nt_milk    = dplyr::case_when(v411  == "yes" ~ 1, v411  != "yes" ~ 0),
        nt_liquids = dplyr::case_when(v410 == "yes" | v412c == "yes" | v413 == "yes" ~ 1, TRUE ~ 0),
        nt_bbyfood = dplyr::case_when(v412a == "yes" ~ 1, v412a != "yes" ~ 0),
        nt_grains  = dplyr::case_when(v412a == "yes" | v414e == "yes" ~ 1, TRUE ~ 0),
        nt_vita    = dplyr::case_when(v414i == "yes" | v414j == "yes" | v414k == "yes" ~ 1, TRUE ~ 0),
        nt_frtveg  = dplyr::case_when(v414l == "yes" ~ 1, v414l != "yes" ~ 0),
        nt_root    = dplyr::case_when(v414f == "yes" ~ 1, v414f != "yes" ~ 0),
        nt_nuts    = dplyr::case_when(v414o == "yes" ~ 1, v414o != "yes" ~ 0),
        nt_meatfish = dplyr::case_when(v414h == "yes" | v414m == "yes" | v414n == "yes" ~ 1, TRUE ~ 0),
        nt_eggs    = dplyr::case_when(v414g == "yes" ~ 1, v414g != "yes" ~ 0),
        nt_dairy   = dplyr::case_when(v414p == "yes" | v414v == "yes" ~ 1, TRUE ~ 0))

    food_groups <- c("group1","group2","group3","group4","group5","group6","group7","group8")
    KR <- KR %>%
      dplyr::mutate(
        group1 = dplyr::case_when(m4 == "still breastfeeding" ~ 1, m4 != "still breastfeeding" ~ 0),
        group2 = dplyr::case_when(nt_formula == 1 | nt_milk == 1 | nt_dairy == 1 ~ 1, TRUE ~ 0),
        group3 = dplyr::case_when(nt_grains == 1 | nt_root == 1 | nt_bbyfood == 1 ~ 1, TRUE ~ 0),
        group4 = dplyr::case_when(nt_vita    == 1 ~ 1, nt_vita    != 1 ~ 0),
        group5 = dplyr::case_when(nt_frtveg  == 1 ~ 1, nt_frtveg  != 1 ~ 0),
        group6 = dplyr::case_when(nt_eggs    == 1 ~ 1, nt_eggs    != 1 ~ 0),
        group7 = dplyr::case_when(nt_meatfish == 1 ~ 1, nt_meatfish != 1 ~ 0),
        group8 = dplyr::case_when(nt_nuts    == 1 ~ 1, nt_nuts    != 1 ~ 0)) %>%
      dplyr::mutate(
        food_nonmiss = rowSums(!is.na(dplyr::select(., dplyr::all_of(food_groups)))),
        foodsum = dplyr::case_when(
          food_nonmiss == 0 ~ NA_real_,
          TRUE ~ rowSums(dplyr::select(., dplyr::all_of(food_groups)), na.rm = TRUE)),
        nt_mdd = dplyr::case_when(
          b19 >= 6 & b19 < 24 & foodsum < 5  ~ 0,
          b19 >= 6 & b19 < 24 & foodsum >= 5 ~ 1))
  } else {
    message("  Skipping dietary diversity variables — v414* columns not in this survey.")
    KR$nt_mdd <- NA_real_
  }

  KR <- safe_mutate(KR, "nt_mmf", "m39", function(df) {
    df %>% dplyr::mutate(
      feedings = dplyr::case_when(m39 > 0 & m39 < 8 ~ totmilkf + m39, TRUE ~ NA_real_),
      nt_mmf   = dplyr::case_when(
        b19 < 6 ~ NA_real_,
        (m4 == "still breastfeeding" & b19 >= 6  & b19 <= 8  & m39 >= 2 & m39 <= 7) |
          (m4 == "still breastfeeding" & b19 >= 9  & b19 <= 23 & m39 >= 3 & m39 <= 7) ~ 1,
        (m4 != "still breastfeeding" & b19 >= 6 & b19 <= 23 & feedings >= 4) ~ 1,
        TRUE ~ 0))
  })

  KR <- KR %>%
    dplyr::mutate(
      foodsum2 = nt_grains + nt_root + nt_nuts + nt_meatfish + nt_vita + nt_frtveg + nt_eggs,
      nt_mad   = dplyr::case_when(
        b19 < 6 ~ NA_real_,
        m4 == "still breastfeeding" & nt_mdd == 1 & nt_mmf == 1 ~ 1,
        m4 != "still breastfeeding" & foodsum2 >= 4 & nt_mmf == 1 & totmilkf >= 2 ~ 1,
        TRUE ~ 0),
      no.mad = ifelse(nt_mad == 1, 0, 1))

  IR <- safe_mutate(IR, "novita.6m", "h34_1", function(df) {
    df %>% dplyr::mutate(novita.6m = dplyr::case_when(
      h34_1 == "yes"       ~ 0,
      h34_1 == "no"        ~ 1,
      h34_1 == "don't know" ~ NA_real_,
      v137 == 0            ~ NA_real_))
  })

  BR <- safe_mutate(BR, "micronutrient.12m", "h80a", function(df) {
    df %>% dplyr::mutate(micronutrient.12m = dplyr::case_when(
      h80a %in% c("no", "don't know") ~ 1, h80a == "yes" ~ 0))
  })

  ## STUNTING AND WASTING ----
  if (dhs == 7) {
    haz_col_ir  <- "hw5_1";  whz_col_ir  <- "hw11_1"
    haz_col_kr  <- "hw5";    whz_col_kr  <- "hw11"
  } else {
    haz_col_ir  <- "hw70_1"; whz_col_ir  <- "hw72_1"
    haz_col_kr  <- "hw70";   whz_col_kr  <- "hw72"
  }

  IR <- safe_mutate(IR, "stunt.cat2.last", haz_col_ir, function(df) {
    df %>% dplyr::mutate(
      haz.last       = dplyr::case_when(as.numeric(df[[haz_col_ir]]) / 100 > 90 ~ NA, is.na(df[[haz_col_ir]]) ~ NA, TRUE ~ as.numeric(df[[haz_col_ir]]) / 100),
      stunt.cat2.last = ifelse(haz.last < -2, 1, 0))
  })

  IR <- safe_mutate(IR, "waste.cat2.last", whz_col_ir, function(df) {
    df %>% dplyr::mutate(
      whz.last       = dplyr::case_when(as.numeric(df[[whz_col_ir]]) / 100 > 90 ~ NA, is.na(df[[whz_col_ir]]) ~ NA, TRUE ~ as.numeric(df[[whz_col_ir]]) / 100),
      waste.cat2.last = ifelse(whz.last < -2, 1, 0))
  })

  IR <- safe_mutate(IR, "undwgt.last", "hw71_1", function(df) {
    df %>% dplyr::mutate(
      waz.last    = dplyr::case_when(as.numeric(hw71_1) / 100 > 90 ~ NA, is.na(hw71_1) ~ NA, TRUE ~ as.numeric(hw71_1) / 100),
      undwgt.last = ifelse(waz.last < -2, 1, 0),
      ovrwgt.last = ifelse(waz.last > 2,  1, 0))
  })

  KR <- safe_mutate(KR, "stunt.cat2", haz_col_kr, function(df) {
    df %>% dplyr::mutate(
      haz        = dplyr::case_when(as.numeric(df[[haz_col_kr]]) / 100 > 90 ~ NA, is.na(df[[haz_col_kr]]) ~ NA, TRUE ~ as.numeric(df[[haz_col_kr]]) / 100),
      stunt.cat2 = ifelse(haz < -2, 1, 0))
  })

  KR <- safe_mutate(KR, "waste.cat2", whz_col_kr, function(df) {
    df %>% dplyr::mutate(
      whz        = dplyr::case_when(as.numeric(df[[whz_col_kr]]) / 100 > 90 ~ NA, is.na(df[[whz_col_kr]]) ~ NA, TRUE ~ as.numeric(df[[whz_col_kr]]) / 100),
      waste.cat2 = ifelse(whz < -2, 1, 0))
  })

  KR <- safe_mutate(KR, "undwgt", "hw71", function(df) {
    df %>% dplyr::mutate(
      waz    = dplyr::case_when(as.numeric(hw71) / 100 > 90 ~ NA, is.na(hw71) ~ NA, TRUE ~ as.numeric(hw71) / 100),
      undwgt = ifelse(waz < -2, 1, 0),
      ovrwgt = ifelse(waz > 2,  1, 0))
  })


  ######################################################################
  # 10 | MENSTRUAL HEALTH ----
  message("[11/14] Menstrual health variables...")

  IR <- safe_mutate(IR, "mens.nopriv", "v248", function(df) {
    df %>% dplyr::mutate(mens.nopriv = ifelse(v248 == "no", 1, 0))
  })

  mens_prod_cols <- intersect(c("v247a","v247b","v247c","v247d"), names(IR))
  if (length(mens_prod_cols) > 0) {
    IR <- IR %>% dplyr::mutate(
      mens.product  = dplyr::if_any(dplyr::all_of(mens_prod_cols), ~ . == "yes") * 1L,
      mens.noproduct = ifelse(mens.product == 0, 1, 0),
      mens.noeither  = ifelse(mens.noproduct == 1 | mens.nopriv == 1, 1, 0))
  } else {
    message("  Skipping menstrual product variables — v247* columns not in this survey.")
    IR$mens.product  <- NA
    IR$mens.noproduct <- NA
    IR$mens.noeither  <- NA
  }


  ######################################################################
  # 11 | CHILD HEALTH ----
  message("[12/14] Child health variables...")

  KR <- KR %>%
    dplyr::mutate(
      bf_curr = dplyr::case_when(
        m4 == "still breastfeeding" ~ 1,
        m4 %in% c("ever breastfed, not currently breastfeeding",
                  "never breastfed", "inconsistent", "don't know") ~ 0))

  v409_extra <- intersect(c("v409a","v410a","v413a","v413b","v413c","v413d"), names(KR))
  v414_solids <- intersect(
    paste0("v414", c("a","b","c","d","e","f","g","h","i","j","k","l","m","n","o","p","r","s","t","u","v","w")),
    names(KR))

  KR <- KR %>%
    dplyr::mutate(
      water   = dplyr::if_else(v409 == "yes", 1, 0),
      liquids = dplyr::if_else(
        v410 == "yes" | v412c == "yes" | v413 == "yes" |
          (length(v409_extra) > 0 & dplyr::if_any(dplyr::any_of(v409_extra), ~ . == "yes")), 1, 0),
      milk    = dplyr::if_else(v411 == "yes" | v411a == "yes", 1, 0),
      solids  = dplyr::if_else(
        length(v414_solids) > 0 &
          dplyr::if_any(dplyr::any_of(c(v414_solids, "m39a")), ~ . == "yes"), 1, 0),
      bf_status = dplyr::case_when(
        bf_curr == 0 ~ 0, solids == 1 ~ 5, milk == 1 ~ 4,
        liquids == 1 ~ 3, water == 1 ~ 2, TRUE ~ 1),
      no.ebf.n = dplyr::case_when(
        b19 < 6 & b9 == "respondent" & bf_status == 1 ~ 0,
        b19 < 6 & b9 == "respondent" & bf_status != 1 ~ 1))

  IR <- safe_mutate(IR, "nobareskin", "m77_1", function(df) {
    df %>% dplyr::mutate(
      nobareskin = ifelse(m77_1 == "put on chest, touching bare skin", 0, 1))
  })

  comp_cols <- intersect(
    c("v409","v410","v411","v412c","v413",
      paste0("v414", c("e","f","g","h","i","j","k","l","m","n","o","p","s","v"))),
    names(IR))
  if (length(comp_cols) > 0) {
    IR <- IR %>%
      dplyr::mutate(comp.feed = dplyr::if_else(
        dplyr::if_any(dplyr::any_of(comp_cols), ~ . == "yes"), 1, 0))
  } else {
    IR$comp.feed <- NA
  }

  IR <- IR %>%
    dplyr::mutate(
      breastfeed.last = dplyr::case_when(
        m4_1 == "never breastfed" ~ 0, is.na(m4_1) ~ NA, TRUE ~ 1))

  IR <- safe_mutate(IR, "breastfeed2.last", c("b19_01","b9_01"), function(df) {
    df %>% dplyr::mutate(
      breastfeed2.last = ifelse(b19_01 < 24 & b9_01 == "respondent", breastfeed.last, NA))
  })

  IR$breastfed2.noexcl <- ifelse(IR$breastfeed2.last == 1 & IR$comp.feed == 0, 0, 1)

  IR <- safe_mutate(IR, "no.imm.breastfeed.last.yn", "m34_1", function(df) {
    df %>% dplyr::mutate(no.imm.breastfeed.last.yn = dplyr::case_when(
      m34_1 %in% c("0", "100") ~ 0, is.na(m34_1) ~ NA_integer_, TRUE ~ 1))
  })

  IR <- safe_mutate(IR, "low.birthwgt", "m19_1", function(df) {
    df %>% dplyr::mutate(
      low.birthwgt = ifelse(m19_1 >= 2500 & m19_1 < 9995, 0, 1),
      low.birthwgt = ifelse(m19_1 %in% c(9998, 9996), NA, low.birthwgt))
  })

  IR <- safe_mutate(IR, "notx.worms.6m", "h43_1", function(df) {
    df %>% dplyr::mutate(notx.worms.6m = dplyr::case_when(
      h43_1 == "yes"       ~ 0,
      h43_1 == "no"        ~ 1,
      h43_1 == "don't know" ~ NA_real_,
      v137 == 0            ~ NA_real_))
  })

  IR <- safe_mutate(IR, "diarrhea.2wks.last", "h11_1", function(df) {
    df %>% dplyr::mutate(diarrhea.2wks.last = dplyr::case_when(
      h11_1 %in% c("yes, last 24 hours","yes, last two weeks") ~ 1,
      h11_1 == "no" ~ 0, h11_1 == "don't know" ~ NA, is.na(h11_1) ~ NA, v137 == 0 ~ NA))
  })

  KR <- KR %>% dplyr::mutate(diarrhea.2wks = dplyr::case_when(
    h11 %in% c("yes, last 24 hours","yes, last two weeks") ~ 1,
    h11 == "no" ~ 0, h11 == "don't know" ~ NA, is.na(h11) ~ NA, v137 == 0 ~ NA))

  IR <- safe_mutate(IR, "fever.2wks.last", "h22_1", function(df) {
    df %>% dplyr::mutate(fever.2wks.last = dplyr::case_when(
      h22_1 == "yes" ~ 1, h22_1 == "no" ~ 0,
      h22_1 == "don't know" ~ NA, is.na(h22_1) ~ NA, v137 == 0 ~ NA))
  })

  KR <- KR %>% dplyr::mutate(fever.2wks = dplyr::case_when(
    h22 == "yes" ~ 1, h22 == "no" ~ 0,
    h22 == "don't know" ~ NA, is.na(h22) ~ NA, v137 == 0 ~ NA))

  IR <- safe_mutate(IR, "cough.2wks.last", "h31_1", function(df) {
    df %>% dplyr::mutate(cough.2wks.last = dplyr::case_when(
      h31_1 %in% c("yes, last 24 hours","yes, last two weeks") ~ 1,
      h31_1 == "no" ~ 0, h31_1 == "don't know" ~ NA, is.na(h31_1) ~ NA, v137 == 0 ~ NA))
  })

  KR <- KR %>% dplyr::mutate(cough.2wks = dplyr::case_when(
    h31 %in% c("yes, last 24 hours","yes, last two weeks") ~ 1,
    h31 == "no" ~ 0, h31 == "don't know" ~ NA, is.na(h31) ~ NA, v137 == 0 ~ NA))

  IR <- IR %>% dplyr::mutate(
    fever.cough.2wks.last = ifelse(fever.2wks.last == 1 | cough.2wks.last == 1, 1, 0))
  KR <- KR %>% dplyr::mutate(
    fever.cough.2wks = ifelse(fever.2wks == 1 | cough.2wks == 1, 1, 0))

  IR <- safe_mutate(IR, "chest.prob.last", "h31c_1", function(df) {
    df %>% dplyr::mutate(chest.prob.last = dplyr::case_when(
      h31c_1 %in% c("both","chest only") ~ 1,
      h31c_1 %in% c("nose only","other") ~ 0,
      h31c_1 == "don't know" ~ NA, is.na(h31c_1) ~ NA, v137 == 0 ~ NA))
  })

  KR <- KR %>% dplyr::mutate(chest.prob = dplyr::case_when(
    h31c %in% c("both","chest only") ~ 1,
    h31c %in% c("nose only","other") ~ 0,
    h31c == "don't know" ~ NA, is.na(h31c) ~ NA, v137 == 0 ~ NA))

  IR <- safe_mutate(IR, "diff.breath.last", "h31b_1", function(df) {
    df %>% dplyr::mutate(diff.breath.last = dplyr::case_when(
      h31b_1 == "yes" ~ 1, h31b_1 == "no" ~ 0,
      h31b_1 == "don't know" ~ NA, is.na(h31b_1) ~ NA, v137 == 0 ~ NA))
  })

  KR <- KR %>% dplyr::mutate(diff.breath = dplyr::case_when(
    h31b == "yes" ~ 1, h31b == "no" ~ 0,
    h31b == "don't know" ~ NA, is.na(h31b) ~ NA, v137 == 0 ~ NA))

  IR <- IR %>% dplyr::mutate(
    ari.last = ifelse(chest.prob.last == 1 & diff.breath.last == 1, 1, 0))
  KR <- KR %>% dplyr::mutate(
    ari = ifelse(chest.prob == 1 & diff.breath == 1, 1, 0))

  h32_cols <- names(KR)[grepl("^h32", names(KR))]
  if (length(h32_cols) > 0) {
    kr_var <- KR %>%
      dplyr::select(survey, caseid, dplyr::all_of(h32_cols)) %>%
      reshape2::melt(id.vars = c("survey","caseid")) %>%
      dplyr::mutate(care = dplyr::case_when(
        variable %in% h32_cols & value == "yes" ~ 1,
        is.na(value) ~ NA_real_,
        TRUE ~ 0)) %>%
      dplyr::group_by(survey, caseid) %>%
      dplyr::mutate(
        care.cnt = ifelse(all(is.na(care)), NA, sum(care, na.rm = TRUE)),
        no.fever.cough.care.yn = ifelse(care.cnt == 0, 1, 0)) %>%
      dplyr::select(survey, caseid, care.cnt, no.fever.cough.care.yn) %>%
      dplyr::distinct()
  } else {
    message("  Skipping no.fever.cough.care.yn — h32* columns not in this survey.")
    kr_var <- KR %>%
      dplyr::select(survey, caseid) %>%
      dplyr::mutate(care.cnt = NA_real_, no.fever.cough.care.yn = NA_real_) %>%
      dplyr::distinct()
  }


  ######################################################################
  # 12 | MATERNAL HEALTH ----
  message("[13/14] Maternal health variables...")

  IR$bmi   <- as.numeric(IR$v445) / 100
  IR$bmi   <- ifelse(IR$bmi > 90, NA, IR$bmi)
  IR <- IR %>%
    dplyr::mutate(
      bmi.cat1 = dplyr::case_when(
        bmi < 16               ~ "Severely thin",
        bmi >= 16 & bmi < 17   ~ "Moderately thin",
        bmi >= 17 & bmi < 18.5 ~ "Mildly thin",
        bmi >= 18.5 & bmi < 25 ~ "Normal",
        bmi >= 25 & bmi < 30   ~ "Overweight",
        bmi >= 30              ~ "Obese"),
      overweight.obese = dplyr::if_else(bmi.cat1 %in% c("Overweight","Obese"), 1, 0))

  IR <- safe_mutate(IR, "preg.termin", "v228", function(df) {
    df %>% dplyr::mutate(preg.termin = dplyr::case_when(
      v228 == "yes" ~ 1, v228 == "no" ~ 0))
  })

  IR <- safe_mutate(IR, "sti.any", c("v763a","v763b","v763c"), function(df) {
    df %>% dplyr::mutate(
      sti.any = dplyr::case_when(
        v763a == "yes" | v763b == "yes" | v763c == "yes" ~ 1,
        v763a == "no"  & v763b == "no"  & v763c == "no"  ~ 0,
        is.na(v763a) ~ NA_integer_, is.na(v763b) ~ NA_integer_, is.na(v763c) ~ NA_integer_),
      sti.yn = dplyr::case_when(
        v763a == "yes" | v763b == "yes" | v763c == "yes" |
          ("v763d" %in% names(df) & v763d == "yes")                           ~ 1,
        is.na(v763a) & is.na(v763b) & is.na(v763c)                           ~ NA_integer_,
        TRUE ~ 0))
  })

  IR <- safe_mutate(IR, "hiv.never.tested", "v781", function(df) {
    df %>% dplyr::mutate(hiv.never.tested = dplyr::case_when(
      v781 == "no"  ~ 1, v781 == "yes" ~ 0, is.na(v781) ~ NA_integer_))
  })

  m3_cols <- paste0("m3", c("a","b","c","d","e","f","g","h","i","j","k","l","m","n"))
  present_m3 <- intersect(m3_cols, names(BR))
  if (length(present_m3) > 0) {
    BR <- BR %>%
      dplyr::mutate(dplyr::across(dplyr::any_of(m3_cols), ~ ifelse(.x == "yes", 1,
                                                                   ifelse(.x == "no", 0, NA_integer_)))) %>%
      dplyr::mutate(
        who.assisted.del = dplyr::case_when(
          m3a == 1 ~ "Doctor",
          m3b == 1 ~ "Nurse/midwife",
          "m3c" %in% present_m3 & m3c == 1 ~ "Auxilary midwife",
          ("m3d" %in% present_m3 & m3d == 1) | ("m3e" %in% present_m3 & m3e == 1) |
            ("m3f" %in% present_m3 & m3f == 1) ~ "Community health extension worker",
          "m3g" %in% present_m3 & m3g == 1 ~ "Traditional birth attendant",
          ("m3h" %in% present_m3 & m3h == 1) | ("m3i" %in% present_m3 & m3i == 1) |
            ("m3j" %in% present_m3 & m3j == 1) | ("m3k" %in% present_m3 & m3k == 1) |
            ("m3l" %in% present_m3 & m3l == 1) | ("m3m" %in% present_m3 & m3m == 1) ~ "Relative/other",
          "m3n" %in% present_m3 & m3n == 1 ~ "No one",
          is.na(m3a) ~ "NA - last birth >5 yrs",
          TRUE ~ NA_character_),
        no_skilled_assist_bin = dplyr::case_when(
          who.assisted.del %in% c("Auxilary midwife","Nurse/midwife","Doctor",
                                  "Community health extension worker") ~ 0,
          is.na(who.assisted.del) ~ NA_real_,
          TRUE ~ 1))
  } else {
    message("  Skipping delivery assistance variables — m3* columns not in this survey.")
    BR$who.assisted.del   <- NA
    BR$no_skilled_assist_bin <- NA
  }

  BR <- safe_mutate(BR, "no.prenatal.tetnus", "m1", function(df) {
    df %>% dplyr::mutate(no.prenatal.tetnus = dplyr::case_when(
      m1 == 0           ~ 1, m1 %in% c(1:8) ~ 0))
  })

  IR <- safe_mutate(IR, "unmet.fp.need", "v626a", function(df) {
    df %>% dplyr::mutate(
      unmet.fp.need = dplyr::case_when(
        v626a == "unmet need for limiting"                                     ~ "Unmet need for limiting",
        v626a == "unmet need for spacing"                                      ~ "Unmet need for spacing",
        v626a == "no unmet need"                                               ~ "No unmet need",
        v626a %in% c("using for limiting","using for spacing")                 ~ "Using contraception",
        v626a %in% c("never had sex","not married and no sex in last 30 days",
                     "infecund, menopausal")                                   ~ "NA (infecund, menopausal, no sex)"),
      unmet.fp.need_yn = dplyr::case_when(
        v626a %in% c("unmet need for limiting","unmet need for spacing")       ~ 1,
        v626a %in% c("no unmet need","using for limiting","using for spacing")  ~ 0,
        v626a %in% c("never had sex","not married and no sex in last 30 days",
                     "infecund, menopausal")                                   ~ NA))
  })


  ######################################################################
  # 13 | SUMMARIZE ----
  message("[14/14] Summarizing and merging outputs...")

  ir.out <- IR

  br.out <- BR %>%
    dplyr::group_by(survey, caseid) %>%
    dplyr::summarize(
      b.rost               = dplyr::n(),
      u1mort.cnt           = ifelse(all(is.na(u1mort)),           NA, sum(u1mort,           na.rm = TRUE)),
      u5mort.cnt           = ifelse(all(is.na(u5mort)),           NA, sum(u5mort,           na.rm = TRUE)),
      unassisted.del.cnt   = ifelse(all(is.na(unassisted.del)),   NA, sum(unassisted.del,   na.rm = TRUE)),
      no.prenatal.tetnus.cnt = ifelse(all(is.na(no.prenatal.tetnus)), NA, sum(no.prenatal.tetnus, na.rm = TRUE)),
      micronutrient.12m.cnt = ifelse(all(is.na(micronutrient.12m)), NA, sum(micronutrient.12m, na.rm = TRUE)),
      home.birth.cnt       = ifelse(all(is.na(home.birth)),       NA, sum(home.birth,       na.rm = TRUE)),
      no_skilled_assist.cnt = ifelse(all(is.na(no_skilled_assist_bin)), NA, sum(no_skilled_assist_bin, na.rm = TRUE)),
      .groups = "drop") %>%
    dplyr::mutate(
      u1mort.yn             = ifelse(u1mort.cnt > 0,           1, 0),
      u5mort.yn             = ifelse(u5mort.cnt > 0,           1, 0),
      unassisted.del.yn     = ifelse(unassisted.del.cnt > 0,   1, 0),
      no.prenatal.tetnus.yn = ifelse(no.prenatal.tetnus.cnt > 0, 1, 0),
      micronutrient.12m.yn  = ifelse(micronutrient.12m.cnt > 0, 1, 0),
      no_skilled_assist.yn  = ifelse(no_skilled_assist.cnt > 0, 1, 0),
      home.birth.yn         = ifelse(home.birth.cnt > 0,       1, 0)) %>%
    dplyr::select(survey, caseid, no_skilled_assist.yn, unassisted.del.yn, unassisted.del.cnt,
                  no.prenatal.tetnus.yn, no.prenatal.tetnus.cnt, u1mort.cnt, u5mort.cnt,
                  home.birth.cnt, u1mort.yn, u5mort.yn, home.birth.yn, micronutrient.12m.yn)

  safe_sum <- function(x) ifelse(all(is.na(x)), NA, sum(x, na.rm = TRUE))

  kr.out <- KR %>%
    dplyr::group_by(survey, caseid) %>%
    dplyr::summarize(
      k.rost                       = dplyr::n(),
      no.breastfeed.cnt            = safe_sum(no.breastfeed.n),
      no.breastfeed2.cnt           = safe_sum(no.breastfeed2.n),
      ovrwgt.cnt                   = safe_sum(ovrwgt),
      undwgt.cnt                   = safe_sum(undwgt),
      stunt.cat2.cnt               = safe_sum(stunt.cat2),
      waste.cat2.cnt               = safe_sum(waste.cat2),
      meas.full.cnt                = safe_sum(measles.full),
      polio.full.cnt               = safe_sum(polio.full),
      dpt.full.cnt                 = safe_sum(dpt.full),
      zerodose.cnt                 = safe_sum(zero.dose),
      diarrhea.cnt                 = safe_sum(diarrhea.2wks),
      fever.cnt                    = safe_sum(fever.2wks),
      cough.cnt                    = safe_sum(cough.2wks),
      chest.prob.cnt               = safe_sum(chest.prob),
      diff.breath.cnt              = safe_sum(diff.breath),
      zero.dose.operational.cnt    = safe_sum(zero.dose.operational),
      zero.dose.4vax.12m.cnt       = safe_sum(zero.dose.4vax.12m),
      not.full.vax.schedule.24m.cnt = safe_sum(not.full.vax.schedule.24m),
      not.full.vax.schedule.12m.cnt = safe_sum(not.full.vax.schedule.12m),
      full.vax.schedule.24m.cnt    = safe_sum(full.vax.schedule.24m),
      full.vax.schedule.12m.cnt    = safe_sum(full.vax.schedule.12m),
      measles.all.cnt              = safe_sum(measles.all),
      measles.none.cnt             = safe_sum(measles.none),
      polio.all.cnt                = safe_sum(polio.all),
      polio.none.cnt               = safe_sum(polio.none),
      dpt.all.cnt                  = safe_sum(dpt.all),
      dpt.none.cnt                 = safe_sum(dpt.none),
      bcg.all.cnt                  = safe_sum(bcg.all),
      bcg.none.cnt                 = safe_sum(bcg.none),
      hepb.all.cnt                 = safe_sum(hepb.all),
      hepb.none.cnt                = safe_sum(hepb.none),
      pneumo.all.cnt               = safe_sum(pneumo.all),
      pneumo.none.cnt              = safe_sum(pneumo.none),
      rota.all.cnt                 = safe_sum(rota.all),
      rota.none.cnt                = safe_sum(rota.none),
      yellowfever.all.cnt          = safe_sum(yellowfever.all),
      yellowfever.none.cnt         = safe_sum(yellowfever.none),
      meningitis.all.cnt           = safe_sum(meningitis.all),
      meningitis.none.cnt          = safe_sum(meningitis.none),
      pentavalent.all.cnt          = safe_sum(pentavalent.all),
      pentavalent.none.cnt         = safe_sum(pentavalent.none),
      pos.basic.antigen.full.12m.cnt = safe_sum(basic.antigen.full.12m),
      not.basic.antigen.full.12m.cnt = safe_sum(not.basic.antigen.full.12m),
      no.ebf.cnt                   = safe_sum(no.ebf.n),
      ari.cnt                      = safe_sum(ari),
      no.mad.cnt                   = safe_sum(no.mad),
      .groups = "drop") %>%
    dplyr::mutate(
      no.breastfeed.yn               = ifelse(no.breastfeed.cnt > 0,            1, 0),
      no.breastfeed2.yn              = ifelse(no.breastfeed2.cnt > 0,           1, 0),
      ovrwgt.yn                      = ifelse(ovrwgt.cnt > 0,                   1, 0),
      undwgt.yn                      = ifelse(undwgt.cnt > 0,                   1, 0),
      stunt.cat2.yn                  = ifelse(stunt.cat2.cnt > 0,               1, 0),
      waste.cat2.yn                  = ifelse(waste.cat2.cnt > 0,               1, 0),
      meas.full.yn                   = ifelse(meas.full.cnt > 0,                1, 0),
      polio.full.yn                  = ifelse(polio.full.cnt > 0,               1, 0),
      dpt.full.yn                    = ifelse(dpt.full.cnt > 0,                 1, 0),
      zerodose.yn                    = ifelse(zerodose.cnt > 0,                 1, 0),
      fever.yn                       = ifelse(fever.cnt > 0,                    1, 0),
      cough.yn                       = ifelse(cough.cnt > 0,                    1, 0),
      diff.breath.yn                 = ifelse(diff.breath.cnt > 0,              1, 0),
      chest.prob.yn                  = ifelse(chest.prob.cnt > 0,               1, 0),
      zero.dose.operational.yn       = ifelse(zero.dose.operational.cnt > 0,    1, 0),
      zero.dose.4vax.12m.yn          = ifelse(zero.dose.4vax.12m.cnt > 0,       1, 0),
      not.full.vax.schedule.24m.yn   = ifelse(not.full.vax.schedule.24m.cnt > 0,1, 0),
      not.full.vax.schedule.12m.yn   = ifelse(not.full.vax.schedule.12m.cnt > 0,1, 0),
      pos.full.vax.schedule.24m.yn   = ifelse(full.vax.schedule.24m.cnt > 0,    1, 0),
      pos.full.vax.schedule.12m.yn   = ifelse(full.vax.schedule.12m.cnt > 0,    1, 0),
      pos.measles.all.yn             = ifelse(measles.all.cnt > 0,              1, 0),
      measles.none.yn                = ifelse(measles.none.cnt > 0,             1, 0),
      pos.polio.all.yn               = ifelse(polio.all.cnt > 0,                1, 0),
      polio.none.yn                  = ifelse(polio.none.cnt > 0,               1, 0),
      pos.dpt.all.yn                 = ifelse(dpt.all.cnt > 0,                  1, 0),
      dpt.none.yn                    = ifelse(dpt.none.cnt > 0,                 1, 0),
      pos.bcg.all.yn                 = ifelse(bcg.all.cnt > 0,                  1, 0),
      bcg.none.yn                    = ifelse(bcg.none.cnt > 0,                 1, 0),
      pos.hepb.all.yn                = ifelse(hepb.all.cnt > 0,                 1, 0),
      hepb.none.yn                   = ifelse(hepb.none.cnt > 0,                1, 0),
      pos.pneumo.all.yn              = ifelse(pneumo.all.cnt > 0,               1, 0),
      pneumo.none.yn                 = ifelse(pneumo.none.cnt > 0,              1, 0),
      pos.rota.all.yn                = ifelse(rota.all.cnt > 0,                 1, 0),
      rota.none.yn                   = ifelse(rota.none.cnt > 0,                1, 0),
      pos.yellowfever.all.yn         = ifelse(yellowfever.all.cnt > 0,          1, 0),
      yellowfever.none.yn            = ifelse(yellowfever.none.cnt > 0,         1, 0),
      pos.meningitis.all.yn          = ifelse(meningitis.all.cnt > 0,           1, 0),
      meningitis.none.yn             = ifelse(meningitis.none.cnt > 0,          1, 0),
      pos.pentavalent.all.yn         = ifelse(pentavalent.all.cnt > 0,          1, 0),
      pentavalent.none.yn            = ifelse(pentavalent.none.cnt > 0,         1, 0),
      pos.basic.antigen.full.12m.yn  = ifelse(pos.basic.antigen.full.12m.cnt > 0,1, 0),
      basic.antigen.full.none.12m.yn = ifelse(not.basic.antigen.full.12m.cnt > 0,1, 0),
      no.ebf.yn                      = ifelse(no.ebf.cnt > 0,                   1, 0),
      ari.yn                         = ifelse(ari.cnt > 0,                      1, 0),
      no.mad.yn                      = ifelse(no.mad.cnt > 0,                   1, 0)) %>%
    dplyr::select(
      survey, caseid,
      no.breastfeed.cnt, no.breastfeed2.cnt, ovrwgt.cnt, undwgt.cnt,
      stunt.cat2.cnt, waste.cat2.cnt, meas.full.cnt, dpt.full.cnt, polio.full.cnt,
      zerodose.cnt, diarrhea.cnt, fever.cnt, cough.cnt, diff.breath.cnt,
      chest.prob.cnt, ari.cnt, no.mad.cnt,
      no.breastfeed.yn, no.breastfeed2.yn, ovrwgt.yn, undwgt.yn,
      stunt.cat2.yn, waste.cat2.yn, meas.full.yn, dpt.full.yn, polio.full.yn,
      zerodose.yn, fever.yn, cough.yn, diff.breath.yn, chest.prob.yn,
      zero.dose.operational.yn, zero.dose.4vax.12m.yn,
      not.full.vax.schedule.24m.yn, not.full.vax.schedule.12m.yn,
      pos.full.vax.schedule.24m.yn, pos.full.vax.schedule.12m.yn,
      pos.measles.all.yn, pos.polio.all.yn, pos.dpt.all.yn,
      pos.bcg.all.yn, pos.hepb.all.yn, pos.pneumo.all.yn, pos.rota.all.yn,
      pos.yellowfever.all.yn, pos.meningitis.all.yn,
      no.mad.yn, no.ebf.yn, measles.none.yn, polio.none.yn, dpt.none.yn,
      bcg.none.yn, hepb.none.yn, pneumo.none.yn, rota.none.yn,
      yellowfever.none.yn, meningitis.none.yn,
      pos.basic.antigen.full.12m.yn, basic.antigen.full.none.12m.yn,
      pentavalent.none.yn, pos.pentavalent.all.yn,
      ari.yn, bcg.all.cnt, zero.dose.4vax.12m.cnt, no.ebf.cnt) %>%
    base::merge(kr_var, by = c("survey","caseid"), all.x = TRUE)

  outcomes <- ir.out %>%
    base::merge(kr.out,   by = c("survey","caseid"), all.x = TRUE) %>%
    base::merge(br.out,   by = c("survey","caseid"), all.x = TRUE) %>%
    base::merge(stl.out,  by = "caseid",             all.x = TRUE)

  drop_vars <- c("anc.1st.mean", "anc.mean")
  outcomes  <- outcomes %>% dplyr::select(-dplyr::any_of(drop_vars))

  rm(IR, KR, BR)
  gc()

  t_end <- proc.time()
  message("Outcome variable generation complete in ",
          round((t_end - t_start)[["elapsed"]] / 60, 1), " minutes.")

  return(outcomes)

}
