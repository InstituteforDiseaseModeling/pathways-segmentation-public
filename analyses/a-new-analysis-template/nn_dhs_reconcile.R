

###################################
# LET'S READ IN THE HVS OUTCOMES TO SET THE VALUE FOR PROPORTIONS
urban_hvs <- readRDS(file = "C:/Users/jeremyco/OneDrive - Bill & Melinda Gates Foundation/Documents/Projects/pathways-hv-score/hvs_input.rds")

hvs_outcomes <- urban_hvs %>%
  dplyr::filter(survey == "NG7BFL") %>%
  dplyr::filter(strata == "urban") %>%
  dplyr::select(caseid, wt, final_segment, anc_less4_last, home_birth_last, nofp_mod_ever, u5mort_yn, waste_cat2_yn) %>%
  reshape2::melt(id.vars=c("caseid", "wt", "final_segment")) %>%
  dplyr::filter(!is.na(value)) %>%
  group_by(variable, final_segment) %>%
  dplyr::summarize(numerator = sum(wt[value == 1]),
                   denominator = sum(wt),
                   prop = numerator/denominator)

hvs_outcomes_conformed <- hvs_outcomes %>%
  dplyr::mutate(variable_conformed = case_when(variable == "anc_less4_last" ~ "anc.less4",
                                               variable == "home_birth_last" ~ "hb.1",
                                               variable == "nofp_mod_ever" ~ "nofp.mod.ever",
                                               variable == "u5mort_yn" ~ "u5mort.yn",
                                               variable == "waste_cat2_yn" ~ "waste.cat2.yn"))


hvs_prop <- urban_hvs %>%
  dplyr::filter(survey == "NG7BFL") %>%
  dplyr::filter(strata == "urban") %>%
  dplyr::select(caseid, wt, final_segment, final_segment_ranked) %>%
  group_by() %>%
  dplyr::mutate(denominator = sum(wt)) %>%
  group_by(final_segment, final_segment_ranked, denominator) %>%
  dplyr::mutate(numerator = sum(wt),
                prop = numerator/denominator) %>%
  dplyr::select(final_segment, final_segment_ranked, numerator, denominator, prop) %>%
  distinct()


###################################
# LET'S READ IN THE FINAL OUTPUT FROM VALERIE TO MAKE SURE THE PROPORTIONS ALIGN WITH THE HVS - YES
urban <- readRDS(paste0(lca_path, "urban_outcomes_vulnerability_class.rds"))

urban_outcomes <- urban %>%
  dplyr::select(caseid, wt, LCA6_class, anc.less4, hb.1, nofp.mod.ever, u5mort.yn, waste.cat2.yn) %>%
  reshape2::melt(id.vars=c("caseid", "wt", "LCA6_class")) %>%
  dplyr::filter(!is.na(value)) %>%
  group_by(variable, LCA6_class) %>%
  dplyr::summarize(numerator = sum(wt[value == 1]),
                   denominator = sum(wt),
                   prop = numerator/denominator)


urban_prop <- urban %>%
  group_by() %>%
  dplyr::mutate(denominator = sum(wt)) %>%
  group_by(LCA6_class, denominator) %>%
  dplyr::mutate(numerator = sum(wt),
                prop = numerator/denominator) %>%
  dplyr::select(LCA6_class, numerator, denominator, prop) %>%
  distinct()


###################################
# LET'S CHECK THE FILE FROM OPENHEXA / SONDER TO MAKE SURE THE SEGMENTS AND PROPORTIONS ARE CORRECT
urban_sonder <- fread(file = paste0(lca_path, "North_Nigeria_2018DHS7_1.0.csv"))

urban_outcomes_sonder <- urban_sonder %>%
  dplyr::filter(strata == "urban") %>%
  dplyr::select(caseid, wt, LCA6_class, segment_name, anc.less4, hb.1, nofp.mod.ever, u5mort.yn, waste.cat2.yn) %>%
  reshape2::melt(id.vars=c("caseid", "wt", "LCA6_class", "segment_name")) %>%
  dplyr::filter(!is.na(value)) %>%
  group_by(variable, LCA6_class, segment_name) %>%
  dplyr::summarize(numerator = sum(wt[value == 1]),
                   denominator = sum(wt),
                   prop = numerator/denominator)

urban_prop_sonder <- urban_sonder %>%
  dplyr::filter(strata == "urban") %>%
  group_by() %>%
  dplyr::mutate(denominator = sum(wt)) %>%
  group_by(LCA6_class, denominator) %>%
  dplyr::mutate(numerator = sum(wt),
                prop = numerator/denominator) %>%
  dplyr::select(LCA6_class, numerator, denominator, prop) %>%
  distinct()


###################################
# OK WE ARE ALIGNED ACROSS ALL 3 FILES FOR PROPORTIONS BY SEGMENTS, NOW LET'S CHECK PROPORTIONS BY INDICATOR

hvs_merge <- hvs_outcomes_conformed %>%
  ungroup() %>%
  dplyr::select(final_segment, variable_conformed, prop) %>%
  setNames(c("segment", "variable", "prop_hvs"))

urban_merge <- urban_outcomes %>%
  dplyr::select(LCA6_class, variable, prop) %>%
  setNames(c("segment", "variable", "prop_urban"))

sonder_merge <- urban_outcomes_sonder %>%
  dplyr::select(LCA6_class, segment_name, variable, prop) %>%
  setNames(c("segment", "sonder_segment_ranked", "variable", "prop_sonder"))

merged <- hvs_merge %>%
  base::merge(urban_merge, by=c("segment", "variable")) %>%
  base::merge(sonder_merge, by=c("segment", "variable"))
