
outcomes <- readRDS(file = paste0(outcomes_file, ".rds"))
vulnerability <- readRDS(file = paste0(vulnerability_file, ".rds"))

vulnerability_vars <- readRDS(vulnerability_excel_file) %>%
  dplyr::filter(lca_include == 1) %>%
  dplyr::select(vulnerability_variable, lca_strata, lca_include)


outcomes_na <- outcomes %>%
  reshape2::melt(id.vars=c("caseid", "wt")) %>%
  dplyr::filter(!variable %in% c("survey", "v001", "v002", "v003", "str", "STL", "LB")) %>%
  dplyr::mutate(is_missing = ifelse(is.na(value), 1, 0)) %>%
  group_by(variable) %>%
  dplyr::summarize(prop_missing = sum(is_missing)/n(),
                   prop_missing_wt = sum(wt[is_missing == 1])/sum(wt))

hist(outcomes_na$prop_missing)


vulnerability_na <- vulnerability %>%
  reshape2::melt(id.vars=c("caseid")) %>%
  dplyr::filter(!variable %in% c("survey", "v021", "v023")) %>%
  dplyr::mutate(is_missing = ifelse(is.na(value), 1, 0)) %>%
  group_by(variable) %>%
  dplyr::summarize(prop_missing = sum(is_missing)/n())

hist(vulnerability_na$prop_missing)


vulnerability_na_1 <- vulnerability_na %>%
  base::merge(vulnerability_vars, by.x=c("variable"), by.y=c("vulnerability_variable"))


#################################

#
# str(IR)
#
# # START WITH 30078
# # FILTER TO PATHWAYS SAMPLE SIZE 17617
# # FILTER TO DECISION MAKING FOR FAMILY PLANNING
#
# table(IR$v632, useNA = "always")
#
# #################################
#
# table(IR$m14_1, useNA = "always")
