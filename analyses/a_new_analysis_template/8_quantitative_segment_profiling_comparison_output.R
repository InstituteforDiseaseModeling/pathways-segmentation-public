

################################################################################
# VULNERABILITY RESPONSE COMPARISON
################################################################################


###################################
# RUN SETUP
source("1_setup.R")

nreps = 100


###################################
# SET PARAMETERS
params_sheet <- readRDS(params_excel_file)

final_models <- params_sheet %>%
  dplyr::select(strata, final_model) %>%
  dplyr::filter(!is.na(strata))

strata_set <- unique(final_models$strata)


###################################
#


for (stratum in strata_set){
  print(stratum)


  # SET SELECTED MODEL TO PROFILE
  n_class <- final_models %>%
    dplyr::filter(strata == stratum) %>%
    pull(final_model)
  print(n_class)

  # GET COMBINED DATASET WITH MODELED SEGMENTS
  path = paste0(lca_path, "nreps", nreps, "/", stratum, "_outcomes_vulnerability_class_ranked.rds")
  df <- readRDS(path)

  df$segment <- df %>% pull(eval(parse(text=n_class)))
  df$model_cat <- n_class
  # df$segment = paste0("segment_", df$segment)
  df$segment = paste0("segment_", df$segment_rank)


  # GET DOMAINS
  domain_set <- params_sheet %>%
    dplyr::select(domains) %>%
    dplyr::filter(!is.na(domains)) %>%
    distinct() %>%
    dplyr::mutate(domains = str_replace_all(domains, " ", "."))


  # GET VULNERABILITY LIST
  vulnerability_vars <- readRDS(vulnerability_excel_file)

  vulnerability_vars_profile <- vulnerability_vars %>%
    dplyr::filter(profile_include == 1) %>%
    dplyr::filter(profile_strata %in% c("both", "all", stratum)) %>%
    dplyr::select(vulnerability_variable, short_name, profile_strata, domain_set$domains) %>%
    reshape2::melt(id.vars=c("vulnerability_variable", "short_name", "profile_strata"), variable.name="domain", value.name="domain_include") %>%
    dplyr::filter(domain_include == 1) %>%
    dplyr::select(vulnerability_variable, short_name, domain) %>%
    setNames(c("variable", "short_name", "domain")) %>%
    distinct()

  vulnerability_names <- names(df)[(names(df) %in% c("caseid", "wt", "survey", "strata", "model_cat", "segment", vulnerability_vars_profile$variable))]
  df_vulnerability <- subset(df, select=c(vulnerability_names))


  # VULNERABILITY RESPONSE PROPORTIONS
  df_vulnerability <- df_vulnerability %>%
    reshape2::melt(id.vars=c("caseid", "wt", "survey", "strata", "model_cat", "segment"), variable.name = "variable", value.name = "value") %>%
    dplyr::mutate(value = ifelse(is.na(value), "*NA", value)) %>%
    group_by(segment, variable) %>%
    dplyr::mutate(total = sum(wt)) %>%
    group_by(segment, variable, value) %>%
    dplyr::mutate(count = sum(wt),
                  prop=round(count/total, 3))


  # VULNERABILITY RESPONSE PROPORTIONS + DOMAINS
  df_vulnerability <- df_vulnerability %>%
    base::merge(vulnerability_vars_profile, by=c("variable")) %>%
    dplyr::select(survey, strata, model_cat, domain, variable, short_name, value, prop, segment) %>%
    distinct() %>%
    reshape2::dcast(survey + strata + model_cat + domain + variable + short_name + value ~ segment, value.var = "prop")

  assign(paste0("df_vulnerability_", stratum), df_vulnerability)

  write.csv(base::get(paste0("df_vulnerability_", stratum)), file = paste0(root_path, user_path, "df_vulnerability_", stratum, ".csv"), row.names = FALSE)


  # GET OUTCOMES LIST
  outcome_vars <- readRDS(outcomes_excel_file)

  outcome_vars_profile <- outcome_vars %>%
    dplyr::filter(profile_include == 1) %>%
    dplyr::select(outcome_variable, short_name, category) %>%
    setNames(c("variable", "short_name", "category")) %>%
    distinct()

  outcome_names <- names(df)[(names(df) %in% c("caseid", "wt", "survey", "strata", "model_cat", "segment", outcome_vars_profile$variable))]
  df_outcomes <- subset(df, select=c(outcome_names))

  # OUTCOMES RESPONSE PROPORTIONS
  df_outcomes <- df_outcomes %>%
    reshape2::melt(id.vars=c("caseid", "wt", "survey", "strata", "model_cat", "segment"), variable.name = "variable", value.name = "value") %>%
    dplyr::filter(!is.na(value)) %>%
    group_by(segment, variable) %>%
    dplyr::mutate(total = sum(wt)) %>%
    group_by(segment, variable, value) %>%
    dplyr::mutate(count = sum(wt),
                  prop=round(count/total, 3))

  df_outcomes <- df_outcomes %>%
    base::merge(outcome_vars_profile, by=c("variable")) %>%
    dplyr::select(survey, strata, model_cat, variable, short_name, value, prop, segment) %>%
    distinct() %>%
    reshape2::dcast(survey + strata + model_cat + variable + short_name + value ~ segment, value.var = "prop") %>%
    dplyr::filter(value == 1)

  assign(paste0("df_outcomes_", stratum), df_outcomes)

  write.csv(base::get(paste0("df_outcomes_", stratum)), file = paste0(root_path, user_path, "df_outcomes_", stratum, ".csv"), row.names = FALSE)


}
