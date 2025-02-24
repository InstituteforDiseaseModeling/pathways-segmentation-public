


################################################################################
# LATENT CLASSIFICATION ANALYSIS (LCA) OUTPUT
################################################################################


###################################
# RUN SETUP
source("1_setup.R")

nreps = 30


###################################
# READ PATHWAYS WORKBOOOK
###################################


params_sheet <- readRDS(params_excel_file)


strata_set <- params_sheet %>%
  dplyr::select(strata) %>%
  dplyr::filter(!is.na(strata)) %>%
  distinct()


vulnerability_vars <- readRDS(vulnerability_excel_file)

vulnerability_vars_lca <- vulnerability_vars %>%
  dplyr::filter(lca_include == 1)


outcomes <- readRDS(file = paste0(outcomes_file, ".rds"))
vulnerability <- readRDS(file = paste0(vulnerability_file, ".rds"))
outcomes_vulnerability <- readRDS(file = paste0(outcomes_vulnerability_file, ".rds"))


##################################
# ADJUST ALL NUMERIC VARIABLES BY ADDING 1 (LCA DOES NOT ALLOW 0 VALUES)


# RECODE VARIABLES AS NUMERIC AND > 0
vulnerability_adj <- vulnerability %>%
  dplyr::mutate_if(is.numeric, ~ .+1) %>%
  data.frame() %>%
  dplyr::mutate(across(-c(caseid, all_of(svy_id_var), all_of(svy_strata_var), wt, survey, strata), ~ as.integer(factor(.)))) %>%
  dplyr::select(caseid, all_of(svy_id_var), all_of(svy_strata_var), wt, survey, strata, everything())


###################################
# LOOP OVER STRATA AND GENERATE LCA OUTPUT


for (stratum in strata_set$strata){
  print(stratum)
  set.seed(99)

  lca_strata_input <- vulnerability_vars_lca %>%
    dplyr::filter(lca_strata %in% c("both", "all", stratum)) %>%
    dplyr::select(vulnerability_variable) %>%
    setNames(c("variable"))

  vulnerability_adj_input <- vulnerability_adj %>%
    dplyr::filter(strata == stratum)

  gen_lca_output(df=vulnerability_adj_input, input_vars=lca_strata_input, nreps=nreps, output_path=lca_path)

}


###################################
#
for (stratum in strata_set$strata){

  path = paste0(lca_path, stratum, "_lca_input.rds")
  lca_input <- readRDS(path)

  for (i in 2:10){

    name <- paste0("LCA", i)
    path = paste0(lca_path, stratum, "_", name, ".rds")
    data <- readRDS(path)
    assign(name, data)

  }

  ###################################
  # SAVE OUTPUT OF VULNERABILITY VARIABLES, OUTCOMES, AND CLASSES
  lca_input_class <- cbind(lca_input, LCA2_class=LCA2$predclass) %>%
    cbind(LCA3_class=LCA3$predclass) %>%
    cbind(LCA4_class=LCA4$predclass) %>%
    cbind(LCA5_class=LCA5$predclass) %>%
    cbind(LCA6_class=LCA6$predclass) %>%
    cbind(LCA7_class=LCA7$predclass) %>%
    cbind(LCA8_class=LCA8$predclass) %>%
    cbind(LCA9_class=LCA9$predclass) %>%
    cbind(LCA10_class=LCA10$predclass)

  outcomes_vulnerability_class <- outcomes_vulnerability %>%
    base::merge(lca_input_class %>% dplyr::select(caseid, survey, strata, LCA2_class, LCA3_class, LCA4_class, LCA5_class, LCA6_class, LCA7_class, LCA8_class, LCA9_class, LCA10_class),
                by=c("caseid", "survey", "strata"))

  path = paste0(lca_path, stratum, "_outcomes_vulnerability_class")
  dir.create(dirname(path), showWarnings = F, recursive = T)
  saveRDS(outcomes_vulnerability_class, file = paste0(path, ".rds"))
  write.csv(outcomes_vulnerability_class, file = paste0(path, ".csv"), row.names = FALSE)

}


###################################
# LOOP OVER STRATA AND GENERATE LCA OUTPUT VISUALS
for (stratum in strata_set$strata){
  print(stratum)
  set.seed(99)

  gen_lca_output_viz(stratum=stratum, data_path=lca_path, plot_path=lca_plots)

}


###################################
# LOOP OVER STRATA AND GENERATE LCA EXPLORATORY PLOTS BASED ON NUMBER OF SEGMENTS
for (stratum in strata_set$strata){


  lca_strata_input <- vulnerability_vars_lca %>%
    dplyr::filter(lca_strata %in% c("both", "all", stratum)) %>%
    dplyr::select(vulnerability_variable, short_name) %>%
    distinct()



  #
  for (i in seq(2,10)){
    print(paste(stratum, i))

    gen_lca_explore(lca_vars=lca_strata_input, stratum=stratum, n_clusters=i, data_path=lca_path, plot_path=lca_plots)

  }
}















