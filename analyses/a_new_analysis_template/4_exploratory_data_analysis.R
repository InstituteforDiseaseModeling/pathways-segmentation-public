


################################################################################
# EXPLORATORY DATA ANALYSIS
################################################################################


# rm(list=ls())
###################################
# RUN SETUP
source("1_setup.R")

# DEFINE WHETHER TO RUN THE EDA IN PARALLEL
run_in_parallel = TRUE


###################################
# READ PATHWAYS WORKBOOOK SHEETS
###################################


outcome_vars <- readRDS(outcomes_excel_file)
vulnerability_vars <- readRDS(vulnerability_excel_file)

outcomes_vars_eda <- outcome_vars %>%
  dplyr::filter(eda_include == 1) %>%
  dplyr::select(outcome_variable)

if (nrow(outcomes_vars_eda) == 0){
  stop("No outcome variables selected for EDA. Select variables in the Pathways Workbook.")
}


vulnerability_vars_eda <- vulnerability_vars %>%
  dplyr::filter(eda_include == 1) %>%
  dplyr::select(vulnerability_variable)

if (nrow(vulnerability_vars_eda) == 0){
  stop("No vulnerability variables selected for EDA. Select variables in the Pathways Workbook.")
}


strata <- readRDS(params_excel_file) %>%
  dplyr::select(strata) %>%
  dplyr::filter(!is.na(strata)) %>%
  pull()


###################################
# GET OUTCOMES AND VULNERABILITY FACTORS
outcomes <- readRDS(file = paste0(outcomes_file, ".rds"))
vulnerability <- readRDS(file = paste0(vulnerability_file, ".rds"))
outcomes_vulnerability <- readRDS(file = paste0(outcomes_vulnerability_file, ".rds"))


###################################
# DEFINE LIST OF OUTCOMES AND VULNERABILITY FACTORS FOR INITIAL BIVARIATE REGRESSOIN ANALYSIS

# OUTCOMES
outcomes.list <- names(outcomes)[names(outcomes) %in% outcomes_vars_eda$outcome_variable]

# VULNERABILITY FACTORS
vulnerability.list <- names(vulnerability)[names(vulnerability) %in% vulnerability_vars_eda$vulnerability_variable]
vulnerability.list <- sort(vulnerability.list)


###################################
# LOOP THROUGH LIST OF VULNERABILITY VARIABLES AND GENERATE PDF OUTPUTS


if (run_in_parallel == TRUE){
  print("Running EDA in parallel")


  # IDENTIFY NUMBER OF CORES AND SPIN UP LOCAL CLUSTER
  ncores <- parallel::detectCores()
  cl <- parallel::makeCluster(ncores)
  # registerDoParallel(cl)
  doSNOW::registerDoSNOW(cl)

  n_tasks <- length(vulnerability.list)
  pb <- utils::txtProgressBar(min = 0, max = n_tasks, style = 3)
  progress <- function(n) utils::setTxtProgressBar(pb, n)
  opts <- list(progress = progress)

  parallel::clusterEvalQ(cl, {
    pacman::p_load(dplyr, forcats, reshape2, ggplot2, survey, gridExtra, stringr, config, data.table, broom)
    TRUE
  })

  # EXPORT FUNCTION AND DATA TO EACH WORKER
  parallel::clusterExport(cl, varlist = c(
    "fun_gen_exploratory_data_analysis",
    "outcomes_vulnerability",
    "outcomes.list",
    "strata",
    "exploratory_plots",
    "vulnerability.list"
  ))

  # RUN IN PARALLEL
  results <- foreach::foreach(
    m = vulnerability.list,
    .packages = c("dplyr", "forcats", "reshape2", "ggplot2", "survey",
                  "gridExtra", "stringr", "config", "data.table", "broom"),
    .options.snow = opts
  ) %dopar% {
    tryCatch(
      fun_gen_exploratory_data_analysis(
        df            = outcomes_vulnerability,
        outcomes.list = outcomes.list,
        measure       = m,
        strata        = strata,
        plot_path     = exploratory_plots
      ),
      error = function(e) {
        message("exploratory data analysis failed for measure ", m, ": ", e$message)
        NULL
      }
    )
  }

  # CLEAN UP
  base::close(pb)
  parallel::stopCluster(cl)


} else {
  print("Running EDA serially")


  for(m in vulnerability.list){

    try({

      output <- fun_gen_exploratory_data_analysis(df = outcomes_vulnerability, outcomes.list = outcomes.list, measure = m, strata=strata, plot_path = exploratory_plots)

    }, silent = TRUE)


  }

}


###################################
# RUN EDA FOR A SINGLE VULNERABILITY VARIABLE - THIS WILL SKIP OVER WHEN RUNNING THE ENTIRE SCRIPT


if (FALSE){

  # DEFINE VULNERABILITY VARIABLE AS "M"
  m = "wealth.index.ur.cat"
  output <- fun_gen_exploratory_data_analysis(df = outcomes_vulnerability, outcomes.list = outcomes.list, measure = m, strata=strata, plot_path = exploratory_plots)

}


###################################
print("4_exploratory_data_analysis.R script complete! Proceed to run 5_principal_component_analysis.R script after updating Pathways Workbook. Refer to the README for instructions if needed.")



