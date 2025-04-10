


################################################################################
# EXPLORATORY DATA ANALYSIS
################################################################################


# rm(list=ls())
###################################
# RUN SETUP
source("1_setup.R")


###################################
# READ PATHWAYS WORKBOOOK SHEETS
###################################


outcome_vars <- readRDS(outcomes_excel_file)
vulnerability_vars <- readRDS(vulnerability_excel_file)

outcomes_vars_eda <- outcome_vars %>%
  dplyr::filter(eda_include == 1) %>%
  dplyr::select(outcome_variable)

vulnerability_vars_eda <- vulnerability_vars %>%
  dplyr::filter(eda_include == 1) %>%
  dplyr::select(vulnerability_variable)

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


for(m in vulnerability.list[1:10]){

  # try({

  output <- gen_eda(df = outcomes_vulnerability, outcomes.list = outcomes.list, measure = m, strata=strata, plot_path = exploratory_plots)

  # }, silent = TRUE)

}


###################################
print("4_exploratory_data_analysis.R script complete! Proceed to run 5_principal_component_analysis.R script after updating Pathways Workbook. Refer to the README for instructions if needed.")



