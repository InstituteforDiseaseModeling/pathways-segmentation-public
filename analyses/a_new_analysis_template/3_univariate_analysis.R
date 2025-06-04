


################################################################################
# UNIVARIATE ANALYSIS
################################################################################


###################################
# RUN SETUP
source("1_setup.R")


###################################
# OUTCOMES
outcomes <- readRDS(file = paste0(outcomes_file, ".rds"))

outcomes_sheet <- readRDS(outcomes_excel_file) %>%
  dplyr::filter(univariate_include == 1) %>%
  dplyr::select(outcome_variable, short_name) %>%
  setNames(c("indicator", "short_name")) %>%
  distinct()

univariate_plots_file = paste0(univariate_plots, "univariate_plots_outcomes.pdf")
fun_gen_univariate_output(df = outcomes, metadata = outcomes_sheet, plot_path = univariate_plots_file)


###################################
# VULNERABILITY
vulnerability <- readRDS(file = paste0(vulnerability_file, ".rds"))

vulnerability_sheet <- readRDS(vulnerability_excel_file) %>%
  dplyr::filter(univariate_include == 1) %>%
  dplyr::select(vulnerability_variable, short_name) %>%
  setNames(c("indicator", "short_name")) %>%
  distinct()

univariate_plots_file = paste0(univariate_plots, "univariate_plots_vulnerability.pdf")
fun_gen_univariate_output(df = vulnerability, metadata = vulnerability_sheet, plot_path = univariate_plots_file)


###################################
print("3_univariate_analysis.R script complete! Proceed to run 4_exploratory_data_analysis.R script after updating Pathways Workbook. Refer to the README for instructions if needed.")



