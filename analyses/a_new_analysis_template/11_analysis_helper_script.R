


################################################################################
# AD-HOC HELPER SCRIPT
################################################################################


###################################
# GET DATA

IR <- readRDS(file = paste0(data_path, "IR.rds"))
BR <- readRDS(file = paste0(data_path, "BR.rds"))
KR <- readRDS(file = paste0(data_path, "KR.rds"))
HH <- readRDS(file = paste0(data_path, "HH.rds"))
MR <- readRDS(file = paste0(data_path, "MR.rds"))


###################################
# TEST NEW VARIABLE BEFORE ADDING TO THE fun_get_vulnerabilities_dhs.R SCRIPT

# TEST CREATE A NEW VARIABLE HERE AND USE THE FOLLOWING FUNCTIONS TO TEST OUT THE DISTRIBUTION
# table(var, useNA = "always")
# hist(var)

# EXAMPLE: CREATE WORK SEASONAL VARIABLE
# FIRST LET'S LOOK AT THE DISTRIBUTION OF THE RAW VARIABLE
table(IR$v731, useNA = "always")

# CREATE VARIABLE AND TEST
# WORK SEASONALITY
IR <- IR %>% dplyr::mutate(work.seasonal = case_when(
  v731 == "no" ~ "No work in past 12 months",
  v732 == "occasional" ~ "Occassional",
  v732 == "seasonal" ~ "Seasonal",
  v732 == "all year" ~ "All year"))

table(IR$work.seasonal, useNA = "always")

# IF THIS IS GOOD, COPY AND PASTE INTO THE fun_get_vulnerabilities_dhs.R SCRIPT DIRECTLY


###################################
# RUN A SINGLE RUN OF THE EXPLORATORY DATA ANALYSIS FUNCTION

# GET outcomes_vulnerability DATAFRAME
outcomes_vulnerability <- readRDS(file = paste0(outcomes_vulnerability_file, ".rds"))


# GET outcomes.list
outcome_vars <- readRDS(outcomes_excel_file)

outcomes_vars_eda <- outcome_vars %>%
  dplyr::filter(eda_include == 1) %>%
  dplyr::select(outcome_variable)

outcomes <- readRDS(file = paste0(outcomes_file, ".rds"))

outcomes.list <- names(outcomes)[names(outcomes) %in% outcomes_vars_eda$outcome_variable]


# GET strata
strata <- readRDS(params_excel_file) %>%
  dplyr::select(strata) %>%
  dplyr::filter(!is.na(strata)) %>%
  pull()


# DEFINE THE VULNERABILITY FACTOR AND RUN FOR ALL OUTCOMES
m = "slum.sum"


output <- fun_gen_exploratory_data_analysis(df = outcomes_vulnerability, outcomes.list = outcomes.list, measure = m, strata=strata, plot_path = exploratory_plots)


###################################











