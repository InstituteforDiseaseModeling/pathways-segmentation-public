


################################################################################
# ENVIRONMNET SETUP
################################################################################


###################################
# RUN 1_libraries.R TO INSTALL LIBRARIES AT THE START OF EACH SESSION
use_renv = TRUE


if (!"pacman" %in% names(sessionInfo()$otherPkgs)){

  print("Running 1_libraries.R to load libraries.")
  source("1_libraries.R")

}


###################################
# SET CONFIG AND GET CONFIG VALUES

# THIS SHOULD BE SET TO "default" UNLESS MULTIPLE PROJECTS ARE BEING MANAGED FROM THE SAME CONFIG FILE
Sys.setenv(R_CONFIG_ACTIVE = "bfa_dhs_2021")

project_name = config::get("project_name")
print(paste0("Loading config for project: ", project_name))

create_new_pathways_workbook = config::get("create_new_pathways_workbook")
pathways_workbook_is_excel = config::get("pathways_workbook_is_excel")
survey_name = config::get("survey_name")

# GET SURVEY DESIGN VARIABELS FROM CONFIG
svy_id_var = config::get("svy_id_var")
svy_strata_var = config::get("svy_strata_var")

# STATE LEVEL VARIABLE FOR MAPPING
data_state_var = config::get("data_state_var")

# SURVEY DESIGN HANDLING
options(survey.lonely.psu="adjust")


###################################
# R VERSION UPGRADE
R.version.string

# IF R VERSION < THEN UPDATE TO VERSION 4.4.2
# https://cran.r-project.org/src/base/R-4/


###################################
# MAIN FILE PATHS
root_path = config::get("root_path")
user_path = config::get("user_path")

dir.create(file.path(paste0(root_path, user_path)), showWarnings = F, recursive = T)


###################################
# ADDITIONAL FILE PATHS

# PATHWAYS WORKBOOK PATH
pathways_workbook_path = paste0(root_path, user_path, config::get("pathways_workbook_name"))
new_pathways_workbook_path = paste0(root_path, user_path, config::get("new_pathways_workbook_name"))

# DHS DATA
data_path = paste0(root_path, "data/")

# SHP FILE FOR MAPPING
shp_path = paste0(data_path, config::get("shp_file"))

# DATA DICTIONARY OUTCOMES EXCEL
dd_outcomes_excel_file = paste0(root_path, user_path, "data_dict_outcomes.rds")

# DATA DICTIONARY VULNERABILITIES EXCEL
dd_vulnerabilities_excel_file = paste0(root_path, user_path, "data_dict_vulnerabilities.rds")

# PARAMS
params_excel_file = paste0(root_path, user_path, "1_params_excel.rds")
dir.create(dirname(params_excel_file), showWarnings = F, recursive = T)

# OUTCOMES EXCEL
outcomes_excel_file = paste0(root_path, user_path, "1_outcomes_excel.rds")
dir.create(dirname(outcomes_excel_file), showWarnings = F, recursive = T)

# VULNERABILITY EXCEL
vulnerability_excel_file = paste0(root_path, user_path, "1_vulnerability_excel.rds")
dir.create(dirname(vulnerability_excel_file), showWarnings = F, recursive = T)

# OUTCOMES
outcomes_file = paste0(root_path, user_path, "2_outcomes")
dir.create(dirname(outcomes_file), showWarnings = F, recursive = T)

# VULNERABILITY FACTORS
vulnerability_file = paste0(root_path, user_path, "2_vulnerability")
dir.create(dirname(vulnerability_file), showWarnings = F, recursive = T)

# OUTCOMES_VULNERABILITY FILE
outcomes_vulnerability_file = paste0(root_path, user_path, "2_outcomes_vulnerability")
dir.create(dirname(outcomes_vulnerability_file), showWarnings = F, recursive = T)

# EXPLORATORY ANALYSIS
exploratory_analysis_file = paste0(root_path, user_path, "exploratory_analysis.rds")
dir.create(dirname(exploratory_analysis_file), showWarnings = F, recursive = T)

# PCA
pca_file = paste0(root_path, user_path, "pca_analysis.rds")
dir.create(dirname(pca_file), showWarnings = F, recursive = T)

# LCA
lca_path = paste0(root_path, user_path, "lca/")
dir.create(dirname(lca_path), showWarnings = F, recursive = T)

# CART
cart_path = paste0(root_path, user_path, "cart/")
dir.create(dirname(cart_path), showWarnings = F, recursive = T)

# UNIVARIATE PLOTS
univariate_plots = paste0(root_path, user_path, "plots/univariate_plots/")
dir.create(dirname(univariate_plots), showWarnings = F, recursive = T)

# EXPLORATORY PLOTS
exploratory_plots = paste0(root_path, user_path, "plots/exploratory_plots/")
dir.create(dirname(exploratory_plots), showWarnings = F, recursive = T)

# PCA PLOTS
pca_plots = paste0(root_path,  user_path, "plots/pca_plots/")
dir.create(dirname(pca_plots), showWarnings = F, recursive = T)

# LCA PLOTS
lca_plots = paste0(root_path, user_path, "/plots/lca_plots/")
dir.create(dirname(lca_plots), showWarnings = F, recursive = T)

# PROFILE PLOTS
profile_plots = paste0(root_path, user_path, "/plots/profile_plots/")
dir.create(dirname(profile_plots), showWarnings = F, recursive = T)

# CART PLOTS
cart_plots = paste0(root_path, user_path, "/plots/cart_plots/")
dir.create(dirname(cart_plots), showWarnings = F, recursive = T)


###################################
# DEFINE FUNCTIONS
###################################
functions = list.files("functions")
for (f in functions){
  source(paste0("functions/",f))
}

# LOAD PROJECT SPECIFIC VULNERABILITY AND OUTCOMES SCRIPT IF NEEDED
tryCatch(
  if (file.exists(config::get("vulnerability_script"))==TRUE) {
    source(config::get("vulnerability_script"))
    print(paste0(config::get("vulnerability_script"), " loaded."))},
  error = function(e) {warning("project specific vulnerability_script config parameter does not exist")})

tryCatch(
  if (file.exists(config::get("outcomes_script"))==TRUE) {
    source(config::get("outcomes_script"))
    print(paste0(config::get("outcomes_script"), " loaded."))},
  error = function(e) {warning("project specific outcomes_script config parameter does not exist")})


###################################
# READ IN DATA DICTIONARY & PATHWAYS WORKBOOK


# READ IN PATHWAYS DATA DICTIONARY
dd_outcomes <- read_excel(path=file.path("pathways data dictionary.xlsx"), sheet="outcomes")
saveRDS(dd_outcomes, file = dd_outcomes_excel_file)

dd_vulnerabilities <- read_excel(path=file.path("pathways data dictionary.xlsx"), sheet="vulnerabilities")
saveRDS(dd_vulnerabilities, file = dd_vulnerabilities_excel_file)


# READ IN PATHWAYS WORKBOOK
if (file.exists(paste0(pathways_workbook_path, ".xlsx")) == TRUE | file.exists(paste0(pathways_workbook_path, " - vulnerabilities.csv")) == TRUE){

  if (pathways_workbook_is_excel == TRUE){

    tryCatch({

      # PARAMS
      params_sheet <- read_excel(paste0(pathways_workbook_path, ".xlsx"), sheet="params")

      saveRDS(params_sheet, file = params_excel_file)


      # OUTCOMES
      outcomes_sheet <- read_excel(paste0(pathways_workbook_path, ".xlsx"), sheet="outcomes")
      outcomes_sheet <- outcomes_sheet %>%
        mutate(short_name = ifelse(is.na(short_name), outcome_variable, short_name))

      saveRDS(outcomes_sheet, file = outcomes_excel_file)


      # VULNERABILITY
      vulnerability_sheet <- read_excel(paste0(pathways_workbook_path, ".xlsx"), sheet="vulnerabilities")
      vulnerability_sheet <- vulnerability_sheet %>%
        mutate(short_name = ifelse(is.na(short_name), vulnerability_variable, short_name))

      saveRDS(vulnerability_sheet, file = vulnerability_excel_file)

      print("Pathways Workbook XLSX imported!")

    }, error = function(e) {

      stop("Unable to read Pathways Workbook XLSX. Ensure the workbook file is not open and is in the correct file location.")

    })

  } else if (pathways_workbook_is_excel == FALSE){

    tryCatch({

      # PARAMS
      params_sheet <- fread(paste0(pathways_workbook_path, " - params.csv"))

      saveRDS(params_sheet, file = params_excel_file)


      # OUTCOMES
      outcomes_sheet <- fread(paste0(pathways_workbook_path, " - outcomes.csv"))
      outcomes_sheet <- outcomes_sheet %>%
        mutate(short_name = ifelse(is.na(short_name), outcome_variable, short_name))

      saveRDS(outcomes_sheet, file = outcomes_excel_file)


      # VULNERABILITY
      vulnerability_sheet <- fread(paste0(pathways_workbook_path, " - vulnerabilities.csv"))
      vulnerability_sheet <- vulnerability_sheet %>%
        mutate(short_name = ifelse(is.na(short_name), vulnerability_variable, short_name))

      saveRDS(vulnerability_sheet, file = vulnerability_excel_file)

      print("Pathways Workbook CSVs imported!")

    }, error = function(e) {

      stop("Unable to read Pathways Workbook CSVs. Ensure the workbook file is not open and is in the correct file location.")

    })

  }

} else {

  print("Pathways Workbook does not exist and therefore not imported.")

}


###################################
print("1_setup.R script complete!")














