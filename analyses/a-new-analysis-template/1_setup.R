


################################################################################
# ENVIRONMNET SETUP
################################################################################


###################################
# RESTORE LIBRARIES FROM RENV LOCK FILE
# if (!require("renv")) install.packages("renv")
# renv::activate()
# renv::restore()
# renv::deactivate()


###################################
# LIBRARY INSTALL
if (!require("pacman")) install.packages("pacman")

# LOAD ALL LIBRARY DEPENDENCIES
pacman::p_load(dplyr, stringr, reshape2, data.table, survey, ggplot2, broom, jtools, readxl, openxlsx, gridExtra, factoextra, modelsummary,
               poLCA, readstata13, fastDummies, huxtable, openxlsx, config, ggdist, sf, scatterpie, networkD3, htmlwidgets, remotes,
               conflicted, webshot, magick, zscorer, haven)

# if (!require("ggsankey")) remotes::install_github("davidsjoberg/ggsankey")
# if (!require("ggradar")) remotes::install_github("ricardo-bion/ggradar")


###################################
# SET CONFIG AND GET CONFIG VALUES
Sys.setenv(R_CONFIG_ACTIVE = "default")

create_new_pathways_workbook = config::get("create_new_pathways_workbook")
project_name = config::get("project_name")

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
pathways_workbook_path = paste0(root_path, user_path, config::get("pathways_workbook_path"))
new_pathways_workbook_path = paste0(root_path, user_path, config::get("new_pathways_workbook_path"))

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


###################################
# READ IN DATA DICTIONARY & PATHWAYS WORKBOOK


# READ IN PATHWAYS DATA DICTIONARY
dd_outcomes <- read_excel(path=file.path("../../pathways data dictionary.xlsx"), sheet="outcomes")
saveRDS(dd_outcomes, file = dd_outcomes_excel_file)

dd_vulnerabilities <- read_excel(path=file.path("../../pathways data dictionary.xlsx"), sheet="vulnerabilities")
saveRDS(dd_vulnerabilities, file = dd_vulnerabilities_excel_file)


# READ IN PATHWAYS WORKBOOK
if(create_new_pathways_workbook == FALSE){

  outcomes_sheet <- read_excel(pathways_workbook_path, sheet="outcomes")

  # PARAMS
  params_sheet <- read_excel(pathways_workbook_path, sheet="params")

  saveRDS(params_sheet, file = params_excel_file)


  # OUTCOMES
  outcomes_sheet <- read_excel(pathways_workbook_path, sheet="outcomes")
  outcomes_sheet <- outcomes_sheet %>%
    mutate(short_name = ifelse(is.na(short_name), outcome_variable, short_name))

  saveRDS(outcomes_sheet, file = outcomes_excel_file)


  # VULNERABILITY
  vulnerability_sheet <- read_excel(pathways_workbook_path, sheet="vulnerabilities")
  vulnerability_sheet <- vulnerability_sheet %>%
    mutate(short_name = ifelse(is.na(short_name), vulnerability_variable, short_name))

  saveRDS(vulnerability_sheet, file = vulnerability_excel_file)

}


###################################














