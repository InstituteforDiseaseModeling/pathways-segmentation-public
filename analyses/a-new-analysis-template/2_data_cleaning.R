


################################################################################
# DATA CLEANING
################################################################################


###################################
# RUN SETUP
source("1_setup.R")


###################################
# PARAMETERS
create_new_pathways_workbook = config::get("create_new_pathways_workbook")


###################################
# READ PATHWAYS WORKBOOOK
###################################


if (file.exists(outcomes_excel_file)){ outcomes_sheet <- readRDS(outcomes_excel_file) }
if (file.exists(vulnerability_excel_file)){ vulnerability_sheet <- readRDS(vulnerability_excel_file) }


###################################
# READ IN SURVEY | DHS
# RUN 1_import_data.R TO IMPORT AND SAVE RAW SURVEY DATA

if (file.exists(paste0(data_path, "IR.rds")) == FALSE){

  print("Running 1_import_data.R to load survey data and save as .rds objects for quicker load.")
  source("1_import_data.R")

}


IR <- readRDS(file = paste0(data_path, "IR.rds"))
BR <- readRDS(file = paste0(data_path, "BR.rds"))
KR <- readRDS(file = paste0(data_path, "KR.rds"))
HH <- readRDS(file = paste0(data_path, "HH.rds"))
MR <- readRDS(file = paste0(data_path, "MR.rds"))


######################################################################
# 1 | GENERATE VULNERABILITY FACTORS
######################################################################


# GENERATE VULNERABILITY FACTORS
vulnerability <- gen_vulnerability_factors_dhs_pca(IR=IR, BR=BR, HH=HH, MR=MR, dhs=7)
# vulnerability <- gen_vulnerability_factors_dhs(IR=IR, BR=BR, HH=HH, MR=MR, dhs=7)
vulnerability_vars <- setdiff(names(vulnerability), unique(c(names(IR), names(BR), names(HH), names(MR))))
vulnerability <- subset(vulnerability, select=unique(c("caseid", "survey", all_of(svy_id_var), all_of(svy_strata_var), all_of(data_state_var), vulnerability_vars)))


# CODE TO CONVERT BINARY VARIABLES TO YES/NO (BEST PRACTICE IS TO CODE THEM INITIALLY AS YES/NO)
for (col in names(vulnerability)){

  if ((all(c(0, 1) %in% na.omit(vulnerability[[col]])) == TRUE) & (all(na.omit(vulnerability[[col]]) %in% c(0, 1)) == TRUE) |
      (all(c("Yes", "No") %in% str_to_title(na.omit(vulnerability[[col]]))) == TRUE) & (all(str_to_title(na.omit(vulnerability[[col]])) %in% c("Yes", "No")) == TRUE)){

    print(col)
    vulnerability[[col]] <- ifelse(vulnerability[[col]] == 1, "Yes", vulnerability[[col]])
    vulnerability[[col]] <- ifelse(vulnerability[[col]] == 0, "No", vulnerability[[col]])
    vulnerability[[col]] <- ifelse(vulnerability[[col]] == "yes", "Yes", vulnerability[[col]])
    vulnerability[[col]] <- ifelse(vulnerability[[col]] == "no", "No", vulnerability[[col]])
  }
}


# SAVE VULNERABILITY DATASET
dir.create(dirname(vulnerability_file), showWarnings = F, recursive = T)
saveRDS(vulnerability, file = paste0(vulnerability_file, ".rds"))
write.csv(vulnerability, file = paste0(vulnerability_file, ".csv"), row.names = FALSE)

vulnerability <- readRDS(file = paste0(vulnerability_file, ".rds"))


######################################################################
# 2 | GENERATE OUTCOMES VARIABLES
######################################################################


# GENERATE HEALTH OUTCOMES
outcomes <- gen_outcome_variables_dhs(IR=IR, KR=KR, BR=BR, DHS=7)
outcomes_vars <- setdiff(names(outcomes), c(names(IR), names(BR), names(KR)))
outcomes <- subset(outcomes, select=unique(c("caseid", "v001", "v002", "v003", "survey", outcomes_vars)))


# ADD IN strata VARIABLE TO OUTCOMES
outcomes <- outcomes %>%
  base::merge(vulnerability[,c("caseid", "wt", "survey")], by=c("caseid", "survey"))

# SAVE OUTCOME DATASET
dir.create(dirname(outcomes_file), showWarnings = F, recursive = T)
saveRDS(outcomes, file = paste0(outcomes_file, ".rds"))
write.csv(outcomes, file = paste0(outcomes_file, ".csv"), row.names = FALSE)

outcomes <- readRDS(file = paste0(outcomes_file, ".rds"))

# MERGE TOGETHER AND SAVE
outcomes_vulnerability <- base::merge(outcomes, vulnerability, by=c("caseid", "wt", "survey"))
saveRDS(outcomes_vulnerability, file = paste0(outcomes_vulnerability_file, ".rds"))
write.csv(outcomes_vulnerability, file = paste0(outcomes_vulnerability_file, ".csv"), row.names = FALSE)


######################################################################
# 3 | CREATE NEW PATHWAYS WORKBOOK
######################################################################


if (create_new_pathways_workbook==TRUE){

  # GET DATA DICTIONARIES
  dd_outcomes <- readRDS(dd_outcomes_excel_file)
  dd_vulnerabilities <- readRDS(dd_vulnerabilities_excel_file)

  # FILTER VARIABLE LIST
  vulnerability_vars_excel <- vulnerability_vars[!vulnerability_vars %in% c("survey", "strata", "wt", all_of(svy_id_var), all_of(svy_strata_var))]
  outcomes_vars_excel <- outcomes_vars[!outcomes_vars %in% c("survey", "strata", "LB", "STL", "str")]

  # CREATE PARAMS SHEET
  df_params <- data.frame(domains = c("Woman and her past experiences",
                                      "Health and mental models",
                                      "Natural and human systems",
                                      "Household relationships",
                                      "Household economics and living conditions",
                                      "Social support"),
                          strata = c("urban",
                                     "rural",
                                     NA, NA, NA, NA),
                          final_model = NA)

  # CREATE OUTCOMES SHEET
  df_outcomes <- data.frame(
    outcome_variable = unique(c(outcomes_vars_excel)),
    univariate_include = 1,
    eda_include = NA,
    profile_include = NA,
    notes = NA
  ) %>%
    base::merge(dd_outcomes, by=c("outcome_variable"), all.x=TRUE) %>%
    dplyr::select(category, outcome_variable, short_name, description, univariate_include, eda_include, profile_include, notes) %>%
    arrange(category, outcome_variable)

  # CREATE VULNERABILITY SHEET
  df_vulnerability <- data.frame(
    vulnerability_variable = unique(c(vulnerability_vars_excel)),
    univariate_include = 1,
    eda_include = NA,
    pca_strata = NA,
    pca_include = NA,
    lca_strata = NA,
    lca_include = NA,
    profile_strata = NA,
    profile_include = NA,
    typing_tool_strata = NA,
    typing_tool_include = NA,
    notes = NA
    ) %>%
    base::merge(dd_vulnerabilities, by=c("vulnerability_variable"), all.x=TRUE) %>%
    dplyr::select(vulnerability_variable, short_name, description, univariate_include, eda_include, pca_strata, pca_include, lca_strata, lca_include, profile_strata, profile_include, typing_tool_strata, typing_tool_include, notes,
                  Woman.and.her.past.experiences, Health.and.mental.models, Natural.and.human.systems, Household.relationships, Household.economics.and.living.conditions, Social.support) %>%
    arrange(vulnerability_variable)


  l <- list("params" = df_params, "outcomes" = df_outcomes, "vulnerabilities" = df_vulnerability)
  path = new_pathways_workbook_path
  write.xlsx(l, file = path)
  print(paste0("New Pathways Workbook Created: ", path))

}


###################################
print("2_data_cleaning.R script complete! Proceed to run 3_univariate_analysis.R script. Refer to the README for instructions if needed.")












