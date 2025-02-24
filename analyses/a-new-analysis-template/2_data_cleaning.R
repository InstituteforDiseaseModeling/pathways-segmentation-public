


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

# IR <- data.table(gen_dhs_data(source = "Local File", file_path = paste0(data_path, "ETIR71FL.DTA"))) %>% dplyr::mutate(survey = "ET71FL")
# BR <- data.table(gen_dhs_data(source = "Local File", file_path = paste0(data_path, "ETBR71FL.DTA"))) %>% dplyr::mutate(survey = "ET71FL")
# KR <- data.table(gen_dhs_data(source = "Local File", file_path = paste0(data_path, "ETKR71FL.DTA"))) %>% dplyr::mutate(survey = "ET71FL")
# HH <- data.table(gen_dhs_data(source = "Local File", file_path = paste0(data_path, "ETHR71FL.DTA"))) %>% dplyr::mutate(survey = "ET71FL")
# MR <- data.table(gen_dhs_data(source = "Local File", file_path = paste0(data_path, "ETMR71FL.DTA"))) %>% dplyr::mutate(survey = "ET71FL")

# IR <- data.table(gen_dhs_data(source = "Local File", file_path = paste0(data_path, "ETIR71FL.DTA"))) %>% dplyr::mutate(survey = "ET71FL")
# BR <- data.table(gen_dhs_data(source = "Local File", file_path = paste0(data_path, "ETBR71FL.DTA"))) %>% dplyr::mutate(survey = "ET71FL")
# KR <- data.table(gen_dhs_data(source = "Local File", file_path = paste0(data_path, "ETKR71FL.DTA"))) %>% dplyr::mutate(survey = "ET71FL")
# HH <- data.table(gen_dhs_data(source = "Local File", file_path = paste0(data_path, "ETHR71FL.DTA"))) %>% dplyr::mutate(survey = "ET71FL")
# MR <- data.table(gen_dhs_data(source = "Local File", file_path = paste0(data_path, "ETMR71FL.DTA"))) %>% dplyr::mutate(survey = "ET71FL")

IR <- data.table(read.dta13(file = paste0(data_path, "ETIR71FL.DTA"), fromEncoding="utf-8")) %>% dplyr::mutate(survey = "ET71FL")
BR <- data.table(read.dta13(file = paste0(data_path, "ETBR71FL.DTA"), fromEncoding="utf-8")) %>% dplyr::mutate(survey = "ET71FL")
KR <- data.table(read.dta13(file = paste0(data_path, "ETKR71FL.DTA"), fromEncoding="utf-8")) %>% dplyr::mutate(survey = "ET71FL")
HH <- data.table(read.dta13(file = paste0(data_path, "ETHR71FL.DTA"), fromEncoding="utf-8")) %>% dplyr::mutate(survey = "ET71FL")
MR <- data.table(read.dta13(file = paste0(data_path, "ETMR71FL.DTA"), fromEncoding="utf-8")) %>% dplyr::mutate(survey = "ET71FL")


###################################
# READ IN SURVEY | PATHWAYS
# survey <-


###################################
# READ IN SURVEY | MISC

# # DTA
# data <- read.dta13(file = file_path, fromEncoding="utf-8")

# SAV


# CSV


######################################################################
# 1 | GENERATE VULNERABILITY FACTORS
######################################################################


# GENERATE VULNERABILITY FACTORS
vulnerability <- gen_vulnerability_factors_dhs(IR=IR, BR=BR, HH=HH, MR=MR, dhs=7)
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
outcomes <- subset(outcomes, select=unique(c("caseid", "v001", "v002", "survey", outcomes_vars)))


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
                                      "Healthcare and mental models",
                                      "Natural and human systems",
                                      "Household relationships",
                                      "Household economics",
                                      "Community Support"),
                          strata = c("urban",
                                     "rural",
                                     NA, NA, NA, NA),
                          final_model = NA)

  # CREATE OUTCOMES SHEET
  df_outcomes <- data.frame(
    outcome_variable = unique(c(outcomes_vars_excel)),
    univariate_include = NA,
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
    univariate_include = NA,
    eda_include = NA,
    pca_strata = NA,
    pca_include = NA,
    lca_strata = NA,
    lca_include = NA,
    profile_strata = NA,
    profile_include = NA,
    typing_tool_strata = NA,
    typing_tool_include = NA,
    notes = NA,
    `Woman and her past experiences` = NA,
    `Healthcare and mental models` = NA,
    `Natural and human systems` = NA,
    `Household relationships` = NA,
    `Household economics` = NA,
    `Community Support` = NA) %>%
    base::merge(dd_vulnerabilities, by=c("vulnerability_variable"), all.x=TRUE) %>%
    dplyr::select(vulnerability_variable, short_name, description, univariate_include, eda_include, pca_strata, pca_include, lca_strata, lca_include, profile_strata, profile_include, typing_tool_strata, typing_tool_include, notes,
                  Woman.and.her.past.experiences, Healthcare.and.mental.models, Natural.and.human.systems, Household.relationships, Household.economics, Community.Support) %>%
    arrange(vulnerability_variable)


  l <- list("params" = df_params, "outcomes" = df_outcomes, "vulnerabilities" = df_vulnerability)
  path = new_pathways_workbook_path
  write.xlsx(l, file = path)
  print(paste0("New Pathways Workbook Created: ", path))

}












