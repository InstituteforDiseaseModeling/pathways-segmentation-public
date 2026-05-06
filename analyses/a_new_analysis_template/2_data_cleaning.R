################################################################################
# DATA CLEANING
################################################################################


###################################
# RUN SETUP
source("1_setup.R")


###################################
# PARAMETERS
create_new_pathways_workbook <- config::get("create_new_pathways_workbook")


###################################
# READ IN SURVEY | DHS
# RUN 1_import_data.R TO IMPORT AND SAVE RAW SURVEY DATA

# BUILD LIST OF EXPECTED RDS FILES FROM CONFIG
# Only checks files that are actually configured (non-empty string in config)
configured_files <- list(
  IR = config::get("dhs_ir_file"),
  BR = config::get("dhs_br_file"),
  KR = config::get("dhs_kr_file"),
  HH = config::get("dhs_hh_file"),
  MR = config::get("dhs_mr_file"),
  PR = config::get("dhs_pr_file")
)

# FILTER TO ONLY CONFIGURED FILES
configured_files <- Filter(function(f) !is.null(f) && nchar(trimws(f)) > 0, configured_files)

# CHECK WHICH CONFIGURED FILES ARE MISSING AS RDS
missing_rds <- names(configured_files)[
  !file.exists(paste0(data_path, names(configured_files), ".rds"))
]

if (length(missing_rds) > 0) {
  message("Missing .rds files for: ", paste(missing_rds, collapse = ", "),
          " — running 1_import_data.R to import survey data.")
  source("1_import_data.R")
} else {
  message("All configured .rds files found — skipping import.")
}

# LOAD AVAILABLE DHS FILES
if (file.exists(paste0(data_path, "IR.rds"))) { IR <- readRDS(paste0(data_path, "IR.rds")); message("IR imported.") }
if (file.exists(paste0(data_path, "BR.rds"))) { BR <- readRDS(paste0(data_path, "BR.rds")); message("BR imported.") }
if (file.exists(paste0(data_path, "KR.rds"))) { KR <- readRDS(paste0(data_path, "KR.rds")); message("KR imported.") }
if (file.exists(paste0(data_path, "HH.rds"))) { HH <- readRDS(paste0(data_path, "HH.rds")); message("HH imported.") }
if (file.exists(paste0(data_path, "MR.rds"))) { MR <- readRDS(paste0(data_path, "MR.rds")); message("MR imported.") }
if (file.exists(paste0(data_path, "PR.rds"))) { PR <- readRDS(paste0(data_path, "PR.rds")); message("PR imported.") }


######################################################################
# 1 & 2 | GENERATE VULNERABILITY AND OUTCOME VARIABLES IN PARALLEL
######################################################################

# Vulnerability and outcome generation are independent — run simultaneously
# on Mac/Linux using process forking.  Windows does not support forking so
# falls back to sequential execution automatically.

message("Generating vulnerability and outcome variables...")

run_parallel <- .Platform$OS.type != "windows"

if (run_parallel) {
  message("Platform supports forking — running vulnerability and outcome generation in parallel.")
} else {
  message("Windows detected — running vulnerability and outcome generation sequentially.")
}


# DEFINE GENERATION TASKS AS A NAMED LIST
# Each element is a list with a function and its arguments explicitly bound.
# This ensures the dataframes are available inside the forked worker processes
# rather than relying on environment capture which is unreliable with mclapply.

gen_tasks <- list(

  vulnerability = list(
    fn   = gen_vulnerability_factors_dhs,
    # SWAP FUNCTION NAME FOR PROJECT-SPECIFIC VERSION IF NEEDED:
    # fn = gen_vulnerability_factors_dhs_bfa,
    args = list(IR=IR, BR=BR, HH=HH, MR=MR, PR=PR, dhs=8)
  ),

  outcomes = list(
    fn   = gen_outcome_variables_dhs,
    # SWAP FUNCTION NAME FOR PROJECT-SPECIFIC VERSION IF NEEDED:
    # fn = gen_outcome_variables_dhs_bfa,
    args = list(IR=IR, KR=KR, BR=BR, dhs=8)
  )

)


# RUN TASKS
if (run_parallel) {

  results <- parallel::mclapply(
    gen_tasks,
    function(task) do.call(task$fn, task$args),
    mc.cores = 2,
    mc.set.seed = FALSE
  )

} else {

  results <- lapply(gen_tasks, function(task) do.call(task$fn, task$args))

}

vulnerability <- results[["vulnerability"]]
outcomes      <- results[["outcomes"]]
rm(results)


######################################################################
# PROCESS VULNERABILITY OUTPUT
######################################################################

vulnerability_vars <- setdiff(
  names(vulnerability),
  unique(c(
    if (exists("IR")) names(IR) else character(),
    if (exists("BR")) names(BR) else character(),
    if (exists("HH")) names(HH) else character(),
    if (exists("MR")) names(MR) else character(),
    if (exists("PR")) names(PR) else character()
  ))
)

vulnerability <- subset(vulnerability, select = unique(c(
  "caseid", "survey",
  all_of(svy_id_var), all_of(svy_strata_var), all_of(data_state_var),
  vulnerability_vars
)))

# CONVERT BINARY VARIABLES TO YES/NO USING SHARED HELPER
message("Recoding binary vulnerability variables to Yes/No...")
vulnerability <- recode_binary_yn(vulnerability)

# SAVE VULNERABILITY DATASET
dir.create(dirname(vulnerability_file), showWarnings = F, recursive = T)
saveRDS(vulnerability, file = paste0(vulnerability_file, ".rds"))
write.csv(vulnerability, file = paste0(vulnerability_file, ".csv"), row.names = FALSE)


######################################################################
# PROCESS OUTCOMES OUTPUT
######################################################################

outcomes_vars <- setdiff(
  names(outcomes),
  unique(c(
    if (exists("IR")) names(IR) else character(),
    if (exists("BR")) names(BR) else character(),
    if (exists("KR")) names(KR) else character()
  ))
)

outcomes_id_vars <- config::get("outcomes_id_vars")
outcomes <- subset(outcomes, select = unique(c(
  "caseid", outcomes_id_vars, "survey", outcomes_vars
)))

# ADD strata AND WEIGHT FROM VULNERABILITY
outcomes <- outcomes %>%
  base::merge(vulnerability[, c("caseid", "wt", "survey")], by = c("caseid", "survey"))

# SAVE OUTCOMES DATASET
dir.create(dirname(outcomes_file), showWarnings = F, recursive = T)
saveRDS(outcomes, file = paste0(outcomes_file, ".rds"))
write.csv(outcomes, file = paste0(outcomes_file, ".csv"), row.names = FALSE)
# note: no need to re-read outcomes from disk; the in-memory object is identical

# MERGE TOGETHER AND SAVE
outcomes_vulnerability <- base::merge(outcomes, vulnerability, by = c("caseid", "wt", "survey"))
saveRDS(outcomes_vulnerability, file = paste0(outcomes_vulnerability_file, ".rds"))
write.csv(outcomes_vulnerability, file = paste0(outcomes_vulnerability_file, ".csv"), row.names = FALSE)


######################################################################
# FREE INPUT FILE MEMORY
######################################################################

message("Freeing input file memory...")
rm(IR, BR, HH)
if (exists("KR")) rm(KR)
if (exists("MR")) rm(MR)
if (exists("PR")) rm(PR)
gc()


######################################################################
# 3 | CREATE NEW PATHWAYS WORKBOOK
######################################################################


if (create_new_pathways_workbook == TRUE) {

  # GET DATA DICTIONARIES
  dd_outcomes        <- readRDS(dd_outcomes_excel_file)
  dd_vulnerabilities <- readRDS(dd_vulnerabilities_excel_file)

  # FILTER VARIABLE LIST
  vulnerability_vars_excel <- vulnerability_vars[!vulnerability_vars %in%
                                                   c("survey", "strata", "wt", all_of(svy_id_var), all_of(svy_strata_var))]

  outcomes_vars_excel <- outcomes_vars[!outcomes_vars %in%
                                         c("survey", "strata", "LB", "STL", "str")]

  # CREATE PARAMS SHEET
  df_params <- data.frame(
    domains     = unique(dd_vulnerabilities$domain),
    strata      = c("urban", "rural", NA, NA, NA, NA, NA),
    final_model = NA
  )

  # CREATE OUTCOMES SHEET
  df_outcomes <- data.frame(
    outcome_variable  = unique(c(outcomes_vars_excel)),
    univariate_include = 1,
    eda_include       = NA,
    ranking_include   = NA,
    profile_include   = NA,
    notes             = NA
  ) %>%
    base::merge(dd_outcomes, by.x = "outcome_variable", by.y = "metric_id", all.x = TRUE) %>%
    dplyr::mutate(ranking_include = case_when(
      outcome_variable %in% c("anc.less4.last", "home.birth.last",
                              "nofp.mod.ever", "u5mort.yn",
                              "waste.cat2.yn") ~ 1)) %>%
    dplyr::select(outcome_theme, outcome_variable, short_name, detailed_description,
                  univariate_include, eda_include, ranking_include, profile_include, notes) %>%
    arrange(outcome_theme, outcome_variable)

  # CREATE VULNERABILITY SHEET
  df_vulnerability <- data.frame(
    vulnerability_variable = unique(c(vulnerability_vars_excel)),
    univariate_include     = 1,
    eda_include            = NA,
    pca_strata             = NA,
    pca_include            = NA,
    lca_strata             = NA,
    lca_include            = NA,
    profile_strata         = NA,
    profile_include        = NA,
    typing_tool_strata     = NA,
    typing_tool_include    = NA,
    notes                  = NA
  ) %>%
    base::merge(dd_vulnerabilities, by.x = c("vulnerability_variable"), by.y = c("metric_id"), all.x = TRUE) %>%
    dplyr::select(vulnerability_variable, short_name, detailed_description, domain,
                  univariate_include, eda_include, pca_strata, pca_include,
                  lca_strata, lca_include, profile_strata, profile_include,
                  typing_tool_strata, typing_tool_include, notes) %>%
    arrange(vulnerability_variable)


  if (pathways_workbook_is_excel == TRUE) {

    l    <- list("params" = df_params, "outcomes" = df_outcomes, "vulnerabilities" = df_vulnerability)
    path <- paste0(new_pathways_workbook_path, ".xlsx")
    write.xlsx(l, file = path)
    message("New Pathways Workbook Excel created: ", new_pathways_workbook_path)

  } else if (pathways_workbook_is_excel == FALSE) {

    write.csv(df_params,        file = paste0(new_pathways_workbook_path, " - params.csv"),          row.names = FALSE)
    write.csv(df_outcomes,      file = paste0(new_pathways_workbook_path, " - outcomes.csv"),         row.names = FALSE)
    write.csv(df_vulnerability, file = paste0(new_pathways_workbook_path, " - vulnerabilities.csv"),  row.names = FALSE)
    message("New Pathways Workbook CSVs created: ", new_pathways_workbook_path)

  }

}


###################################
message("2_data_cleaning.R complete! Proceed to 3_univariate_analysis.R.")
message("If you created a new Pathways Workbook, set create_new_pathways_workbook = FALSE in config.yml and rename the workbook to match pathways_workbook_name before continuing.")
