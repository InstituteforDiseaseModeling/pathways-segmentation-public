


################################################################################
# IMPORT DATA
################################################################################


###################################
# READ IN SURVEY | DHS | PATHWAYS

# WOMAN'S RECODE
if (file.exists(paste0(data_path, config::get("dhs_ir_file")))) {
  IR <- data.table(read.dta13(file = paste0(data_path, config::get("dhs_ir_file")), fromEncoding="utf-8")) %>% dplyr::mutate(survey = config::get("survey_name"))
}

# BIRTH RECODE
if (file.exists(paste0(data_path, config::get("dhs_br_file")))) {
  BR <- data.table(read.dta13(file = paste0(data_path, config::get("dhs_br_file")), fromEncoding="utf-8")) %>% dplyr::mutate(survey = config::get("survey_name"))
}

# CHILD RECODE
if (file.exists(paste0(data_path, config::get("dhs_kr_file")))) {
  KR <- data.table(read.dta13(file = paste0(data_path, config::get("dhs_kr_file")), fromEncoding="utf-8")) %>% dplyr::mutate(survey = config::get("survey_name"))
}

# HOUSEHOLD RECODE
if (file.exists(paste0(data_path, config::get("dhs_hh_file")))) {
  HH <- data.table(read.dta13(file = paste0(data_path, config::get("dhs_hh_file")), fromEncoding="utf-8")) %>% dplyr::mutate(survey = config::get("survey_name"))
}

# MEN'S RECODE
if (file.exists(paste0(data_path, config::get("dhs_mr_file")))) {
  MR <- data.table(read.dta13(file = paste0(data_path, config::get("dhs_mr_file")), fromEncoding="utf-8")) %>% dplyr::mutate(survey = config::get("survey_name"))
}


# survey <-


###################################
# SAVE AS RDS

if (exists("IR")) {saveRDS(IR, file = paste0(data_path, "IR.rds"))}
if (exists("BR")) {saveRDS(BR, file = paste0(data_path, "BR.rds"))}
if (exists("KR")) {saveRDS(KR, file = paste0(data_path, "KR.rds"))}
if (exists("HH")) {saveRDS(HH, file = paste0(data_path, "HH.rds"))}
if (exists("MR")) {saveRDS(MR, file = paste0(data_path, "MR.rds"))}
# saveRDS(survey, file = )


###################################
print("1_import_data.R script complete!  Survey data can now be loaded directly in 2_data_cleaning.R")





