


################################################################################
# IMPORT DATA
################################################################################


###################################
# READ IN SURVEY | DHS | PATHWAYS

IR <- data.table(read.dta13(file = paste0(data_path, config::get("dhs_ir_file")), fromEncoding="utf-8")) %>% dplyr::mutate(survey = config::get("survey_name"))
BR <- data.table(read.dta13(file = paste0(data_path, config::get("dhs_br_file")), fromEncoding="utf-8")) %>% dplyr::mutate(survey = config::get("survey_name"))
KR <- data.table(read.dta13(file = paste0(data_path, config::get("dhs_kr_file")), fromEncoding="utf-8")) %>% dplyr::mutate(survey = config::get("survey_name"))
HH <- data.table(read.dta13(file = paste0(data_path, config::get("dhs_hh_file")), fromEncoding="utf-8")) %>% dplyr::mutate(survey = config::get("survey_name"))
# MR <- data.table(read.dta13(file = paste0(data_path, config::get("dhs_mr_file")), fromEncoding="utf-8")) %>% dplyr::mutate(survey = config::get("survey_name"))
# survey <-


###################################
# SAVE AS RDS

saveRDS(IR, file = paste0(data_path, "IR.rds"))
saveRDS(BR, file = paste0(data_path, "BR.rds"))
saveRDS(KR, file = paste0(data_path, "KR.rds"))
saveRDS(HH, file = paste0(data_path, "HH.rds"))
# saveRDS(MR, file = paste0(data_path, "MR.rds"))
# saveRDS(survey, file = )


###################################
print("1_import_data.R script complete!  Survey data can now be loaded directly in 2_data_cleaning.R")





