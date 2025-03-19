


################################################################################
# IMPORT DATA
################################################################################


###################################
# READ IN SURVEY | DHS

IR <- data.table(read.dta13(file = paste0(data_path, "ETIR71FL.DTA"), fromEncoding="utf-8")) %>% dplyr::mutate(survey = "ET71FL")
BR <- data.table(read.dta13(file = paste0(data_path, "ETBR71FL.DTA"), fromEncoding="utf-8")) %>% dplyr::mutate(survey = "ET71FL")
KR <- data.table(read.dta13(file = paste0(data_path, "ETKR71FL.DTA"), fromEncoding="utf-8")) %>% dplyr::mutate(survey = "ET71FL")
HH <- data.table(read.dta13(file = paste0(data_path, "ETHR71FL.DTA"), fromEncoding="utf-8")) %>% dplyr::mutate(survey = "ET71FL")
MR <- data.table(read.dta13(file = paste0(data_path, "ETMR71FL.DTA"), fromEncoding="utf-8")) %>% dplyr::mutate(survey = "ET71FL")


###################################
# SAVE AS RDS

saveRDS(IR, file = paste0(data_path, "IR.rds"))
saveRDS(BR, file = paste0(data_path, "BR.rds"))
saveRDS(KR, file = paste0(data_path, "KR.rds"))
saveRDS(HH, file = paste0(data_path, "HH.rds"))
saveRDS(MR, file = paste0(data_path, "MR.rds"))


###################################
print("1_import_data.R script complete!  Survey data can now be loaded directly in 2_data_cleaning.R")





