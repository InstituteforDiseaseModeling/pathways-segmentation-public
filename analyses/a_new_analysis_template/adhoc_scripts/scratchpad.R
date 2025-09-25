

vulnerability <- readRDS(file = paste0(vulnerability_file, ".rds"))

vulnerability_sheet <- readRDS(vulnerability_excel_file) %>%
  dplyr::filter(univariate_include == 1) %>%
  dplyr::select(vulnerability_variable, short_name) %>%
  setNames(c("indicator", "short_name")) %>%
  distinct()

setdiff(names(vulnerability), vulnerability_sheet$indicator)


# SURVEY DATA
IR$v021
IR$v023
IR$v024
