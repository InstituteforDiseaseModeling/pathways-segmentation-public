


################################################################################
# PRINCIPAL COMPONENT ANALYSIS (PCA)
################################################################################


###################################
# RUN SETUP
source("1_setup.R")


###################################
# READ PATHWAYS WORKBOOOK
###################################


params_sheet <- readRDS(params_excel_file)

strata_set <- params_sheet %>%
  dplyr::select(strata) %>%
  dplyr::filter(!is.na(strata)) %>%
  distinct()

domain_set <- params_sheet %>%
  dplyr::select(domains) %>%
  dplyr::filter(!is.na(domains)) %>%
  distinct() %>%
  dplyr::mutate(domains = str_replace_all(domains, " ", "."))


vulnerability_vars <- readRDS(vulnerability_excel_file)

vulnerability_vars_pca <- vulnerability_vars %>%
  dplyr::filter(pca_include == 1) %>%
  dplyr::select(vulnerability_variable, short_name, pca_strata, domain_set$domains) %>%
  reshape2::melt(id.vars=c("vulnerability_variable", "short_name", "pca_strata"), variable.name="domain", value.name="domain_include") %>%
  dplyr::filter(domain_include == 1)


###################################
# GET VULNERABILITY VARIABLES DATASET
# CONVERT FACTOR DATA TYPE TO CHARACTER TYPE

vulnerability <- readRDS(file = paste0(vulnerability_file, ".rds")) %>%
  mutate_if(is.factor, as.character)


###################################
#
for (stratum in strata_set$strata){

  pca_strata_input <- vulnerability_vars_pca %>%
    dplyr::filter(tolower(pca_strata) %in% c("both", "all", stratum))

  if (nrow(pca_strata_input) == 0){
    stop("No variables selected for PCA. Select variables in the Pathways Workbook.")
  }


  vulnerability_input <- vulnerability %>%
    dplyr::filter(strata == stratum)

  file_name = paste0(pca_plots, "pca_plots_", stratum, ".pdf")
  fun_gen_pca_output(df=vulnerability_input, stratum=stratum, pca_varlist=pca_strata_input, file_name=file_name)

}


###################################
print("5_principal_component_analysis.R script complete! Proceed to run 6_latent_class_analysis.R script after updating Pathways Workbook. Refer to the README for instructions if needed.")







