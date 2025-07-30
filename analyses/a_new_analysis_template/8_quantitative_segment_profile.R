

################################################################################
# SEGMENT PROFILING PLOTS
################################################################################


###################################
# RUN SETUP
source("1_setup.R")

nreps = 50


###################################
# SET PARAMETERS
params_sheet <- readRDS(params_excel_file)

final_models <- params_sheet %>%
  dplyr::select(strata, final_model) %>%
  dplyr::filter(!is.na(strata))

strata_set <- unique(final_models$strata)


###################################
# GET INPUT SHAPE FILE FOR MAPPING
shp_file <- read_sf(shp_path)

shp_file <- shp_file %>%
  mutate(NAME_1 = stringi::stri_trans_general(NAME_1, "latin-ascii"))


###################################
#


for (stratum in strata_set){
  print(stratum)

  # SET SELECTED MODEL TO PROFILE
  n_class <- final_models %>%
    dplyr::filter(strata == stratum) %>%
    pull(final_model)
  print(n_class)

  # GET COMBINED DATASET WITH MODELED SEGMENTS
  path = paste0(lca_path, "nreps", nreps, "/", stratum, "_outcomes_vulnerability_class_ranked.rds")
  df <- readRDS(path)

  fun_gen_quantitative_segment_profile(df=df, stratum=stratum, n_class=n_class, shp_file=shp_file)

}


