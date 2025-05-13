


################################################################################
# TYPING TOOL
################################################################################
set.seed(99)

###################################
# RUN SETUP
source("1_setup.R")

nreps = 20


###################################
# SET PARAMETERS
params_sheet <- readRDS(params_excel_file)

final_models <- params_sheet %>%
  dplyr::select(strata, final_model) %>%
  dplyr::filter(!is.na(strata))

strata_set <- unique(final_models$strata)


###################################
# GET DATA INPUTS
vulnerability_vars <- readRDS(vulnerability_excel_file)

vulnerability_vars_tt <- vulnerability_vars %>%
  dplyr::filter(typing_tool_include == 1)


###################################
# LOOP OVER STRATA


for (stratum in strata_set){
  print(paste0("Generating typing tool for ", stratum))

  final_seg <- final_models %>% dplyr::filter(strata == stratum) %>% pull(final_model)

  path = paste0(lca_path, stratum, "_outcomes_vulnerability_class.rds")
  df_sol <- readRDS(path)

  df_sol$n_class <- df_sol %>% pull(eval(parse(text=final_seg)))

  tt_vars <- vulnerability_vars_tt %>%
    dplyr::filter(typing_tool_strata %in% c("both", "all", stratum)) %>%
    dplyr::select(vulnerability_variable) %>%
    setNames(c("variable"))

  tt_vars <- names(df_sol)[(names(df_sol) %in% unique(c("n_class", tt_vars$variable)))]


  fun_gen_typing_tool(df=df_sol, stratum=stratum, tt_vars=tt_vars, final_seg=final_seg)


}









