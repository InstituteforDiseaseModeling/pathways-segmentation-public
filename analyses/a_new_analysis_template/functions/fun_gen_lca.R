


################################################################################
# GENERATE LCA OUTPUT
################################################################################

###################################
# RUN SETUP


###################################
# DEFINE FUNCTION TO GENERATE LCA OUTPUT
###################################


fun_gen_lca <- function(df=NULL, input_vars=NULL, nreps=NULL, output_path=NULL){

  #
  stratum = df$strata[[1]]

  #
  lca_input_names <- names(df)[(names(df) %in% input_vars$variable)]
  lca_input <- df %>%
    subset(select=c("caseid", "survey", "strata", lca_input_names)) %>%
    na.omit()

  # SAVE INPUT DF FOR LCA
  path = paste0(output_path, "nreps", nreps, "/", stratum, "_lca_input.rds")
  dir.create(dirname(path), showWarnings = F, recursive = T)
  saveRDS(lca_input, path)

  #
  f <- do.call(cbind, lca_input[,lca_input_names])~1

  ### FITTING N CLASSES
  for (i in 2:10){
    print(paste0(stratum, "_", i, "_classes"))

    name <- paste0(stratum, "_LCA", i)
    data <- poLCA(f, data=lca_input, nclass=i, maxiter=3000, nrep=nreps)
    assign(name, data)

    path = paste0(output_path, "nreps", nreps, "/", name, ".rds")
    dir.create(dirname(path), showWarnings = F, recursive = T)
    saveRDS(data, path)

  }

}

