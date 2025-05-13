
library(foreach)
library(doParallel)

# 1. Spin up a cluster on all cores
ncores <- parallel::detectCores()
cl <- makeCluster(ncores)
registerDoParallel(cl)

clusterEvalQ(cl, {
  pacman::p_load(dplyr, forcats, reshape2, ggplot2, survey, gridExtra, stringr, config, data.table, broom)
  TRUE
})

# 2. Export your function and data to each worker
clusterExport(cl, varlist = c(
  "gen_exploratory_data_analysis",
  "outcomes_vulnerability",
  "outcomes.list",
  "strata",
  "exploratory_plots"
))

# 3. Run in parallel
results <- foreach(
  m = vulnerability.list,
  .packages = character(0)    # no packages needed
) %dopar% {
  tryCatch(
    gen_exploratory_data_analysis(
      df            = outcomes_vulnerability,
      outcomes.list = outcomes.list,
      measure       = m,
      strata        = strata,
      plot_path     = exploratory_plots
    ),
    error = function(e) {
      message("gen_eda failed for measure ", m, ": ", e$message)
      NULL
    }
  )
}

# 4. Clean up
stopCluster(cl)
