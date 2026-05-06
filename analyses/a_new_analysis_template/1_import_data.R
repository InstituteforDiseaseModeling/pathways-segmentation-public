################################################################################
# IMPORT DATA
# Reads DHS .dta files (or a single survey file) and saves as .rds for faster
# subsequent loads.  DHS files are read in parallel where multiple are present.
################################################################################


###################################
# RUN SETUP
source("1_setup.R")


###################################
# DEFINE DHS FILE MAP
# Maps short names to config keys.  Only entries with a non-empty config value
# are included, so missing files are skipped cleanly rather than erroring.

dhs_file_map <- list(
  IR = config::get("dhs_ir_file"),
  BR = config::get("dhs_br_file"),
  KR = config::get("dhs_kr_file"),
  HH = config::get("dhs_hh_file"),
  MR = config::get("dhs_mr_file"),
  PR = config::get("dhs_pr_file")
)

# Keep only files that are actually configured
dhs_file_map <- Filter(function(f) !is.null(f) && nchar(trimws(f)) > 0, dhs_file_map)

survey_file <- config::get("survey_file")
use_survey_file <- !is.null(survey_file) && nchar(trimws(survey_file)) > 0


###################################
# IMPORT DHS FILES IN PARALLEL

if (length(dhs_file_map) > 0) {

  message("Importing ", length(dhs_file_map), " DHS file(s) in parallel: ",
          paste(names(dhs_file_map), collapse = ", "))

  n_cores <- min(length(dhs_file_map), max(1, parallel::detectCores() - 1))
  cl <- parallel::makeCluster(n_cores)

  # Export the data path to each worker
  parallel::clusterExport(cl, varlist = c("data_path", "survey_name", "dhs_file_map"), envir = environment())

  results <- parallel::parLapply(cl, names(dhs_file_map), function(nm) {

    library(haven)

    file_name <- dhs_file_map[[nm]]  # now available via clusterExport
    full_path <- paste0(data_path, file_name)

    if (!file.exists(full_path)) {
      warning(nm, ": file not found at ", full_path, " — skipping.")
      return(NULL)
    }

    message(nm, ": reading ", file_name, "...")
    df <- haven::read_dta(full_path)

    # Convert labelled columns to character for compatibility downstream
    df <- haven::as_factor(df, levels = "labels")
    df$survey <- survey_name

    # Save as RDS
    out_path <- paste0(data_path, nm, ".rds")
    saveRDS(df, file = out_path)
    message(nm, ": saved to ", out_path)

    return(nm)

  })

  parallel::stopCluster(cl)

  imported <- unlist(Filter(Negate(is.null), results))
  message("Import complete. Files saved: ", paste(imported, collapse = ", "))


  ###################################
  # IMPORT SINGLE SURVEY FILE (NON-DHS)

} else if (use_survey_file) {

  message("Importing survey file: ", survey_file)
  full_path <- paste0(data_path, survey_file)

  if (!file.exists(full_path)) {
    stop("Survey file not found at: ", full_path)
  }

  df <- haven::read_dta(full_path)
  df <- haven::as_factor(df, levels = "labels")
  df$survey <- survey_name

  saveRDS(df, file = paste0(data_path, "survey.rds"))
  message("Survey file saved as survey.rds")

} else {

  stop("No data files configured. Set at least one DHS file or survey_file in config.yml.")

}


###################################
print("1_import_data.R complete!")
