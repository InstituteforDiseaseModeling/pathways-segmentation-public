


################################################################################
# ENVIRONMNET SETUP | LIBRARIES
################################################################################


###################################
# RESTORE LIBRARIES FROM RENV LOCK FILE
if (use_renv == TRUE){
  print("Restoring libraries from renv lock.file")

  tryCatch({

    if (!require("renv")) install.packages("renv")
    renv::restore(prompt = FALSE)
    message("renv environment successfully restored.")

  }, error = function(e) {

    message("renv restore failed: ", e$message)
    message("Continuing with global library.")

  })

} else {

  message("Using global R library (renv not used).")

}


###################################
# LIBRARY INSTALL
if (!require("pacman")) install.packages("pacman")
if (!require("ggsankey")) remotes::install_github("davidsjoberg/ggsankey")
if (!require("ggradar")) remotes::install_github("ricardo-bion/ggradar")


# LOAD ALL LIBRARY DEPENDENCIES
pacman::p_load(dplyr, stringr, reshape2, data.table, survey, ggplot2, broom, jtools, readxl, openxlsx, gridExtra, factoextra,
               poLCA, readstata13, fastDummies, openxlsx, config, ggdist, sf, scatterpie, networkD3, htmlwidgets, remotes,
               conflicted, webshot2, magick, zscorer, haven, ggsankey, ggradar, foreach, doParallel, dunn.test, rpart,
               rpart.plot, caret, rattle)


###################################
print("1_libraries.R script complete!  Libraries loaded for this session.")

