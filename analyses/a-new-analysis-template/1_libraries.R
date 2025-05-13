


################################################################################
# ENVIRONMNET SETUP | LIBRARIES
################################################################################


###################################
# RESTORE LIBRARIES FROM RENV LOCK FILE
# if (!require("renv")) install.packages("renv")
# renv::activate()
# renv::restore()
# renv::deactivate()


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

