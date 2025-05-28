


################################################################################
# GET DHS DATA
################################################################################




###################################
# DEFINE FUNCTION

gen_dhs_data <- function(source=NULL, collist=NULL, catalog=NULL, schema=NULL, table=NULL, file_path=NULL){
  if (!require("pacman")) install.packages("pacman")


  if (source == "Databricks Platform"){

    print("Reading from Databricks Unity Catalog")
    library(SparkR)
    data <- SparkR::sql(paste0("SELECT ", collist, " FROM ", catalog, ".", schema, ".", table)) %>%
      SparkR::as.data.frame()

  } else if (source == "Databricks Local") {

    print("Reading from Databricks Unity Catalog into local IDE")
    pacman::p_load(RODBC)
    conn <- odbcConnect("IDMAzureDatabricks_DSN")

    data <- sqlQuery(conn, paste0("SELECT ", collist, " FROM ", catalog, ".", schema, ".", table))
    # odbcClose(conn)

  } else if (source == "Local File") {

    print("Reading from local file")
    pacman::p_load(readstata13)
    data <- read.dta13(file_path, fromEncoding="utf-8")

  } else {

    print("Options for Source: Databricks Platform, Databricks Local, Local File")

  }

}

