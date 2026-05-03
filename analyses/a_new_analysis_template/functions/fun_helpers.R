################################################################################
# SHARED HELPER FUNCTIONS
# These utilities are used across multiple phases of the Pathways workflow.
# This file is sourced automatically by 1_setup.R via the functions/ loop.
################################################################################


################################################################################
# recode_binary_yn()
#
# Converts binary 0/1 columns (or "yes"/"no" variants) to standardised
# "Yes" / "No" character values.  Only columns whose non-NA values are
# exclusively in {0, 1} or {"yes", "no"} (case-insensitive) are touched;
# all other columns are returned unchanged.
#
# Args:
#   df  : a data.frame or tibble
#
# Returns:
#   The same data.frame with qualifying columns recoded to "Yes" / "No".
################################################################################

recode_binary_yn <- function(df) {

  for (col in names(df)) {

    vals <- na.omit(df[[col]])

    is_binary_01 <- length(vals) > 0 &&
      all(vals %in% c(0, 1)) &&
      all(c(0, 1) %in% vals)

    is_binary_yn <- length(vals) > 0 &&
      all(tolower(as.character(vals)) %in% c("yes", "no"))

    if (is_binary_01 || is_binary_yn) {
      message("  recode_binary_yn: recoding column '", col, "'")
      df[[col]] <- dplyr::case_when(
        df[[col]] %in% c(1, "yes", "Yes", "YES") ~ "Yes",
        df[[col]] %in% c(0, "no",  "No",  "NO")  ~ "No",
        TRUE ~ as.character(df[[col]])
      )
    }
  }

  df
}


################################################################################
# sanitize_varname()
#
# Replaces characters that are invalid or problematic in R column names and
# poLCA / ggplot variable labels.  Used in the PCA phase when building dummy-
# variable column names from categorical factor levels.
#
# Args:
#   x  : character vector of variable/column names
#
# Returns:
#   Character vector with the same length as x, names sanitized.
################################################################################

sanitize_varname <- function(x) {
  x <- gsub(" ",    "_",     x)
  x <- gsub("'",    "",      x)
  x <- gsub("-",    "",      x)
  x <- gsub("/",    "_",     x)
  x <- gsub("\\+",  "p",     x)
  x <- gsub("<",    "lt",    x)
  x <- gsub(">",    "gt",    x)
  x <- gsub("=",    "equal", x)
  x <- gsub(",",    "",      x)
  x
}


################################################################################
# validate_vars()
#
# Checks that every variable in `selected` actually exists in `available`.
# Stops with an informative error listing any missing names so the analyst
# can fix the Pathways Workbook before the pipeline runs.
#
# Args:
#   selected  : character vector of variable names chosen in the workbook
#   available : character vector of names present in the data (e.g. names(df))
#   context   : short string identifying the calling phase, used in the error
#               message (e.g. "LCA", "PCA", "Typing tool")
#
# Returns:
#   Invisibly TRUE if all variables are present; stops otherwise.
################################################################################

validate_vars <- function(selected, available, context = "Analysis") {

  missing <- setdiff(selected, available)

  if (length(missing) > 0) {
    stop(
      context, ": the following variables are listed in the Pathways Workbook ",
      "but were not found in the data.\n",
      "  Missing: ", paste(missing, collapse = ", "), "\n",
      "  Check for typos in the workbook or confirm the variable was generated ",
      "in the data cleaning step.",
      call. = FALSE
    )
  }

  invisible(TRUE)
}
