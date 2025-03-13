#' Process Drug-Related ICD-10 Codes in Claims Data
#'
#' This function identifies drug-related diagnoses in healthcare claims data using ICD-10-CM codes.
#' It creates binary indicators for various substance use disorders, poisonings, and related conditions.
#'
#' @param data A data frame containing ICD-10 diagnosis codes
#' @param dx_vars A character vector containing the names of variables in the data that contain ICD-10 diagnosis codes.
#'                Default is "I10_DX1" through "I10_DX58", which are common variable names in claims datasets.
#'
#' @return The original data frame with additional columns for each drug-related indicator:
#' \itemize{
#'   \item Alcohol-related: alcohol_use_disorder, alcohol_withdrawal, alcohol_psychosis, alcohol_other, alcohol_poisoning
#'   \item Opioid-related: opioid_use_disorder, opioid_withdrawal, opioid_psychosis, opioid_other, opioid_poisoning,
#'         heroin_poisoning, methadone_poisoning, fentanyl_poisoning
#'   \item Cannabis-related: cannabis_use_disorder, cannabis_withdrawal, cannabis_psychosis, cannabis_other, cannabis_poisoning
#'   \item Sedative-related: sedative_use_disorder, sedative_withdrawal, sedative_psychosis, sedative_other, sedative_poisoning
#'   \item Cocaine-related: cocaine_use_disorder, cocaine_withdrawal, cocaine_psychosis, cocaine_other, cocaine_poisoning
#'   \item Stimulant-related: stimulant_use_disorder, stimulant_withdrawal, stimulant_psychosis, stimulant_other, stimulant_poisoning
#'   \item Hallucinogen-related: hallucinogen_use_disorder, hallucinogen_withdrawal, hallucinogen_psychosis, hallucinogen_other, hallucinogen_poisoning
#'   \item Nicotine-related: nicotine_use_disorder, nicotine_withdrawal, nicotine_other, nicotine_poisoning
#'   \item Inhalant-related: inhalant_use_disorder, inhalant_withdrawal, inhalant_psychosis, inhalant_other, inhalant_poisoning
#'   \item Multiple/Other drugs: polydrug_use_disorder, polydrug_withdrawal, polydrug_psychosis, polydrug_other, polydrug_poisoning
#'   \item Other specific drug categories: antidepressant_poisoning, antipsychotic_poisoning
#'   \item Neonatal conditions: neonatal_withdrawal, neonatal_exposure
#'   \item Summary indicators: any_drug_use_disorder, any_drug_withdrawal, any_drug_poisoning, any_drug_psychosis, any_opioid_related
#' }
#'
#' @details
#' This function is based on the ICD-10-CM coding guidelines from the National Center for Health Statistics (NCHS).
#' It searches for specific patterns in the diagnosis fields to identify various drug-related conditions.
#'
#' The summary indicators combine individual flags to provide broader categories of drug-related diagnoses:
#' - any_drug_use_disorder: Patient has any substance use disorder
#' - any_drug_withdrawal: Patient has any withdrawal syndrome
#' - any_drug_poisoning: Patient has any drug poisoning
#' - any_drug_psychosis: Patient has any drug-induced psychosis
#' - any_opioid_related: Patient has any opioid-related condition
#'
#' @examples
#' \dontrun{
#' # Using default ICD10 variable names (I10_DX1:I10_DX58)
#' processed_data <- process_drug_icd10_vectorized(claims_data)
#'
#' # Using custom ICD10 variable names
#' custom_vars <- c("DX1", "DX2", "DX3", "DX4", "DX5")
#' processed_data <- process_drug_icd10_vectorized(claims_data, dx_vars = custom_vars)
#' }
#'
#' @importFrom stringr str_detect
#' @export
process_drug_icd10_vectorized <- function(data, dx_vars = paste0("I10_DX", 1:58)) {
  # Check for stringr package
  if (!requireNamespace("stringr", quietly = TRUE)) {
    stop("The stringr package is required for this function. Please install it with install.packages('stringr')")
  }

  # Use the provided dx_vars (default to "I10_DX1" to "I10_DX58")
  dx_vars <- dx_vars[dx_vars %in% names(data)]
  if (length(dx_vars) == 0) {
    stop("None of the specified ICD10 variable names were found in the data.")
  }

  # Helper function to check for a pattern across the chosen diagnosis columns.
  # Returns an integer vector (0/1) for each row.
  check_pattern <- function(pattern) {
    matches <- sapply(dx_vars, function(col) {
      res <- stringr::str_detect(data[[col]], pattern)
      res[is.na(res)] <- FALSE
      res
    })
    if (!is.matrix(matches)) {
      return(as.integer(matches))
    } else {
      return(as.integer(apply(matches, 1, any)))
    }
  }

  # --- Compute drug-specific indicators ---

  # Alcohol
  data$alcohol_use_disorder <- check_pattern("^F10[0123]")
  data$alcohol_withdrawal   <- check_pattern("^F104")
  data$alcohol_psychosis    <- check_pattern("^F105")
  data$alcohol_other        <- check_pattern("^F10[6-9]")
  data$alcohol_poisoning    <- check_pattern("^T51")

  # Opioids
  data$opioid_use_disorder  <- check_pattern("^F11[0123]")
  data$opioid_withdrawal    <- check_pattern("^F114")
  data$opioid_psychosis     <- check_pattern("^F115")
  data$opioid_other         <- check_pattern("^F11[6-9]")
  data$opioid_poisoning     <- check_pattern("^T40[0246]")
  data$heroin_poisoning     <- check_pattern("^T401")
  data$methadone_poisoning  <- check_pattern("^T403")
  data$fentanyl_poisoning   <- check_pattern("^T404")

  # Cannabis
  data$cannabis_use_disorder <- check_pattern("^F12[0123]")
  data$cannabis_withdrawal   <- check_pattern("^F124")
  data$cannabis_psychosis    <- check_pattern("^F125")
  data$cannabis_other        <- check_pattern("^F12[6-9]")
  data$cannabis_poisoning    <- check_pattern("^T407")

  # Sedatives
  data$sedative_use_disorder <- check_pattern("^F13[0123]")
  data$sedative_withdrawal   <- check_pattern("^F134")
  data$sedative_psychosis    <- check_pattern("^F135")
  data$sedative_other        <- check_pattern("^F13[6-9]")
  data$sedative_poisoning    <- check_pattern("^T42[3467]")

  # Cocaine
  data$cocaine_use_disorder  <- check_pattern("^F14[0123]")
  data$cocaine_withdrawal    <- check_pattern("^F144")
  data$cocaine_psychosis     <- check_pattern("^F145")
  data$cocaine_other         <- check_pattern("^F14[6-9]")
  data$cocaine_poisoning     <- check_pattern("^T405")

  # Stimulants (Other)
  data$stimulant_use_disorder <- check_pattern("^F15[0123]")
  data$stimulant_withdrawal   <- check_pattern("^F154")
  data$stimulant_psychosis    <- check_pattern("^F155")
  data$stimulant_other        <- check_pattern("^F15[6-9]")
  data$stimulant_poisoning    <- check_pattern("^T436")

  # Hallucinogens
  data$hallucinogen_use_disorder <- check_pattern("^F16[0123]")
  data$hallucinogen_withdrawal   <- check_pattern("^F164")
  data$hallucinogen_psychosis    <- check_pattern("^F165")
  data$hallucinogen_other        <- check_pattern("^F16[6-9]")
  data$hallucinogen_poisoning    <- check_pattern("^T40[89]")

  # Nicotine
  data$nicotine_use_disorder <- check_pattern("^F17[0123]")
  data$nicotine_withdrawal   <- check_pattern("^F174")
  data$nicotine_other        <- check_pattern("^F17[6-9]")
  data$nicotine_poisoning    <- check_pattern("^T652")

  # Inhalants
  data$inhalant_use_disorder <- check_pattern("^F18[0123]")
  data$inhalant_withdrawal   <- check_pattern("^F184")
  data$inhalant_psychosis    <- check_pattern("^F185")
  data$inhalant_other        <- check_pattern("^F18[6-9]")
  data$inhalant_poisoning    <- check_pattern("(^T52)|(^T53)")

  # Multiple/Other drugs
  data$polydrug_use_disorder <- check_pattern("^F19[0123]")
  data$polydrug_withdrawal   <- check_pattern("^F194")
  data$polydrug_psychosis    <- check_pattern("^F195")
  data$polydrug_other        <- check_pattern("^F19[6-9]")
  data$polydrug_poisoning    <- check_pattern("^T509")

  # Additional specific categories
  data$antidepressant_poisoning <- check_pattern("^T43[012]")
  data$antipsychotic_poisoning  <- check_pattern("^T43[345]")

  # Neonatal
  data$neonatal_withdrawal   <- check_pattern("^P961")
  data$neonatal_exposure     <- check_pattern("^P04[1234]|^P049")

  # --- Summary indicators (combine individual flags) ---
  data$any_drug_use_disorder <- as.integer(
    (data$alcohol_use_disorder == 1) |
      (data$opioid_use_disorder == 1) |
      (data$cannabis_use_disorder == 1) |
      (data$sedative_use_disorder == 1) |
      (data$cocaine_use_disorder == 1) |
      (data$stimulant_use_disorder == 1) |
      (data$hallucinogen_use_disorder == 1) |
      (data$nicotine_use_disorder == 1) |
      (data$inhalant_use_disorder == 1) |
      (data$polydrug_use_disorder == 1)
  )

  data$any_drug_withdrawal <- as.integer(
    (data$alcohol_withdrawal == 1) |
      (data$opioid_withdrawal == 1) |
      (data$cannabis_withdrawal == 1) |
      (data$sedative_withdrawal == 1) |
      (data$cocaine_withdrawal == 1) |
      (data$stimulant_withdrawal == 1) |
      (data$hallucinogen_withdrawal == 1) |
      (data$nicotine_withdrawal == 1) |
      (data$inhalant_withdrawal == 1) |
      (data$polydrug_withdrawal == 1)
  )

  data$any_drug_poisoning <- as.integer(
    (data$alcohol_poisoning == 1) |
      (data$opioid_poisoning == 1) |
      (data$cannabis_poisoning == 1) |
      (data$sedative_poisoning == 1) |
      (data$cocaine_poisoning == 1) |
      (data$stimulant_poisoning == 1) |
      (data$hallucinogen_poisoning == 1) |
      (data$nicotine_poisoning == 1) |
      (data$inhalant_poisoning == 1) |
      (data$polydrug_poisoning == 1) |
      (data$antidepressant_poisoning == 1) |
      (data$antipsychotic_poisoning == 1)
  )

  data$any_drug_psychosis <- as.integer(
    (data$alcohol_psychosis == 1) |
      (data$opioid_psychosis == 1) |
      (data$cannabis_psychosis == 1) |
      (data$sedative_psychosis == 1) |
      (data$cocaine_psychosis == 1) |
      (data$stimulant_psychosis == 1) |
      (data$hallucinogen_psychosis == 1) |
      (data$inhalant_psychosis == 1) |
      (data$polydrug_psychosis == 1)
  )

  data$any_opioid_related <- as.integer(
    (data$opioid_use_disorder == 1) |
      (data$opioid_withdrawal == 1) |
      (data$opioid_psychosis == 1) |
      (data$opioid_other == 1) |
      (data$opioid_poisoning == 1) |
      (data$heroin_poisoning == 1) |
      (data$methadone_poisoning == 1) |
      (data$fentanyl_poisoning == 1)
  )

  return(data)
}
