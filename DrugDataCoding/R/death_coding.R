#' Process Drug-Related ICD-10 Codes in Death Records
#'
#' This function processes death records to identify drug-related causes of death using ICD-10 codes.
#' It creates binary indicators for drug overdose deaths and specific drug poisonings based on
#' CDC definitions from the National Center for Health Statistics (NCHS).
#'
#' @param data A data frame containing ICD-10 cause of death codes
#' @param icd_vars A character vector containing the names of variables in the data that contain ICD-10 codes.
#'                Default is "MCOD1" through "MCOD10", which are common multiple causes of death variables in mortality data.
#'
#' @return The original data frame with additional columns for each drug-related indicator:
#' \itemize{
#'   \item Broad indicators: drug_overdose_death, alcohol_attributable_death
#'   \item Poisoning indicators by drug category: alcohol_poisoning, opioid_poisoning,
#'         heroin_poisoning, methadone_poisoning, fentanyl_poisoning, cannabis_poisoning,
#'         sedative_poisoning, cocaine_poisoning, stimulant_poisoning, hallucinogen_poisoning,
#'         nicotine_poisoning, inhalant_poisoning, polydrug_poisoning, antidepressant_poisoning,
#'         antipsychotic_poisoning
#'   \item Summary indicator: any_drug_poisoning
#' }
#'
#' @details
#' This function is based on the ICD-10 coding guidelines from the National Center for Health Statistics (NCHS).
#' It identifies drug overdose deaths based on underlying cause codes (X40-X44, X60-X64, X85, Y10-Y14) and
#' specific drug poisonings based on multiple-cause-of-death codes (T-codes).
#'
#' The function uses regular expressions to search for specific ICD-10 code patterns in the specified
#' variables. The CDC defines drug overdose deaths based on specific underlying causes of death, while
#' the specific drugs involved are identified using T-codes in the multiple causes of death fields.
#'
#' @examples
#' \dontrun{
#' # Using default ICD10 variable names (MCOD1:MCOD10)
#' processed_data <- process_death_records_extended(mortality_data)
#'
#' # Using custom ICD10 variable names
#' custom_vars <- c("CAUSE1", "CAUSE2", "CAUSE3", "CAUSE4")
#' processed_data <- process_death_records_extended(mortality_data, icd_vars = custom_vars)
#' }
#'
#' @references
#' Hedegaard H, Miniño AM, Warner M. Drug overdose deaths in the United States, 1999–2018.
#' NCHS Data Brief, no 356. Hyattsville, MD: National Center for Health Statistics. 2020.
#'
#' @importFrom stringr str_detect
#' @export
process_death_records_extended <- function(data, icd_vars = paste0("MCOD", 1:10)) {
  # Check for stringr package
  if (!requireNamespace("stringr", quietly = TRUE)) {
    stop("The stringr package is required for this function. Please install it with install.packages('stringr')")
  }

  # Keep only those ICD-10 columns that exist in the data
  icd_vars <- icd_vars[icd_vars %in% names(data)]
  if (length(icd_vars) == 0) {
    stop("None of the specified ICD10 variable names were found in the data.")
  }

  # Helper function to check if any ICD-10 field in a row matches the regex pattern.
  # Returns a 0/1 integer indicator for each record.
  check_pattern <- function(pattern) {
    matches <- sapply(icd_vars, function(col) {
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

  # --- Broad Indicators ---

  # Drug overdose deaths (based on underlying cause codes per CDC definitions):
  # X40-X44 (accidental poisoning), X60-X64 (intentional self-poisoning),
  # X85 (assault by drugs), Y10-Y14 (poisoning of undetermined intent)
  data$drug_overdose_death <- check_pattern("^(X4[0-4]|X6[0-4]|X85|Y1[0-4])")

  # Alcohol-attributable deaths (includes a range of alcohol-related codes):
  # F10 (alcohol use disorders), K70 (alcoholic liver disease),
  # X45 (accidental poisoning by alcohol), T51 (toxic effect of alcohol)
  data$alcohol_attributable_death <- check_pattern("^(F10|K70|X45|T51)")

  # --- Poisoning Indicators by Drug Category ---

  # Alcohol poisoning (toxic effect of alcohol)
  data$alcohol_poisoning <- check_pattern("^T51")

  # Opioid poisoning (includes natural, semi-synthetic, synthetic opioids)
  # T40.0, T40.2, T40.4, T40.6 (opium, other opioids, synthetic opioids, other/unspecified narcotics)
  data$opioid_poisoning <- check_pattern("^T40[0246]")

  # Heroin poisoning
  data$heroin_poisoning <- check_pattern("^T401")

  # Methadone poisoning
  data$methadone_poisoning <- check_pattern("^T403")

  # Fentanyl poisoning (subset of synthetic opioids)
  data$fentanyl_poisoning <- check_pattern("^T404")

  # Cannabis poisoning
  data$cannabis_poisoning <- check_pattern("^T407")

  # Sedative poisoning (barbiturates and other sedatives)
  # T42.3, T42.4, T42.6, T42.7 (barbiturates, benzodiazepines, other sedatives, unspecified sedatives)
  data$sedative_poisoning <- check_pattern("^T42[3467]")

  # Cocaine poisoning
  data$cocaine_poisoning <- check_pattern("^T405")

  # Stimulant poisoning (psychostimulants with abuse potential, e.g., methamphetamine)
  data$stimulant_poisoning <- check_pattern("^T436")

  # Hallucinogen poisoning
  # T40.8, T40.9 (lysergide and other hallucinogens)
  data$hallucinogen_poisoning <- check_pattern("^T40[89]")

  # Nicotine poisoning
  data$nicotine_poisoning <- check_pattern("^T652")

  # Inhalant poisoning (organic solvents)
  # T52, T53 (toxic effects of organic solvents and halogenated hydrocarbons)
  data$inhalant_poisoning <- check_pattern("(^T52)|(^T53)")

  # Polydrug poisoning (unspecified drugs/mixed substances)
  data$polydrug_poisoning <- check_pattern("^T509")

  # Additional specific poisoning categories
  # T43.0, T43.1, T43.2 (tricyclic, tetracyclic, and other antidepressants)
  data$antidepressant_poisoning <- check_pattern("^T43[012]")

  # T43.3, T43.4, T43.5 (phenothiazine, butyrophenone, and other antipsychotics)
  data$antipsychotic_poisoning <- check_pattern("^T43[345]")

  # Summary indicator for any drug poisoning (combines all poisoning indicators)
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

  return(data)
}
