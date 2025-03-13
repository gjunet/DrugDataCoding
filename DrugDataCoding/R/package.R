#' @keywords internal
"_PACKAGE"

#' DrugDataCoding: Functions for Processing Drug-Related ICD-10 Codes
#'
#' This package provides functions for processing drug-related ICD-10 codes in
#' healthcare claims data and mortality records. It creates standardized indicator
#' variables for substance use disorders, drug overdoses, and specific drug poisonings
#' based on coding definitions from the National Center for Health Statistics (NCHS)
#' and the Centers for Disease Control and Prevention (CDC).
#'
#' @section Main functions:
#' The package includes two main functions:
#'
#' \code{\link{process_drug_icd10_vectorized}}: For processing drug-related ICD-10 codes in healthcare claims data.
#'
#' \code{\link{process_death_records_extended}}: For processing drug-related ICD-10 codes in mortality data.
#'
#' @section ICD-10 Code Definitions:
#' This package uses standardized ICD-10 code definitions based on CDC guidelines
#' for identifying drug-related conditions. The specific codes used for each indicator
#' are documented in the respective function documentation.
#'
#' @docType package
#' @name DrugDataCoding-package
#' @aliases DrugDataCoding
NULL
