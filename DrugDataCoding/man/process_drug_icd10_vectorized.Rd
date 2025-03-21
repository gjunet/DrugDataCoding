% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/claims_coding.R
\name{process_drug_icd10_vectorized}
\alias{process_drug_icd10_vectorized}
\title{Process Drug-Related ICD-10 Codes in Claims Data}
\usage{
process_drug_icd10_vectorized(data, dx_vars = paste0("I10_DX", 1:58))
}
\arguments{
\item{data}{A data frame containing ICD-10 diagnosis codes}

\item{dx_vars}{A character vector containing the names of variables in the data that contain ICD-10 diagnosis codes.
Default is "I10_DX1" through "I10_DX58", which are common variable names in claims datasets.}
}
\value{
The original data frame with additional columns for each drug-related indicator:
\itemize{
\item Alcohol-related: alcohol_use_disorder, alcohol_withdrawal, alcohol_psychosis, alcohol_other, alcohol_poisoning
\item Opioid-related: opioid_use_disorder, opioid_withdrawal, opioid_psychosis, opioid_other, opioid_poisoning,
heroin_poisoning, methadone_poisoning, fentanyl_poisoning
\item Cannabis-related: cannabis_use_disorder, cannabis_withdrawal, cannabis_psychosis, cannabis_other, cannabis_poisoning
\item Sedative-related: sedative_use_disorder, sedative_withdrawal, sedative_psychosis, sedative_other, sedative_poisoning
\item Cocaine-related: cocaine_use_disorder, cocaine_withdrawal, cocaine_psychosis, cocaine_other, cocaine_poisoning
\item Stimulant-related: stimulant_use_disorder, stimulant_withdrawal, stimulant_psychosis, stimulant_other, stimulant_poisoning
\item Hallucinogen-related: hallucinogen_use_disorder, hallucinogen_withdrawal, hallucinogen_psychosis, hallucinogen_other, hallucinogen_poisoning
\item Nicotine-related: nicotine_use_disorder, nicotine_withdrawal, nicotine_other, nicotine_poisoning
\item Inhalant-related: inhalant_use_disorder, inhalant_withdrawal, inhalant_psychosis, inhalant_other, inhalant_poisoning
\item Multiple/Other drugs: polydrug_use_disorder, polydrug_withdrawal, polydrug_psychosis, polydrug_other, polydrug_poisoning
\item Other specific drug categories: antidepressant_poisoning, antipsychotic_poisoning
\item Neonatal conditions: neonatal_withdrawal, neonatal_exposure
\item Summary indicators: any_drug_use_disorder, any_drug_withdrawal, any_drug_poisoning, any_drug_psychosis, any_opioid_related
}
}
\description{
This function identifies drug-related diagnoses in healthcare claims data using ICD-10-CM codes.
It creates binary indicators for various substance use disorders, poisonings, and related conditions.
}
\details{
This function is based on the ICD-10-CM coding guidelines from the National Center for Health Statistics (NCHS).
It searches for specific patterns in the diagnosis fields to identify various drug-related conditions.

The summary indicators combine individual flags to provide broader categories of drug-related diagnoses:
\itemize{
\item any_drug_use_disorder: Patient has any substance use disorder
\item any_drug_withdrawal: Patient has any withdrawal syndrome
\item any_drug_poisoning: Patient has any drug poisoning
\item any_drug_psychosis: Patient has any drug-induced psychosis
\item any_opioid_related: Patient has any opioid-related condition
}
}
\examples{
\dontrun{
# Using default ICD10 variable names (I10_DX1:I10_DX58)
processed_data <- process_drug_icd10_vectorized(claims_data)

# Using custom ICD10 variable names
custom_vars <- c("DX1", "DX2", "DX3", "DX4", "DX5")
processed_data <- process_drug_icd10_vectorized(claims_data, dx_vars = custom_vars)
}

}
