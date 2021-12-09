#' Drug Resistance Identified for Pseudomonas Aeruginosa From
#' Build in Canada Innovation Program Experiment.
#'
#' The experiment identifying the whole genome seqeunce of 1984 Pseudomonas aeruginosa isolates.
#' Subsequent data cleaning and RGI blast process obtain their ARO name, drug class,
#' antimicrobial agents, and resistant mechanism information conducted in 2021.
#'
#' @source PHAC’s National Microbiology Laboratory (NML)
#'         https://www.canada.ca/en/public-health/programs/national-microbiology-laboratory.html
#'
#' @format A matrix with columns:
#' \describe{
#'  \item{Sample ID}{The sample ID of each isolate}
#'  \item{ARO name}{The antimicrobial resistant gene predicted for each sample}
#'  \item{Drug Class}{The name of the drug class that the sample resistant}
#'  \item{Antimicrobial Agent}{The antimicrobial agent of each sample predicted from the whole genome sequence}
#'  \item{Resistance Mechanism}{The resistance mechanismm that sample used to develop resistance}
#' }
#' @examples
#' \dontrun{
#'  DrugResistance
#' }
"DrugResistance"



#' WHONET 2019 software: <http://www.whonet.org/software.html>
#'
#' The experiment provided RSI information for each patient together with metadata.
#'
#' @source AMR(for R) package on Github
#'         https://github.com/msberends/AMR/blob/main/data/WHONET.rda
#'
#' @format A matrix with columns:
#' \describe{
#'  \item{Identification number}{The ID of each patient}
#'  \item{Specimen number}{The ID of the specimen}
#'  ...
#'  \item{Country}{The country of origin}
#'  \item{AMP_ND10:CIP_EE}{Different antibiotics.}
#'  }
#' @examples
#' \dontrun{
#'  pca_data
#' }
"pca_data"

#' Intensity of Antibiotic Resistance for Pseudomonas Aeruginosa From
#' RSI Experiment.
#'
#' The experiment identifying the intensity of antibiotic resistance of Pseudomonas Aeruginosa conducted in 2020.
#'
#' @source PHAC’s National Microbiology Laboratory (NML)
#'         https://www.canada.ca/en/public-health/programs/national-microbiology-laboratory.html
#'
#' @format A matrix with columns:
#' \describe{
#'  \item{AMK}{Whether the isolate is susceptable to amikacin}
#'  ...
#'  \item{TZP}{Whether the isolate is susceptable to tazobactam}
#' }
#' @examples
#' \dontrun{
#'  RSI_table
#' }
"RSI_table"

# [END] written by Xiaolin Zhou
