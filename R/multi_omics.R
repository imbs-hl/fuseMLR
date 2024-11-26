#' Simulated multiomics data for 70 training participants and 23 testing participants,
#' each with an effect size of 20 on each layer. Each layer includes 50 participants for
#' training and 20 for testing. Participants do not perfectly overlap across layers.
#' The simulation is based on the R package \code{interSIM}.

#'
#' The dataset is a list containing training and testing data,
#' called \code{training} and \code{testing} respectively. Each data is a list
#' containing the following multi_omics at each layer.
#'
#' \itemize{
#'   \item \code{methylation}: A \code{data.frame} containing the simulated methylation dataset.
#'   \item \code{genexpr}    : A \code{data.frame} containing the gene expression dataset.
#'   \item \code{proteinexpr}: A \code{data.frame} containing the protein expression dataset.
#'   \item \code{target}: A \code{data.frame} with two columns, containing patient IDs and values of target variable.
#' }
#'
#' @docType data
#' @keywords datasets
#' @name multi_omics
#' @usage data(multi_omics)
#' @format A list with training and testing data contaning methylation, gene expressions and protein expressions data.
"multi_omics"
