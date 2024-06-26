#' Simulated multiomics data for 100 training and 100 testing participants
#' the same effect size (20) on each layer. Simulation is based on the R package
#'  \code{interSIM}.
#'
#' The dataset is a list containing training and testing data,
#' called \code{training} and \code{testing} respectively. Each data is a list
#' containing the following entities at each layer.
#'
#' \itemize{
#'   \item \code{methylation}: A \code{data.frame} containing the simulated methylation dataset.
#'   \item \code{genexpr}    : A \code{data.frame} containing the gene expression dataset.
#'   \item \code{proteinexpr}: A \code{data.frame} containing the protein expression dataset.
#' }
#'
#' @docType data
#' @keywords datasets
#' @name entities
#' @usage data(entities)
#' @format A list with training and testing data contaning methylation, gene expressions and protein expressions data.
"entities"
