#' \code{SegmentedCellExperiment} package
#'
#' This package describes an S4 class for storing data from segmented 
#' imaging cytometry and spatial omics data. It extends DataFrame and defines 
#' methods that take advantage of DataFrame nesting to represent elements of 
#' cell-based experiments with spatial orientation that are commonly 
#' encountered. This object is able to store information on a cell's spatial 
#' location, cellType, morphology, intensity of gene/protein marks as well as 
#' image level phenotype information.
#'
#' Development version can be found on 
#' \href{https://github.com/ellispatrick/SegmentedCellExperiment}{GitHub}
#'
#' @docType package
#' @name SegmentedCellExperiment-package
NULL

## quiets concerns of R CMD check re: the x's etc that appear in pipelines
#if(getRversion() >= "2.15.1")  utils::globalVariables(c("x", "y", "cellType"))