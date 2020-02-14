#' \code{SegmentedCellExperiment} package
#'
#' S4 Class for segmented spatial 'omics data
#'
#' See the README on
#' \href{https://cran.r-project.org/package=googlesheets/README.html}{CRAN}
#' or \href{https://github.com/jennybc/googlesheets#readme}{GitHub}
#'
#' @docType package
#' @name SegmentedCellExperiment
NULL

## quiets concerns of R CMD check re: the x's etc that appear in pipelines
if(getRversion() >= "2.15.1")  utils::globalVariables(c("x", "y", "cellType"))