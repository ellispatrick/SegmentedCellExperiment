#' A basic plot for SegmentedCellExperiment object
#' 
#' This function generates a basic plot of the location and cellType data.
#'
#' @param x A SegmentedCellExperiment object.
#' @param imageID The image that should be plotted
#' 
#' @examples
#' ### Something that resembles cellProfiler data
#'
#' set.seed(51773)
#'
#' n = 10
#'
#' cells <- data.frame(row.names = seq_len(n))
#' cells$ObjectNumber <- seq_len(n)
#' cells$ImageNumber <- rep(1:2,c(n/2,n/2))
#' cells$AreaShape_Center_X <- runif(n)
#' cells$AreaShape_Center_Y <- runif(n)
#' cells$AreaShape_round <- rexp(n)
#' cells$AreaShape_diameter <- rexp(n, 2)
#' cells$Intensity_Mean_CD8 <- rexp(n, 10)
#' cells$Intensity_Mean_CD4 <- rexp(n, 10)
#'
#' cellExp <- SegmentedCellExperiment(cells, cellProfiler = TRUE)
#' 
#' ### Cluster cell types
#' intensities <- intensity(cellExp)
#' kM <- kmeans(intensities,2)
#' cellType(cellExp) <- paste('cluster',kM$cluster, sep = '')
#' 
#' plot(cellExp, imageID=1)
#' 
#' @name plot.SegmentedCellExperiment
#' 
#' @rdname plotSegmentedCellExperiment
#' @importFrom ggplot2 ggplot aes geom_point theme_classic

if (!isGeneric("plot")) setGeneric("plot", function(x, y, ...) standardGeneric("plot"))

setMethod("plot", signature(x = "SegmentedCellExperiment", y = "missing"), function(x, 
    y, ...) {
    plot.SegmentedCellExperiment(x, ...)
})

plot.SegmentedCellExperiment <- function(cellData, imageID = NULL) {
    if (is.null(imageID)) {
        imageID <- imageID(cellData)[1]
    }
    
    loc <- as.data.frame(location(cellData, imageID = imageID))
    if (is.na(loc$cellType[1])) {
        ggplot(loc, aes(x = .data$x, y = .data$y)) + geom_point() + theme_classic()
    } else {
        ggplot(loc, aes(x = .data$x, y = .data$y, colour = cellType)) + geom_point() + 
            theme_classic()
    }
}
