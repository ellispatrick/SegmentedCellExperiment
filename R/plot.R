#' The plotting a segmented cell experiment
#' 
#' This function generates a basic plot of the location and cellType data.
#'
#' @param cellData A SegmentedCellExperiment object.
#' @param image The image that should be plotted
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
#' cells$x <- runif(n)
#' cells$y <- runif(n)
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
#' plot(cellExp, image=1)
#' 
#' @export
#' @rdname plot.SegmentedCellExperiment
#' @import ggplot2
plot.SegmentedCellExperiment <- function(cellData, imageID=NULL){
  if(is.null(imageID)){
    imageID <- imageID(cellData)[1]
  }
  
   loc <- as.data.frame(location(cellData, imageID=imageID))
   if(is.na(loc$cellType[1])){
     ggplot(loc, aes(x, y)) + geom_point() + theme_classic()
   }else{
  ggplot(loc, aes(x, y, colour = cellType)) + geom_point() + theme_classic()
   }
}
