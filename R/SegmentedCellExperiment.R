#' The SegmentedCellExperiment class
#'
#' @param cellData A data frame that contains at least the columns [x] and [y] giving the location of each cell.
#' @param cellProfiler A logical indicating that [cellData] is in a format similar to what cellProfiler outputs.
#' @param spatialCoords The column names corresponding to spatial coordinates. eg. x, y, z...
#' @param cellTypeString The name of the column that contains cell type calls.
#' @param intensityString A string which can be used to identify the columns which contain marker intensities. (This needs to be extended to take the column names themselves.)
#' @param morphologyString A string which can be used to identify the columns which contains morphology information.
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
#' cells$AreaShape_round <- rexp(10)
#' cells$AreaShape_diameter <- rexp(10, 2)
#' cells$Intensity_Mean_CD8 <- rexp(10, 10)
#' cells$Intensity_Mean_CD4 <- rexp(10, 10)
#'
#' cellExp <- SegmentedCellExperiment(cells, cellProfiler = TRUE)
#' 
#' ### Cluster cell types
#' intensities <- intensity(cellExp)
#' kM <- kmeans(intensities,2)
#' cellType(cellExp) <- paste('cluster',kM$cluster, sep = '')
#' location(cellExp)
#' 
#' @export
#' @rdname SegmentedCellExperiment
#' @importFrom methods new
#' @importClassesFrom S4Vectors DataFrame
SegmentedCellExperiment <- function(cellData, cellProfiler = FALSE, spatialCoords = c("x", 
    "y"), cellTypeString = NULL, intensityString = NULL, morphologyString = NULL) {
    
    if (!cellProfiler) {
        
        if (is.null(cellData$cellID)) {
            cellData$cellID <- paste("cell", seq_len(nrow(cellData)), sep = "_")
        }
        
        
        if (is.null(cellData$x)) {
            stop("You need to include a 'x' column in the data.frame")
        }
        
        if (is.null(cellData$y)) {
            stop("You need to include a 'y' column in the data.frame")
        }
        
        if (is.null(cellData$imageCellID)) {
            cellData$imageCellID <- paste("cell", seq_len(nrow(cellData)), sep = "_")
        }
        if (length(cellData$imageCellID) != nrow(cellData)) 
            stop("The number of rows in cells does not equal the number of imageCellIDs")
        
        if (is.null(cellData$imageID)) {
            cat("There is no imageID. I'll assume this is only one image and create an arbitrary imageID")
            cellData$imageID <- "image1"
        }
    }
    
    
    if (cellProfiler) {
        
        cellData$imageID <- as.factor(cellData$ImageNumber)
        cellData$cellID <- cellData$ObjectNumber
        cellData$imageCellID <- cellData$ObjectNumber
        
        if (is.null(intensityString) & any(grepl("Intensity_Mean_", colnames(cellData)))) {
            intensityString <- "Intensity_Mean_"
        }
        
        if (is.null(morphologyString) & any(grepl("AreaShape_", colnames(cellData)))) {
            morphologyString <- "AreaShape_"
        }
        
    }
    
    
    
    df <- DataFrame(row.names = unique(cellData$imageID))
    
    if (!is.null(cellTypeString)) {
        cellData$cellType <- cellData[, cellTypeString]
        location <- split(DataFrame(cellData[, c("imageCellID", "cellID", "imageID", 
            spatialCoords, "cellType")]), cellData$imageID)
    } else {
        cellData$cellType <- NA
        location <- S4Vectors::split(DataFrame(cellData[, c("imageCellID", "cellID", 
            "imageID", spatialCoords, "cellType")]), cellData$imageID)
        cat(unlist(lapply(location, dim)))
    }
    
    df$location <- location
    
    df$intensity <- S4Vectors::split(DataFrame(), cellData$imageID)
    df$morphology <- S4Vectors::split(DataFrame(), cellData$imageID)
    
    if (!is.null(intensityString)) {
        intensity <- cellData[, grep(intensityString, colnames(cellData))]
        colnames(intensity) <- gsub(intensityString, "", colnames(intensity))
        df$intensity <- S4Vectors::split(DataFrame(intensity), cellData$imageID)
    }
    
    if (!is.null(morphologyString)) {
        morphology <- cellData[, grep(morphologyString, colnames(cellData))]
        colnames(morphology) <- gsub(morphologyString, "", morphology)
        df$morphology <- S4Vectors::split(DataFrame(morphology), cellData$imageID)
    }
    
    df$phenotype <- S4Vectors::split(DataFrame(), cellData$imageID)
    df$images <- S4Vectors::split(DataFrame(), cellData$imageID)
    df$masks <- S4Vectors::split(DataFrame(), cellData$imageID)
    
    df <- new("SegmentedCellExperiment", df)
    df
}
