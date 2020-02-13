#' Accessors for SegmentedCellExperiment
#' 
#' Methods to access various components of the `SegmentedCellExperiment` object.
#'
#' @usage location(x, imageID = NULL, bind = TRUE)
#' @usage location(x, imageID = NULL) <- value
#' @usage intensity(x, imageID = NULL, bind = TRUE)
#' @usage intensity(x, imageID = NULL) <- value
#' @usage morphology(x, imageID = NULL, bind = TRUE)
#' @usage morphology(x, imageID = NULL) <- value
#' @usage phenotype(x, imageID = NULL, bind = TRUE, expand = FALSE)
#' @usage phenotype(x, imageID = NULL) <- value
#' @usage imageID(x, imageID = NULL)
#' @usage cellID(x, imageID = NULL)
#' @usage cellID(x) <- value
#' @usage imageCellID(x, imageID = NULL)
#' @usage imageCellID(x) <- value
#' @usage cellType(x, imageID = NULL)
#' @usage cellType(x, imageID = NULL) <- value
#' 
#' @param x A `SegmentedCellExperiment` object.
#' @param imageID A vector of imageIDs to specifically extract.
#' @param bind When false outputs a list of DataFrames split by imageID
#' @param expand Used to expand the phenotype information from per image to per cell.
#' @param value The relevant information used to replace.
#'
#' @section Descriptions:
#' \describe{
#' \item{`location`:}{
#' Retrieves the DataFrame containing `x` and `y` coordinates of each cell as well as `cellID`, `imageID` and `cellType`.
#' imageID can be used to select specific images and bind=FALSE outputs the information as a list split by imageID.
#' }
#'
#' \item{`morphology`:}{
#' Retrieves the DataFrame containing morphology information.
#' }
#'
#' \item{`intensity`:}{
#' Retrieves the DataFrame containing intensity of gene or protein markers.
#' }
#' 
#' \item{`phenotype`:}{
#' Retrieves the DataFrame containing the phenotype information. Using expand = TRUE will produce a DataFrame with the number of rows equal to the number of cells.
#' }
#' }
#' 
#' @return DataFrame or a list of DataFrames
#' @name Accessors
#' 
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
#' location(cellExp, imageID = 1)
#' 
#' @aliases 
#' location,SegmentedCellExperiment-method
#' location<-,SegmentedCellExperiment-method
#' intensity,SegmentedCellExperiment-method
#' intensity<-,SegmentedCellExperiment-method
#' morphology,SegmentedCellExperiment-method
#' morphology<-,SegmentedCellExperiment-method
#' phenotype,SegmentedCellExperiment-method
#' phenotype<-,SegmentedCellExperiment-method
#' imageID,SegmentedCellExperiment-method
#' cellType,SegmentedCellExperiment-method
#' cellType<-,SegmentedCellExperiment-method
#' imageCellID,SegmentedCellExperiment-method
#' imageCellID<-,SegmentedCellExperiment-method
#' cellID,SegmentedCellExperiment-method
#' cellID<-,SegmentedCellExperiment-method
#' location
#' location<-
#' intensity
#' intensity<-
#' morphology
#' morphology<-
#' phenotype
#' phenotype<-
#' imageID
#' cellType
#' cellType<-
#' imageCellID
#' imageCellID<-
#' cellID
#' cellID<-


### Get location information for each cell.

#' @export
#' @import BiocGenerics
#' @import IRanges
setGeneric("location", function(x, imageID = NULL, bind = TRUE) standardGeneric("location"))
setMethod("location", "SegmentedCellExperiment", function(x, imageID = NULL, bind = TRUE) {
    if (!is.null(imageID)) {
        x <- x[imageID, ]
    }
    if (bind == FALSE) {
        return(x$location)
    }
    if (bind == TRUE) {
        class(x$location)
        return(cbind(imageID = rep(rownames(x), unlist(lapply(x[, "location"], nrow))), 
            BiocGenerics::do.call("rbind", x$location)))
    }
    
})

#' @export
setGeneric("location<-", function(x, imageID = NULL, value) standardGeneric("location<-"))
setMethod("location<-", "SegmentedCellExperiment", function(x, imageID = NULL, value) {
    if (is.null(imageID)) 
        imageID <- rownames(x)
    if (nrow(value) == length(imageID)) {
        x[imageID, ]@listData$location <- value
        return(x)
    }
    
    if (nrow(value) == length(imageID(x, imageID))) {
        value <- value[, c("cellID", "imageCellID", "x", "y", "cellType")]
        by <- rep(imageID, unlist(lapply(x[imageID, "location"], nrow)))
        by <- factor(by, levels = unique(by))
        x[imageID, ]@listData$location <- S4Vectors::split(value, by)
        return(x)
    }
})




### Get imageIDs for each cell, not sure if this should also report rownames(df)

#' @export
setGeneric("imageID", function(x, imageID = NULL) standardGeneric("imageID"))
setMethod("imageID", "SegmentedCellExperiment", function(x, imageID = NULL) {
    if (!is.null(imageID)) {
        x <- x[imageID, ]
    }
    rep(rownames(x), unlist(lapply(x$location, nrow)))
})




### Get cellIDs

#' @export
setGeneric("cellID", function(x, imageID = NULL) standardGeneric("cellID"))
setMethod("cellID", "SegmentedCellExperiment", function(x, imageID = NULL) {
    if (!is.null(imageID)) {
        x <- x[imageID, ]
    }
    BiocGenerics::do.call("rbind", x$location)$cellID
})

#' @export
setGeneric("cellID<-", function(x, value) standardGeneric("cellID<-"))
setMethod("cellID<-", "SegmentedCellExperiment", function(x, value) {
    loc <- location(x)
    
    if (nrow(loc) != length(value)) {
        stop("There is not enough or too many cellIDs")
    }
    
    loc$cellID <- value
    location(x) <- loc
})




### Get imageCellID

#' @export
setGeneric("imageCellID", function(x, imageID = NULL) standardGeneric("imageCellID"))
setMethod("imageCellID", "SegmentedCellExperiment", function(x, imageID = NULL) {
    if (!is.null(imageID)) {
        x <- x[imageID, ]
    }
    BiocGenerics::do.call("rbind", x$location)$imageCellID
})

#' @export
setGeneric("imageCellID<-", function(x, value) standardGeneric("imageCellID<-"))
setMethod("imageCellID<-", "SegmentedCellExperiment", function(x, value) {
    loc <- location(x)
    
    if (nrow(loc) != length(value)) {
        stop("There is not enough or too many imageCellIDs")
    }
    
    loc$imageCellID <- value
    
    location(x) <- loc
})




### Get intensity information

#' @export
setGeneric("intensity", function(x, imageID = NULL, bind = TRUE) standardGeneric("intensity"))
setMethod("intensity", "SegmentedCellExperiment", function(x, imageID = NULL, bind = TRUE) {
    if (!is.null(imageID)) {
        x <- x[imageID, ]
    }
    if (bind == FALSE) {
        return(x$intensity)
    }
    if (bind == TRUE) {
        class(x$intensity)
        return(BiocGenerics::do.call("rbind", x$intensity))
    }
    
})

#' @export
setGeneric("intensity<-", function(x, imageID = NULL, value) standardGeneric("intensity<-"))
setMethod("intensity<-", "SegmentedCellExperiment", function(x, imageID = NULL, value) {
    if (is.null(imageID)) 
        imageID <- rownames(x)
    if (nrow(value) == length(imageID)) {
        x[imageID, ]@listData$intensity <- value
        return(x)
    }
    
    if (nrow(value) == length(imageID(x))) {
        by <- rep(rownames(x), unlist(lapply(x$intensity, nrow)))
        by <- factor(by, levels = unique(by))
        x[imageID, ]@listData$intensity <- S4Vectors::split(value, by)
        return(x)
    }
})




### Get morphology information

#' @export
setGeneric("morphology", function(x, imageID = NULL, bind = TRUE) standardGeneric("morphology"))
setMethod("morphology", "SegmentedCellExperiment", function(x, imageID = NULL, bind = TRUE) {
    if (!is.null(imageID)) {
        x <- x[imageID, ]
    }
    if (bind == FALSE) {
        return(x$morphology)
    }
    if (bind == TRUE) {
        class(x$morphology)
        return(BiocGenerics::do.call("rbind", x$morphology))
    }
    
})

#' @export
setGeneric("morphology<-", function(x, imageID = NULL, value) standardGeneric("morphology<-"))
setMethod("morphology<-", "SegmentedCellExperiment", function(x, imageID = NULL, 
    value) {
    if (is.null(imageID)) 
        imageID <- rownames(x)
    if (nrow(value) == length(imageID)) {
        x[imageID, ]@listData$morphology <- value
        return(x)
    }
    
    if (nrow(value) == length(imageID(x, imageID))) {
        by <- rep(rownames(x), unlist(lapply(x$morphology, nrow)))
        by <- factor(by, levels = unique(by))
        
        x[imageID, ]@listData$morphology <- S4Vectors::split(value, by)
        return(x)
    }
})




### Get cell type information

#' @export
setGeneric("cellType", function(x, imageID = NULL) standardGeneric("cellType"))
setMethod("cellType", "SegmentedCellExperiment", function(x, imageID = NULL) {
    if (!is.null(imageID)) {
        x <- x[imageID, ]
    }
    BiocGenerics::do.call("rbind", x$location)$cellType
})

#' @export
setGeneric("cellType<-", function(x, imageID = NULL, value) standardGeneric("cellType<-"))
setMethod("cellType<-", "SegmentedCellExperiment", function(x, imageID = NULL, value) {
    if (is.null(imageID)) 
        imageID <- rownames(x)
    loc <- location(x, imageID = imageID)
    
    if (nrow(loc) != length(value)) {
        stop("There is not enough or too many cellTypes")
    }
    
    loc$cellType <- value
    
    location(x, imageID = imageID) <- loc
    x
})





### Get and add phenotype data to the object

#' @export
setGeneric("phenotype", function(x, imageID = NULL, bind = TRUE, expand = FALSE) standardGeneric("phenotype"))
setMethod("phenotype", "SegmentedCellExperiment", function(x, imageID = NULL, bind = TRUE, 
    expand = FALSE) {
    if (!is.null(imageID)) {
        x <- x[imageID, ]
    }
    if (expand) {
        pheno <- BiocGenerics::do.call("rbind", x$phenotype)
        rownames(pheno) <- pheno$imageID
        return(pheno[imageID(x), ])
    } else {
        pheno <- BiocGenerics::do.call("rbind", x$phenotype)
        rownames(pheno) <- pheno$imageID
        return(pheno[rownames(x), ])
    }
})

#' @export
setGeneric("phenotype<-", function(x, imageID = NULL, value) standardGeneric("phenotype<-"))
setMethod("phenotype<-", "SegmentedCellExperiment", function(x, imageID = NULL, value) {
    if (is.null(imageID)) 
        imageID <- rownames(x)
    use <- intersect(value$imageID, imageID)
    rownames(value) <- value$imageID
    x[use, ]@listData$phenotype <- S4Vectors::split(value[use, ], use)
    x[unique(use), ]
})




