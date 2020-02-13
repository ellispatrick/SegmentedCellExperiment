### Get location information for each cell.

#' @export
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
setGeneric("location<-", function(x, value, imageID = NULL) standardGeneric("location<-"))
setMethod("location<-", "SegmentedCellExperiment", function(x, value, imageID = NULL) {
    if (is.null(imageID)) 
        imageID <- rownames(x)
    if (nrow(value) == length(imageID)) {
        x[imageID, ]@listData$location <- value
        return(x)
    }
    
    if (nrow(value) == length(imageID(x,imageID))) {
        value <- value[, c("cellID", "imageCellID", "x", "y", "cellType")]
        by <- rep(imageID, unlist(lapply(x[imageID, "location"], nrow)))
        by <- factor(by, levels = unique(by))
        x[imageID, ]@listData$location <- S4Vectors::split(value, by )
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
setGeneric("intensity", function(x, imageID = NULL, bind = FALSE) standardGeneric("intensity"))
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
setGeneric("intensity<-", function(x, value, imageID = NULL) standardGeneric("intensity<-"))
setMethod("intensity<-", "SegmentedCellExperiment", function(x, value, imageID = NULL) {
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
setGeneric("morphology", function(x, imageID = NULL, bind = FALSE) standardGeneric("morphology"))
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
setGeneric("morphology<-", function(x, value, imageID = NULL) standardGeneric("morphology<-"))
setMethod("morphology<-", "SegmentedCellExperiment", function(x, value, imageID = NULL) {
    if (is.null(imageID)) 
        imageID <- rownames(x)
    if (nrow(value) == length(imageID)) {
        x[imageID, ]@listData$morphology <- value
        return(x)
    }
    
    if (nrow(value) == length(imageID(x,imageID))) {
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
setGeneric("cellType<-", function(x, value, imageID = NULL) standardGeneric("cellType<-"))
setMethod("cellType<-", "SegmentedCellExperiment", function(x, value, imageID = NULL) {
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
setMethod("phenotype", "SegmentedCellExperiment", function(x, imageID = NULL, bind = TRUE, expand = FALSE) {
  if (!is.null(imageID)) {
    x <- x[imageID, ]
  }
  if(expand){
    ph <- BiocGenerics::do.call("rbind", x$phenotype)
    rownames(ph) <- ph$imageID
    return(ph[imageID(x),])
  }else{
    return(BiocGenerics::do.call("rbind", x$phenotype))
  }
})

#' @export
setGeneric("phenotype<-", function(x, value, imageID = NULL) standardGeneric("phenotype<-"))
setMethod("phenotype<-", "SegmentedCellExperiment", function(x, value, imageID = NULL) {
  if (is.null(imageID)) 
    imageID <- rownames(x)
  use <- intersect(value$imageID, imageID)
  rownames(value) <- value$imageID
  x[use, ]@listData$phenotype <- S4Vectors::split(value[use,], use)
  x[unique(use), ]
})




