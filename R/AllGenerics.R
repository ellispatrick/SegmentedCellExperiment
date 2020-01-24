### Get location information for each cell.

#' @export
setGeneric("location", function(x, image = NULL, bind = FALSE) standardGeneric("location"))
setMethod("location", "SegmentedCellExperiment", function(x, image = NULL, bind = TRUE) {
    if (!is.null(image)) {
        x <- x[image, ]
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
setGeneric("location<-", function(x, value, image = NULL) standardGeneric("location<-"))
setMethod("location<-", "SegmentedCellExperiment", function(x, value, image = NULL) {
    if (is.null(image)) 
        image <- rownames(x)
    if (nrow(value) == length(image)) {
        x[image, ]@listData$location <- value
        return(x)
    }
    
    if (nrow(value) == length(imageID(x,image))) {
        value <- value[, c("cellID", "imageCellID", "x", "y", "cellType")]
        x[image, ]@listData$location <- S4Vectors::split(value, rep(image, unlist(lapply(x[image, 
            "location"], nrow))))
        return(x)
    }
})




### Get imageIDs for each cell, not sure if this should also report rownames(df)

#' @export
setGeneric("imageID", function(x, image = NULL) standardGeneric("imageID"))
setMethod("imageID", "SegmentedCellExperiment", function(x, image = NULL) {
    if (!is.null(image)) {
        x <- x[image, ]
    }
    rep(rownames(x), unlist(lapply(x$location, nrow)))
})




### Get cellIDs

#' @export
setGeneric("cellID", function(x, image = NULL) standardGeneric("cellID"))
setMethod("cellID", "SegmentedCellExperiment", function(x, image = NULL) {
    if (!is.null(image)) {
        x <- x[image, ]
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
setGeneric("imageCellID", function(x, image = NULL) standardGeneric("imageCellID"))
setMethod("imageCellID", "SegmentedCellExperiment", function(x, image = NULL) {
    if (!is.null(image)) {
        x <- x[image, ]
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
setGeneric("intensity", function(x, image = NULL, bind = FALSE) standardGeneric("intensity"))
setMethod("intensity", "SegmentedCellExperiment", function(x, image = NULL, bind = TRUE) {
    if (!is.null(image)) {
        x <- x[image, ]
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
setGeneric("intensity<-", function(x, value, image = NULL) standardGeneric("intensity<-"))
setMethod("intensity<-", "SegmentedCellExperiment", function(x, value, image = NULL) {
    if (is.null(image)) 
        image <- rownames(x)
    if (nrow(value) == length(image)) {
        x[image, ]@listData$intensity <- value
        return(x)
    }
    
    if (nrow(value) == length(imageID(x))) {
        x[image, ]@listData$intensity <- S4Vectors::split(value, rep(rownames(x), 
            unlist(lapply(x$intensity, nrow))))
        return(x)
    }
})




### Get morphology information

#' @export
setGeneric("morphology", function(x, image = NULL, bind = FALSE) standardGeneric("morphology"))
setMethod("morphology", "SegmentedCellExperiment", function(x, image = NULL, bind = TRUE) {
    if (!is.null(image)) {
        x <- x[image, ]
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
setGeneric("morphology<-", function(x, value, image = NULL) standardGeneric("morphology<-"))
setMethod("morphology<-", "SegmentedCellExperiment", function(x, value, image = NULL) {
    if (is.null(image)) 
        image <- rownames(x)
    if (nrow(value) == length(image)) {
        x[image, ]@listData$morphology <- value
        return(x)
    }
    
    if (nrow(value) == length(imageID(x,image))) {
        x[image, ]@listData$morphology <- S4Vectors::split(value, rep(rownames(x), 
            unlist(lapply(x$morphology, nrow))))
        return(x)
    }
})




### Get cell type information

#' @export
setGeneric("cellType", function(x, image = NULL) standardGeneric("cellType"))
setMethod("cellType", "SegmentedCellExperiment", function(x, image = NULL) {
    if (!is.null(image)) {
        x <- x[image, ]
    }
    BiocGenerics::do.call("rbind", x$location)$cellType
})

#' @export
setGeneric("cellType<-", function(x, value, image = NULL) standardGeneric("cellType<-"))
setMethod("cellType<-", "SegmentedCellExperiment", function(x, value, image = NULL) {
    if (is.null(image)) 
        image <- rownames(x)
    loc <- location(x, image = image)
    
    if (nrow(loc) != length(value)) {
        stop("There is not enough or too many cellTypes")
    }
    
    loc$cellType <- value
    
    location(x, image = image) <- loc
    x
})





### Get and add phenotype data to the object

#' @export
setGeneric("phenotype", function(x, image = NULL, bind = TRUE, expand = FALSE) standardGeneric("phenotype"))
setMethod("phenotype", "SegmentedCellExperiment", function(x, image = NULL, bind = TRUE, expand = FALSE) {
  if (!is.null(image)) {
    x <- x[image, ]
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
setGeneric("phenotype<-", function(x, value, image = NULL) standardGeneric("phenotype<-"))
setMethod("phenotype<-", "SegmentedCellExperiment", function(x, value, image = NULL) {
  if (is.null(image)) 
    image <- rownames(x)
  use <- intersect(value$imageID, image)
  rownames(value) <- value$imageID
  x[use, ]@listData$phenotype <- S4Vectors::split(value[use,], use)
  x
})




