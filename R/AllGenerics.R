setGeneric("location", function(x, image = NULL, bind = FALSE) standardGeneric("location"))
setMethod("location", "SegmentedCellExperiment", function(x, image = NULL, bind = TRUE){
  if(!is.null(image)){
    x = x[image,]
  }
  if(bind==FALSE){
    return(x$location)
  }
  if(bind==TRUE){
    class(x$location)
    return(BiocGenerics::do.call('rbind',x$location))
  }
  
  })

setGeneric("location<-", function(x, value, image = NULL) standardGeneric("location<-"))
setMethod("location<-", "SegmentedCellExperiment", function(x, value, image = NULL) {
  if(is.null(image))image = rownames(x)
  if(nrow(value)==length(image)){
    x[image,]@listData$location <- value
    return(x)
  }
  
  if(nrow(value)==length(imageID(x))){
    x[image,]@listData$location <- S4Vectors::split(value,rep(image,unlist(lapply(x[image,'location'],nrow))))
    return(x)
  }
})



### Get imageIDs for each cell, not sure if this should also report rownames(df)

setGeneric("imageID", function(x, image = NULL) standardGeneric("imageID"))
setMethod("imageID", "SegmentedCellExperiment", function(x, image = NULL){
  if(!is.null(image)){
    x = x[image,]
  }
  rep(rownames(x),unlist(lapply(x$location,nrow)))
})




### Get cellID


setGeneric("cellID", function(x, image = NULL) standardGeneric("cellID"))
setMethod("cellID", "SegmentedCellExperiment", function(x, image = NULL){
  if(!is.null(image)){
    x = x[image,]
  }
  BiocGenerics::do.call('rbind',x$location)$cellID
})

setGeneric("cellID<-", function(x, value) standardGeneric("cellID<-"))

setMethod("cellID<-", "SegmentedCellExperiment", function(x, value) {
  loc <- location(x)
  
  if(nrow(loc)!=length(value)){
    stop('There is not enough or too many cellIDs')
  }
  
  loc$cellID <- value
  location(x) <- loc
  })


### Get imageCellID


setGeneric("imageCellID", function(x, image = NULL) standardGeneric("imageCellID"))
setMethod("imageCellID", "SegmentedCellExperiment", function(x, image = NULL){
  if(!is.null(image)){
    x = x[image,]
  }
  BiocGenerics::do.call('rbind',x$location)$imageCellID
})

setGeneric("imageCellID<-", function(x, value) standardGeneric("imageCellID<-"))

setMethod("imageCellID<-", "SegmentedCellExperiment", function(x, value) {
  loc <- location(x)
  
  if(nrow(loc)!=length(value)){
    stop('There is not enough or too many imageCellIDs')
  }
  
  loc$imageCellID <- value
  
  location(x) <- loc
})





#### Access and add phenotype data to the object

setGeneric("phenotype", function(x, image = NULL, bind = TRUE) standardGeneric("phenotype"))
setMethod("phenotype", "SegmentedCellExperiment", function(x, image = NULL, bind = TRUE){
  if(!is.null(image)){
    x = x[image,]
  }
  BiocGenerics::do.call('rbind',x$phenotype)
})

setGeneric("phenotype<-", function(x, value, image = NULL) standardGeneric("phenotype<-"))
setMethod("phenotype<-", "SegmentedCellExperiment", function(x, value, image = NULL) {
  if(is.null(image)) image <- rownames(x)
  use <- intersect(value$imageID,image)
  x[image,]@listData$phenotype <- split(value,image)
  x
  })




setGeneric("intensity", function(x, image = NULL, bind = FALSE) standardGeneric("intensity"))
setMethod("intensity", "SegmentedCellExperiment", function(x, image = NULL, bind = TRUE){
  if(!is.null(image)){
    x = x[image,]
  }
  if(bind==FALSE){
    return(x$intensity)
  }
  if(bind==TRUE){
    class(x$intensity)
    return(BiocGenerics::do.call('rbind',x$intensity))
  }
  
})

setGeneric("intensity<-", function(x, value, image = NULL) standardGeneric("intensity<-"))
setMethod("intensity<-", "SegmentedCellExperiment", function(x, value, image = NULL) {
  if(is.null(image))image = rownames(x)
  if(nrow(value)==length(image)){
    x[image,]@listData$intensity <- value
    return(x)
  }
  
  if(nrow(value)==length(imageID(x))){
    x[image,]@listData$intensity <- S4Vectors::split(value,rep(rownames(x),unlist(lapply(x$intensity,nrow))))
    return(x)
  }
})




### Get imageCellID


setGeneric("cellType", function(x, image = NULL) standardGeneric("cellType"))
setMethod("cellType", "SegmentedCellExperiment", function(x, image = NULL){
  if(!is.null(image)){
    x = x[image,]
  }
  BiocGenerics::do.call('rbind',x$location)$cellType
})

setGeneric("cellType<-", function(x, value, image=NULL) standardGeneric("cellType<-"))

setMethod("cellType<-", "SegmentedCellExperiment", function(x, value, image=NULL) {
  if(is.null(image))image = rownames(x)
  loc <- location(x,image = image)
  
  if(nrow(loc)!=length(value)){
    stop('There is not enough or too many cellTypes')
  }
  
  loc$cellType <- value
  
  location(x, image=image) <- loc
  x
})




