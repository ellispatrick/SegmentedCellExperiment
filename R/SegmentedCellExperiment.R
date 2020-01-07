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

SegmentedCellExperiment <- function(cellData, cellProfiler = FALSE, spatialCoords = c('x','y'), cellTypeString = NULL, intensityString = NULL, morphologyString = NULL) {

  if(!cellProfiler){

       if (is.null(cellData$cellID)) {
           cellData$cellID <- paste("cell", seq_len(nrow(cellData)), sep = "_")
       }


    if (is.null(cellData$x)) {
      stop("You need to include a 'x' column in the data.frame")
    }

    if (is.null(cellData$y)) {
      stop("You need to include a 'y' column in the data.frame")
    }

      if(is.null(cellData$imageCellID)){
        cellData$imageCellID <- paste("cell", seq_len(nrow(cellData)), sep = "_")
      }
       if (length(cellData$imageCellID) != nrow(cellData) )
           stop("The number of rows in cells does not equal the number of imageCellIDs")

     if (is.null(cellData$imageID)) {
       cat("There is no imageID. I'll assume this is only one image and create an arbitrary imageID")
       cellData$imageID <- "image1"
     }
  }


  if(cellProfiler){

      cellData$imageID <- as.factor(cellData$ImageNumber)
      cellData$cellID <- cellData$ObjectNumber
      cellData$imageCellID <- cellData$ObjectNumber

      if(is.null(intensityString)&any(grepl('Intensity_Mean_', colnames(cellData)))){
        intensityString <- 'Intensity_Mean_'
      }

      if(is.null(morphologyString)&any(grepl('AreaShape_', colnames(cellData)))){
        morphologyString <- 'AreaShape_'
      }

  }



  df = DataFrame(row.names = unique(cellData$imageID))

  if(!is.null(cellTypeString)){
    cellData$cellType <- cellData[,cellTypeString]
    location <- split(DataFrame(cellData[,c('imageCellID','cellID','imageID',spatialCoords,'cellType')]), cellData$imageID)
  }else{
    cellData$cellType = NA
    location <- S4Vectors::split(DataFrame(cellData[,c('imageCellID','cellID','imageID',spatialCoords, 'cellType')]), cellData$imageID)
    cat(unlist(lapply(location,dim)))
    }
  
  df$location <- location

  df$intensity = S4Vectors::split(DataFrame(), cellData$imageID)
  df$morphology = S4Vectors::split(DataFrame(), cellData$imageID)

  if(!is.null(intensityString)){
    intensity <- cellData[,grep(intensityString, colnames(cellData))]
    colnames(intensity) <- gsub(intensityString, '', colnames(intensity))
    df$intensity = S4Vectors::split(DataFrame(intensity), cellData$imageID)
  }

  if(!is.null(morphologyString)){
    morphology <- cellData[,grep(morphologyString, colnames(cellData))]
    colnames(morphology) <- gsub(morphologyString, '', morphology)
    df$morphology = S4Vectors::split(DataFrame(morphology), cellData$imageID)
  }

  df$phenotype = S4Vectors::split(DataFrame(), cellData$imageID)
  df$images = S4Vectors::split(DataFrame(), cellData$imageID)
  df$masks = S4Vectors::split(DataFrame(), cellData$imageID)

  df <- new("SegmentedCellExperiment", df)
  df
}
# 
# 
# 
# 
# 
# 
# 
# 
# 
# 
# 
# 
# 
# 
# ### Some functions to get and make location data
# 
# location <- function(x, image = NULL, bind = FALSE){
#   if(!is.null(image)){
#     x = x[image,]
#   }
#   if(bind==FALSE){
#     return(x$location)
#   }
#   if(bind==TRUE){
#     return(do.call('rbind',x$location))
#   }
# }
# 
# 
# 'location<-' <- function(x, value){
#   if(nrow(value)==nrow(x)){
#     x$location <- value
#     return(x)
#   }
# 
#   if(nrow(value)==length(imageID(x))){
#     x$location <- split(value,rep(rownames(x),unlist(lapply(x$location,nrow))))
#     return(x)
#   }
# 
# }
# 
# 
# ### Get imageIDs for each cell, not sure if this should also report rownames(df)
# 
# imageID <- function(x, image = NULL){
#   if(!is.null(image)){
#     x = x[image,]
#   }
#   rep(rownames(x),unlist(lapply(x$location,nrow)))
# }
# 
# ### Get cellTypes for each cell
# 
# cellType <- function(x, image = NULL){
#   if(!is.null(image)){
#     x = x[image,]
#   }
#   do.call('rbind',x$location)$cellType
# }
# 
# 'cellType<-' <- function(x, value){
# 
#   loc <- location(x, bind = TRUE)
# 
#   if(nrow(loc)!=nrow(x)){
#     stop('There is not enough or too many cell types')
#   }
# 
#   loc$cellType <- value
# 
#   location(x) <- loc
# 
# }
# 
# 
# ### Get cellID
# 
# cellID <- function(x, image = NULL){
#   if(!is.null(image)){
#     x = x[image,]
#   }
#   do.call('rbind',x$location)$cellID
# }
# 
# 
# 'cellID<-' <- function(x, value){
# 
#   loc <- location(x, bind = TRUE)
# 
#   if(nrow(loc)!=nrow(x)){
#     stop('There is not enough or too many cellIDs')
#   }
# 
#   loc$cellID <- value
# 
#   location(x) <- loc
# 
# }
# 
# 
# 
# ### Get uniqueCellID
# 
# uniqueCellID <- function(x, image = NULL){
#   if(!is.null(image)){
#     x = x[image,]
#   }
#   do.call('rbind',x$location)$uniqueCellID
# }
# 
# 
# makeUniqueCellID <- function(x){
#   loc <- location(x,bind=TRUE)
#   loc$uniqueCellID <- paste('cell', seq_len(nrow(loc)),sep = '')
#   location(x) <- loc
#   x
# }
# 
# 
# 
# 
# #### I can access and add phenotype data to the object
# 
# phenotype <- function(x, image = NULL, bind = FALSE){
#   if(!is.null(image)){
#     x = x[image,]
#   }
#   do.call('rbind',x$phenotype)
# }
# 
# 
# 
# 'phenotype<-' <- function(x, value, image = NULL){
#   if(is.null(image)) image <- rownames(x)
#   use <- intersect(value$imageID,image)
#   x <- x[image,]
#   x$phenotype <- split(value,image)
#   x
# }
# 




# library(S4Vectors)
#
#
#
### Something that resembles cellProfiler data

# cells <- data.frame(row.names = 1:10)
# cells$cellID <- paste('cell',1:10,sep = '')
# cells$imageID <- paste('image',rep(1:2,c(4,6)),sep = '')
# cells$x <- 1:10
# cells$y <- 10:1
# cells$cellType <- paste('cellType',rep(1:2,5),sep = '')
# cells$Area_round <- 1
# cells$Area_diameter <- 2
# cells$Intensity_mean_CD8 <- 3
# cells$Intensity_mean_CD4 <- 11:20



### Lets make a CellImagingExperiment... obviously we can make this a class

# df <- as.ImagingCytometryExperiment(cells)
#
#
#
# phenotypeData <- DataFrame(sex = c('M',"F"), height = c(120,210), weight = c(30, 140), imageID = c('image1','image2'))
# phenotype(df) <- phenotypeData
#
# ### I can get the locations and set them again
# z <- location(df)
# z
#
# ### If I give it a list of DataFrames
# location(df) <- z
#
#
# z <- location(df, bind = TRUE)
# z
# ### If I give it a single DataFrame
# location(df) <- z
#
#
# ### I can pull out information for specific images
# df['image1',]
# location(df['image1',])
# location(df,'image1')
# location(df,c('image1','image2'),TRUE)
#
# cellType(df)
#
#
#
# ### We could also add an image(df) which contained stacks maybe even image(df,'image1', 'CD8')
