## ---- include = FALSE---------------------------------------------------------
knitr::opts_chunk$set(
  collapse = TRUE,
  comment = "#>"
)
library(BiocStyle)

## ----setup, message=FALSE-----------------------------------------------------
library(SegmentedCellExperiment)

## -----------------------------------------------------------------------------

### Something that resembles cellProfiler data

set.seed(51773)

n = 10

cells <- data.frame(row.names = seq_len(n))
cells$ObjectNumber <- seq_len(n)
cells$ImageNumber <- rep(1:2,c(n/2,n/2))
cells$x <- runif(n)
cells$y <- runif(n)
cells$AreaShape_round <- rexp(n)
cells$AreaShape_diameter <- rexp(n, 2)
cells$Intensity_Mean_CD8 <- rexp(n, 10)
cells$Intensity_Mean_CD4 <- rexp(n, 10)


## -----------------------------------------------------------------------------
cellExp <- SegmentedCellExperiment(cells, cellProfiler = TRUE)

## -----------------------------------------------------------------------------
loc <- location(cellExp)
head(loc)

location(cellExp) <- loc


## -----------------------------------------------------------------------------
intensities <- intensity(cellExp)
kM <- kmeans(intensities,2)
cellType(cellExp) <- paste('cluster',kM$cluster, sep = '')

loc <- location(cellExp)
head(loc)

## -----------------------------------------------------------------------------
set.seed(51773)

n = 10

cells <- data.frame(row.names = seq_len(n))
cells$cellID <- seq_len(n)
cells$imageCellID <- rep(seq_len(n/2),2)
cells$imageID <- rep(1:2,c(n/2,n/2))
cells$x <- runif(n)
cells$y <- runif(n)
cells$shape_round <- rexp(n)
cells$shape_diameter <- rexp(n, 2)
cells$intensity_CD8 <- rexp(n, 10)
cells$intensity_CD4 <- rexp(n, 10)
cells$cellType <- paste('cluster',sample(1:2,n,replace = TRUE), sep = '_')


## -----------------------------------------------------------------------------

cellExp <- SegmentedCellExperiment(cells, cellTypeString = 'cellType', intensityString = 'intensity_', morphologyString = 'shape_')


## -----------------------------------------------------------------------------
morph <- morphology(cellExp)
head(morph)


## -----------------------------------------------------------------------------
phenoData <- DataFrame(imageID = c('1','2'), age = c(21,81), status = c('dead','alive'))
phenotype(cellExp) <- phenoData
phenotype(cellExp)
phenotype(cellExp, expand = TRUE)

## -----------------------------------------------------------------------------


set.seed(51773)

n = 10

cells <- data.frame(row.names = seq_len(n))
cells$x <- runif(n)
cells$y <- runif(n)
cellExp <- SegmentedCellExperiment(cells)



## -----------------------------------------------------------------------------
loc <- location(cellExp)
head(loc)


