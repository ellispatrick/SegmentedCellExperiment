# SegmentedCellExperiment


A `SegmentedCellExperiment` is an object designed to store data from imaging cytometry (FISH, IMC, CycIF, spatial transcriptomics, ... ) that has already been segmented and reduced to individual cells. A `SegmentedCellExperiment` extends DataFrame and defines methods that take advantage of DataFrame nesting to represent various elements of cell-based experiments with spatial orientation that are commonly encountered. This object is able to store information on a cell's spatial location, cellType, morphology, intensity of gene/protein marks as well as image level phenotype information. Ideally this type of data can be used for cell clustering, point process models or nearest neighbour analysis. 
## Installation

You can install the released version of SegmentedCellExperiment from GitHub with:

``` r
devtools::install_github("ellispatrick/SegmentedCellExperiment")
```

## Example


Here we create toy data that can be used to make a `SegmentedCellExperiment` object

``` r
set.seed(51773)

n = 10

cells <- data.frame(row.names = seq_len(n))
cells$cellID <- seq_len(n)
cells$imageID <- rep(1:2,c(n/2,n/2))
cells$x <- runif(n)
cells$y <- runif(n)
cells$shape_round <- rexp(n)
cells$shape_diameter <- rexp(n, 2)
cells$intensity_CD8 <- rexp(n, 10)
cells$intensity_CD4 <- rexp(n, 10)
cells$cellType <- paste('cluster',sample(1:2,n,replace = TRUE), sep = '_')

```

We can then create a `SegmentedCellExperiment` object.

``` r

cellExp <- SegmentedCellExperiment(cells, cellTypeString = 'cellType', intensityString = 'intensity_', morphologyString = 'shape_')
cellExp

```


Extract location information

``` r
loc <- location(cellExp)
head(loc)

```



Extract morphology information

``` r
morph <- morphology(cellExp)
head(morph)

```
