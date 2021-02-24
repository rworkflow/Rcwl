# Rcwl

The Rcwl package can be a simple and user-friendly way to manage command line tools and build data analysis pipelines in R using Common Workflow Language (CWL).

## Installation

The package can be installed from Bioconductor (>= 3.9):

``` r
if (!requireNamespace("BiocManager", quietly = TRUE))
    install.packages("BiocManager")
BiocManager::install("Rcwl")
	
# Or from github
BiocManager::install("rworkflow/Rcwl")
```

## Hello world!
``` r
library(Rcwl)
inputs <- InputParamList(
    InputParam(id = "sth")
)
echo <- cwlProcess(baseCommand = "echo", inputs)
echo$sth <- "Hello World!"
res <- runCWL(echo)
```

## User Guide

``` r
vignette(package = "Rcwl")
```
