# ihw-forest-paper
This build of the adaptMT package provides necessary modifications for using AdaPT with gradient boosted trees via the xgboost library. Please see our pre-print, Application of post-selection inference to multi-omics data yields insights into the etiologies of human diseases for details regarding our implementation. An updated tutorial to using both the adapt_xgboost and adapt_xgboost_cv functions will be available soon. The goal is to integrate the functionality of these modifications into the original adaptMT package that is available on the CRAN.


This repository serves as companion to the following paper: TODO

All numerical results and figures in the aforementioned paper are made third-party reproducible herein.

We note that the Bioconductor package IHW provides a user-friendly implementation of the IHW-Forest from version TODO.

This repository depends on `IHWStatsPaper`, a R package wrapping/implementing the different methods compared, the simulation functions, as well as the benchmarking code. It can be installed as follows. It further depends on the forest version of IHW.
```r
devtools::install_github("Huber-group-EMBL/covariate-powered-cross-weighted-multiple-testing",
                         subdir="IHWStatsPaper")
devtools::install_github("FridljDa/IHW", ref = "forest")                         
```

## Pre-rendered vignettes
First, below we provide links to pre-rendered vignettes that reproduce figures.
* Simulations  [[Vignette]](http://htmlpreview.github.io/?https://github.com/FridljDa/ihw-forest-paper/blob/main/vignettes/simulation.html)



### **scripts/**
R scripts that run the simulations.
 
### **precomputed_results/**
Simulation results from scripts in the *scripts* directory that have been pre-computed/cached.

### **vignettes/**
R markdown documents that produce the pre-rendered vignettes above upon being compiled (using files saved in the *precomputed_results* folder). 

### **data/**
input data, mostly external real biological data
