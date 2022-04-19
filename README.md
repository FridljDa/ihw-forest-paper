# ihw-forest-paper
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
TODO 

### **scripts/**
R scripts that run the simulations.
 
### **precomputed_results/**
Simulation results from scripts in the *scripts* directory that have been pre-computed/cached.

### **vignettes/**
R markdown documents that produce the pre-rendered vignettes above upon being compiled (using files saved in the *precomputed_results* folder). 

### **data/**
input data, mostly external real biological data
