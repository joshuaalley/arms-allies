# arms-allies
This repository contains TeX files as well as R and STAN code for my work on alliance participation and military spending. In the manuscript I argue that deep alliances reduce free-riding by non-major powers. 

The data folder contains everything needed to replicate the data construction process and analysis. Start by loading packages and functions with `setup-script.R`. Then use `alliance-measures`, followed by `dataset construction and summary.R` to create the dataset. `analysis-split-sample` estimates the association between alliance participation and military spending in separate samples of major and non-major powers and generates the results in the manuscript. `analysis-joint.R` runs a varying slopes model on the full dataset. The script for `single-level-regression` checks the multilevel regression with some single-level regressions which are reported in the appendix. 

Last, `simulation-check.R` simualtes fake data and checks whether the multilevel model can recover known parameters. 

`alliance-measures` includes code to generate a latent measure of treaty depth, using observed data from the [ATOP project](http://www.atopdata.org/)

`model comparison.R` compares the full model to two simpler specifications using loo and WAIC. This script is not up to date.  
