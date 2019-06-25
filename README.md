# arms-allies
This repository contains TeX files as well as R and STAN code for a paper on alliance participation and military spending. I argue that the strength of treaty commitment shapes military spending by member states, with different effects in more or less capable states.  

The data folder contains everything needed to replicate the data construction process and analysis. Use `alliance-measures`, then `dataset construction and summary.R` to create the dataset. `analysis-split-sample` estimates the association between alliance participation and military spending in separate samples of major and non-major powers. `analysis-joint.R` runs the model I present in the paper on the full dataset, implements some diagnostic checks, and summarizes the output. 

Last, `simulation-check.R` simualtes fake data and checks whether the multilevel model can recover known parameters. 

`alliance-measures` includes code to generate a latent measure of treaty strength, using observed data from the [ATOP project](http://www.atopdata.org/)

`model comparison.R` compares the full model to two simpler specifications using loo and WAIC. This part of the project is not up to date. 
