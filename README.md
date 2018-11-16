# arms-allies
This repository contains TeX files as well as R and STAN code for a paper on alliance participation and military spending. I argue that the strength of treaty commitment shapes military spending by member states. 

The data folder contains everything needed to replicate the data construction process and analysis. Use `alliance-measures`, then `dataset construction and summary.R` to create the dataset. `analysis.R` runs the main model I present in the paper, some diagnostic checks, and summarizes the output. `model comparison.R` compares the full model to two simpler specifications using loo and WAIC.  
