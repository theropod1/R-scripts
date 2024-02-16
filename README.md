# R-scripts

This repository is intended as a public archive for various custom scripts defining functions in R, in the hopes that they might be useful to someone. Documentation and explanations may be a bit subpar, but I’ll try to briefly explain essential functionalities as comments. Some of these might end up in a package some day, if there is demand. If there are any questions or suggestions, feel free to contact me!

CONTENT
###colors
ggcol.R #ggplot color palette
darken.R #darken/lighten colours
col_asign.R #automatically assign colours
addalpha.R #add transparency

###statistical parameters
rowmeans.R #calculates a sequence of means (or other parameters) for rows in a data.frame or matrix
rmean.R #two different rolling means
seprop.R #standard error of the population proportion, also normal standard error for continuous variable
geomean.R #geometric mean
modelp.R #p value for lm

###paleontology
div_functions.R #various functions for downloading, analyzing and visualizing paleobiodiversity-related data from paleobiodb– see file for details
proc_dist.R #calculate procrustes distances based on aligned procrustes coordinates in morphometric analysis

###miscellaneous and plotting
cos.int.R #cosine interpolation
export.R #function to export current plot window in various sizes
viol.R #plot violin plots for distributions in R base graphics (doesn’t require ggplot2)
