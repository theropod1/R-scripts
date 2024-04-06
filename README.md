# R-scripts

This repository is intended as a public archive for various custom scripts defining functions in R, in the hopes that they might be useful to someone. Documentation and explanations may be a bit subpar, but I’ll try to briefly explain essential functionalities as comments. Some of these might end up in a package some day, if there is demand. If there are any questions or suggestions, feel free to contact me!

CONTENT
###colors
col_asign.R #automatically assign colours

###statistical parameters
rowmeans.R #calculates a sequence of means (or other parameters) for rows in a data.frame or matrix
seprop.R #standard error of the population proportion, also normal standard error for continuous variable
geomean.R #geometric mean
modelp.R #p value for lm

###paleontology
paleoDiv.R # Source code of the upcoming paleoDiv package. Includes content of previous files div_functions.R, viol.R, addalpha.R, darken.R, tsperiods.R, rmeans.R and phylospindles.R in one file useful for graphing biodiversity and abundance patterns in relation to time and phylogeny, accompanied by various utility functions.
proc_dist.R #calculate procrustes distances based on aligned procrustes coordinates in morphometric analysis

###miscellaneous and plotting
cos.int.R #cosine interpolation
export.R #function to export current plot window in various sizes
ci.lm.R #shortcut for plotting linear model with confidence/prediction interval
datamatch.r #match two sets of data of different length and/or order based on a common identifier variable
