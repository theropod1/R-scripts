# R-scripts

This repository is intended as a public archive for various utility functions in R, just in case they are useful to someone. Documentation and explanations are somewhat rudimentary – feel free to ask in case of questions. Here is a quick rundown of the available functions:

CONTENT
###statistical parameters
rowmeans.R #calculates a sequence of means (or other parameters) for rows in a data.frame or matrix
seprop.R #standard error of the population proportion, also normal standard error for continuous variable
geomean.R #geometric mean
modelp.R #p value for lm

###paleontology
paleoDiv.R # Source code of the paleoDiv package. Includes content of previous files div_functions.R, viol.R, addalpha.R, darken.R, tsperiods.R, rmeans.R and phylospindles.R in one file. useful for graphing biodiversity and abundance patterns in relation to time and phylogeny, accompanied by various utility functions. See package for details.
gdi.r # Source code of the gdi package. Functions for calculating volume, rotational inertia and second moment of area for various shapes. See package for details.
proc_dist.R #calculate procrustes distances based on aligned procrustes coordinates in morphometric analysis

###miscellaneous and plotting
col_asign.R #automatically assign colours based on unique factor levels of a variable
cos.int.R #cosine interpolation between two values
export.R #function for exporting current plot window to jpeg or png file
ci.lm.R #shortcut for plotting linear model with confidence/prediction interval
lab.lm.R #shortcut for adding model formula (from a lm()-object) to a plot
datamatch.r #match two sets of data of different length and/or order based on a common identifier variable
