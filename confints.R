#install_github("mhoehle/quantileCI")

weighted.ttest.ci <- function(x, weights, conf.level = 0.95) {
    #require(Hmisc)
    nx <- length(x)
    df <- nx - 1
    vx <- Hmisc::wtd.var(x, weights, normwt = TRUE) ## From Hmisc
    mx <- weighted.mean(x, weights)
    stderr <- sqrt(vx/nx)
    tstat <- mx/stderr ## not mx - mu
    alpha <- 1 - conf.level
    cint <- qt(1 - alpha/2, df)
    cint <- tstat + c(-cint, cint)
    cint * stderr
}


wwilcox = function( x, y, wx  ){

U = 0
## Loop over the selection branches
for( iy in y ){

## Neutral branches smaller or equal
smaller = which(  x < iy )
equal = which( x == iy )

## Count
sumSmaller = sum(wx[smaller])
sumEqual = sum(wx[equal]/2)
sumTot = sumSmaller + sumEqual

## Total rank
U = U + sumTot
}

## U statistics
nY = length(y)
nX = sum(wx)

## Large sample: U follows a Gaussian
mU = nY * nX / 2
sigU = sqrt( ( nY * nX * ( 1 + nY + nX ) ) / 12 )
zU = ( U - mU ) / sigU

## p-value, one-sided
pU = erfc( zU / sqrt(2) ) /2

return(pU)
}

## Complementary error function
erfc = function(x) 2 * pnorm(x * sqrt(2), lower = FALSE)
#courtesy of https://stackoverflow.com/questions/24648052/weighted-wilcoxon-test-in-r


##for bootstrap functions, see:
source("bootstraps.R")
