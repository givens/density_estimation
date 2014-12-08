## Function `sshistx` uses a modification of sshist to return
## the optimal number of bins in a histogram.  It is used for
## density estimation.
##
## Optimization principle is to minimize the expected L2 loss
## function between the histogram and an unknown density
## function.
##
## Original Paper:
## Hideaki Shimazaki and Shigeru Shinomoto
## A method for selecting the bin size of a time histogram
## Neural Computation 19(6), 1503-1527, 2007
## http://dx.doi.org/10.1162/neco.2007.19.6.1503
##
## The original function is modified to employ an
## adhoc smoother function to sequentially smooth
## the raw cost function.  The minimum of the smoothed
## cost is employed to determine optimality.  The smoother
## is a local regression called loess.
##
## Computations are more efficient by using `mapply`.
##
## Example usage:
## source('histbreaks.R')
## x <- rnorm(1000,0,1)
## hist(x,breaks=sshistx(x))
##
## Input arguments:
##  x - Sample data set
##  N (optional) - a vector that specifies the maximum bin size
##                  to be examined.  Default value is 500.
##  alpha (optional) - a smoothing parameter, where [0.05,0.25]
##                      is recommended.
##
## Output list arguments:
##  edges - Optimal segmentation of edges
##
## Implementation by Brendhan Givens.
sshistx <- function(x,N=500,alpha=0.10) {

    #################################
    # Parameters Settings

    x <- as.vector(x)
    x.min <- min(x)
    x.max <- max(x)

    buf <- abs(diff(sort(x)))
    dx = min(buf[buf!=0])
    N_MIN=2 # minimum number of bins
    N_MAX=min(floor(x.max-x.min)/(2*dx),N) # Maximum number of bins
    N=N_MIN:N_MAX

    SN = 30
    D = (x.max-x.min)/N # Bin size vector

    ##################################
    # Computation of the Cost Function

    C <- mapply(compute.cost,
                D=D,N=N,x.min=x.min,x.max=x.max,
                MoreArgs=list(x=x))

    ###########################################
    # Smooth Raw Signal which is noisy.
    # Smoother is sensitive to alpha.

    smoothed <- loess(C~N,span=alpha)
    C.lo <- predict(smoothed,N)

    ###########################################
    # Optimal Bin Segmentation Selection

    idx <- which.min(C.lo) # Location of optimal smoothed cost
    seq(x.min,x.max,length=N[idx]+1) # Optimal segmentation
}

##############################
# Compute Histogram Cost Function Formula

compute.cost <- function(x,D,N,x.min,x.max) {
    edges <- seq(x.min,x.max,length=N+1) # bin edges
    v <- hist(x,
              breaks=seq(x.min,x.max,length=N+1),
              plot=F) # count number of events in bins
    k <- mean(v$counts) # mean of the event count
    v <- sum((v$counts-k)^2)/N # variance of the event count
    C <- (2*k-v)/D^2 # the cost function
}
