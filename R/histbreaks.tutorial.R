# Histbreaks tutorial

# Source
source('histbreaks.R')

# Define parameters
N = 1000
x <- rexp(N,5)
y <- runif(N,1.5,2.5)
z <- rnorm(N,1,0.1)
w <- c(x,y,z)

# Plot
par(mfrow=c(2,2))
hist(w,breaks="Sturges",main="Using Sturges")
hist(w,breaks=sshistx(w),main="Using Sshistx")

# # Another example
# d <- faithful$waiting
# hist(d,breaks="Sturges")
# hist(d,breaks=sshistx(d))
