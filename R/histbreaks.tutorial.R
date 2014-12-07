# Histbreaks example

# Source
source('histbreaks.R')

# Define parameters
N = 1000
x <- rexp(N,5)
y <- runif(N,1.5,2.5)
z <- rnorm(N,1,0.1)
w <- c(x,y,z)

# Open graphics device
png(filename="hist_example.png",bg="transparent")

# Plot
par(mfrow=c(2,1))
hist(w,breaks="Sturges",main="Using Sturges")
#hist(w,breaks="Scott",main="Using Scott")
hist(w,breaks=sshistx(w),main="Using Sshistx")

# Save
dev.off()

# # Another example
# d <- faithful$waiting
# hist(d,breaks="Sturges")
# hist(d,breaks=sshistx(d))
