# Some histbreaks sensitivity tests

# Define parameters
set.seed(1111)
N = 1000
x <- rexp(N,5)
y <- runif(N,1.5,2.5)
z <- rnorm(N,1,0.1)
w <- c(x,y,z)

ph1 = "Using Sshistx"
ph2 = "Using Freedman-Diaconis's (FD)"
ph3 = "Using Sturges's"
ph4 = "Using Scott's"



## Sensitivity to Inliers
png("hist_sensitivity_inliers.png",bg="transparent")
val = 1.25
par(mfrow=c(2,1))
hist(w,breaks=sshistx(w),
     main=paste(ph1,"Without inliers",sep=", "))
w1 <- c(w,rep(val,n))
hist(w1,breaks=sshistx(w1),,
     main=paste(ph1,"With inliers",sep=", "))
dev.off()

# Sensitivity to Outliers
png("hist_sensitivity_outliers.png",bg="transparent")
val = 4
n = 20
par(mfrow=c(2,1))
hist(w,breaks=sshistx(w),xlim=c(0,val),
     main=paste(ph1,"Without outliers",sep=","))
w1 <- c(w,rep(val,n))
hist(w1,breaks=sshistx(w1),xlim=c(0,val),
     main=paste(ph1,"With outliers",sep=","))
dev.off()

# Sensitivity to Population Size
png("hist_sensitivity_popsize.png",bg="transparent")
p = c(1/10,1/4,1,3)*N
par(mfrow=c(length(p),2))
for (k in seq_along(p)) {
    w1 = sample(w,p[k])
    hist(w1,breaks=sshistx(w1),
         main=sprintf("%s, Population = %i",ph1,p[k]))
    hist(w1,breaks="Scott",
         main=sprintf("%s, Population = %i",ph4,p[k]))
}
dev.off()

# Comparison of Normals for different populations
png("hist_sensitivity_normals.png",bg="transparent")
v = rnorm(3*N)
par(mfrow=c(length(p),2))
for (k in seq_along(p)) {
    v1 = sample(v,p[k])
    hist(v1,breaks=sshistx(v1),
         main=sprintf("%s, Normal, Population = %i",ph1,p[k]))
    hist(v1,breaks="Scott",
         main=sprintf("%s, Normal, Population = %i",ph4,p[k]))
}
dev.off()

# Going toward asymptotic
png("hist_sensitivity_asymp.png",bg="transparent")
M = 1e5
par(mfrow=c(2,1))
r = rnorm(M)
hist(r,breaks=sshistx(r,N=200),
     main=sprintf("%s, Large Population",ph1)) # This takes a long time
hist(r,breaks="Scott",
     main=sprintf("%s, Large Population",ph4))
dev.off()

