"%not.in%" <- function(x,y) {
    x[!x %in% y]
}

N = 100
M = 25
spl = 1/2
B = 10

eps=1e-10

x = rnorm(N)
breaks = seq(min(x)-eps,max(x)+eps,length=B+1)

c = cut(x,breaks=breaks,label=F)
t = table(c) # should be same as histogram counts
t.dens = t/sum(t) # provides density

for (k in 1:M) {
    kk = sample(N,spl*N,replace=F)
    x1 = x[kk]
    x2 = x[1:N %not.in% kk]
    c1 = cut(x1,breaks=breaks,label=F)
    t1 = tabulate(c1,B)
    c2 = cut(x2,breaks=breaks,label=F)
    t2 = tabulate(c2,B)
    out[k] = sum(abs(t1-t2))
}

out2 = mean(out)

out

out2
