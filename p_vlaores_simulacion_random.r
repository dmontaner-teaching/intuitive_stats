##simulacion_random.r
##2010-03-21 dmontaner@cipf.es
##simulamos pvalores

## FOLDCHANGE

library (genefilter)

datos <- matrix (rnorm (200000), ncol = 20)

dim (datos)

myfactor <- factor (rep (0:1, times = 10)) #20 individuos
myfactor

tt <- rowttests (datos, fac = myfactor, tstatOnly = FALSE)

tt[1:3,]

boxplot (tt[,3])

hist (tt[,3], breaks = 100, freq = FALSE)
lines (density (tt[,3]), col = 'red')

##################################################

R <- 100   # repeats
N <- 10

x <- rnorm (N)
y <- rnorm (N)

summary (x)
summary (y)
boxplot (x, y)

t.test (x,y)
tout <- t.test (x,y)
tout$statistic
tout$p.value

for (i in 1:R) {
    print (i)
    x <- rnorm (N)
    y <- rnorm (N)
    tout <- t.test (x,y)
    print (tout$statistic)
    tout$p.value
}


stat <- NULL
pval <- NULL
for (i in 1:R) {
    print (i)
    x <- rnorm (N)
    y <- rnorm (N)
    tout <- t.test (x,y)
    stat <- c (stat, tout$statistic)
    pval <- c (pval, tout$p.value)
}

stat
pval

plot (stat, pval)


N <- 10
R <- 1000
stat <- NULL
pval <- NULL
mist <- NULL
mifc <- NULL
milf <- NULL
mite <- NULL
for (i in 1:R) {
    #print (i)
    r <- sample (-5:5, 1)
    x <- rnorm (N + r)
    y <- rnorm (N + r)
    tout <- t.test (x,y)
    stat <- c (stat, tout$statistic)
    pval <- c (pval, tout$p.value)
    tt <- mean (x) - mean (y)
    fc <- mean (x) / mean (y)
    lf <- log (fc)
    mist <- c (mist, tt)
    mifc <- c (mifc, fc)
    milf <- c (milf, lf)
    ##ts <- (mean (x) - mean (y)) * sqrt (length (c (x,y)))/ sd (c(x, y)) ## mi estadistico de la T
    #ts <- (mean (x) - mean (y)) / sqrt (0.5*var (x) + 0.5 * var (y))
    ts <- (mean (x) - mean (y)) / (sd (c(x,y)) / sqrt (length (x)))
    mite <- c (mite, ts)
}
plot (stat, pval)
##
plot (stat, mist)
abline (v = 2)

plot (stat, mifc)
abline (v = 2)

plot (stat, milf)
abline (v = 2)

plot (stat, mifc, ylim = c(-10, 10))
abline (v = 2)

plot (stat, mite)
abline (0, 1)




length (stat)
hist (stat, breaks = 30, freq = FALSE)
lines (density (stat), col = "red")
plot (density (stat))









