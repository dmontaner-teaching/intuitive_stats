
rm (list = ls ())

################################################################################

### Dos poblaciones
SD <- 4
X <- rnorm (1000000, mean = 10, sd = SD)
Y <- rnorm (1000000, mean = 5, sd = SD)

length (X)
length (Y)

X[1:10]
Y[1000:1010]

summary (X)
summary (Y)

par (mfrow = c (2, 1))
##
hist (X, xlim = c(-8, 18))
abline (v = 10, lwd = 3, col = "blue")
abline (v = 5, lwd = 3, col = "green")
##
hist (Y, xlim = c(-8, 18))
abline (v = 10, lwd = 3, col = "blue")
abline (v = 5, lwd = 3, col = "green")

boxplot (X, Y)
abline (h = 10, col = "blue")
abline (h = 5, col = "green")

################################################################################

## MUESTREO ALEATORIO REPETIDO Y TEST DE LAsu T

N <- 100
R <- 1000
rept <- NULL
repp <- NULL
repm <- NULL
repf <- NULL
for (i in 1:R) {
    print (i)
    
    ##muestreo
    x <- sample (X, size = N)
    y <- sample (Y, size = N)
    
    ## t-test
    tt <- t.test (x, y)
    
    ##guardamos
    rept <- c (rept, tt$statistic)
    repp <- c (repp, tt$p.value)
    repm <- c (repm, tt$estimate[1] - tt$estimate[2])
    repf <- c (repf, tt$estimate[1] / tt$estimate[2])
}

summary (repf)
boxplot (repf)
boxplot (log (repf))

plot (rept, repp)
abline (h = 0.05, col = "red")
abline (v = c (-1.96, 1.96), col = "grey")

boxplot (x, y)

plot (rept, repm)
abline (0, 1, col = "red")

plot (repm, repp)
abline (0, 1, col = "red")

plot (repm, log (repp))
abline (0, 1, col = "red")
