
rm (list = ls ())

################################################################################

### Dos poblaciones
SD <- 3
X <- rnorm (1000000, mean = 1, sd = SD)
Y <- rnorm (1000000, mean = 0, sd = SD)

length (X)
length (Y)

X[1:10]
Y[1000:1010]

summary (X)
summary (Y)

par (mfrow = c (2, 1))
##
hist (X, xlim = c(-8, 8))
abline (v = 1, lwd = 3, col = "blue")
abline (v = 0, lwd = 3, col = "green")
##
hist (Y, xlim = c(-8, 8))
abline (v = 1, lwd = 3, col = "blue")
abline (v = 0, lwd = 3, col = "green")

boxplot (X, Y)
abline (h = 1, col = "blue")
abline (h = 0, col = "green")

################################################################################

## MUESTREO ALEATORIO REPETIDO Y TEST DE LA T

N <- 200
R <- 1000
rept <- NULL
repp <- NULL
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
}

plot (rept, repp)
abline (h = 0.05, col = "red")
abline (v = c (-1.96, 1.96), col = "grey")


#### POTENCIA ALCANZADA (capacidad para detectar la diferencia poblacional cuando existe)

table (repp < 0.05) / length (repp)

## falsos negativos
#table (repp > 0.05)
#table (repp > 0.05) / length (repp)
