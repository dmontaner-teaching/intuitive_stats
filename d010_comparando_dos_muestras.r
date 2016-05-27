
rm (list = ls ())

################################################################################

### Dos poblaciones

X <- rnorm (1000000, mean = 0, sd = 1)
Y <- rnorm (1000000, mean = 0, sd = 1)

length (X)
length (Y)

X[1:10]
Y[1:10]
Y[1000:1010]

summary (X)
summary (Y)

hist (X)
x11()
hist (Y)

boxplot (X, Y)

################################################################################

## MUESTREO ALEATORIO

N <- 100 ## sample size
N

x <- sample (X, size = N)
y <- sample (Y, size = N)

x
y

################################################################################

## Exploramosla muestra

summary (x)
summary (y)
boxplot (x, y)

## parametro de centralidad
mx <- mean (x)
my <- mean (y)

mx
my

##difencia de medias
mx - my

##fold change
mx/my

##log fold change
log (mx/my)

##varianza
var (x)
var (y)

##desviacion estandar
sd (x)
sd (y)

################################################################################

## Inferencia sobre la poblacion a partir de la muestra

## estadistico de la t (mas o menos)
t <- (mx - my) * sqrt (N/2) / sd (c (x, y))
t

##funcion directa en R
t.test (x, y)

## CAMBIAMOS LOS PARAMETROS POBLACIONALES
## VEMOS EL SIGNO DEL ESTADISTICO

################################################################################

## distribucion del estadistico bajo la HIPOTESIS NULA

R <- 1000
rept <- NULL
repmd <- NULL
reped <- NULL
for (i in 1:R) {
    print (i)

    ##muestreo
    x <- sample (X, size = N)
    y <- sample (Y, size = N)
    
    ## resumen: busqueda de estadistico suficiente
    mx <- mean (x)
    my <- mean (y)
    md <- mx - my
    ed <- sd (c(x, y)) / sqrt (N/2)
    t <- md / ed
    
    ##guardamos
    rept <- c (rept, t)
    repmd <- c (repmd, md)
    reped <- c (reped, ed)
}
rept[1:10]

hist (rept)

plot (repmd, reped)

extremo <- abs (rept) > 2
table (extremo)

plot (repmd, reped, col = c ("blue", "red")[extremo + 1])


################################################################################

##PVALORES

# para una unica muestar
x <- sample (X, size = N)
y <- sample (Y, size = N)

## resumen: busqueda de estadistico suficiente
mx <- mean (x)
my <- mean (y)
md <- mx - my
ed <- sd (c(x, y)) / sqrt (N/2)
t <- md / ed
t

hist (rept)
abline (v = t)
abline (v = 2, col = "red")

## el pvalor es el area que queda por debajo de t si es negativo o por encima si t es positivo
rept < t
sum (rept < t)
sum (rept < t) / length (rept)

sign (t)

if (sign (t) == 1) {
    pvalor <- 2 * sum (rept > t) / length (rept)
} else {
    pvalor <- 2 * sum (rept < t) / length (rept)    
}
pvalor


##funcion directa en R
t.test (x, y)

################################################################################

### REPETIMOS
tt <- t.test (x, y)
tt
tt$statistic
tt$p.value
tt$estimate[1]
tt$estimate[2]
tt$estimate[1] - tt$estimate[2]

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


### POR QUE USAR EL P-VALOR Y NO EL ESTADISTICO
N
N2 <- 5
R <- 1000
rept2 <- NULL
repp2 <- NULL
for (i in 1:R) {
    print (i)
    
    ##muestreo
    x <- sample (X, size = N2)
    y <- sample (Y, size = N2)
    
    ## t-test
    tt <- t.test (x, y)
    
    ##guardamos
    rept2 <- c (rept2, tt$statistic)
    repp2 <- c (repp2, tt$p.value)
}

#plot (rept2, repp2, col = "green", pch = 20)
points (rept2, repp2, col = "green", pch = 20)


### FALSOS POSITIVOS
table (repp2 < 0.05) / length (repp2)
table (repp  < 0.05) / length (repp)
