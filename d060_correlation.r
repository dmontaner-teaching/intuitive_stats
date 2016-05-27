
rm (list = ls ())

################################################################################

N <- 15 
x <- rnorm (N, sd = 1)
z <- rnorm (N, sd = 1)
e <- rnorm (N, sd = 1)
y <- 2 + 3 * x + 10 * z + e
plot (x, y)
plot (z, y)

summary (lm (y ~ x + z))


abline (lm (y ~ x), col = "red")
summary (lm (y ~ x))

cor.test (x, y)



plot (t, x, ylim = c(95, 105))
for (i in 2:10){
    x <- rnorm (N, 100,)
    t <- runif (N)
    points (t, x, col = i)
    abline (lm (x~t), col = i)
}

################################################################################


y









