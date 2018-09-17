
# Carregando pacotes utilizados

library(ggplot2)
library(gridExtra)
library(dplyr)

# 1 -----------------------------------------------------------------------

#a)
unif <- runif(100, min = 0, max = 10)

#b)
gam <- rgamma(100, shape = 2, rate = 3)

#c)
norm <- rnorm(100, mean = 3, sd = 9)

#d)
expo <- rexp(100, rate = 7)

# 2 -----------------------------------------------------------------------

grafb <- ggplot(data = data.frame(x=gam), aes(x=x)) +
  geom_histogram(aes(y=..density..), bins = 10) +
  stat_function(fun = dgamma, args = list(shape = 2, rate = 3), color = "red") +
  theme_minimal() +
  labs(title = "Distribuição Gama com shape=2 e rate=3", y="f(x)")

grafc <- ggplot(data = data.frame(x=norm), aes(x=x)) +
  geom_histogram(aes(y=..density..), bins = 10) +
  stat_function(fun = dnorm, args = list(mean = 3, sd = 9), color = "red") +
  theme_minimal() +
  labs(title = "Distribuição Normal com media=3 e desv. padrao=9", y="f(x)")

grafd <- ggplot(data = data.frame(x=expo), aes(x=x)) +
  geom_histogram(aes(y=..density..), bins = 10) +
  stat_function(fun = dexp, args = list(rate = 7), color = "red") +
  theme_minimal() +
  labs(title = "Distribuição Exponencial com rate=7", y="f(x)")

grid.arrange(grafb, grafc, grafd, ncol=2)

# 3 -----------------------------------------------------------------------

#a)
ppois(0, lambda = 5, lower.tail = F)

#b)
ppois(11, lambda = 5) - ppois(2, lambda = 5)

#c)
qpois(0.76, lambda = 5)

#d)
qpois(0.73, lambda = 5, lower.tail = F)
## ou
qpois(0.27, lambda = 5)

# 4 -----------------------------------------------------------------------

#a) 
pbeta(0.7, 3, 10, lower.tail = F)

#b)
pbeta(0.25, 3, 10) - pbeta(0.2, 3, 10)

#c)
qbeta(0.20, 3, 10)

#d)
qbeta(0.12, 3, 10, lower.tail = F)
## ou
qbeta(0.88, 3, 10)

# 5 -----------------------------------------------------------------------

#a)
qqplot(
  x = rnorm(200, mean = 10, sd = 5),
  y = rchisq(200, 2)
)

#b)
qqplot(
  x = rgamma(200, 3, 4),
  y = rchisq(200, 2)
)

#c)
qqplot(
  x = rpois(200, 2),
  y = rchisq(200, 2)
)

#d)
qqplot(
  x = rchisq(200, 2),
  y = rchisq(200, 2)
)

# 6 -----------------------------------------------------------------------

#a)
pexp(5, 5, lower.tail = F)

#b)
pexp(9, 5) - pexp(2, 5)

#c)
dexp(0, 5)

#d)
pexp(2, 5)

#e)
dbinom(2, 15, 0.3)
## ou
pbinom(2, 15, 0.3) - pbinom(1, 15, 0.3)

#f)
pbinom(4, 15, 0.3)

#g)
pbinom(5, 15, 0.3) - pbinom(-2, 15, 0.3)

#h)
pbinom(9, 15, 0.3) - pbinom(2, 15, 0.3)

#i)
pbinom(3, 15, 0.3, lower.tail = F)
## ou
1 - pbinom(3, 15, 0.3)

# 7 -----------------------------------------------------------------------

fun7 <- function(y) {
  if(y<1) return(0)
  if(y>3) return(1)
  return(integrate(
    (function(x) (x^3)/20), lower = 1, upper = y
  )[[1]]
  )
}

#a)
1 - fun7(2)

#b)
fun7(2.6) - fun7(2)

#c)
1 - fun7(2.8)

#d)
fun7(2) + (1 - fun7(2.5))

# 8 -----------------------------------------------------------------------

#a)
ggplot(data = data.frame(x=c(-1,11)), aes(x=x)) +
  stat_function(fun = punif, args = list(min = 0, max = 10)) +
  labs(y="F(x)", title="Distribuição acumulada de uma Uniforme(0,10)") + 
  theme_minimal()

#b)
ggplot(data = data.frame(x=c(0,5)), aes(x=x)) +
  stat_function(fun = pgamma, args = list(shape = 2 , rate = 3)) + 
  labs(y="F(x)", title="Distribuição acumulada de uma Gama(2,3)") + 
  theme_minimal()

#c)
ggplot(data = data.frame(x=c(-30,40)), aes(x=x)) + 
  stat_function(fun = pnorm, args = list(mean = 3, sd = 9)) +
  labs(y="F(x)", title="Distribuição acumulada de uma N(3,9)") + 
  theme_minimal()

#d)
ggplot(data = data.frame(x=c(0,2)), aes(x=x)) +
  stat_function(fun = pexp, args = list(7)) +
  labs(y="F(x)", title="Distribuição acumulada de uma Exponecial(7)") + 
  theme_minimal()

#e)
ggplot(data = data.frame(x=c(0,4)), aes(x=x)) + 
  stat_function(fun = Vectorize(fun7)) + 
  labs(y="F(x)", title="Distribuição acumulada de x^3/20") +
  theme_minimal()

#f)


# 9 -----------------------------------------------------------------------

#a)
# graf1 <- 
## ou
plot(dbinom(-1:110,100,0.85), type = 'h')

graf2 <- ggplot(data = data.frame(x=c(-1:110)), aes(x=x)) + 
  stat_function(fun = pbinom, args = list(100, 0.85)) +
  labs(y="F(x)"); graf2
## ou
plot(pbinom(-1:110,100,0.85), type = 's')

#b)
ggplot(data = data.frame(x=c(-5:25)), aes(x=x)) + 
  stat_function(fun = ppois, args = list(5)) +
  labs(y="F(x)")

# 10 ----------------------------------------------------------------------

expo <- rexp(1000, 5)

mat.amostras = matrix(NA, ncol=1000, nrow=200)

for(i in 1:1000){
  mat.amostras[,i] = sample(expo,200,replace = TRUE)
}

ggplot(data = data.frame(x=expo), aes(x=x)) +
  geom_histogram(aes(y=..density..)) +
  stat_function(fun = dexp, args = list(5), color="red") +
  geom_vline(aes(xintercept = mean(apply(mat.amostras,2,mean))), linetype = "dashed", color = "red" ) +
  geom_vline(aes(xintercept = mean(expo)), linetype = "dashed", color = "blue") +
  theme_minimal() +
  labs(y="")


