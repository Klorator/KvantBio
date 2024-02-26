# Fibonacci test

## Fråga 1
fib <- c()
fib[1] <- 10
fib[2] <- 20

for (i in 3:12) {
  fib[i] <- fib[i-1] + fib[i-2]
}

## Fråga 2
plot(fib)



# Seminarie
y<-c()
y[1]<-100 
y[2]<-140 
for (i in 3:20) {
  y[i] <- y[i-1] + y[i-2] 
} 
y

kvot <- c()
for (i in 2:20) {
  kvot[i] <- y[i] / y[i-1]
}
kvot

curve(0.061*x^1.09,
      col = "darkgreen",
      xlim = c(28,29))
curve(0.065*x^1.071,
      col = "blue",
      add = T)

## Calculate intersect
# 0.061*x^1.09 = 0.065*x^1.071
f1 <- function(x) 0.061*x^1.09
f2 <- function(x) 0.065*x^1.071

curve(f1)
curve(f2,add=TRUE,col=2)

uniroot(function(x) f1(x)-f2(x), c(1,100))$root
