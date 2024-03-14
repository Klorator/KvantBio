# Differensekvation ####

# Vi kan tyvärr inte be R att fixa en explicit lösning

# b - beräkna x_7

y<-c()
y[1]<-5
y[2]<-6

for(i in 1:10){
  y[i+2]=0*y[i+1]+4*y[i]
}

y[8]

z <- c(5,6)

for (i in 3:12) {
  z[i] <- 0 * z[i-1] + 4 * z[i-2]
}

z[8]


# Derivator ####

# b - Bestäm kritiska punkter, dvs då f'(x) = 0

fun <- function(x){x^2*exp(1-x^2)}

fun_deriv <- function(x){2*x*(1-x^2)*exp(1-x^2)}

plot(fun_deriv, xlim=c(-4,4), ylim=c(-2,2), col=2); abline(h=0)

rotter <- uniroot(fun_deriv, interval=c(-2, -0.5)); x_1 <- rotter$root
rotter <- uniroot(fun_deriv, interval=c(-0.5, 0.5)); x_2 <- rotter$root
rotter <- uniroot(fun_deriv, interval=c(0.5, 4)); x_3 <- rotter$root

# d - Bestäm tangentlinjen i punkten x = 2

lutning <- fun_deriv(2)

m <- fun(2)-lutning*2

cat("Tangentlinjens lutning blir f(x)=",lutning,"*x+",m)

# Matriser ####

library(deSolve)

A <- matrix(c(2,6,0,-1), byrow=T, nrow=2)

egen <- eigen(A)

# a - Bestäm egenvärden
lambda_1 <- egen$values[1] 
lambda_2 <- egen$values[2]

# b - Bestäm egenvektorer
v_1 <- egen$vectors[,1]
v_2 <- egen$vectors[,2]


# c - Diagonaliserina A
D <- matrix(c(lambda_1,0,0,lambda_2), byrow=T, nrow=2)

C <- matrix(c(v_1[1],v_2[1],v_1[2],v_2[2]), byrow=T, nrow=2)

C_inv <- solve(C)

# d - Beräkna A^n
library(expm)

n <- 8

C%*%(D%^%n)%*%C_inv

A%^%n

# Diskreta Dynamiska system ####

Diskret_Funk <- function(x){(5*x^2)/(x^2+6)}

# Diskret_Funk <- f(x) = (5*x^2)/(x^2+6)

# b - bestäm jämviktspunkterna

Diskret_Funk_Eq <- function(x){((5*x^2)/(x^2+6))-x}

plot(Diskret_Funk_Eq, ylim=c(-0.5,0.5), xlim=c(-1,5)); abline(h=0)

rotter <- uniroot(Diskret_Funk_Eq, interval=c(-1, 1)); x_1 <- rotter$root
rotter <- uniroot(Diskret_Funk_Eq, interval=c(1, 2.5)); x_2 <- rotter$root
rotter <- uniroot(Diskret_Funk_Eq, interval=c(2.5, 5)); x_3 <- rotter$root

print(x_1);print(x_2);print(x_3)

# c - Avgör om de är stabila

dX <- function(x) {fderiv(Diskret_Funk,x)}

abs(dX(x_1)) > 1 # Stabil
abs(dX(x_2)) > 1 # Instabil
abs(dX(x_3)) > 1 # Stabil

check_stabilitet <- function(x) {
  if (abs(x) > 1) {
    print(paste(x, "är stabil"))
  } else {
    print(paste(x,"är instabil"))
  }
}
check_stabilitet(x_1)
check_stabilitet(x_2)
check_stabilitet(x_3)
