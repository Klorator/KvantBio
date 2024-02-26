# Datalabb 4 Mattematik

## Fråga 1 ----
library(expm)
L <- rbind(
  r1 = c(    0,  2,   3),
  r2 = c(13/32,  0,   0),
  r3 = c(    0, 2/13, 0)
)
L5 <- L %^% 5
MASS::fractions(L5)

## Fråga 2 ----
library(pracma)
charpoly(L, info = T)
# $cp -- gives the coefficients of the Characteristic Polynom
# $det -- the determinant
# $inv -- the inverse matrix


## Fråga 3 ----
# egenvärden
L_eigen <- eigen(L)
L_eigVal <- L_eigen$values
# L_eigVal[1] * L_eigVal[2] * L_eigVal[3]
# det(L)
MASS::fractions(L_eigVal[1] * L_eigVal[2] * L_eigVal[3])
MASS::fractions(det(L))


## Fråga 4 ----
MASS::fractions(L_eigen[["values"]])
MASS::fractions(L_eigen[["vectors"]])

# Alla dessa är fel...
# lamda 1 = 1    -> 16, 13/2, 1   ??
# lamda 2 = -3/4 -> 9, -39/8, 1   ??
# lamda 3 = -1/4 -> 1, -13/8, 1   ??


# Fråga 5 ----
# Future population distribution
x0 <- c(300,200,100)

future_dist <- (L %^% 1000) %*% x0
future_dist


## Fråga 6 ----
library(deSolve)
### Function
my_ode <- function(t, # Gets `times` argument from ode
                   state, # Gets `y` argument from ode
                   parms # Gets `parms` argument from ode
                   ) {
  y <- state[1]
  
  dydt <- rep(0, length(state))
  
  dydt[1] <- t - y^2
  
  return(list(dydt))
}

### Parameters
init <- c( # Vector with initial values
  y = 5
)

t <- seq(1, 3, 0.1) # Från 1 till 3 med steglängd 0.1

params <- c( # Parameters to use in custom function
  k = 0.2 # Andra parametrar (bara exempel, används inte)
)

### Solve the problem
out <- deSolve::ode(
  y = init, # Goes to `state`
  times = t, # Goes to `t`
  func = my_ode, # Custom function to use
  parms = params # Goes to `parms`
)

### Plot
plot(
  out, 
  type = "l",
  xlab = "Time",
  ylab = "Stuff"
)


## Fråga 7 ----
### Function
my_ode_2 <- function(t, # Gets `times` argument from ode
                   state, # Gets `y` argument from ode
                   parms # Gets `parms` argument from ode
) {
  y <- state[1]
  
  dydt <- rep(0, length(state))
  
  dydt[1] <- 0.5 * (10 - y) * y
  
  return(list(dydt))
}

### Parameters
init2 <- c( # Vector with initial values
  y = 2
)

t2 <- seq(0, 2, 0.01) # Från 0 till 2 med steglängd 0.01

params2 <- c( # Parameters to use in custom function
  k = 0.2 # Andra parametrar (bara exempel, används inte)
)

### Solve the problem
out2 <- deSolve::ode(
  y = init2, # Goes to `state`
  times = t2, # Goes to `t`
  func = my_ode_2, # Custom function to use
  parms = params2 # Goes to `parms`
)

### Plot
plot(
  out2, 
  type = "l",
  xlab = "Time",
  ylab = "Stuff"
)


## Fråga 8
my_ode_3 <- function(t, # Gets `times` argument from ode
                     state, # Gets `y` argument from ode
                     parms # Gets `parms` argument from ode
) {
  x <- state[1]
  y <- state[2]
  
  result <- rep(0, length(state))
  
  result[1] <- x - 0.5*x*y  # dxdt
  result[2] <- 0.2*x*y - 2*y  # dydt
  
  return(list(result))
}

### Parameters
init3 <- c( # Vector with initial values
  x = 5, # index 1
  y = 3  # index 2
)

t3 <- seq(0, 10, 0.01) # Från 0 till 2 med steglängd 0.01

params3 <- c( # Parameters to use in custom function
  k = 0.2 # Andra parametrar (bara exempel, används inte)
)

### Solve the problem
out3 <- deSolve::ode(
  y = init3, # Goes to `state`
  times = t3, # Goes to `t`
  func = my_ode_3, # Custom function to use
  parms = params3 # Goes to `parms`
)

### Plot
plot( # Ser okej ut
  out3[ , 1],
  out3[ , 2],
  col = "darkblue",
  type = "l",
  xlab = "Time",
  ylab = "Stuff"
)
lines( # Inte helt nöjd men kan lösa uppgiften
  out3[ , 1],
  out3[ , 3],
  col = "darkorange"
)


## Fråga 9 ----
# Jämviktslösning
plot(
  out3[ , 2],
  out3[ , 3],
  col = "darkblue",
  type = "l",
  xlab = "x(t)",
  ylab = "y(t)"
)

# Ögonmåttade lösningen utifrån bilden i quizet