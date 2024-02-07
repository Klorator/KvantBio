# Matte datalab 2

## Fråga 1
options(digits = 10)
pi
options(digits = 7)
exp(1)

## Fråga 2
#     x^4 - 7x^3 - 23x^2 + 39x + 54 = 0
# => +54 +39x -23x^2 -7x^3 +1x^4 = 0
# => c(54, 39, -23, -7, 1)
polynom <- c(54, 39, -23, -7, 1) 
roots <- polyroot(polynom)
roots

## Fråga 3
#     x^2 -3x -1/2 = 0
# => c(-1/2, -3, 1)
polynom2 <- c(-1/2, -3, 1)
roots2 <- polyroot(polynom2)
roots2

# mer exakta lösningar
roots2.1 <- (+3+sqrt(11))/2 # roots2[2]
roots2.2 <- (+3-sqrt(11))/2 # roots2[1]

## Fråga 4
ekv1 <- function(x) {
  exp(x) + 2*x -4
}

curve(
  ekv1,
  xlim = c(-1,2),
  col = "blue"
)
abline(h = 0)

root3 <- uniroot(ekv1, c(0,2))
root3$root

## Fråga 5
ekv2 <- function(x) {
  x^3 -(2/3)*x^2 -7
}

Deriv::Deriv(ekv2)      # Fel: x * (2 * (x - 0.666666666666667) + x)
Deriv::Deriv(ekv2, "x") # Rätt: x * (3 * x - 1.33333333333333)
3*5^2 -(4/3)*5 # x=5
5 * (3 * 5 - 1.33333333333333) # x=5

## Fråga 6
ekv3 <- function(x) {
  ( log(x)+log(x) ) / ( log(x) + 1 )
}

Deriv::Deriv(ekv3, "x",
             cache.exp = F) 
# => (2 - 2 * (log(x)/(1 + log(x))))/(x * (1 + log(x)))

## Fråga 7
ekv4 <- function(t) {
  t^2 * exp(t^2 + 7)
}

Deriv::Deriv(ekv4, "t",
             cache.exp = F)
# t * (2 + 2 * t^2) * exp(7 + t^2)

## Fråga 8
ekv5 <- function(x) {
  -x^2 +4*x -7
}

ekv5_deriv <- Deriv::Deriv(ekv5, "x")

curve(ekv5_deriv, xlim = c(0,3))
abline(h = 0)

ekv5_deriv_root <- uniroot(ekv5_deriv, c(0,3))
ekv5_deriv_root$root
























