# Individuell tillväxt med von Bertalanffys tillväxtekvation

## Fråga 1-2
L <- function(t, L_inf = 317.65, k = 0.057, L0 = -44.54) {
  L <- L_inf + (L0 - L_inf) * exp(-k * t)
  return(L)
}

L(10)


## Fråga 3-5
ekvation <- function(
    t, 
    L_inf = 317.65, 
    k = 0.057, 
    L0 = -44.54, 
    L_t1 = 141) {
  result <- L(t, L_inf, k, L0) - L_t1
}
L_t1 <- 141

root <- uniroot(ekvation, c(0, 100))
t1 <- root$root

## Fråga 6-7
t2 <- t1 + 1
L_t2 <- L(t2)

## Fråga 8
W <- function(L) {
  weight <- 8.11 * 10^(-6) * L^(3.08)
  return(weight)
}

W(L_t1)
W(L_t2)
