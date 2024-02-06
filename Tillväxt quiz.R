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
  return(result)
}
L_t1 <- 141

root <- uniroot(ekvation, c(0, 100))
t1 <- root$root

## Fråga 6-7
t2 <- t1 + 1
L_t2 <- L(t2)

## Fråga 8-9
W <- function(L) {
  weight <- 8.11 * 10^(-6) * L^(3.08)
  return(weight)
}

W(L_t1)
W(L_t2)

curve(8.11 * 10^(-6) * L^(3.08),
      xname = "L",
      xlim = c(0,100),
      xlab = "Length",
      ylab = "Weight")

Weight_gain <- W(L_t2) - W(L_t1) # G i quizet
# Weight_gain_alt <- L_t2 - L_t1

## Fråga 10-11
energy_in_shark <- 5400 # kJ/kg of shark
energy_gain <- Weight_gain * energy_in_shark / 365 # kJ/dag, Produktion

# Konsumtion = Metabolism + Produktion + Exkretion
M <- 1166 # kJ/dag, Metabolism
P <- energy_gain


# Exkretion = Konsumtion * 0.27
# => K = M + P + 0.27*K
# => 0 = M + P + 0.27*K - K
Konsumtion_equ <- function(K) {
  K <- 1166 + 114.7418 + 0.27*K - K
  return(K)
}
uniroot(Konsumtion_equ,
        interval = c(0,50000)
        )$root # Energy konsumtion / day



## Fråga 12
L_t1_2 <- 120 # Length cm
ekvation2 <- function( # Ny ekvation för att beräkna ålder
    t, 
    L_inf = 317.65, 
    k = 0.057, 
    L0 = -44.54, 
    L_t1 = 120) {
  result <- L(t, L_inf, k, L0) - L_t1
  return(result)
}

t1_2 <- uniroot(ekvation2, c(0,100))$root # beräkna ålder
t2_2 <- t1_2 + 1 # Add 1 year 
L_t2_2 <- L(t2_2) # Length after 1 year

W_gain_2 <- W(L_t2_2) - W(L_t1_2)
energy_gain_2 <- W_gain_2 * energy_in_shark / 365

M_2 <- 757 # kJ/day, Metabolism

Konsumtion_equ_2 <- function(K) { # Ändra siffrorna i formeln!
  K <- 757 + 93.85071 + 0.27*K - K
  return(K)
}

K_daily_2 <- uniroot(Konsumtion_equ_2,
                     c(0,5000))$root


## Fråga 13

# L(t) = 340 - c * exp(-k * t)

L0 <- 2 # L(0) = 2 cm
L4 <- 150 # L(4) = 150 cm

# 2 = 340 - c * exp(-k * 0)
# => 2 = 340 - c * exp(0)
# => 2 = 340 - c * 1
find_c <- function(c) {
  340 - c * exp(0) - 2
}
c <- uniroot(find_c, c(0,100000))$root

# 150 = 340 - 338 * exp(-k * 4)
find_k <- function(k) {
  340 - 338 * exp(-k * 4) - 150
}
k <- uniroot(find_k, c(0,10))$root

340 - c * exp(-k * 4) # Ska ge 150

# Svar: L(t) = 340 - 338 * e^(-0.14 * t)

