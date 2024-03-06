# KvantBio block 8

# Quiz 1 ----
library(deSolve)
# dSdt = (1 - (S/P))*c - ((S*m)/P)

# Funktion med ekvationssystem
island_fun <- function(times, # Intervall
                       y,     # Begynnelsevärden
                       parms  # Andra värden
                       ) {
  # variabler att använda
  c <- parms["c"]
  P <- parms["P"]
  m <- parms["m"]
  S <- y["S"]
  
  # Differentialekvationen att lösa numeriskt
  dSdt <- (1 - (S/P))*c - ((S*m)/P)
  
  # Spara & returnera resultatet
  result_vec <- c(
    dSdt
  )
  result_list <- list(result_vec)
  return(result_list)
}

# Vektor med begynnelsevärden
init_cond <- c(
  S = 5
)

# Vektor med andra värden för formeln
parameters <- c(
  c = 2,
  P = 50,
  m = 4
)

# Vektor med alla tidssteg (t) att sätta in
time_span <- seq(
  from = 0, # Start på intervall
  to = 100, # Slut på intervall
  by =   1  # Steglängd
)

# Lösa ekvationerna numeriskt
solution <- ode(
  y = init_cond,      # Begynnelsevärden
  times = time_span,  # Intervall
  func = island_fun,  # Funktionen
  parms = parameters, # Andra värden
  method = "rk4"      # Runge-Kutta version 4
)
# Convertera till data frame för lättare hantering
sol <- as.data.frame(solution)

# Plot time vs antal arter
plot(
  sol$time,
  sol$S,
  type = "l",
  xlab = "Tid",
  ylab = "Antal arter",
  ylim = c(0,30),
  col = "blue"
)
# lines(sol$time,
#       sol$S)

# c = koloniseringshastigheten
# ökar -> ökar antalet arter vid jämvikt & ökar fortare

# P = antal arter som kan kolonisera ön
# ökar -> ökar antalet arter vid jämvikt & ökar fortare

# m = utdöende hastighet
# ökar -> minskar antal arter vid jämvikt & når jämnvikt fortare

# Sista värdet av S
sista <- tail(sol,
              n = 1)$S
sista
# Hitta sista värdet med uniroot (därför att differentialekvationen redan är en derivata?)
S_eq <- function(S,
                 c = 2,
                 P = 50,
                 m = 4
) {
  (1 - (S/P))*c - ((S*m)/P)
}
uniroot(S_eq, c(0,20))$root


# Quiz 2 ----

# ekv. 1: dIdt = alfa * I - beta * I * S
# ekv. 2: dSdt = delta * I * S - gamma * S

LV_fun <- function(times, # Intervall
                   y,     # Begynnelsevärden
                   parms  # Andra värden
) {
  alfa <- parms["alfa"]
  beta <- parms["beta"]
  delta <- parms["delta"]
  gamma <- parms["gamma"]
  
  I <- y["I"]
  S <- y["S"]
  
  dIdt <- (alfa * I - beta * I * S)
  dSdt <- (delta * I * S - gamma * S)
  
  result_vec <- c(
    dIdt,
    dSdt
  )
  
  result_list <- list(result_vec)
  return(result_list)
}

time_span_LV <- seq(
  0,
  30,
  by = 0.1
)

init_LV <- c(
  I = 200,
  S = 10
)

params_LV <- c(
  alfa = 2.5,
  beta = 0.15,
  delta = 0.02,
  gamma = 1.5
)

sol_LV <- ode(
  y = init_LV,           # Begynnelsevärden
  times = time_span_LV,  # Intervall
  func = LV_fun,         # Funktionen
  parms = params_LV,     # Andra värden
  method = "rk4"         # Runge-Kutta version 4
)
sol_LV <- as.data.frame(sol_LV)

plot(
  sol_LV$time,
  sol_LV$I,
  type = "l",
  col = "blue",
  xlab = "Time",
  ylab = "Number of individuals"
)
lines(
  sol_LV$time,
  sol_LV$S,
  col = "darkorange"
)
legend(
  "topright",
  legend = c("Insects [millions]", 
             "Spiders [thousands]"),
  
  lty = c(1, 1), # Måste ange line type för att färgen på linjer ska synas
  col = c("blue",
          "darkorange")
)


# "Fasporträtt" (två pop. mot varandra där tiden blir att följa linjen)
plot(
  sol_LV$I,
  sol_LV$S,
  type = "l",
  col = "darkgreen",
  lwd = 3,
  xlab = "Insects",
  ylab = "Spiders"
)



## Fråga 10 ----
LV_fun <- function(times, # Intervall
                   y,     # Begynnelsevärden
                   parms  # Andra värden
) {
  alfa <- parms["alfa"]
  beta <- parms["beta"]
  delta <- parms["delta"]
  gamma <- parms["gamma"]
  
  I <- y["I"]
  S <- y["S"]
  
  dIdt <- (alfa * I - beta * I * S)
  dSdt <- (delta * I * S - gamma * S)
  
  result_vec <- c(
    dIdt,
    dSdt
  )
  
  result_list <- list(result_vec)
  return(result_list)
}

time_span_LV <- seq(
  0,
  30,
  by = 0.1
)

init_LV <- c(
  I = 200 * 0.01,
  S = 5 * 0.50
)

params_LV <- c(
  alfa = 2.5,
  beta = 0.15,
  delta = 0.02,
  gamma = 1.5
)

sol_LV <- ode(
  y = init_LV,           # Begynnelsevärden
  times = time_span_LV,  # Intervall
  func = LV_fun,         # Funktionen
  parms = params_LV,     # Andra värden
  method = "rk4"         # Runge-Kutta version 4
)
sol_LV <- as.data.frame(sol_LV)

plot(
  sol_LV$time,
  sol_LV$I,
  type = "l",
  col = "blue",
  xlab = "Time",
  ylab = "Number of individuals"
)
lines(
  sol_LV$time,
  sol_LV$S,
  col = "darkorange"
)

# "Fasporträtt" (två pop. mot varandra där tiden blir att följa linjen)
plot(
  sol_LV$I,
  sol_LV$S,
  type = "l",
  col = "darkgreen",
  lwd = 3,
  xlab = "Insects",
  ylab = "Spiders"
)

