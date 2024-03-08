# KvantBio seminarium

## Fråga 1-2 ----
library(deSolve)
# Funktion med ekvationssystemet att lösa
LV_fun <- function(times, # Intervall
                   y,     # Begynnelsevärden
                   parms  # Andra värden
) {
  # variabler att använda (från parms)
  alfa <- parms["alfa"]
  beta <- parms["beta"]
  delta <- parms["delta"]
  gamma <- parms["gamma"]
  
  # Begynnelsevärden (från y)
  B <- y["B"]
  P <- y["P"]
  
  # Differentialekvationen att lösa numeriskt
  dIdt <- (alfa * B - beta * B * P)
  dSdt <- (delta * B * P - gamma * P)
  
  # Spara & returnera resultatet
  result_vec <- c(
    dIdt,
    dSdt
  )
  result_list <- list(result_vec)
  return(result_list)
}

# Vektor med alla tidssteg (t) att sätta in
time_span_LV <- seq(
  0,
  50,
  by = 0.01
)

# Vektor med begynnelsevärden
init_LV <- c(
  B = 500,
  P = 0.03
)

# Vektor med andra värden för formeln
params_LV <- c(
  alfa = 2.5,    # Tillväxt byten
  beta = 0.3,    # Byten som äts
  delta = 0.02,  # Tillväxt predator
  gamma = 1.5    # Död predator
)

# Lösa ekvationerna numeriskt
sol_LV <- ode(
  y = init_LV,           # Begynnelsevärden
  times = time_span_LV,  # Intervall
  func = LV_fun,         # Funktionen
  parms = params_LV,     # Andra värden
  method = "rk4"         # Runge-Kutta version 4
)
sol_LV <- as.data.frame(sol_LV)


# Plotta antal insekter [milljoner] respektive spindlar [tusen] mot tid
plot(
  sol_LV$time,
  sol_LV$B,
  type = "l",
  
  col = "blue",
  
  xlab = "Time",
  ylab = "Number of individuals",
  main = "Lotka-Volterra model"
)
lines(
  sol_LV$time,
  sol_LV$P,
  
  col = "darkorange"
)
legend(
  "topright",
  legend = c("Byten [thousands]", 
             "Predator [thousands]"),
  
  lty = c(1, 1),
  col = c("blue",
          "darkorange")
)


# "Fasporträtt" (två pop. mot varandra där tiden blir att följa linjen)
plot(
  sol_LV$B,
  sol_LV$P,
  type = "l",
  
  col = "darkgreen",
  
  xlab = "Byten",
  ylab = "Predator",
  main = "Fasporträtt"
)

# 2.a)
max(sol_LV$B)


## Fråga 4 ----
# Funktion med ekvationssystemet att lösa
LV_fun <- function(times, # Intervall
                   y,     # Begynnelsevärden
                   parms  # Andra värden
) {
  # variabler att använda (från parms)
  alfa <- parms["alfa"]
  beta <- parms["beta"]
  delta <- parms["delta"]
  gamma <- parms["gamma"]
  k <- parms["k"] # Bärkraft
  
  # Begynnelsevärden (från y)
  B <- y["B"]
  P <- y["P"]
  
  # Differentialekvationen att lösa numeriskt
  dBdt <- (alfa * B * (1 - B/k) - beta * B * P)
  dPdt <- (delta * B * P - gamma * P)
  
  # Spara & returnera resultatet
  result_vec <- c(
    dBdt,
    dPdt
  )
  result_list <- list(result_vec)
  return(result_list)
}

# Vektor med alla tidssteg (t) att sätta in
time_span_LV <- seq(
  0,
  50,
  by = 0.01
)

# Vektor med begynnelsevärden
init_LV <- c(
  B = 500,
  P = 0.03
)

# Vektor med andra värden för formeln
params_LV <- c(
  alfa = 2.5,    # Tillväxt byten
  beta = 0.3,    # Byten som äts
  delta = 0.02,  # Tillväxt predator
  gamma = 1.5,   # Död predator
  k = 500        # Bärkraft
)

# Lösa ekvationerna numeriskt
sol_LV <- ode(
  y = init_LV,           # Begynnelsevärden
  times = time_span_LV,  # Intervall
  func = LV_fun,         # Funktionen
  parms = params_LV,     # Andra värden
  method = "rk4"         # Runge-Kutta version 4
)
sol_LV <- as.data.frame(sol_LV)


# Plotta antal insekter [milljoner] respektive spindlar [tusen] mot tid
plot(
  sol_LV$time,
  sol_LV$B,
  type = "l",
  
  col = "blue",
  
  xlab = "Time",
  ylab = "Number of individuals",
  main = "Lotka-Volterra model"
)
lines(
  sol_LV$time,
  sol_LV$P,
  
  col = "darkorange"
)
legend(
  "topright",
  legend = c("Byten [thousands]", 
             "Predator [thousands]"),
  
  lty = c(1, 1),
  col = c("blue",
          "darkorange")
)


# "Fasporträtt" (två pop. mot varandra där tiden blir att följa linjen)
plot(
  sol_LV$B,
  sol_LV$P,
  type = "l",
  
  col = "darkgreen",
  
  xlab = "Byten",
  ylab = "Predator",
  main = "Fasporträtt"
)