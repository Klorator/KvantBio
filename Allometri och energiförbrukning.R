# Allometri & energiförbrukning

## Fråga 1

## Fråga 2-7
curve( log10(10^(0.36*S/1.88 + 1.945)), # Needs an x variable,
       xname = "S",   # x var can be named
       # add = TRUE,  # To add to previous graph
       xlim = c(0,3), # axis limits/range
       col = "blue",  # Color
       lwd = 2,       # Line width
       # lty = 2,     # Line type
       # bty = "n",   # Border type
       # log = "y",   # Log y-axis (not values)
       xlab = "Simhastighet [kroppslängd/s]",
       ylab = "Syreförbrukning [ml/h]")

## Fråga 9
TL <- c(1.88,1.70,1.54) # Total length [m]
W <- c(33.8,26.3,20.6)  # Weight [kg]
S <- c(0.71,0.60,0.57)  # Swim speed [m/s]

b <- 0.86 # Scaling constant
E <- 19.54 # Oxycaloric value [J/ml O2] 
# Omräkningsfaktor till kalorier cal: 1/4.184

## Fråga 10-11
O2_fun <- function(S, TL) {
  result <- 10^(0.36*S/TL + 1.945)
  return(result)
}
O2_1kg <- O2_fun(S, TL) # => A i nästa steg


O2_adj_fun <- function(A, W, b) {
  result <- A * W^b
  return(result)
}
O2_adj <- O2_adj_fun(O2_1kg, W, b)


## Fråga 12
shark_col <- c("blue", "darkgreen", "darkorchid")
add_curve <- FALSE
for (i in seq_along(O2_1kg)) {
  curve( O2_1kg[i] * W^b, # Needs an x variable,
         xname = "W",   # x var can be named
         add = add_curve,  # To add to previous graph
         xlim = c(0,100), # axis limits/range
         col = shark_col[i],  # Color
         lwd = 1,       # Line width
         # lty = 2,     # Line type
         # bty = "n",   # Border type
         # log = "y",   # Log y-axis (not values)
         xlab = "Simhastighet [kroppslängd/s]",
         ylab = "Justerad Syreförbrukning [ml/h]")

  add_curve <- TRUE
}

## Fråga 13
curve( O2_1kg[1] * W^1, # Needs an x variable,
      xname = "W",   # x var can be named
      add = T,  # To add to previous graph
      xlim = c(0,100), # axis limits/range
      col = "black",  # Color
      lwd = 1,       # Line width
      # lty = 2,     # Line type
      # bty = "n",   # Border type
      # log = "y",   # Log y-axis (not values)
      xlab = "Simhastighet [kroppslängd/s]",
      ylab = "Justerad Syreförbrukning [ml/h]")

## Fråga 14
M <- O2_adj * E * 24 * 10^-3 # Energiförbrukning [kJ/dygn]

M_cal <- M/4.18

M_cal_W <- c()
for (i in 1:3) {
  M_cal_W[i] <- M_cal[i]/W[i] # Energiförbrukning [kcal/dygn/kg]
}

## Fråga 15
# Mifflin-St. Jeor ekvationen
# (9.99 x vikt i kilo) + (6.25 x längd i cm) - (4.92 x ålder i år).
# För män addera 5 till resultatet och för kvinnor subtrahera 161

my_O2 <- (9.99*56) + (6.25*187) - (4.92 * 28) + 5

## Fråga 16
# B = 2.57 * M^0.792

## Fråga 17
curve( 2.57 * M^0.792, # Needs an x variable,
       xname = "M",   # x var can be named
       add = F,  # To add to previous graph
       xlim = c(0,100), # axis limits/range
       col = "darkgreen",  # Color
       lwd = 2,       # Line width
       xlab = "Massa [kg]",
       ylab = "Basal metabolism [watt]")

curve( 2.57 * M^1, # Needs an x variable,
       xname = "M",   # x var can be named
       add = T,  # To add to previous graph
       xlim = c(0,100), # axis limits/range
       col = "purple",  # Color
       lwd = 2,       # Line width
       xlab = "Massa [kg]",
       ylab = "Basal metabolism [watt]")

## Fråga 18
curve( (2.57 * M^0.792)/M, # Needs an x variable,
       xname = "M",   # x var can be named
       add = F,  # To add to previous graph
       xlim = c(0,100), # axis limits/range
       col = "red",  # Color
       lwd = 2,       # Line width
       xlab = "Massa [kg]",
       ylab = "Specifik metabolism [watt]")
