# Seminarium

# Fråga 3
dogs <- rbind(
  #         1     2     3     4      5     6
  r0 <- c(   0,     0,  2.8,  2.8,  2.8,  2.8),
  r1 <- c(0.32,  0.40,    0,    0,    0,    0),
  r2 <- c(   0,  0.40, 0.43,    0,    0,    0),
  r3 <- c(   0,     0, 0.45, 0.45,    0,    0),
  r4 <- c(   0,     0,    0, 0.45,    0,    0),
  r5 <- c(   0,     0,    0,    0,    0,    0)
)

dogs_eigen <- eigen(dogs)
dogs_eigen$values <- Re(dogs_eigen$values)
max(dogs_eigen$values) # Tillväxtfaktor

# Fråga 4
dogs_eigen$vectors <- Re(dogs_eigen$vectors)
relativ_dist <- dogs_eigen$vectors[1] / 
