# Seminarium

# Fråga 3
dogs <- rbind(
  #         0     1     2     3      4     5
  r0 <- c(   0,     0,  2.8,  2.8,  2.8,  2.8),
  r1 <- c(0.32,  0.40,    0,    0,    0,    0),
  r2 <- c(   0,  0.40, 0.43,    0,    0,    0),
  r3 <- c(   0,     0, 0.43, 0.45,    0,    0),
  r4 <- c(   0,     0,    0, 0.45, 0.45,    0),
  r5 <- c(   0,     0,    0,    0, 0.45,    0)
)

dogs_eigen <- eigen(dogs)
dogs_eigen$values <- Re(dogs_eigen$values)
max(dogs_eigen$values) # Tillväxtfaktor

# Fråga 4
dogs_eigen$vectors <- Re(dogs_eigen$vectors)
relativ_dist <- (dogs_eigen$vectors[,1] / sum(dogs_eigen$vectors[,1])) *100
relativ_dist


# Fråga 5
dogs_2 <- rbind(
  #         0     1     2     3      4     5
  r0 <- c(   0,     0,  2.8,  2.8,  2.8,  2.8),
  r1 <- c(0.22,  0.30,    0,    0,    0,    0),
  r2 <- c(   0,  0.30, 0.33,    0,    0,    0),
  r3 <- c(   0,     0, 0.33, 0.35,    0,    0),
  r4 <- c(   0,     0,    0, 0.35, 0.35,    0),
  r5 <- c(   0,     0,    0,    0, 0.35,    0)
)


dogs_2_eigen <- eigen(dogs_2)
dogs_2_eigen$values <- Re(dogs_2_eigen$values)
max(dogs_2_eigen$values) # Tillväxtfaktor

dogs_2_eigen$vectors <- Re(dogs_2_eigen$vectors)
relativ_dist_2 <- (dogs_2_eigen$vectors[,1] / sum(dogs_2_eigen$vectors[,1])) *100
relativ_dist_2





dogs_3 <- rbind(
  #         0     1     2     3      4     5
  r0 <- c(   0,     0,  2.8,  2.8,  2.8,  2.8),
  r1 <- c(0.12,  0.38,    0,    0,    0,    0),
  r2 <- c(   0,  0.38, 0.41,    0,    0,    0),
  r3 <- c(   0,     0, 0.41, 0.43,    0,    0),
  r4 <- c(   0,     0,    0, 0.43, 0.43,    0),
  r5 <- c(   0,     0,    0,    0, 0.43,    0)
)

dogs_3_eigen <- eigen(dogs_3)
dogs_3_eigen$values <- Re(dogs_3_eigen$values)
max(dogs_3_eigen$values) # Tillväxtfaktor

dogs_3_eigen$vectors <- Re(dogs_3_eigen$vectors)
relativ_dist_3 <- (dogs_3_eigen$vectors[,1] / sum(dogs_3_eigen$vectors[,1])) *100
relativ_dist_3



# Fråga 6
relativ_dist
obs_dist <- c(156,60,34,21,12,5)
obs_dist <- (obs_dist / sum(obs_dist)) *100
obs_dist


# Fråga 9
tabell5 <- cbind(
  commercial = c(3254, 150, 198),
  Community = c(566, 162, 37)
)

tabell5_test <- chisq.test(tabell5)
tabell5_test


# Fråga 10
tabell5_test$expected
