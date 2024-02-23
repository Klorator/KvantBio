# Naturvård med matriser

# Quiz 1 ----
## Data ----
### Leslie Matrisen ----
L <- rbind(
  r1 = c( 0     , 0     , 0     , 0     , 127   , 4     , 80    ),
  r2 = c( 0.6747, 0.7370, 0     , 0     , 0     , 0     , 0     ),
  r3 = c( 0     , 0.0486, 0.6610, 0     , 0     , 0     , 0     ),
  r4 = c( 0     , 0     , 0.0147, 0.6907, 0     , 0     , 0     ),
  r5 = c( 0     , 0     , 0     , 0.0518, 0     , 0     , 0     ),
  r6 = c( 0     , 0     , 0     , 0     , 0.8091, 0     , 0     ),
  r7 = c( 0     , 0     , 0     , 0     , 0     , 0.8091, 0.8091)
)
# r1 <- c( 0     , 0     , 0     , 0     , 127   , 4     , 80    )
# r2 <- c( 0.6747, 0.7370, 0     , 0     , 0     , 0     , 0     )
# r3 <- c( 0     , 0.0486, 0.6610, 0     , 0     , 0     , 0     )
# r4 <- c( 0     , 0     , 0.0147, 0.6907, 0     , 0     , 0     )
# r5 <- c( 0     , 0     , 0     , 0.0518, 0     , 0     , 0     )
# r6 <- c( 0     , 0     , 0     , 0     , 0.8091, 0     , 0     )
# r7 <- c( 0     , 0     , 0     , 0     , 0     , 0.8091, 0.8091)
# L <- rbind(r1,r2,r3,r4,r5,r6,r7)

### Eigenvalues ----
eigen <- eigen(L)

Re(eigen$values[1])

dist <- eigen$vectors[,1]
dist <- Re(dist)

dist_relativ <- ( dist / sum(dist) ) *100
dist_relativ


## Naturvårdsplaner ----
gf <- c() # store results


### Plan 1 ----
L_plan1 <- L
L_plan1[2,1] <- 1 # Change P(growth) in class 1 to 100%

gf["Plan_1"] <- max(Re(eigen(L_plan1)$values))


### Plan 2 ----
L_plan2 <- L
L_plan2[2,2] <- 0.9514 # Change P(survival) in class 2 to 95.14%

gf["Plan_2"] <- max(Re(eigen(L_plan2)$values)) # Store dominant eigenvalue


### Plan 3 ----
L_plan3 <- L
L_plan3[3,3] <- 0.9853 # Change P(survival) in class 3 to 98.53%

gf["Plan_3"] <- max(Re(eigen(L_plan3)$values)) # Store dominant eigenvalue


### Plan 4 ----
L_plan4 <- L
L_plan4[4,4] <- 0.9482 # Change P(survival) in class 4 to 94.82%

gf["Plan_4"] <- max(Re(eigen(L_plan4)$values)) # Store dominant eigenvalue


### Plan 5 ----
L_plan5 <- L
L_plan5[6,5] <- 1 # Change P(growth) in class 5 to 100%

gf["Plan_5"] <- max(Re(eigen(L_plan5)$values)) # Store dominant eigenvalue


### Plan 6 ----
L_plan6 <- L
L_plan6[7,6] <- 1 # Change P(growth) in class 6 to 100%

gf["Plan_6"] <- max(Re(eigen(L_plan6)$values)) # Store dominant eigenvalue



## Plot results ----
barplot(
  gf,
  xlab = "Naturvårds plan",
  ylab = "Dominant egenvärde",
  col = RColorBrewer::brewer.pal(6,"Dark2"),
  ylim = c(0,1.5)
)
abline(h = 1)



# Quiz 2 ----
## Data ----
S <- rbind(#Stage 2        3        4        5        6        7
  r1 = c(0.6507	, 0      , 0      , 0      , 0      , 32.0679, 190.0144),
  r2 = c(0.0146	, 0.834	 , 0      , 0      , 0      , 0      , 0       ),
  r3 = c(0      , 0.0339 , 0.8316 , 0      , 0      , 0      , 0       ),
  r4 = c(0      , 0      , 0.0981	, 0.9187 , 0      , 0      , 0       ),
  r5 = c(0      , 0      , 0      , 0.0473 , 0.9334 , 0      , 0       ),
  r6 = c(0      , 0      , 0      , 0      , 0.05  	, 0.971  , 0       ),
  r7 = c(0      , 0      , 0      , 0      , 0      , 0.0184 , 0.9868	 )
)

## Eigenvalues ----
S_eigen <- eigen(S)
S_eigen_val <- Re(S_eigen$values)
S_eigen_vec <- Re(S_eigen$vectors)

relative_dist <- ( S_eigen_vec[,1] / sum(S_eigen_vec[,1]) ) *100




# Quiz 3 ----
## Data (turtles) ----
turtles<-c(2998, 12459, 2882, 197, 10, 1, 12)

relativ_fordelning <- c(
  0.2065519760, 0.6697304431, 0.1145746547, 0.0066208692, 0.0003628892, 
  0.0003106745, 0.0018484934
  )

turtles_M <- rbind(turtles)

## Chi-squared test ----
chisq_result <- chisq.test(turtles_M, p = relativ_fordelning)

## Data (gazell) ----
gem <- c(5, 7, 12, 67)
rhb <- c(6, 8, 54, 13)

G <- rbind(gem, rhb)

## Chi-squared test ----
chi_G <- chisq.test(G)


## Repeat ----
Sten <- rbind(c(wild = 198, national = 244))

Sten_test <- chisq.test(Sten)

