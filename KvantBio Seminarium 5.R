# Seminarium 5

## Fråga 1
#SEM

## Fråga 2
# which()
# option c or e


# 
# test.fun <- function(z) {
#   browser()
#   
#   x <- 5
#   y <- 8
#   
#   my_return <- function(a, b, c) {
#     result_1 <- a * b
#     result_2 <- b + c
#     
#     results <- list(result_1, result_2)
#     
#     p <- plot(result_1, result_2)
#     
#     return(results)
#   }
#   
#   res <- my_return(x, y, z)
#   print(res)
# }
# 
# test.fun(4)
# 






## Fråga 3

dlt <- function(n, r, k) {
  n_ny <- n + r*n*(1 - (n/k) )
  return(n_ny)
}

k <- 10000 # bärkraft, individer
sluttid <- 20 # år
t <- 1:(sluttid+1)

### Model 1
stab_1 <- c()
for (k in 1:10) {

  n <- c()
  n[1] <- 50 # startvärde, individer
  
  # collapse <- 0 # färre individer = pop. ded
  planting <- 50
  
  plot( # Draw blank graph window
    NULL,
    xlim = c(0,sluttid),
    ylim = c(0,11000),
    type = "l",
    xlab = "Years",
    ylab = "Salmon"
  )
  
  # pop_ded <- 0 # counter for failed pop.
  year_20 <- c()
  for (j in 1:1000) {
    
    for (i in 1:sluttid) {
      n[i+1] <- dlt( # ekv.
        n = n[i],
        r = rnorm( # slumpad reproduktion
          1,
          0.1,
          0.4),
        k = k)
      n[i+1] <- n[i+1] + planting
    } # End line +++++++++++++++++++++
    
    year_20[j] <- n[(sluttid+1)]
    # farbe <- rgb(runif(1),runif(1),runif(1)) # random color
    lines( # add pop. line
      x = t,
      y = n,
      lwd = 1
      # col = farbe
    )
    
  } # End modeling +++++++++++++++
  
  stab <- year_20[year_20 >= 2000]
  stab_1[k] <- length(stab)
  
} # End repeats +++++++++++++++++++++


### Model 2 ++++++++++++++++++++++++++++++++++++++++++++++
stab_2 <- c()
for (k in 1:10) {
  
  n <- c()
  n[1] <- 25 # startvärde, individer
  
  collapse <- 0 # färre individer = pop. ded
  planting <- 25
  
  plot( # Draw blank graph window
    NULL,
    xlim = c(0,sluttid),
    ylim = c(0,11000),
    type = "l",
    xlab = "Years",
    ylab = "Salmon"
  )
  year_20 <- c()
  for (j in 1:1000) {
    
    for (i in 1:sluttid) {
      n[i+1] <- dlt( # ekv.
        n = n[i],
        r = rnorm( # slumpad reproduktion
          n = 1,
          mean = 0.18,
          sd = 0.4),
        k = k)
      n[i] <- n[i] + planting
    } # End line +++++++++++++++++
    
    year_20[j] <- n[(sluttid+1)]
    # farbe <- rgb(runif(1),runif(1),runif(1)) # random color
    lines( # add pop. line
      x = t,
      y = n,
      lwd = 1
      # col = farbe
    )
    
  } # End modeling +++++++++++++++++++
  
  stab <- year_20[year_20 >= 2000]
  stab_2[k] <- length(stab)
  
} # End repeats +++++++++++++++


stab_1
stab_2

t.test(
  stab_1,
  stab_2
)
