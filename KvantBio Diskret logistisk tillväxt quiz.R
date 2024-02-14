# Diskret logistisk tillväxt

# Quiz 1 ----

## Fråga 1-7

dlt <- function(n, r, k) {
  n_ny <- n + r*n*(1-n/k)
  return(n_ny)
}

n <- 10 # startvärde
sluttid <- 60
for (i in 1:sluttid) {
  n[i+1] <- dlt(
    n = n[i],
    r = 0.40,
    k = 1000
  )
}
n

## Fråga 8
t <- 1:(sluttid+1)

plot(
  x = t,
  y = n,
  ylim = c(0,2000),
  type = "l",
  lwd = 2
)


# Quiz 2 ----

## Fråga 1-4

n2 <- 3000 # startvärde
for (i in 1:sluttid) {
  n2[i+1] <- dlt(
    n = n2[i],
    r = 0.40,
    k = 1000
  )
}
n2

lines(
  x = t,
  y = n2,
  type = "l",
  lwd = 2,
  col = "blue"
)

## Fråga 5-6

r_exploration <- function(r) {
  n3 <- 10 # startvärde
  sluttid <- 60
  for (i in 1:sluttid) {
    n3[i+1] <- dlt(
      n = n3[i],
      r = r,
      k = 1000
    )
  }
  # n3
  t <- 1:(sluttid+1)
  
  plot(
    x = t,
    y = n3,
    ylim = c(0,1500),
    type = "l",
    lwd = 2
  )
}

r_exploration(2.5)

n3 <- 10 # startvärde
sluttid <- 60
for (i in 1:sluttid) {
  n3[i+1] <- dlt(
    n = n3[i],
    r = 3.1,
    k = 1000
  )
}
n3

plot(
  x = t,
  y = n3,
  ylim = c(0,1500),
  type = "l",
  lwd = 2
)

## Fråga 7

m <- c()
m[1] <- 17 # startvärde, fåglar som migrerar till ön
k <- 450 # bärkraft
r <- 0.32

goal <- k * 0.90 # 90 % av bärkraft
sluttid <- 60
t <- 1:(sluttid+1)

for (i in 1:sluttid) {
  m[i+1] <- dlt(
    n = m[i],
    r = r,
    k = k
  )
}

plot(
  x = t,
  y = m,
  type = "l",
  lwd = 2
)
abline(h = goal)

m[m < goal]
m[1:20] # "börjar på år 1" innebär att antal år efter start
# är vector position minus 1

## Fråga 8

rand <- rnorm( # 1000 tal mellan -0.28 till 0.92
  n = 1000,
  mean = 0.32,
  sd = 0.30
)
rand[rand < 0.28]
rand[rand > 0.92]
hist(rand)

## Fråga 9-12
s <- c()
s[1] <- 17
sluttid <- 50
t <- 1:(sluttid+1)

for (i in 1:sluttid) {
  r <- rnorm(
    n = 1,
    mean = 0.32,
    sd = 0.30
  )
  k <- rnorm(
    n = 1,
    mean = 450,
    sd = 50
  )
  s[i+1] <- dlt(
    n = s[i],
    r = r,
    k = k
  )
}

plot(
  x = t,
  y = s,
  ylim = c(0,600),
  type = "l"
)
