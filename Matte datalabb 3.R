# Frågor 7-12


## Fråga 7

A <- rbind(
  r1 = c(-2,  7,  3),
  r2 = c( 2,  3, -2),
  r3 = c( 3, -3,  5)
)
B <- rbind(
  r1 = c( 3, -7,  3),
  r2 = c( 1,  2, -2),
  r3 = c(-4, -2,  2)
)

AB <- A %*% B
AB_sum <- sum(AB)


## Fråga 9
C <- rbind(
  r1 = c( c1 = 2, c2 = -3,  c3 = 0),
  r2 = c(      3,       1,       2),
  r3 = c(      0,       2,       7)
)
C_eq <- c(r1 = 2, r2 = -3, r3 = 5)

# solve(C, C_eq)
MASS::fractions(solve(C, C_eq))


## Fråga 10-11
A <- rbind(
  r1 = c( c1 = 2, c2 = -3,  c3 = 0),
  r2 = c(      3,       1,       2),
  r3 = c(      0,       2,       7)
)
b <- c(r1 = 2, r2 = -3, r3 = 5)

I3 <- rbind(
  r1 = c( c1 = 1, c2 =  0,  c3 = 0),
  r2 = c(      0,       1,       0),
  r3 = c(      0,       0,       1)
)

# A %*% A-1 == I
MASS::fractions(solve(A, I3))

A_inv <- solve(A, I3)

# eller A %*% v == b  =>  A-1 %*% b == v
v <- solve(A, b)
MASS::fractions(solve(A_inv, v))


# ++++++++++++
A <- rbind(
  r1 = c( c1 = 2, c2 = -3,  c3 = 0),
  r2 = c(      3,       1,       2),
  r3 = c(      0,       2,       7)
)
b <- c(21, -4, 8)
A_inv <- solve(A) # I3 is implied, gives inverse

# A %*% v == b  =>  A-1 %*% b == v
v <- solve(A, b)
# solve(A_inv, v)
MASS::fractions(v)


## Fråga 12
D <- rbind(
  r1 = c( c1 = 1, c2 =  2,  c3 = sqrt(3)),
  r2 = c(      2,       4,       8),
  r3 = c(      2,       9,       1)
)
D_det <- det(D)
