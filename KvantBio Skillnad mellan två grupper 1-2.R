# Skillnad mellan två grupper

# Del 1

## Load data
zink <- read.table(
  file = "zntill.csv",
  sep = ";",
  dec = ",",
  header = T
)

## Check normal distribution
hist(zink$ktrl_p)
qqnorm(
  zink$ktrl_p,
  col = "red",
  pch = 16
)
qqline(zink$ktrl_p)

hist(zink$tih_p)
qqnorm(
  zink$tih_p,
  col = "orange",
  pch = 16
)
qqline(zink$tih_p)

## t-test
test.plasma <- t.test(
  zink$ktrl_p,
  zink$tih_p
)
test.plasma

## CI plot
