# Lektion 2

## Fråga 1
pnorm(1, 2, 1) # p(Y <= 1), E(2), SD = 1

## Fråga 7
1 - pbinom(16, 20, 0.5) # one-sided test, only higher than p = 50 %
# pbinom(16, 20, 0.5, lower.tail = FALSE) # or this

# 2*(1 - pbinom(16, 20, 0.5)) # two-sided test, either higher or lower

# eller så kan man addera enskilda sannolikheter
p17 <- dbinom(17, 20, 0.5)
p18 <- dbinom(18, 20, 0.5)
p19 <- dbinom(19, 20, 0.5)
p20 <- dbinom(20, 20, 0.5)
p_over_16 <- p17 + p18 + p19 + p20
# Ger samma svar som ovanför

## Fråga 8
1 - pbinom(14, 20, 0.75)
# pbinom(14, 20, 0.75, lower.tail = FALSE) # eller

# eller
dbinom(15, 20, 0.75) +
  dbinom(16, 20, 0.75) +
  dbinom(17, 20, 0.75) +
  dbinom(18, 20, 0.75) +
  dbinom(19, 20, 0.75) +
  dbinom(20, 20, 0.75)


# Inlämning
## Fråga 3
dbinom(9, 10, 0.93) + dbinom(10, 10, 0.93)

## Fråga 4
pnorm(46, 45, 2, lower.tail = F)^3
