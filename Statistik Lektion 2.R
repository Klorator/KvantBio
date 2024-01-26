# Lektion 2

## Fråga 1
pnorm(1, 2, 1) # p(Y <= 1), E(2), SD = 1

## Fråga 7
1 - pbinom(16, 20, 0.5) # one-sided test, only higher than p = 50 %
# pbinom(16, 20, 0.5, lower.tail = FALSE) # or this

# 2*(1 - pbinom(16, 20, 0.5)) # two-sided test, either higher or lower

# eller så kan man addera enskillda sannolikheter
p17 <- dbinom(17, 20, 0.5)
p18 <- dbinom(18, 20, 0.5)
p19 <- dbinom(19, 20, 0.5)
p20 <- dbinom(20, 20, 0.5)
p_over_16 <- p17 + p18 + p19 + p20
# Ger samma svar som ovanför

## 