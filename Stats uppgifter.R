# Stats uppgifter

## Fråga 13.3
ingrep.tot <- 347
ingrep.ded <- 83
ingrep.alive <- ingrep.tot - ingrep.ded

obs.tot <- 348
obs.ded <- 106
obs.alive <- obs.tot - obs.ded

pros <- data.frame(
  ingrep = c(ingrep.alive, ingrep.ded),
  observation = c(obs.alive, obs.ded)
)

chisq.test(pros)

## Fråga 14.9 ----
abs_metal <- data.frame(
  Fe = c(17,22,35,43,80,85,91,92,96,100),
  Pb = c(8, 17,18,25,58,59,41,30,43,58)
)

### a)
plot(abs_metal$Fe,
     abs_metal$Pb)
# Svar: Positiv

### b)
cor.test(abs_metal$Fe,
         abs_metal$Pb)
# Pearson's product-moment correlation
# t = 4.3927
# df = 8
# R^2 = 0.8407822

# Svar: R^2 = 0.8407822 vilket är ett starkt positivt samband

### c)
abs_metal_lm <- lm(
  Fe ~ Pb,
  data = abs_metal
)
summary(abs_metal_lm)

hist(abs_metal$Fe) # kinda skewed
qqnorm(abs_metal$Fe);qqline(abs_metal$Fe) # good

hist(abs_metal$Pb) # fine
qqnorm(abs_metal$Pb);qqline(abs_metal$Pb) # good

plot(abs_metal_lm, which = 1) # not linear?
plot(abs_metal_lm, which = 2) # good
plot(abs_metal_lm, which = 3) # not terrible
plot(abs_metal_lm, which = 4) # sad?

abs_predicted <- predict(
  abs_metal_lm,
  newdata = data.frame(Pb = 15)
)

abs_predicted
