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

