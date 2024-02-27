# Slump & simulering

## Load data ----
peng <- palmerpenguins::penguins

## Filter for species == "Gentoo"
gentoo <- peng[peng$species == "Gentoo",]
# gentoo2 <- subset(peng, species == "Gentoo")

## Mean bill length by sex ----
aggregate(bill_length_mm ~ sex,
          data = gentoo,
          FUN = mean)

## Sampling ----
urval <- sample(1:nrow(gentoo), 20)
gentoo_sample <- gentoo[urval,]

### Sample means
aggregate(bill_length_mm ~ sex,
          data = gentoo_sample,
          FUN = mean)

## Normal dist. ----
bills <- rnorm(10000, 45, 2) # rnorm( n, väntevärde, SD)

hist(bills, freq = F)
curve(dnorm(x, mean = 45, sd = 2),
      col = "red",
      add = T)

## Sjöhästar ----
seahorse <- rexp(10000, 1.5)

hist(seahorse)
mean(seahorse) # mean lifetime in years
mean(seahorse*12) # mean lifetime in months

### Sjöhästar som når vuxen ålder > 6 månader
seahorse_adult <- seahorse * 12 # convert to months
seahorse_adult <- seahorse_adult[seahorse_adult > 6]

mean(seahorse_adult)

## Browns rörelse ----
x = c(0) # Tidpunkt 1
y = c(0)

for (i in 2:10000) {
  x[i] <- x[i-1] + rnorm(1)
  y[i] <- y[i-1] + rnorm(1)
}

plot(x,y, type = "l")

## Back to Gentoo bills ----
intervall <- t.test(bills)$conf.int

## Check that CI is 45 in over 95% of cases
intervall2_accuracy <- c()
for (i in 1:10000) {
  bills2 <- rnorm(20, 45, 2)
  intervall2 <- t.test(bills2)$conf.int
  intervall2_accuracy[i] <- ifelse(intervall2[1] < 45 & intervall2[2] > 45, T, F)
}
mean(intervall2_accuracy)

# Inlämnings quiz ----
## Fråga 1
gentoo_f <- gentoo[gentoo$sex == "female",]
urval <- sample(1:nrow(gentoo_f), 20)
gentoo_sample <- gentoo_f[urval,]
bills3 <- gentoo_sample$bill_length_mm
# bills3 <- na.omit(bills3)
bills3 <- gentoo$bill_length_mm

t.test(bills3, conf.level = 0.16)$conf.int
t.test(bills3, conf.level = 0.84)$conf.int
# t.test(bills3, conf.level = 0.90)$conf.int
t.test(bills3, conf.level = 0.95)$conf.int
# t.test(bills3, conf.level = 0.99)$conf.int

bills4 <- rnorm(20, 45, 2)
t.test(bills4, conf.level = 0.66)$conf.int
t.test(bills4, conf.level = 0.84)$conf.int
t.test(bills4, conf.level = 0.95)$conf.int

