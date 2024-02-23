# Stats Linear regression

library(HistData)

# plot(Galton$parent, Galton$child)

m <- lm(
  child ~ parent,
  data = Galton
)
model_summary <- summary(m)
intercept <- m$coefficients["(Intercept)"]
lutning <- m$coefficients["parent"]

plot( # Plot points
  Galton$parent, 
  Galton$child, 
  col = "skyblue"
)
grid() # Add grid lines
abline( # Add linear regression
  coef(m), # extract coefficients
  col = "darkorange",
  lty = 2,
  lwd = 2
)

## Check that assumptions are fulfilled ----
# Check fitted vs residual
plot(m, which = 1)

# Check normal distribution
plot(m, which = 2)

# Check equal variance
plot(m, which = 3)


## Prediction ----
predict(
  m,
  newdata = data.frame(parent = 69),
  interval = "prediction",
  level = 0.95
)


# Data ----
library(boot)

## Model ----
m2 <- lm(total ~ conc,
         data = nitrofen)
m2_summary <- summary(m2)

## Plot ----
plot(total ~ conc,
     data = nitrofen)
abline(
  coef(m2),
  col = 2
)

## QC (check assumptions)
plot(m2, which = 1) # Linear?              NO
plot(m2, which = 2) # Normal distribution? Y
plot(m2, which = 3) # Equal variance?      Y


## Log total & new model
nitrofen$logtotal <- log(nitrofen$total + 0.1)

m3 <- lm(logtotal ~ conc, data = nitrofen)

plot(logtotal ~ conc, data = nitrofen)
abline(coef(m3), col = 2)

plot(m3, which = 1) # Linear? Still not good

## Polynom model ----
m4 <- lm(total ~ I(conc^2), data = nitrofen)
plot(total ~ conc, data = nitrofen)
lines(0:350,
      predict(m4,
              newdata = data.frame(conc = 0:350)),
      col = 2)

plot(m4, which = 1) # Linear? Better
plot(m4, which = 2) # Normal dist? OK
plot(m4, which = 3) # Equal variance? questionable


## Bootstrap ----
library(boot.pval)

boot_summary(m4) # Assumes equal variance

boot_summary(m4, method = "case") # not equal variance
