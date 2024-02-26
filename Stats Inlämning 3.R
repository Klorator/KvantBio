library(palmerpenguins)
View(penguins)

## Fråga 1 ----
# Correlation body mass & flipper length
cor(penguins$flipper_length_mm,
    penguins$body_mass_g,
    use = "na.or.complete")

 # alt.
peng2 <- penguins[!is.na(penguins$flipper_length_mm), ]
peng2 <- peng2[!is.na(peng2$body_mass_g), ]
cor(peng2$flipper_length_mm,
    peng2$body_mass_g)


## Fråga 2 ----
# linear regression model
m1 <- lm(flipper_length_mm ~ body_mass_g,
         data = peng2)
summary(m1) # Adjusted R-squared


## Fråga 3 ----
# Check assumptions
exdata1 <- data.frame(
  x = c(2.99, 5.01, 8.84, 6.18, 8.57, 8.23, 8.48, 0.04, 6.80,
        7.62, 7.94, 6.30, 4.21, 3.61, 7.08, 3.50, 9.05, 1.06,
        0.65, 8.66, 0.08, 1.48, 2.96, 2.54, 4.45),
  y = c(5.25, -0.80, 4.38, -0.75, 9.93, 13.79, 19.75, 24.65,
        6.84, 11.95, 12.24, 7.97, -1.20, -1.76, 10.36, 1.17,
        15.41, 15.83, 18.78, 12.75, 24.17, 12.49, 4.58, 6.76, -2.92))

exdata2 <- data.frame(
  x = c(5.70, 8.03, 8.86, 0.82, 1.23, 2.96, 0.13, 8.53, 8.18,
        6.88, 4.02, 9.11, 0.19, 6.91, 0.34, 4.19, 0.25, 9.72,
        9.83, 6.77, 4.40, 4.70, 6.03, 5.87, 7.49),
  y = c(21.66, 26.23, 19.82, 2.46, 2.83, 8.86, 0.25, 16.08,
        17.67, 24.86, 8.19, 28.45, 0.52, 19.88, 0.71, 12.19,
        0.64, 25.29, 26.72, 18.06, 10.70, 8.27, 15.49, 15.58,
        19.17))

plot(exdata1) # No linear correlation
plot(exdata2) # Positive linear correlation

exdata1_lm <- lm(y ~ x, data = exdata1)
plot(exdata1_lm, which = 3) # Equal variance, ok
exdata2_lm <- lm(y ~ x, data = exdata2)
plot(exdata2_lm, which = 3) # Equal variance, not good


## Fråga 4 ----
# predict flipper length based on 90 kg (90000 g) penguin

predict_flipper <- predict(m1, newdata = data.frame(body_mass_g = 90000))
predict_flipper / 10 # Flipper length in cm
