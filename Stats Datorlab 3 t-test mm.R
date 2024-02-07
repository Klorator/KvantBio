# Datorlab S3: t-test, konfidensintervall och normalfördelning

klorofyll <- c(14.9, 18.2, 23.6, 29.0, 10.7, 
               8.5, 30.5, 14.8, 21.2, 15.5, 
               27.3, 19.6, 16.8, 9.9, 14.1, 
               11.9, 10.8, 20.3, 22.4, 19.2)

hist(klorofyll, freq = F)
curve(
  dnorm(x, 
        mean = mean(klorofyll), 
        sd = sd(klorofyll)),
  col = "blue",
  add = T
)


par(mfrow = c(3,3)) # Rita histogrammen i ett 3x3-rutnät
for(i in 1:8) {
  y <- rnorm(20)
  hist(y, freq = FALSE, main = "Simulerade normalfördelade data")
  curve(dnorm(x, mean(y), sd(y)), col = 2, add = TRUE)
}
hist(klorofyll, freq = FALSE, main = "Klorofylldata")
curve(dnorm(x, mean(klorofyll), sd(klorofyll)), col = 2, add = TRUE)


par(mfrow = c(1,1)) # Återgå till att bara ha en figur i Plot-fönstret
qqnorm(klorofyll); qqline(klorofyll)

# Hur ser Q-Q-plottar ut när vi har normalfördelade data?
par(mfrow = c(3,3)) # Rita qqplottarna i ett 3x3-rutnät
for(i in 1:8) {
  x <- rnorm(20)
  qqnorm(x, main = "Simulerade normalfördelade data"); qqline(x)
}
qqnorm(klorofyll, main = "Klorofylldata"); qqline(klorofyll)

# Sedan provar vi att simulera data från exponentialfördelningen, som är väldigt olik normalfördelningen i sin form (se föreläsning 2):
  
# Hur ser Q-Q-plottar ut när vi inte har normalfördelade data?
par(mfrow = c(3,3)) # Rita qqplottarna i ett 3x3-rutnät
for(i in 1:8) {
  x <- rexp(20)
  qqnorm(x, main = "Simulerade icke-normalfördelade data"); qqline(x)
}
qqnorm(klorofyll, main = "Klorofylldata"); qqline(klorofyll)

par(mfrow = c(1,1))


## Fråga 1
data1 <- c(22.5, 18.9, 24.7, 19.2, 24.9, 24.5, 21.1, 26.9, 27.9, 25.7, 
           29.9, 19.6, 20.3, 29.8, 26.0, 19.7, 20.4, 27.1, 21.1, 17.6, 
           28.8, 27.6, 25.1, 19.3, 21.3, 28.3, 25.5, 24.0, 28.2, 19.9, 
           30.0)

data2 <- c(2.9, 0.9, 11.1, 0.8, 5.3, 2.4, 5.9, 6.0, 18.4, 1.4, 
           4.0, 3.8, 7.6, 1.3, 3.2, 1.4, 1.2, 0.4, 1.6, 9.2, 
           1.8, 2.2, 1.5, 3.0, 4.0, 3.4, 1.2, 2.5, 1.2, 7.9)

data3 <- c(4.4, 5.4, 6.0, 6.5, 5.5, 6.1, 3.2, 5.1, 5.7, 4.2, 
           5.1, 3.5, 2.5, 4.6, 4.5, 5.6, 7.1, 3.0, 4.8, 5.3, 
           5.6, 6.4, 3.3, 4.9, 6.4, 6.7, 5.3, 7.7, 7.0, 7.4)

par(mfrow = c(1,3))

hist(data1)
hist(data2)
hist(data3)

qqnorm(data1); qqline(data1)
qqnorm(data2); qqline(data2)
qqnorm(data3); qqline(data3)

par(mfrow = c(1,1))


## Fråga 2
hanar <- c(5200, 5200, 5400, 5200, 5150, 
           5550, 5150, 5250)
honor <- c(4900, 4850, 4750, 4800, 4800, 
           4950, 4850, 4900)

ttest_mf <- t.test(hanar, honor)
ttest_mf$p.value # Significant difference

honor[8] <- 5900

ttest_mf2 <- t.test(hanar, honor)
ttest_mf2$p.value

#########################################

vegfa <- c(276.70521, 1427.79981, 356.30008, 1196.00110, 509.23261, 
           11796.42430, 1310.99730, 1123.37197, 594.77945, 3146.80268, 
           387.09021, 881.72075, 93.59243, 388.94706, 134.02358, 
           531.34276, 453.40139, 253.06883, 9501.20644, 2206.76609,
           291.76904, 1825.95184, 194.53501, 804.66474, 1463.09730, 
           745.44133, 3829.71416, 506.53986, 346.49363, 289.47822, 
           3465.16420, 575.99201, 666.90155, 588.46630, 40.73986, 
           760.25838, 270.74971, 98.40727, 1011.29137, 1787.31351, 
           369.82245, 4253.98432, 26.86122, 5950.22703, 660.93198, 
           652.61847, 1406.71076, 217.30913, 474.04625, 1391.25003, 
           354.73552, 851.00335, 1837.26625, 952.69856, 6912.65400, 
           339.90152, 205.87360, 456.78200, 752.64827, 28462.01140, 
           607.46101, 4810.26280, 525.76343, 73.61633, 1563.96770, 
           425.86430, 269.13328, 722.66339, 1324.01615, 111.68116, 
           85.35531, 2180.75200, 245.57927, 173.11069, 461.91129, 
           2621.96007, 1639.08846, 1164.64698, 298.51177, 840.54893, 
           1272.62001, 240.23324, 839.38391, 102.66944, 355.69922, 
           2567.21071, 4279.33231, 1025.67717, 116.18017, 239.98809, 
           56.99277, 1505.90391, 225.31835, 2249.19339, 6380.29317, 
           224.28728, 1282.58436, 2769.40820, 14579.73624, 423.06866)

par(mfrow = c(1,2))
hist(vegfa, 
     main = "100 mätningar av VEGF-A i plasma", 
     xlab = "Uttrycksnivå", 
     ylab = "Frekvens", 
     breaks = 10)
hist(log2(vegfa), 
     main = "100 mätningar av VEGF-A (log2-transformerat)", 
     xlab = "Uttrycksnivå (log2-skala)", 
     ylab = "Frekvens")


x <- c(1, 2, 5)
y <- c(2, 2, 3)
mean(x) - mean(y) # x har högre medelvärde än y

mean(log(x)) - mean(log(y)) # Efter logaritmering har y högre medelvärde än x!

