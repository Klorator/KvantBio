# Hello world!
print("Hello World")

number <- 5 # A number
string <- "5" # Text, not a number


# Plotta fibonacci

## Fråga 2
y <- c() # Empty vector

y[1] <- 1
y[2] <- 1

# Generate fibonacci sequence
for (i in 3:25) {
  y[i] <- y[i-1] + y[i-2]
}

## Fråga 8
plot(y)

## Fråga 9
plot(y,
     ylab = "Fibonaccital",
     xlab = "n")

plot(log(y),
     ylab = "Fibonaccital",
     xlab = "n")

## Fråga 10
Kvot <- c()

for (i in 2:10) {
  Kvot[i] <- y[i]/y[i-1]
}

plot(Kvot,
     type = "l",
     col = "blue",
     lwd = 2)

