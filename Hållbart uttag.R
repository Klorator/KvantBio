# Hållbart uttag

# Funktion för populationstillväxt
# f(S) = 2.17 * sqrt(S) * log10(S+1)

f_of_S <- function(S) { # Function for pop. size next year
  2.17 * sqrt(S) * log(S+1)
}

curve(f_of_S(S), # Draw x = current pop. size, y = pop. next year
      xname = "S",
      xlim = c(0,150),
      xlab = "Kaniner i år",
      ylab = "Kaniner nästa år",
      col = "blue")
curve(1*S,       # Add constant pop. size
      xname = "S",
      add = T,
      col = "orange")


# Skördekurvan
# h(S) = f(S) - S
h_of_S <- function(S) { # Function for how many individuals can be harvested
  f_of_S(S) - S
}

curve(h_of_S(S), # Draw harvest curve
      xname = "S",
      xlim = c(0,150),
      xlab = "Kaniner, pop. storlek",
      ylab = "Kaniner som kan skördas",
      col = "darkgreen")

h_deriv <- function(S) { # Function for derivativ of harvest curve
  pracma::fderiv(h_of_S, S)
}

curve(h_deriv(S), # Draw derivative of harvest curve
      xname = "S",
      xlim = c(0,150),
      col = "purple")
abline(h = 0) # Add line marking y = 0

S_mhu <- uniroot(h_deriv, c(30,40))$root # pop. size for maximalt hållbart uttag

h_mhu <- h_of_S(S_mhu) # Max number of rabbits we can sustainably harvest

#++++++++++++++++++++++++++++++++++++++++++++++++++++++++#

# f(x) = -0.0019*x^2 + 1.38*x
fx <- function(x) {
  -0.0019*x^2 + 1.38*x
}

curve(fx(x),
      xlim = c(0,250),
      col = "blue",
      xlab = "Fjärilar i år",
      ylab = "Fjärilar nästa år")
curve(1*x,
      col = "orange",
      add = T)

fh <- function(x) {
  fx(x) - x
}

curve(fh(x),
      xlim = c(0,250),
      main = "Harvest curve",
      col = "darkgreen")
abline(h = 0)

fh_deriv <- function(x) {
  pracma::fderiv(fh,x)
}

curve(fh_deriv(x),
      xlim = c(0,250),
      col = "purple",
      main = "Derivative of harvest curve")
abline(h=0)

# Butterfly pop. for max harvest
f_mhu_pop <- uniroot(fh_deriv, c(90,110))$root

# Max sustainable butterfly harvest
f_mhu <- fh(f_mhu_pop)
