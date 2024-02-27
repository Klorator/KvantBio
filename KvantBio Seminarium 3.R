# Seminariefrågor 3

## Maximalt hållbart uttag
f_of_S <- function(S) {
  (3.255*S) / ((1+0.0073*S)^0.8178)
}

curve(f_of_S(S),
      xname = "S",
      xlim = c(0,500),
      col = "blue",
      xlab = "Skalbaggar pop.",
      ylab = "Skalbaggar nästa år")
curve(1*S,
      xname = "S",
      add = T,
      col = "orange")

f_of_S(200)
f_of_S(400)

hf <- function(S) {
  f_of_S(S) - S
}

curve(hf(S),
      xname = "S",
      col = "darkgreen",
      xlim = c(0,500),
      xlab = "Skalbaggar pop.",
      ylab = "Hållbart uttag")
abline(h = 0)

hf_deriv <- function(S) {
  pracma::fderiv(hf, S)
}

curve(hf_deriv(S),
      xname = "S",
      xlab = "Skalbaggar pop.",
      ylab = "Skalbaggar uttag [derivata]",
      xlim = c(0,500),
      col = "purple")
abline(h = 0)

f_mhu <- uniroot(hf_deriv, c(0,500))$root
max_harvest <- hf(f_mhu)

case1_harvest <- hf(350)

