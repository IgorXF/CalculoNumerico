a <- 1
b <- 2
n <- 4

f <- function(x) {
  return(1/x)  
}


trapezios <- function(f, a, b, n) {
  h <- (b - a) / n
  x <- seq(a, b, by = h)
  y <- f(x)
  integral <- (h / 2) * (y[1] + 2 * sum(y[2:n]) + y[n + 1])
  return(integral)
}


ponto_medio <- function(f, a, b, n) {
  h <- (b - a) / n
  midpoints <- seq(a + h/2, b - h/2, by = h)
  y <- f(midpoints)
  integral <- h * sum(y)
  return(integral)
}


simpson13 <- function(f, a, b, n) {
  if (n %% 2 != 0) {
    stop("n deve ser par para a regra 1/3 de Simpson.")
  }
  h <- (b - a) / n
  x <- seq(a, b, by = h)
  y <- f(x)
  integral <- (h / 3) * (y[1] +
                           4 * sum(y[seq(2, n, by = 2)]) +
                           2 * sum(y[seq(3, n - 1, by = 2)]) +
                           y[n + 1])
  return(integral)
}

trapezios(f, a, b, n)
ponto_medio(f, a, b, n)
simpson13(f, a, b, n)
