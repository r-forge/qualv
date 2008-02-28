standard01 <- function (x) {
  xn <- (x - min(x)) / (max(x) - min(x))
  xn
}

steps <- function (x, difx, tol = 1e-6 * max(abs(difx))) {
  if (length(x) > 2 && any(abs(diff(difx)) > tol))
    warning ("time steps may be unequal", immediate. = TRUE)
}

f.slope <- function (x, y, f = 0.1) {
  N(x, y)
   
  difx <- diff(x)
  steps(x, difx)
  
  dify <- diff(y)
  difq <- c(0, dify / difx) 
  daten <- data.frame(difq, v = rep(0, length(difq)))
  epsilon <- mean(abs(difq[-1])) * f
  
  # increase
  plus <- which(daten$difq > epsilon)
  daten$v[plus] <- "A"
  # decrease
  minus <- which(daten$difq < -epsilon)
  daten$v[minus] <- "B" 
  # constant
  null <- which(daten$difq >= -epsilon & daten$difq <= epsilon)
  daten$v[null] <- "C"
  
  v <- daten$v
  v
}

f.curve <- function (x, y, f = 0.1) {
  N(x, y)
  
  difx1 <- diff(x)
  steps(x, difx1)
  
  dify1 <- diff(y)
  difq1 <- dify1 / difx1

  difx2 <- diff(x, lag = 2)
  dify2 <- diff(difq1)
  difq2 <- dify2 / difx2
  
  daten <- data.frame(difq2 = difq2, v = rep(0, length(difq2)))
  epsilon <- mean(abs(difq2)) * f
  
  # convex
  plus <- which(daten$difq2 > epsilon)
  daten$v[plus] <- "K"
  # concave
  minus <- which(daten$difq2 < -epsilon)
  daten$v[minus] <- "I" 
  # constant
  null <- which(daten$difq2 >= -epsilon & daten$difq2 <= epsilon)
  daten$v[null] <- "J"
  
  v <- c(0, paste(c(0, daten$v), c(daten$v, 0), sep = ""))
  v
}

f.steep <- function (x, y, f1 = 1, f2 = 0.1) {
  N(x, y)
  
  y <- standard01(y)
  difx <- diff(x)
  steps(x, difx)
  
  dify <- diff(y)
  difq <- c(0, dify / difx)
  alpha <- abs(atan(difq)) * 180 / pi
  daten <- data.frame(difq = difq, alpha = alpha, v = rep(0, length(difq)))    
     
  # very steep
  ss <- which(daten$alpha > f1)
  daten$v[ss] <- "S"
  # steep
  steep <- which(daten$alpha >= f2 & daten$alpha <= f1)
  daten$v[steep] <- "T" 
  # not steep
  nsteep <- which(daten$alpha < f2)
  daten$v[nsteep] <- "U"
  
  v <- daten$v 
  v
}

f.level <- function (y, high = 0.8, low = 0.2) {
  if(length(y) == 0) stop ("vector of length zero")

  y <- standard01(y)
  daten <- data.frame(y, v = rep(0, length(y)))
  
  H <- which(daten$y >= high)
  daten$v[H] <- "H"
  M <- which(daten$y > low & daten$y < high)
  daten$v[M] <- "M"
  N <- which(daten$y <= low)
  daten$v[N] <- "L"

  v <- daten$v
  v
}
