#rm(list = ls(all = TRUE))
dir <- function (x1,x2,x3) {
  delta = 0.06
  a = 0
  alpha <- vector(mode="numeric", length=25)
  p <- vector(mode="numeric", length=25)
  for (i in 1:25) {
    d = 3
    a = a + delta
    alpha[i] = a
    B = gamma(a)^d/gamma(a*d)
    x = c(x1,x2,x3)
    f = 1
    for (j in 1:length(x)) {
      f = f*x[j]^(a-1)
    }
    p[i] = f/B
  }
  return(p)
}
dir2 <- function (a) {
  x <- vector(mode="numeric", length=25)
  y <- vector(mode="numeric", length=25)
  z <- vector(mode="numeric", length=25)
  x[1] = 0
  y[1] = 1
  for (i in 2:25) {
    delta = 0.04
    d = 2
    B = gamma(a)^d/gamma(a*d)
    x[i] = x[i-1] + delta
    y[i] = 1 - x[i]
    theta = c(x[i],y[i])
    f = 1
    for (j in 1:length(theta)) {
      f = f*theta[j]^(a-1)
    }
    z[i] = f/B
  }
  print(z)
  scatter3D(y,x,z, type= "l", theta=-30, alpha=1)
}