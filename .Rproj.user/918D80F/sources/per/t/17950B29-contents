# 初期値
b <- 0.8
x <- 3.0
y <- 9.0

i_max <- 10000

# 再現してみる。

plot(0,0, xlim = c(-10,10), ylim = c(-10,10),type = "n")
title(c(paste("Gibbs sampler",paste("b=",b))))

for(iter in 1:i_max){
  xold  <- x
  x_new_mean <- b*x
  x    <- rnorm(1, mean = x_new_mean, sd = 1)
  
  lines(c(xold,x),c(y,y), type = "b", col = 4, lwd = 2)
  #scan(stdin())
  
  yold = y
  y_new_mean <- b*y
  y    <- rnorm(1, mean = y_new_mean, sd = 1)
  
  lines(c(x,x),c(yold,y), type = "b", col = 4, lwd = 2)
  #scan(stdin())
  
}
