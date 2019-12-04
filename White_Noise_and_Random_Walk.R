set.seed(1234)
eta <- rnorm(1000, mean = 0, sd = 1) # 普通に時系列する

plot(eta, type = "b") # ガチャガチャしてる
summary(eta)

rand_walk <- function(eta){
  alpha    <- numeric(length(eta))
  alpha[1] <- eta[1]
  for(time in 2:length(eta)){
    alpha[time] <- sum(eta[1:(time-1)])
  }
  return(alpha)
}

plot(rand_walk(eta), type = "b") # 確かに発散してそう。
summary(rand_walk(eta))

## 自己回帰モデル

model_arima <- arima(eta)

fitted_arima <- predict(model_arima, times = c(1:100))
fitted_arima

## 
plot(eta, type = "o")
plot(rand_walk(eta), type ="o")
plot(cumsum(rand_walk(eta)), type = "o")
