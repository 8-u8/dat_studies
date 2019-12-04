library(KFAS)
library(pscl)


# 深山さんソースを一般化。とりあえずpoisson分布を任意のデータに適用できるように拡張
DummyData <- rpois(400, 30) # 疑似データ

nloglik <- function(lambda){
  output <- dpois(DummyData, lambda, log = TRUE)
  output <- -sum(output)
  return(output)
}

result <- optimize(f = nloglik, interval = c(0,1e6)) # 最適化で頑張る

result

hist(DummyData)
hist(rpois(0:4000, result$minimum))

## 推定不確実性の評価

bres <- vector(length = 1000)
for(n in 1:1000){
  y_rep <- rpois(length(DummyData), result$minimum)
  
  local_n_loglik <- function(lambda){
    output <- dpois(y_rep, lambda, log = TRUE)
    output <- -sum(output)
    return(output)
    
  }
  local_result <- optimize(local_n_loglik, interval = c(0,1e6))
  bres[n]      <- local_result$minimum
}

hist(bres)
abline(v = result$minimum)

# モデル当てはまり評価
bdev <- vector(length = 1000) # 尤離度

for(n in 1:1000){
  y_rep <- rpois(length(DummyData), result$minimum)
  local_n_loglik <- function(lambda){
    output <- dpois(y_rep, lambda, log = TRUE)
    output <- -sum(output)
    return(output)
  }
  
  local_result <- optimize(local_n_loglik, interval = c(0,1e6))
  bres         <- local_result$minimum
  # residual deviance
  local_deviance_Obse  <- 2 * sum(dpois(y_rep, y_rep, log = TRUE))
  local_deviance_Pred  <- 2 * sum(dpois(y_rep, bres,  log = TRUE)) 
  bdev[n]              <- local_deviance_Obse - local_deviance_Pred
}


hist(bdev)
global_deviance_Obse <- 2 * sum(dpois(DummyData, DummyData)     , log = TRUE)
global_deviance_Pred <- 2 * sum(dpois(DummyData, result$minimum), log = TRUE)

global_deviance_value <- global_deviance_Obse - global_deviance_Pred

abline(v = global_deviance_value)

