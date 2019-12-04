Cars_Data <- mtcars

library(ggplot2)

# あとでグラフ再現して💢
graph1 <- ggplot(data = Cars_Data) + 
  geom_point(aes(x = hp, y = mpg, colour = am))+
  geom_abline()

graph1

model1 <- lm(mpg~1, data = Cars_Data)
model2 <- lm(mpg~hp, data = Cars_Data)
model3 <- lm(mpg~am, data = Cars_Data)
model4 <- lm(mpg~hp + am, data = Cars_Data)
model5 <- lm(mpg~hp * am, data = Cars_Data)
models <- list(model1, model2, model3, model4, model5)


for(model in models){
  print(paste0("R square is ",summary(model)$r.square))
  print(paste0("AIC score is ", AIC(model)))
}
