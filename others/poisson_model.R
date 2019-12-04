install.packages("unmarked")

library(unmarked)
UseData <- data(issj)


formula_list <- list(as.formula("count ~ 1"),
                     as.formula("count ~ elevation"),
                     as.formula("count ~ chapparal"),
                     as.formula("count ~ elevation"),
                     as.formula("count ~ elevation + chapparal"))
for(formulas in formula_list){
  model_list <- list()
  model_list[formulas] <- glm(formulas, data = issj, family = "poisson")
}