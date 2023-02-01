library(naivebayes)
library(palmerpenguins)
library(dplyr)

dane <- penguins
# Usuwam kolumny z brakującymi danymi
dane <- dane %>% filter(!is.na(sex))

# Wybieram tylko te kolumny które mnie interesują
cols_to_keep <- c("species", "bill_length_mm", "bill_depth_mm", "flipper_length_mm", "body_mass_g")
dane <- dane %>% select(all_of(cols_to_keep))

xtabs(~species,dane)

Dokladnosc <- function(){
  idx <- sample(2,nrow(dane),replace=T,prob=c(0.5,0.5))
  train <- dane[idx==1,]
  test <- dane[idx==2,]
  
  model <- naive_bayes(as.character(species) ~ ., data=train, usekernel=T)
  
  p <- predict(model, select(test, -species))
  cf <- table(p, test$species)
  print(cf)
  
  return (sum(diag(cf))/sum(cf))
}
results <- replicate(3, Dokladnosc())

dokladnosc <- mean(results)
odchylenie <- sd(results)

cat("Jakosc klasyfikatora:", dokladnosc, "\n")
cat("Odchylenie standardowe:", odchylenie, "\n")



