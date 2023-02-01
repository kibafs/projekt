library(class)
library(palmerpenguins)
library(dplyr)

data <- penguins
# Usuwam kolumny z brakującymi danymi
data <- data %>% filter(!is.na(sex))

# Wybieram tylko te kolumny które mnie interesują
cols_to_keep <- c("species", "bill_length_mm", "bill_depth_mm", "flipper_length_mm", "body_mass_g")
data <- data %>% select(all_of(cols_to_keep))

Dokladnosc <- function(){
  idx <- sample(1:nrow(data), 0.8*nrow(data))
  train <- data[idx,]
  test <- data[-idx,]
  cl <- train[, 1, drop = TRUE]
  
  model <- knn(train[, 2:5,], test[, 2:5,], cl, k = 10)

  cf <- table(model, test$species)
  print(cf)
  
  return(sum(diag(cf))/sum(cf))
}

results <- replicate(3, Dokladnosc())


dokladnosc <- mean(results)
odchylenie <- sd(results)

cat("Jakosc klasyfikatora:", dokladnosc, "\n")
cat("Odchylenie standardowe:", odchylenie, "\n")
  

