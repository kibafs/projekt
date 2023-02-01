library(palmerpenguins)
library(dplyr)
library(rpart)
library(caret)

data <- penguins
# Usuwam kolumny z brakującymi danymi
data <- data %>% filter(!is.na(sex))

# Wybieram tylko te kolumny które mnie interesują
cols_to_keep <- c("species", "bill_length_mm", "bill_depth_mm", "flipper_length_mm", "body_mass_g")
data <- data %>% select(all_of(cols_to_keep))

data$species <- as.factor(data$species)

results <- c()
for (i in 1:3) {
  Idx <- sample(1:nrow(data), 0.5 * nrow(data))
  train <- data[Idx, ]
  test <- data[-Idx, ]
  
  train$species <- as.factor(train$species)
  test$species <- as.factor(test$species)
  
  model <- rpart(species ~ ., data = train, method = "class")
  
  pred <- predict(model, newdata = test, type = "class")
  cf <- confusionMatrix(pred, test$species)
  print(cf)
  
  results[i] <- cf$overall[1]
}

dokladnosc <- mean(results)
odchylenie <- sd(results)

cat("Jakosc klasyfikatora:", dokladnosc, "\n")
cat("Odchylenie standardowe:", odchylenie, "\n")

