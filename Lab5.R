library(readr)
library(e1071)
library(class)
library(randomForest)

# read in data
wine <- read_csv("wine.data", col_names = FALSE)

names(wine) <- c("Type","Alcohol","Malic.acid","Ash","Alcalinity.of.ash","Magnesium",
                 "Total.phenols","Flavanoids","Nonflavanoid.Phenols","Proanthocyanins",
                 "Color.Intensity","Hue","OD280","Proline")

wine$Type <- as.factor(wine$Type)

# use the same top 4 variables identified from PCA in lab 4
wine.sub <- wine[, c("Type","Flavanoids","Proline","OD280","Total.phenols")]

# train/test split 70/30
set.seed(42)
n <- nrow(wine.sub)
train.idx <- sample(1:n, size = 0.7 * n)

train <- wine.sub[train.idx, ]
test  <- wine.sub[-train.idx, ]

Y.test <- test$Type


### 1. SVM with linear kernel ###

# tune C for linear kernel
tune.linear <- tune.svm(Type ~ ., data = train,
                        kernel = "linear",
                        cost = c(0.01, 0.1, 1, 10, 100))
print(tune.linear)
best.linear <- tune.linear$best.model

pred.linear <- predict(best.linear, test)
table.linear <- table(Predicted = pred.linear, Actual = Y.test)
print(table.linear)


### 2. SVM with radial (RBF) kernel ###

# tune C and gamma for radial kernel
tune.radial <- tune.svm(Type ~ ., data = train,
                        kernel = "radial",
                        cost  = c(0.1, 1, 10, 100),
                        gamma = c(0.01, 0.1, 1, 10))
print(tune.radial)
best.radial <- tune.radial$best.model

pred.radial <- predict(best.radial, test)
table.radial <- table(Predicted = pred.radial, Actual = Y.test)
print(table.radial)


### 3. Random Forest ###

rf.model <- randomForest(Type ~ ., data = train, ntree = 500)
print(rf.model)

pred.rf <- predict(rf.model, test)
table.rf <- table(Predicted = pred.rf, Actual = Y.test)
print(table.rf)


### 4. Compare all three models ###

calc.metrics <- function(conf.table) {
  classes <- rownames(conf.table)
  results <- data.frame()
  for (cls in classes) {
    tp <- conf.table[cls, cls]
    fp <- sum(conf.table[cls, ]) - tp
    fn <- sum(conf.table[, cls]) - tp
    precision <- tp / (tp + fp)
    recall    <- tp / (tp + fn)
    f1        <- 2 * precision * recall / (precision + recall)
    results <- rbind(results, data.frame(Class     = cls,
                                         Precision = round(precision, 3),
                                         Recall    = round(recall, 3),
                                         F1        = round(f1, 3)))
  }
  return(results)
}

cat("\n=== SVM Linear Kernel ===\n")
print(table.linear)
print(calc.metrics(table.linear))

cat("\n=== SVM Radial Kernel ===\n")
print(table.radial)
print(calc.metrics(table.radial))

cat("\n=== Random Forest ===\n")
print(table.rf)
print(calc.metrics(table.rf))