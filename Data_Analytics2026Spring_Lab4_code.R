library(ggplot2)
library(ggfortify)
library(GGally)
library(e1071)
library(class)
library(psych)
library(readr)

# read in data
wine <- read_csv("wine.data", col_names = FALSE)

# set column names
names(wine) <- c("Type","Alcohol","Malic acid","Ash","Alcalinity of ash","Magnesium",
                 "Total phenols","Flavanoids","Nonflavanoid Phenols","Proanthocyanins",
                 "Color Intensity","Hue","Od280/od315 of diluted wines","Proline")

# convert Type to factor
wine$Type <- as.factor(wine$Type)

head(wine)

# separate features and labels
X <- wine[,-1]
Y <- wine$Type


### 1. PCA ###

# scale the data - important because variables have very different units
X.scaled <- scale(X)

# compute principal components
pca <- princomp(X.scaled)

# summary shows how much variance each PC explains
summary(pca)

# loadings show how much each variable contributes to each PC
print(pca$loadings)

# plot PC1 vs PC2 scores colored by wine type
scores <- as.data.frame(pca$scores)
scores$Type <- Y

ggplot(scores, aes(x = Comp.1, y = Comp.2, color = Type)) +
  geom_point(size = 2) +
  labs(title = "PCA: PC1 vs PC2", x = "PC1", y = "PC2")


### 2. Variables contributing most to PC1 ###

# get absolute loadings for PC1 and sort
pc1.loadings <- sort(abs(pca$loadings[,1]), decreasing = TRUE)
print(pc1.loadings)

# plot them
barplot(pc1.loadings, las = 2, main = "Variable Contributions to PC1",
        ylab = "Absolute Loading")


### 3. kNN on original variables ###

# pick top 4 variables from PC1 loadings: Flavanoids, Proline, OD, Total phenols
X.sub <- wine[, c("Flavanoids", "Proline", "Od280/od315 of diluted wines", "Total phenols")]

# scale the subset
X.sub.scaled <- scale(X.sub)

# train/test split - 70% train, 30% test
set.seed(42)
n <- nrow(wine)
train.idx <- sample(1:n, size = 0.7 * n)

X.train.orig <- X.sub.scaled[train.idx, ]
X.test.orig  <- X.sub.scaled[-train.idx, ]
Y.train <- Y[train.idx]
Y.test  <- Y[-train.idx]

# run kNN with k=5
knn.orig <- knn(train = X.train.orig, test = X.test.orig, cl = Y.train, k = 5)

# contingency table
table.orig <- table(Predicted = knn.orig, Actual = Y.test)
print(table.orig)


### 4. kNN on PCA scores ###

# use first 2 PC scores as features
X.pca <- pca$scores[, 1:2]

X.train.pca <- X.pca[train.idx, ]
X.test.pca  <- X.pca[-train.idx, ]

# run kNN with k=5
knn.pca <- knn(train = X.train.pca, test = X.test.pca, cl = Y.train, k = 5)

# contingency table
table.pca <- table(Predicted = knn.pca, Actual = Y.test)
print(table.pca)


### 5. Compare models - precision, recall, F1 ###

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
    results <- rbind(results, data.frame(Class=cls, Precision=round(precision,3),
                                         Recall=round(recall,3), F1=round(f1,3)))
  }
  return(results)
}

cat("\n=== Original Variables Model ===\n")
print(table.orig)
print(calc.metrics(table.orig))

cat("\n=== PCA Model ===\n")
print(table.pca)
print(calc.metrics(table.pca))