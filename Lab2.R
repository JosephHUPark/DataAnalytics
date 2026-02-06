library(ggplot2)
library(readr)

# Load data
dataset <- read_csv("C:/Users/josep/Downloads/NY-House-Dataset.csv")

# Optional filter
dataset <- dataset[dataset$PRICE < 195000000, ]

# ---- Models ----
names(dataset) <- toupper(names(dataset))

# Expecting: PRICE, PROPERTYSQFT, BEDS, BATH
needed <- c("PRICE", "PROPERTYSQFT", "BEDS", "BATH")
missing_cols <- setdiff(needed, names(dataset))
if (length(missing_cols) > 0) {
  stop(paste("Missing columns in dataset:", paste(missing_cols, collapse = ", ")))
}

# ---- Basic cleaning ----
dataset <- dataset %>%
  select(PRICE, PROPERTYSQFT, BEDS, BATH) %>%
  filter(
    is.finite(PRICE), is.finite(PROPERTYSQFT), is.finite(BEDS), is.finite(BATH),
    PRICE > 0, PROPERTYSQFT > 0,
    BEDS >= 0, BATH >= 0
  ) %>%
  na.omit()

# Outlier trimming (keeps middle 99% by default; tweak if needed)
trim_q <- function(x, lo = 0.005, hi = 0.995) {
  q <- quantile(x, probs = c(lo, hi), na.rm = TRUE)
  x >= q[1] & x <= q[2]
}

dataset <- dataset %>%
  filter(
    trim_q(PRICE),
    trim_q(PROPERTYSQFT),
    trim_q(BEDS, 0.001, 0.999),
    trim_q(BATH, 0.001, 0.999)
  )

# ---- Transformations ----
dataset <- dataset %>%
  mutate(
    logPRICE = log10(PRICE),
    logSQFT  = log10(PROPERTYSQFT)
  )

# ---- Models (3 combinations) ----
m1 <- lm(logPRICE ~ logSQFT, data = dataset)
m2 <- lm(logPRICE ~ logSQFT + BEDS, data = dataset)
m3 <- lm(logPRICE ~ logSQFT + BEDS + BATH, data = dataset)
models <- list(m1, m2, m3)

# ---- Loop (summaries + predictor plots stay the same) ----
for (i in seq_along(models)) {
  
  m <- models[[i]]
  
  # 1. Print summary
  print(summary(m))
  
  # Most significant variable
  coefs <- summary(m)$coefficients
  sig_var <- rownames(coefs)[which.min(coefs[-1, 4]) + 1]
  
  # Predictor vs Price (ggplot)
  print(
    ggplot(dataset, aes(x = .data[[sig_var]], y = PRICE)) +
      geom_point() +
      geom_smooth(method = "lm", se = FALSE) +
      labs(title = paste("PRICE vs", sig_var))
  )
}

par(mfrow = c(1, 3))  # 3 plots side by side

plot(fitted(m1), resid(m1),
     main = "Model 1 Residuals",
     xlab = "Fitted", ylab = "Residuals")
abline(h = 0, col = "red")

plot(fitted(m2), resid(m2),
     main = "Model 2 Residuals",
     xlab = "Fitted", ylab = "Residuals")
abline(h = 0, col = "red")

plot(fitted(m3), resid(m3),
     main = "Model 3 Residuals",
     xlab = "Fitted", ylab = "Residuals")
abline(h = 0, col = "red")

par(mfrow = c(1, 1))  # reset layout
