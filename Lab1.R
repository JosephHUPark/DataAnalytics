library(readr)
library(EnvStats)
library(nortest)

# read in data
epi.data <- read_csv("epi_results_2024_pop_gdp.csv")

# check column names
colnames(epi.data)

# pull out the two variables I'm using
ECO <- epi.data$ECO.new
AIR <- epi.data$AIR.new

# summaries
summary(ECO)
summary(AIR)

# remove NAs
ECO.complete <- ECO[!is.na(ECO)]
AIR.complete <- AIR[!is.na(AIR)]

# boxplots
boxplot(ECO.complete, AIR.complete, names = c("ECO", "AIR"))

# histogram for ECO with normal curve overlaid
x.eco <- seq(min(ECO.complete), max(ECO.complete), length.out = 100)
hist(ECO.complete, prob = TRUE, main = "ECO histogram", xlab = "ECO")
lines(density(ECO.complete, bw = "SJ"))
lines(x.eco, dnorm(x.eco, mean = mean(ECO.complete), sd = sd(ECO.complete)))
rug(ECO.complete)

# histogram for AIR with normal curve overlaid
x.air <- seq(min(AIR.complete), max(AIR.complete), length.out = 100)
hist(AIR.complete, prob = TRUE, main = "AIR histogram", xlab = "AIR")
lines(density(AIR.complete, bw = "SJ"))
lines(x.air, dnorm(x.air, mean = mean(AIR.complete), sd = sd(AIR.complete)))
rug(AIR.complete)

# ecdf plots
plot(ecdf(ECO.complete), do.points=FALSE, verticals=TRUE)
plot(ecdf(AIR.complete), do.points=FALSE, verticals=TRUE)

# qq plots vs normal
qqnorm(ECO.complete); qqline(ECO.complete)
qqnorm(AIR.complete); qqline(AIR.complete)

# qq plot of the two variables against each other
qqplot(ECO.complete, AIR.complete, xlab = "Q-Q plot for ECO & AIR")

# normality tests
shapiro.test(ECO.complete)
ad.test(ECO.complete)

shapiro.test(AIR.complete)
ad.test(AIR.complete)

# test if the two variables have the same distribution
ks.test(ECO.complete, AIR.complete)
wilcox.test(ECO.complete, AIR.complete)
