x <- sample(x=1:100, size=100, replace=TRUE)

summary(x)
quantile(x)

# Summary statistics: describe()
install.packages("psych")
library(psych)
describe(x)

# Prepare mpg dataset
install.packages("ggplot2")
library(ggplot2)
?mpg
mpg <- ggplot2::mpg
class(mpg)
mpg <- data.frame(mpg)
class(mpg)
View(mpg)

### Relationship with two variables ###
# Correlation #
# 1. Pearson correlation
cor(mpg$cty, mpg$hwy)
with(mpg, cor(cty, hwy))

# 2. Kental / Spearman
with(mpg, cor(cty, hwy, method = "kendall"))
with(mpg, cor(cty, hwy, method = "spearman"))

# Covariance #
with(mpg, cov(cty, hwy))
with(mpg, cov(cty, hwy), method = "kendal")
with(mpg, cov(cty, hwy), method = "spearman")

### Statistical Analysis ###
# Difference test #
# 1. T-Test: t.test()
# 2. Variance analysis: anova()

# Correlation test #
# 1. Correlation: cor()
# 2. Regression: lm()


### Numerical Variable ###

# Use mpg dataset
library(ggplot2)
View(mpg)

# Summary statistics
summary(mpg$hwy)
describe(mpg$hwy)

# Visualization
par(mfrow=c(2,2))
hist(mpg$hwy)
boxplot(mpg$hwy)
plot(mpg$hwy)

# Check normality
qqnorm(mpg$hwy)     # Q-Q plot: Quantile-Quantile plot


# Hypothesis test
mu0 <- 22.9
t.test(mpg$hwy, mu=mu0, alternative = "greater")
t.test(mpg$hwy)
t.test(mpg$hwy, conf.level = 0.99)

# Check for outlier
boxplot(mpg$hwy)

c(mean(mpg$hwy), sd(mpg$hwy))
c(median(mpg$hwy), mad(mpg$hwy))        # MAD: Median Absolute Deviance
