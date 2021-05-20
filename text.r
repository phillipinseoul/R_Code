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

# Example: sleep dataset
library(psych)

head(sleep)
summary(sleep)
describe(sleep)

class(sleep$extra)
class(sleep$group)
class(sleep$ID)

# Descriptive statistics of sleep
y <- sleep$extra[sleep$group == 1]
summary(y)
sd(y)

par(mfrow=c(2,2))
hist(y)
boxplot(y)
qqnorm(y); qqline(y)
hist(y, prob=TRUE)          # prob=TRUE: y축 density (원래는 frequency)
lines(density(y), lty=2)

t.test(y)
t.test(y, alternative="greater")



### Categorical Variable ###

# Summary Statistics
set.seed(2000)
n <- 100
p <- 0.5
x <- rbinom(n, 1, p)        # Random generation for binomial distribution
x <- factor(x, levels = c(0,1), labels = c("fail", "success"))
table(x)
prop.table(table(x))
barplot(table(x))

# Test for probability of success
binom.test(x=length(x[x=='success']), 100, p = 0.5)
binom.test(x=5200, n=10000)

# Sample size (n) vs. Margin of error (moe)
n <- c(100, 1000, 2000, 10000, 1e6)
data.frame(n=n, moe=round(1.96*sqrt(1/(4*n)), 4))


### Numerical x - Numerical y ###
# Scatter plot
library(ggplot2)
plot(mpg$cty, mpg$hwy)
ggplot(mpg, aes(x=cty, y=hwy)) + geom_jitter() + geom_smooth(method="lm")

# Correlation coefficient
# 1. Pearson
cor(mpg$cty, mpg$hwy)
with(mpg, cor(cty, hwy))

# 2. Kendall & Spearman
attach(mpg)
cor(cty, hwy)       # pearson
cor(cty, hwy, method = "kendall")
cor(cty, hwy, method = "spearman")
detach(mpg)

# Fit the linear regression model
hwy_lm <- lm(hwy ~ cty, data=mpg)     # simple regression
hwy_lm
summary.lm(hwy_lm)

# Prediction
predict(hwy_lm)

resid(hwy_lm)

predict(hwy_lm, newdata = data.frame(cty=c(10, 20, 30)))

predict(hwy_lm, newdata = data.frame(cty=c(10, 20, 30)), se.fit = TRUE)     # se: standard error

opar = par(mfrow = c(2,2))
plot(hwy_lm, las = 1)
par(opar)

# Robust Linear Regression
library(MASS)
lm(hwy ~ cty, data = mpg)
lqs(hwy ~ cty, data = mpg)      # Robust linear regression

?lqs

set.seed(123)
lm(stack.loss ~ ., data = stackloss)    # linear regression
lqs(stack.loss ~ ., data = stackloss)   # Robust linear regression

# Smoothing method
# LOESS (LOcally weighted Scatterplot Smoothing)
plot(hwy ~ displ, data=mpg)
mpg_lo <- loess(hwy ~displ, data=mpg)
mpg_lo
summary(mpg_lo)

# Smoothing method & Visualization
# 1. R basic graph
xs <- seq(2, 7, length.out=100)
mpg_pre <- predict(mpg_lo, newdata=data.frame(displ=xs), se=TRUE)
lines(xs, mpg_pre$fit)
lines(xs, mpg_pre$fit - 1.96*mpg_pre$se.fit, lty=2)
lines(xs, mpg_pre$fit + 1.96*mpg_pre$se.fit, lty=2)

# 2. ggplot2 graph
ggplot(mpg, aes(displ, hwy)) + geom_point() + geom_smooth()


# Variance Analysis (ANOVA)
install.packages("dplyr")
library(dplyr)

mpg %>% ggplot(aes(class, hwy)) + geom_boxplot()

hwy_lm2 <- lm(hwy ~ class, data=mpg)
hwy_lm2
summary(hwy_lm2)

predict(hwy_lm2, newdata=data.frame(class="pickup"))

opar <- par(mfrow = c(2,2), oma = c(0, 0, 1.1, 0))
plot(hwy_lm2, las = 1)


### Categorical x - Numerical y ###
# Variance analysis (ANOVA)
mpg %>% ggplot(aes(class, hwy)) + geom_boxplot()

hwy_lm2 = lm(hwy ~ class, data=mpg)
hwy_lm2
summary(hwy_lm2)

    # i) Fit an analysis of Variance Model
aov.result = aov(hwy ~ class, data=mpg)
summary(aov.result)

anova(lm(hwy ~ class, data=mpg))

    # ii) Post-hoc analysis
pairwise.t.test(mpg$hwy, mpg$class, data=mpg, p.adj = "bonf")


### Numerical x - Categorical y ###













