dat <- read_csv("Hw2_data.csv")

## 3.1
#  a. Fit MLR model to x2, x7, x8
model <- lm(y~x2 + x7 + x8, data = dat)
model

#  b. Construct
# ANOVA table
anova(model)
# Test for significance of regression
summary(model)

#  c. Calculate t statistics for testing the hypotheses under H0
summary(model)

#  d. Calculate R^2 and adjusted R^2
rsq <- summary(model)$r.squared
adj_rsq <- summary(model)$adj.r.squared
rsq
adj_rsq

#  e. Determine contribution of x7 using partial F test -> excluding x7
model_reduced <- lm(y~x2 + x8, data = dat)
anova(model_reduced, model)

## 3.2
cor(model$fitted.values, dat$y)
cor(model$fitted.values, dat$y)^2
rsq

## 3.3 
#  a. Find 95% CI on beta_7
confint(model, 'x7', level = 0.95)

#  b. Find 95% CI on mean number of games won when x2 = 2300, x7 = 56.0, x8 = 2100
newdata <- data.frame(x2 = 2300, x7 = 56.0, x8 = 2100)
predict(model, newdata = newdata, interval = "confidence")

## 3.4
out <- lm(y~x7 + x8, data = dat)

#  a. Test for significance of regression
summary(out)

#  b. Calculate R^2 and adjusted R^2
rsq <- summary(out)$r.squared
adj_rsq <- summary(out)$adj.r.squared
rsq
adj_rsq

#  c. Calculate 
# 95% CI on beta_7
confint(out, 'x7', level = 0.95)
# 95% CI on mean number of games won when x7 = 56.0, x8 = 2100
newdata <- data.frame(x7 = 56.0, x8 = 2100)
predict(out, newdata = newdata, interval = "confidence")