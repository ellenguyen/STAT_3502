## Problem 2.1
# Part a
hw2_data <- read_csv("Hw2_data.csv")
model <- lm(y~x8, data = hw2_data)
model

# Part b
# Analysis-of-variance table
anova(model)

# Test for significance of regression
summary(model)

# Part c
confint(model, 'x8', level = 0.95)

# Part d - check R^2

# Part e
new_x8 <- data.frame(x8 = c(2000))
predict(model, new_x8, interval = "confidence", level = 0.95)


## Problem 2.2
new_x8 <- data.frame(x8 = c(1800))
predict(model, new_x8, interval = "predict", level = 0.90)