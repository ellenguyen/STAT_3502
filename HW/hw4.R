library("tidyverse")
library("car")
viscosity <- read_csv("HW/viscosity.csv")
vapor <- read_csv("HW/vapor.csv")
games <- read_csv("HW/Hw2_data-1.csv")

## 4.1
# a. Construct a normal probability plot of the residuals
games_lm <- lm(y ~ x8, data = games)
summary(games_lm)

residuals <- games_lm$residuals
hist(residuals)
qqnorm(residuals); qqline(residuals, col = 'red')

shapiro.test(residuals)

# b. Construct and interpret a plot of the residuals versus the predicted response.
predicted <- games_lm$fitted.values
plot(predicted, residuals); abline(h = 0, col = "blue")

# c. Plot the residuals versus the team passing yardage, x2.
games_lm_x2 <- lm(y ~ x8 + x2, data = games)
summary(games_lm_x2)

new_residuals <- games_lm_x2$residuals
plot(games$x2, new_residuals); abline(h = 0, col = "blue")

## 4.2
# a. Construct a normal probability plot of the residuals.
games_mlr <- lm(y ~ x2 + x7 + x8, data = games)
summary(games_mlr)

mlr_residuals <- games_mlr$residuals
hist(mlr_residuals)
qqnorm(mlr_residuals); qqline(mlr_residuals, col = 'red')

shapiro.test(mlr_residuals)

# b. Construct and interpret a plot of the residuals versus the predicted response.
mlr_predicted <- games_mlr$fitted.values
plot(mlr_predicted, mlr_residuals); abline(h = 0, col = "blue")

# c. Construct plots of the residuals versus each of the regressor variables.
par(mfrow = c(1, 3))
for (var in c("x2", "x7", "x8")) {
  plot(games[[var]], residuals,
       main = paste("Residuals vs.", var),
       xlab = var,
       ylab = "Residuals")
  abline(h = 0, col = "blue")
}

# d. Construct the partial regression plots for this model. 
# Compare the plots with the plots of residuals versus regressors from part c above.
par(mfrow = c(3, 3))
avPlots(games_mlr, ask = FALSE, main = "Partial Regression Plots")

## 5.1
# a. Plot a scatter diagram
plot(viscosity$temp, viscosity$visc); abline(h = 0, col = "blue")

# b. Fit the straight-line model. Compute the summary statistics and the residual plots.
visc_lm <- lm(visc ~ temp, data = viscosity)
summary(visc_lm)

visc_pred <- visc_lm$fitted.values; visc_residuals <- visc_lm$residuals
plot(visc_pred, visc_residuals); abline(h = 0, col = "blue")

# c. Repeat part b using the appropriate transformation based on this information.
viscosity$log_visc <- log(viscosity$visc)
visc_expo_lm <- lm(log_visc ~ temp, data = viscosity)
summary(visc_expo_lm)

new_visc_pred <- visc_expo_lm$fitted.values; new_visc_residuals <- visc_expo_lm$residuals
plot(new_visc_pred, new_visc_residuals); abline(h = 0, col = "blue")

## 5.2
# a. Plot a scatter diagram.
plot(vapor$temp, vapor$vapor); abline(h = 0, col = "blue")

# b. Fit the straight-line model. Compute the summary statistics and the residual plots.
vapor_lm <- lm(vapor ~ temp, data = vapor)
summary(vapor_lm)

vapor_pred <- vapor_lm$fitted.values; vapor_residuals <- vapor_lm$residuals
plot(vapor_pred, vapor_residuals); abline(h = 0, col = "blue")

# c. Repeat part b using the appropriate transformation based on this information.
vapor$log_vapor <- log(vapor$vapor)
vapor$trans_temp <- -1/vapor$temp
new_vapor_lm <- lm(log_vapor ~ trans_temp, data = vapor)
summary(new_vapor_lm)

new_vapor_pred <- new_vapor_lm$fitted.values; new_vapor_residuals <- new_vapor_lm$residuals
plot(new_vapor_pred, new_vapor_residuals); abline(h = 0, col = "blue")