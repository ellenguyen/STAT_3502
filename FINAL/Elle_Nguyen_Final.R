# ELLE NGUYEN

# Question 1 -------------------------------------------------------------------
# read the data
mydata = read.csv("FINAL/beer.csv",header=T)

# log transformation of cases sold of 18 packs as response y
y <- log(mydata$CASES.18PK)

# log transformation of price of 12 packs, 18 packs, 30 packs as x1, x2, x3
x1 <- log(mydata$PRICE.12PK)
x2 <- log(mydata$PRICE.18PK)
x3 <- log(mydata$PRICE.30PK)

# fit a multiple linear regression with y as response and x1, x2, x3 as predictors
mlr_model <- lm(y ~ x1 + x2 + x3)

# part a -----------------------------------------------------------------------
# Do the predictors have significant effects on the response?
summary(mlr_model)

# part b -----------------------------------------------------------------------
# Plot the residual from the model vs variable week
residuals <- mlr_model$residuals
plot(mydata$Week, residuals); abline(h = 0, col = "blue")

# part c -----------------------------------------------------------------------
# denote week as x4
x4 <- mydata$Week

# fit a multiple linear regression with y as response and x1, x2, x3, x4 as predictors
mlr_model2 <- lm(y ~ x1 + x2 + x3 + x4)
summary(mlr_model2)

# formal test to see if x4 has a positive effect on y
anova(mlr_model, mlr_model2)
summary(lm(y ~ x4))

# Question 2 -------------------------------------------------------------------
# read the data
mydata = read.table("FINAL/auto.data",header=F)
colnames(mydata) <- c("mpg","cylinders","displacement", "hp","weight","ac","year","origin","name") 

# part a -----------------------------------------------------------------------
# scatterplot between mpg vs. weight
mpg <- mydata$mpg
weight <- mydata$weight
plot(weight, mpg)

# part b -----------------------------------------------------------------------
# create new variable y (gallons per 100 miles) and x1 (weight in 1000 pounds)
y <- 100 / mpg
x1 <- weight / 1000

# scatterplot between y vs. x1
plot(x1, y)

# part c -----------------------------------------------------------------------
# create new variable x2 that corresponds to the model year of the auto-mobile
x2 <- mydata$year

# fit a multiple linear regression with y as response and x1, x2 as predictors
model <- lm(y ~ x1 + x2)

# write the fitted regression model
summary(model)

# part d -----------------------------------------------------------------------
# create new variable y_tilted (gallons per mile)
y_tilted <- 1 / mpg

# fit a multiple linear regression with y_tilted as response and x1, x2 as predictors
model_2 <- lm(y_tilted ~ x1 + x2)

# compare
summary(model_2)

# Question 3 -------------------------------------------------------------------
# read the data
mydata = read.table("FINAL/fish.txt",header=F)

# remove 14th observation where weight is missing
# remove 47th observation where weight is 0
mydata <- mydata[mydata$V3 != 0 & !(is.na(mydata$V3)),]

# part a -----------------------------------------------------------------------
# implement the log transformation
trans_weight <- log(mydata$V3)
trans_length <- log(mydata$V6)

# fit a linear regression model
trans_model <- lm(trans_weight ~ trans_length)
summary(trans_model)

# part b -----------------------------------------------------------------------
# create indicator z
# z = 1 if the fish species is smelt or pike, z = 0 otherwise
z <- as.factor(ifelse((mydata$V2 == 5 | mydata$V2 == 6), 1, 0))
z <- relevel(z, ref = "1")

# fit a multiple linear regression model with z as additional predictor
new_trans_model <- lm(trans_weight ~ trans_length + z)
summary(new_trans_model)

# formally test if z has a significant effect on the response
anova(trans_model, new_trans_model)

# part c -----------------------------------------------------------------------
# denote continuous predictor in part a as x
x <- trans_length

# interaction between x and z
int_model <- lm(trans_weight ~ x + z + x*z)

# analysis to test interaction effect
summary(int_model)
anova(new_trans_model, int_model)

# plot residuals vs fitted response
par(mfrow = c(1, 2))
plot(new_trans_model$fitted.values, new_trans_model$residuals, 
     xlab="fitted response", ylab="residual", main="without interaction"); abline(h=0,lty=2)

plot(int_model$fitted.values, int_model$residuals, 
     xlab="fitted response", ylab="residual", main="with interaction"); abline(h=0,lty=2)
``