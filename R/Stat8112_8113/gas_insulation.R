
## Categorical predictor
insulgas <- read.table("data/insulgas.txt", header=TRUE)
insulgas$Insulate <- as.factor( insulgas$Insulate )

insulgas
plot(Gas[Insulate == "Before"] ~ Temp[Insulate == "Before"], xlab = "Outside Temperature", ylab = "Gas Consumption",  data = insulgas)
points(Gas[Insulate == "After"] ~ Temp[Insulate == "After"], pch = 2, col = 2, data=insulgas)
legend("topright",legend = c("Before","After"),pch = 1:2, bty="n", col=1:2)


gas.fit <- lm( Gas ~ Insulate + Temp, data=insulgas )

insulgas$dummy_before = (insulgas$Insulate=="Before")*1
insulgas$dummy_after = (insulgas$Insulate=="After")*1

gas.fit.dummy <- lm( Gas~  dummy_before + Temp, data=insulgas )
summary(gas.fit)
summary(gas.fit.dummy)


## If wanting to change the baseline level.
insulgas2 <- within(insulgas, Insulate <- relevel(Insulate, ref = "Before") )
gas.fit2 <- lm( Gas ~ Insulate + Temp, data= insulgas2 )
summary( gas.fit2 )

## Model without the constant.
gas.fit.dummy.all <- lm( Gas~  dummy_before + dummy_after + Temp, data=insulgas )
gas.fit3 <- lm( Gas ~ Insulate + Temp -1, data= insulgas )
summary( gas.fit3)

summary( gas.fit )
summary( gas.fit2 )
summary( gas.fit3 )


## ANOVA F test
null.model.insul <- lm( Gas ~ 1, data=insulgas )
reduced.model.insul <- lm( Gas ~ Temp, data=insulgas)
full.model.insul <- lm( Gas ~ Insulate + Temp, data=insulgas)
anova( full.model.insul )

anova( reduced.model.insul, full.model.insul )


################################################
################################################################################################
################################################
################################################
null.model.insul <- lm( Gas ~ 1, data=insulgas )
full.model.insul <- lm( Gas ~ Insulate + Temp, data=insulgas)
anova( null.model.insul,full.model.insul )

################################################
################################################################################################
################################################
################################################

anova( gas.fit )
anova( lm( Gas ~ Temp + Insulate, data=insulgas ) )
anova( lm( Gas ~ Temp, data=insulgas), gas.fit ) 




## ###################################################################################################
## ###################################################################################################
## ###################################################################################################
## Normality check
gas.fit <- lm( Gas ~ Insulate + Temp, data=insulgas )
gas.fit.2 <- lm( Gas ~  Temp, data=insulgas )
ks.test( gas.fit$residuals/sqrt( var(gas.fit$residuals) ), 'pnorm' )


## ANCOVA
ancova_model <- aov(Gas ~Insulate +  Temp, data = insulgas)
Anova(ancova_model, type="II")


## ANCOVA
ancova_model <- aov(Gas ~Insulate +  Temp + Insulate:Temp, data = insulgas)
Anova(ancova_model, type="II")

