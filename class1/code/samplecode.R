## Annette Dobson (1990) "An Introduction to Generalized Linear Models".
## Page 9: Plant Weight Data.
ctl <- c(4.17,5.58,5.18,6.11,4.50,4.61,5.17,4.53,5.33,5.14)
trt <- c(4.81,4.17,4.41,3.59,5.87,3.83,6.03,4.89,4.32,4.69)
group <- gl(2, 10, 20, labels = c("Ctl","Trt"))
weight <- c(ctl, trt)
t.test(ctl,trt)
mean(ctl)

lm.D9 <- lm(weight ~ group)
summary(lm.D9)

lm.D90 <- lm(weight ~ group - 1)

anova(lm.D9)
summary(lm.D90)
plot(lm.D9, las = 1)      # Residuals, Fitted, ...
par(opar)


#install.packages("car")
library(car)
help("Prestige")
Prestige
dim(Prestige)
summary(Prestige)
hist(Prestige$income)
hist(log2(Prestige$income))
prestige = Prestige$prestige;
attach(Prestige)
detach(Prestige)
reg1 = lm(prestige~education+log2(income)+women,data = Prestige)
summary(reg1)
reg2 = lm(prestige~education+income+women,data = Prestige)
summary(reg2)
plot(reg1)
summary(reg1)
#变量选择 
reg1.aic = step(reg1,)  #AIC
reg1.bic = step(reg1,k=log(nrow(Prestige)))  #BIC
Anova(reg1.aic,type = 3)

prestige_predict = fitted(reg1)
plot(prestige_predict)
prestige_resid = residuals(reg1)

#add variable
reg2 = lm(prestige~education+log2(income)+women+type,data=Prestige)
summary(reg2)
#interaction
reg2 = lm(prestige~education+log2(income)+women*type,data=Prestige)
summary(reg2)
pres.data = Prestige;
pres.data$type = as.character(pres.data$type)
pres.data[is.na(pres.data$type),]$type="bc";
pres.data[pres.data$type=="prof",]$type = "bc"

summary(lm(prestige~women+type,data=pres.data));
summary(lm(prestige~women*type,data=pres.data));



#0-1 regression
reg4 = lm(am~hp+wt,data=mtcars);
summary(reg4)
help(mtcars)
am.glm.logit = glm(formula=am ~ hp + wt,  data=mtcars, family=binomial(link = "logit"))
summary(am.glm.logit)


am.glm.probit = glm(formula=am ~ hp + wt,  data=mtcars, family=binomial(link = "probit"))
summary(am.glm.probit)
plot(am.glm.probit$fitted.values)



