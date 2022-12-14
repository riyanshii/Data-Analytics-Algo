weight.kg <- c(56,42,72,36,63,47,55,49,38,42,68,60)
height.cm <- c(147,125,180,118,149,128,150,145,115,140,152,155)
df<-data.frame(weight.kg, height.cm)
relation <- lm(weight.kg~height.cm)
print(relation)
print(summary(relation))
plot(weight.kg~height.cm,col="blue",main="Height v/s Weight",pch=16,
xlab = "Weight(kg)",ylab = "Height(cm)")
abline(relation, col="red", lwd=3)
library(datasets)
relation.multiple = lm(Ozone~Temp+Solar.R+Wind , data = airquality)
print(relation.multiple)
summary(relation.multiple)
cat("Coefficients of multiple regression equation:\n\n")
intercept.multiple <- coef(relation.multiple)[1]
coeff.Temp <- coef(relation.multiple)[2]
coeff.Solar<- coef(relation.multiple)[3]
coeff.Wind<- coef(relation.multiple)[4]
print(intercept.multiple)
print(coeff.Solar)
print(coeff.Temp)
print(coeff.Wind)
xvalues <- c(1.6,2.1,2,2.23,3.71,3.25,3.4,3.86,1.19,2.21)
yvalues <- c(5.19,7.43,6.94,8.11,18.75,14.88,16.06,19.12,3.21,7.58)
model <- nls(yvalues ~ b1*xvalues^2+b2,start = list(b1 = 1,b2 = 1))
model
print(sum(resid(model)^2))
print(confint(model))
plot(xvalues, yvalues, col="red")
# Plot the chart with new data by fitting it to a prediction from 100 data points.
new.data <- data.frame(xvalues = seq(min(xvalues),max(xvalues),len = 100))
lines(new.data$xvalues,predict(model,newdata = new.data))
dff<-airquality
for (i in 1:nrow(dff)){
if(is.na(dff[i,"Ozone"])){
dff[i,"Ozone"]<- mean(dff[which(dff[,"Month"]==dff[i,"Month"]),"Ozone"],na.rm = TRUE)

}
if(is.na(dff[i,"Solar.R"])){
dff[i,"Solar.R"]<- mean(dff[which(dff[,"Month"]==airquality[i,"Month"]),"Solar.R"],na.rm = TRUE)
}
}
modelnew <- lm(dff$Ozone ~ dff$Wind + I(dff$Wind^2))
plot(dff$Ozone~dff$Wind)
lines(smooth.spline(dff$Wind, predict(modelnew)), col="blue", lwd=3)
dff<-airquality
for (i in 1:nrow(dff)){
if(is.na(dff[i,"Ozone"])){
dff[i,"Ozone"]<- mean(dff[which(dff[,"Month"]==dff[i,"Month"]),"Ozone"],na.rm = TRUE)

}
if(is.na(dff[i,"Solar.R"])){
dff[i,"Solar.R"]<- mean(dff[which(dff[,"Month"]==airquality[i,"Month"]),"Solar.R"],na.rm = TRUE)
}
}
modelnew <- lm(dff$Ozone ~ dff$Wind + I(dff$Wind^2))
plot(dff$Ozone~dff$Wind)
lines(smooth.spline(dff$Wind, predict(modelnew)), col="blue", lwd=3)
library(ggplot2)
dff.clean.fac<-dff
dff.clean.fac$Day<-as.factor(dff$Day)
dff.clean.fac$Month<-as.factor(dff$Month)
plot3<-ggplot(dff.clean.fac, aes(x=Temp, y=Ozone, color=Month)) + geom_point()
plot3<-plot3+ stat_smooth(method = "lm", se=FALSE, formula = (y ~ exp(x)), size = 1)
print(plot3 + labs(y="Mean Ozone Concentration(ppb)", x="Temperature(F)")
+ ggtitle("Mean ozone Concentration v/s Temperature"))
x <- c(56,42,72,36,63,47,55,49,38,42,68,60)
y <- c(147,125,180,118,149,128,150,145,115,140,152,155)
fit<-lm(log(y)~log(x))
power.model <- nls(y ~ a* (x^b), start = list(a =exp(coef(fit)[1]), b = coef(fit)[2]))
print(power.model)
summary(power.model)
plot(x,y,col="blue", main = "Height v/s Weight",cex = 1.3,
pch = 16,xlab = "Weight in kg",ylab = "Height in cm")
lines.default(smooth.spline(x, predict(power.model)),col='red', lwd=3)
curve(18.9863*(x^0.5099), 0, 1000)

install.packages("glmnet")
library(glmnet)
weight.kg <- c(56,42,72,36,63,47,55,49,38,42,68,60)
height.cm <- c(147,125,180,118,149,128,150,145,115,140,152,155)
df<-data.frame(weight.kg, height.cm)
ridge.mod<-glmnet(df,df$height.cm,alpha=0,lambda=1)
summary(ridge.mod)
predict(ridge.mod,s=0,type='coefficients')
lasso.mod<-glmnet(df,df$height.cm,alpha=1,lambda=1)
summary(lasso.mod)
predict(lasso.mod,s=0,type='coefficients')
slope <- function(x, y){
 mean_x <- mean(x)
 mean_y <- mean(y)
 nom <- sum((x - mean_x)*(y-mean_y))
 denom <- sum((x - mean_x)^2)
 m <- nom / denom
 return(m)
}
slope2 <- function(x, y){
 return(cov(x, y)/var(x))
}
intercept <- function(x, y, m){
 b <- mean(y) - (m * mean(x))
 return(b)
}
my_slope <- slope(mtcars$wt, mtcars$mpg)
my_intercept <- intercept(mtcars$wt,mtcars$mpg, my_slope)
my_slope
my_intercept
plot(mtcars$wt, mtcars$mpg, pch=19)
abline(my_intercept, my_slope)
relation <- lm(mtcars$wt~mtcars$mpg)
print(relation)
plot(mtcars$wt~mtcars$mpg,col="blue",main="Wt v/s MPG",pch=16,
xlab = "Wt",ylab = "MPG")
abline(relation, col="red", lwd=1)