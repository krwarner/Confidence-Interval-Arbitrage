
# Upload data
#setwd("~/Desktop/Data")
diamonds         <- read.csv("diamonds.csv", as.is = T)
diamonds$cut     <- factor(diamonds$cut)
diamonds$color   <- factor(diamonds$color)
diamonds$clarity <- factor(diamonds$clarity)

# Plot full data set
plot(log(diamonds$carat), log(diamonds$price),col="grey")

# For the linear model:
# Price = Carat+cut+Carat*cut+error
model <- lm(log(price)~log(carat)+cut+log(carat)*cut,data=diamonds)
summary(model)

# Predict the response values (Price) for different carat values and 
# only the cut level ideal. 
# Here we also produce 95% prediction intervals (PI) for the prices
carat.values <- seq(min(diamonds$carat),max(diamonds$carat),by=.01)
ideal <- factor(rep("Ideal",length(carat.values)))
predict.interval <- predict(model,newdata = data.frame(carat=carat.values,cut=ideal),interval = c("prediction"))

# look at some predicitons and intervals
head(predict.interval)

# Plot Price~carat with the estimated model and PI bands
plot(log(diamonds$carat), log(diamonds$price),col="grey",main="Kenny Plot")
lines(log(carat.values),predict.interval[,1])
lines(log(carat.values),predict.interval[,2],col="purple")
lines(log(carat.values),predict.interval[,3],col="purple")
legend("bottomright",legend=c("Mean","Lower PI","Upper PI"),lty=c(1,1,1),col=c("black","purple","purple"),cex=.75)


