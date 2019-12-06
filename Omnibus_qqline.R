## Simple Linear Regression

data1 = read.csv("MA206ProjectDataSpringMonths.csv")     # names(dat)
# View(dat)

#plot(AFQT ~ Math, data = dat, main = 'AFQT vs Math')
model1 <- lm(data1$pm2.5 ~ data1$temp + data1$dewp + data1$pres + data1$wspm)
output1 = summary(model1)
output1

# Standardized Residual vs Predicted Value (omnibus)
standard.resid = rstandard(model1)    
yhat = fitted(model1)                    

plot(x = yhat, y = standard.resid,
     xlab = 'Predicted Pm2.5', ylab = 'Standardized Residuals',
     main = 'Standardized Residuals vs Predicted PM2.5')
abline(h=c(-2,0,2), lty = 3)

# Standardized residual vs x plots
plot(x = data1$pm2.5, y = standard.resid,
     xlab = 'PM2.5', ylab = 'Standardized Residuals',
     main = 'Standardized Residuals vs PM2.5')
abline(h=c(-2,0,2), lty = 3)

# Potential outliers
pm2.5boxplot = boxplot(standard.resid)
outliers = pm2.5boxplot$out
outliers

# Observed vs Predicted 
plot(x = data1$pm2.5, y = yhat,
     xlab = 'Observed PM2.5', ylab = 'Predicted PM2.5',
     main = 'Predicted PM2.5 vs Observed PM2.5')
abline(a = 0, b = 1, lty = 6)

# Assess normality of the standardized residuals
qqnorm(standard.resid)
qqline(standard.resid)



###END