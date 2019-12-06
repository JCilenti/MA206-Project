####### MARCH-MAY STARTS HERE FOR 2013-2016 #######
data1 = read.csv("MA206ProjectDataSpringMonths.csv")
summary(data1)
head(data1)
names(data1)
model1 <- lm(data1$pm2.5 ~ data1$temp + data1$dewp + data1$pres + data1$wspm)
summary(model1)

####### PLOTS START HERE #######
# pm2.5 -> temp + dewp
with (data1,
      (splot <- scatterplot3d(pm2.5 ~ temp + dewp, type = "h", pch = 19)))
scatter3d(pm2.5 ~ temp + dewp, data = data1)

s3d <-scatterplot3d(data1$temp,data1$dewp,data1$pm2.5, pch=16, highlight.3d=TRUE,
                    type="h", main="MARCH - MAY")
fit <- lm(data1$pm2.5 ~ data1$temp+data1$dewp)
s3d$plane3d(fit)

# pm2.5 -> pres + wspm
with (data1,
      (splot <- scatterplot3d(pm2.5 ~ pres + wspm, type = "h", pch = 19)))
scatter3d(pm2.5 ~ pres + wspm, data = data1)

s3d <-scatterplot3d(data1$pres,data1$wspm,data1$pm2.5, pch=16, highlight.3d=TRUE,
                    type="h", main="MARCH - MAY")
fit <- lm(data1$pm2.5 ~ data1$pres+data1$wspm)
s3d$plane3d(fit)

