####### JUNE-AUGUST STARTS HERE FOR 2013-2016 #######
data2 = read.csv("MA206ProjectDataSummerMonths.csv")
summary(data2)
names(data2)
model2 <- lm(data2$pm2.5 ~ data2$temp + data2$dewp + data2$pres + data2$wspm)
summary(model2)

####### PLOTS START HERE #######
# pm2.5 -> temp + dewp
with (data2,
      (splot <- scatterplot3d(pm2.5 ~ temp + dewp, type = "h", pch = 19)))
scatter3d(pm2.5 ~ temp + dewp, data = data2)

s3d <-scatterplot3d(data2$temp,data2$dewp,data2$pm2.5, pch=16, highlight.3d=TRUE,
                    type="h", main="JUNE - AUGUST")
fit <- lm(data2$pm2.5 ~ data2$temp+data2$dewp)
s3d$plane3d(fit)

# pm2.5 -> pres + wspm
with (data2,
      (splot <- scatterplot3d(pm2.5 ~ pres + wspm, type = "h", pch = 19)))
scatter3d(pm2.5 ~ pres + wspm, data = data2)

s3d <-scatterplot3d(data2$pres,data2$wspm,data2$pm2.5, pch=16, highlight.3d=TRUE,
                    type="h", main="JUNE - AUGUST")
fit <- lm(data2$pm2.5 ~ data2$pres+data2$wspm)
s3d$plane3d(fit)
