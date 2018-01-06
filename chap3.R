library(car)
library(gvlma)

# 回归的理论及代码实现
# 参考R in Action: http://rpubs.com/shenxiangzhuang/regression

# 课后题
# T2
# fit
x <- c(700, 800, 900, 1000, 1100, 1200, 1260, 1340)
y <- c(9, 9.6, 10.2, 11.6, 12.4, 13.0, 13.8, 14.6)
lm.result <- lm(y~x)
summary(lm.result)
# Global validation of linear model assumption
gvmmodel <- gvlma(lm.result)
summary(gvmmodel)

# preditc
new.x <- c(1400)
predict(lm.result, data.frame(x=new.x), level=0.95, interval="confidence")  #14.99939 


#T3
# fit
adfee <- c(4100, 5400, 6300, 5400, 4800, 4600, 6200, 6100, 6400, 7100)
sales <- c(12.5, 13.8, 14.25, 14.25, 14.5, 13.0, 14, 15, 15.75, 16.5)
lm.result <- lm(sales~adfee)
summary(lm.result)
print(paste("相关系数：", as.character(cor(adfee, sales))))
# D-W test
durbinWatsonTest(lm.result)

# Global validation of linear model assumption
gvmmodel <- gvlma(lm.result)
summary(gvmmodel)

# predict
new.adfee <- c(6700)
predict(lm.result, data.frame(adfee=new.adfee), level=0.95, interval="confidence")


#T7
# fit
x <- c(150, 140, 160, 170, 150, 162, 185, 165, 190, 185)
y <- c(40, 42, 48, 55, 65, 79, 88, 100, 120, 140)
lm.result <- lm(y~x)
summary(lm.result)
print(paste("相关系数：", as.character(cor(x, y))))

durbinWatsonTest(lm.result)
# Global validation of linear model assumption
gvmmodel <- gvlma(lm.result)
summary(gvmmodel)



#T8
# fit
like_prec <- c(61.6, 53.2, 65.5, 64.9, 72.7, 52.2, 50.2, 44, 53.8, 53.8)
year_income <- c(6, 4.4, 9.1, 8.1, 9.7, 4.8, 7.6, 4.4, 9.1, 6.7)
edu_index <- c(6.3, 5.5, 3.6, 5.8, 6.8, 7.9, 4.2, 6.0, 2.8, 6.7)
lm.result <- lm(like_prec~year_income+edu_index)
summary(lm.result)

# D-W test
durbinWatsonTest(lm.result)
# Global validation of linear model assumption
gvmmodel <- gvlma(lm.result)
summary(gvmmodel)

new.income <- 5
new.edu <- 6
predict(lm.result, data.frame(year_income=new.income, edu_index=new.edu),
        level=0.95, interval="confidence")




