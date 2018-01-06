# 时间序列的平滑预测法
# Chap4，Chap5, Chap7(chap6 为自适应过滤法)大都是关于ARMA的，可以参考另外一个repository
# https://github.com/shenxiangzhuang/TimeSeriesAnalysis


# 课后题
# 移动平均法[E5.1]
library(TTR)
data5.1 <- c(46, 50, 59, 57, 55, 64, 55, 61, 45, 49, 46)
x <- ts(data5.1)
x.ma <- SMA(x, n=3)
plot(x, type='o')
lines(x.ma, col=2, lwd=2)

# 指数平滑法

# 一次指数平滑
library(forecast)
data <- c(97,95,95,92,95,95,98,97,99,95,95,96,97,98,94,95)
x <- ts(data)
x.es <- ses(x, alpha = 0.1, h=1, initial = 'simple')
# MSE,第一个是保留原始的上期数据，不计算
MSE <- mean(x.es$residuals[-1]^2)


# E5.2线性二次移动平均法
library(TTR)
data5.2 <- c(125,135,195,197.5,186,175,155,190,220,219,226,198,260,245)
x <- ts(data5.2)

LQMA <- function(x, N, dT, Tn){
  # x: time series data
  # N: Number of periods to average over
  # dT: T 由目前周期t到预测周期的周期间隔个数
  # Tn: 一共要往后预测的期数（比如月度数据Tn=3，就是预测未来三个月的值）
  
  S1 <- SMA(x, n=N)
  S2 <- SMA(S1, n=N)
  At <- 2*S1 - S2
  Bt <- 2*(S1-S2)/(N-1)
  Ft <- c()
  
  # 拟合
  last_index <- length(At)
  for( i in 1:(last_index-1)){
    if(!is.na(At[i])){
      Ft[i+1] <- At[i]+Bt[i]*dT
    }
  }
  
  # 预测
  for(dT in 1:Tn){
    Ft[last_index+dT] <- At[last_index]+Bt[last_index]*dT
  }
  return(Ft)
}

# 例题测试
LQMA(x, N=3, dT=1, Tn=2)


# 线性二次指数平滑法

data5.3 <- c(125,135,195,197.5,186,175,155,
             190,220,219,226,198,260,245)
x <- ts(data5.3)
x.fit <- holt(x,initial = "simple", alpha=0.5, gamma=0.8)  #给定了alpha,gamma
x.fore <- forecast(x.fit, h=2)

# 二次曲线指数平滑法

# 这里按照课本的格式ses不太好用，就参考下面这种方法来手动实现简单指数平滑
exp_smooth = function(x, alpha) {
  # Performs exponential smoothing by taking a weighted average of the
  # current and previous data points
  #
  # x     : numeric vector
  # alpha : number between 0 and 1, weight assigned to current data value
  #
  # Returns a numeric vector of the same length as x and values as the 
  #       weighted averages of the (current, previous) consecutive pairs
  s = numeric(length(x) + 1) # make s 1 cell longer than x
  for (i in seq_along(s)) {
    if (i == 1) { # set the initial value of s the same as that of x
      s[i] = x[i]
    } else {
      # weight current value with alpha and previous value 
      # with 1-alpha, and sum
      s[i] = alpha * x[i - 1] + (1 - alpha) * s[i - 1]
    }
  }
  s[-1] # drop the 1st element in s because it's extra
}

data5.5 <- c(12.9, 14.91, 15.96, 14.41, 14.57, 14.60, 
             15.35, 15.84, 16.90, 18.26, 17.40, 18.71,
             19.53, 20.82, 22.87, 24.59, 25.93, 28.04, 
             29.45, 31.47, 33.99, 39.56, 48.08, 53.67)

x <- ts(data5.5, start=1990)

Alpha = 0.5
m = 1

S1 <- exp_smooth(x, alpha=Alpha)
S2 <- exp_smooth(S1, alpha = Alpha)
S3 <- exp_smooth(S2, alpha = Alpha)

At <- 3*S1-3*S2+S3
At <- At[-1]  # 去除第一个数据
Bt <- ((6-5*Alpha)*S1 - (10-8*Alpha)*S2 + 
         (4-3*Alpha)*S3)*(Alpha^2)/((1-Alpha)^2)
Bt <- Bt[-1]
Ct <- (S1-2*S2+S3)*(Alpha^2)/((1-Alpha)^2)
Ct <- Ct[-1]

Ft <- c()
Tn <- 1
# 拟合
last_index <- length(At)
for( i in 1:(last_index)){
    Ft[i+2] <- At[i]+Bt[i]*m+Ct[i]*m*m/2
}

# 预测
for(m in 1:Tn){
  Ft[last_index+1+m] <- At[last_index]+Bt[last_index]*m+Ct[last_index]*m*m/2
}


# 温特线性与季节指数平滑法
data5.6 <- c(362, 385, 432, 341, 382, 409, 498, 387,
             473, 513, 582, 474, 544, 582, 681, 557,
             628, 707, 773, 592, 627, 725, 854, 661)

x <- ts(data5.6, frequency = 4, start = c(2009,1))
x.fit <- hw(x,initial = "simple", 
            alpha=0.2, gamma=0.1, beta = 0.05  #给定了alpha,gamma,beta
            ,seasonal = "multiplicative")  
x.fore <- forecast(x.fit, h=4)



# 习题

# T7[一次移动平均法]
data7 <- c(430, 380, 330, 410, 440, 390, 380, 400, 450, 420, 390)
x <- ts(data7)
x.ma <- SMA(x, n=4)

# T8[一次指数平滑法]
x.es <- ses(x, alpha = 0.2, h=1, initial = 'simple')
# MSE,第一个是保留原始的上期数据，不计算
MSE <- mean(x.es$residuals[-1]^2)


# T9
data9 <- c(140, 159, 136, 157, 173, 131, 177, 188, 154)
x <- ts(data9)
LQMA(x, N=5, dT=1, Tn=3)

# T14
exp_smooth = function(x, alpha) {
  # Performs exponential smoothing by taking a weighted average of the
  # current and previous data points
  #
  # x     : numeric vector
  # alpha : number between 0 and 1, weight assigned to current data value
  #
  # Returns a numeric vector of the same length as x and values as the 
  #       weighted averages of the (current, previous) consecutive pairs
  s = numeric(length(x) + 1) # make s 1 cell longer than x
  for (i in seq_along(s)) {
    if (i == 1) { # set the initial value of s the same as that of x
      s[i] = x[i]
    } else {
      # weight current value with alpha and previous value 
      # with 1-alpha, and sum
      s[i] = alpha * x[i - 1] + (1 - alpha) * s[i - 1]
    }
  }
  s[-1] # drop the 1st element in s because it's extra
}

data14 <- c(19.98, 29.56, 20.96, 12.94, 31.95, 36.16, 
            43.76, 56.86, 75.06, 82.12, 96.04, 99.93,
            115.5, 124.3, 119.29, 138.13)
x <- ts(data14, start = 1999)

Alpha = 0.5
m = 1

S1 <- exp_smooth(x, alpha=Alpha)
S2 <- exp_smooth(S1, alpha = Alpha)
S3 <- exp_smooth(S2, alpha = Alpha)

At <- 3*S1-3*S2+S3
At <- At[-1]  # 去除第一个数据
Bt <- ((6-5*Alpha)*S1 - (10-8*Alpha)*S2 + 
         (4-3*Alpha)*S3)*(Alpha^2)/((1-Alpha)^2)
Bt <- Bt[-1]
Ct <- (S1-2*S2+S3)*(Alpha^2)/((1-Alpha)^2)
Ct <- Ct[-1]

Ft <- c()
Tn <- 1
# 拟合
last_index <- length(At)
for( i in 1:(last_index)){
  Ft[i+2] <- At[i]+Bt[i]*m+Ct[i]*m*m/2
}

# 预测
for(m in 1:Tn){
  Ft[last_index+1+m] <- At[last_index]+Bt[last_index]*m+Ct[last_index]*m*m/2
}


