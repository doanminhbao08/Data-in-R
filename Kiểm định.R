setwd("C:/Users/Admin/Documents/Bách Khoa/Suy luận thống kê/R")
size <- read.csv("size.csv",header = TRUE)
# Tổng thể =====================
overall <- size[1:10000, c(16)]
overall
hist(overall)
mean(overall) 
var(overall)
sd(overall)

# Lấy mẫu giả thiết ===========
n0 <- 500 # Chọn số lượng mẫu giả thiết
s0 <- sample(overall, n0)
mean(s0) 

# Lấy mẫu nghiên cứu ==========
n <- 100 # Chọn số lượng mẫu nghiên cứu
s <- sample(overall, n)
mean(s) 
# Cho rằng chênh lệch v$ là chấp nhận được
v <- 5

# Sử dụng kiểm định p-giá trị

# Xác suất mắc sai lầm loại 1: =====
# H0: M = mean(s0)
# H1: M != mean(s0) 
a <- 2*(1-pnorm(v/sd(overall)*sqrt(n)))
a
# Xác suất mắc sai lầm loại 2: =====
# H0: M = mean(s0)
# H1: M = mean(overall)
b <- pnorm((mean(s0)+v-mean(overall))/sd(overall)*sqrt(n)) - pnorm((mean(s0)-v-mean(overall))/sd(overall)*sqrt(n))
b

# Hình vẽ cho xác suất mắc sai lầm loại 1
x <- seq(-6,6,length=1000) + mean(s0)
y <- dnorm(x,mean=mean(s0),sd=sd(overall)/sqrt(n))
plot(x, y, type="n", xlab="", ylab="", axes=1)
axis(col='red',line = 2,1, at = c(mean(s0)-v,mean(s0),mean(s0)+v), labels = c(mean(s0)-v, mean(s0),mean(s0)+v))
abline(v=c(mean(s0)-v,mean(s0),mean(s0)+v), col='red', lty=2)
lines(x, y, lwd = 2, col='blue')
arrows(x0=mean(s0)-v, y0=0.1, x1=mean(s0)+v, y1=0.1, code=3)
text(x=c(mean(s0),mean(s0)+0.5), y=0.11,labels=c(round((1-a)*100,digits=2),"%"), col='blue')

# Hình vẽ cho xác suất mắc sai lầm loại 2
x <- seq(-6,6,length=1000) + mean(s0)
y1 <- dnorm(x,mean=mean(s0),sd=sd(overall)/sqrt(n))
plot(x, y1, type="n", xlab="", ylab="", axes=1)
axis(col='red',line = 2,1, at = c(mean(s0)-v,mean(s0),mean(s0)+v), labels = c(mean(s0)-v, round(mean(s0),digits=1),mean(s0)+v))
abline(v=c(mean(s0)-v,mean(s0),mean(s0)+v), col='red',lty=2)
lines(x, y1, lwd = 2, col='blue')
par(new=TRUE)
x <- seq(-6,6,length=1000) + mean(s0)
y2 <- dnorm(x,mean=mean(overall),sd=sd(overall)/sqrt(n))
axis(col='orange',line = 2,1, at = mean(overall), labels = round(mean(overall),digits=1))
abline(v=mean(overall), col='orange',lty=2)
lines(x, y2, lwd = 2, col='blue',lty=2)


qnorm(0.9)
