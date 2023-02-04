# Anova
group <- c(1,1,1,1,1,1,1,2,2,2,2,2,2,2,3,3,3,3,3,3,3,3)
group <- as.factor(group)
qua <- c(6.3,7.0, 6.5, 6.6,7.2,6.9, 6.4, 7.2, 6.6, 6.1, 5.8, 6.8, 7.1, 5.9, 6.3,5.8, 6.0, 5.5, 5.2, 6.5, 5.3, 6.2)
#data <- data.frame(group, qua)
#attach(data)
#analysis <- lm(qua ~ group)
#anova(analysis)

# Đọc dữ liệu =========================================
data <- read.csv("mark.csv",header=TRUE)
x <- data[,c(1)]
mean(x)
y <- data[,c(2)]
mean(y)
n <- length(x)



# Tìm hệ số tương quan =======
# C1
syy <- var(y)*(n-1)
sxx <- var(x)*(n-1)
sxy <- cov(x,y)*(n-1)
sxy/sqrt(sxx*syy)
# C2
r <- cov(x,y)/(sd(x)*sd(y))
r
# C3 dùng hàm của máy
cor.test(x,y) 

# Kiểm định hệ số tương quan mức ý nghĩa a =========
a <- 0.05
t0 = r*sqrt(l-2)/sqrt(1-r^2)
cat(t0)

t2 <- -qt(a/2,l-2)
t <- -qt(a,l-2)
# two.sided
cat("(-inf;",-t2,")U(",t2,";inf)")

# greater
cat("(",t,";inf)")

# less
cat("(-inf;",-t,")")

# PT đường phù hợp ========================
# C1
b <- r*(sd(y)/sd(x))
a <- mean(y) - b*mean(x)
b
a

# PT đường HQTT đơn biến ==========
# C1
b1 <- sxy/sxx
b0 <- mean(y) - b1*mean(x)
b1
b0
# C2
lm(y ~ x)
# Thông tin phần dư ================
sst <- syy
ssr <- b1*sxy
sse <- sst - ssr
sse
# Sai số tiêu chuẩn hàm hồi quy
s <- sqrt(sse/(n-2))
s
# Hệ số xác định
ssr/sst
# Khoảng tin cậy hệ số hồi quy mức ý nghĩa a
cat(b1-t2*sqrt(v/sxx),"<= B1 <=",b1+t2*sqrt(v/sxx))
cat(b0-t2*sqrt(v*(1/n+mean(x)^2/sxx)),"<= B0 <=",b0+t2*sqrt(v*(1/n+mean(x)^2/sxx)))

# Vẽ đám mây điểm
reg <- lm(y ~ x)
plot(y ~ x, pch =20)
abline(reg)
# Dùng hàm của máy
summary(reg)




