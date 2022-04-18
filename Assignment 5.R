d1x <- c(10.0, 8.0, 13.0, 9.0, 11.0, 14.0, 6.0, 4.0, 12.0, 7.0, 5.0)
d1y <- c(8.04, 6.95, 7.58, 8.81, 8.33, 9.96, 7.24, 4.26, 10.84, 4.82, 5.68)
d1 <- cbind(d1x, d1y)
colnames(d1) <- c("x", "y")
d1 <- data.frame(d1)


d2x <- d1x
d2y <- c(9.14, 8.14, 8.74, 8.77, 9.26, 8.10, 6.13, 3.10, 9.13, 7.26, 4.74)
d2 <- cbind(d2x, d2y)
colnames(d2) <- c("x", "y")
d2 <- data.frame(d2)


d3x <- d1x
d3y <- c(7.46, 6.77, 12.74, 7.11, 7.81, 8.84, 6.08, 5.39, 8.15, 6.42, 5.73)
d3 <- cbind(d3x, d3y)
colnames(d3) <- c("x", "y")
d3 <- data.frame(d3)

d4x <- c(8.0, 8.0, 8.0, 8.0, 8.0, 8.0, 8.0, 19.0, 8.0, 8.0, 8.0)
d4y <- c(6.58, 5.76, 7.71, 8.84, 8.47, 7.04, 5.25, 12.50, 5.56, 7.91, 6.89)
d4 <- cbind(d4x, d4y)
colnames(d4) <- c("x", "y")
d4 <- data.frame(d4)

sapply(d1, mean)
sapply(d1, var)
cor(d1)
d1xy <- lm(y~x, d1)
d1xy
plot(d1, xlab = "x", ylab = "y", main = "Data I")
abline(d1xy, col = 10)

sapply(d2, mean)
sapply(d2, var)
cor(d2)
d2xy <- lm(y~x, d2)
d2xy
plot(d2, xlab = "x", ylab = "y", main = "Data II")
abline(d2xy, col = 12)

sapply(d3, mean)
sapply(d3, var)
cor(d3)
d3xy <- lm(y~x, d3)
summary(d3xy)
plot(d3, xlab = "x", ylab = "y", main = "Data III")
abline(d3xy, col = 14)

sapply(d4, mean)
sapply(d4, var)
cor(d4)
d4xy <- lm(y~x, d4)
summary(d4xy)
plot(d4, xlab = "x", ylab = "y", main = "Data IV")
abline(d4xy, col = 11)

## Evaluate and compare models

plot(d1xy, main = "Data I")
plot(d2xy, main = "Data II")
plot(d3xy, main = "Data III")
plot(d4xy, main = "Data IV")

d1_ypred <- predict(d1xy, d1)
mse(d1_ypred, d1$y)
mae(d1_ypred, d1$y)

d2_ypred <- predict(d2xy, d2)
mse(d2_ypred, d2$y)
mae(d2_ypred, d2$y)

d3_ypred <- predict(d3xy, d3)
mse(d3_ypred, d3$y)
mae(d3_ypred, d3$y)


