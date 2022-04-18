library(readr)

library(readxl)

## Time Series Models

CitiBike_Oct2016_BikeDemand <- read_excel("~/IE575_Online/CitiBike_Oct2016_BikeDemand.xlsx")

citibike_firsttwo <- CitiBike_Oct2016_BikeDemand[1:3]



# YOU HAVE TO MODIFY THE ABOVE MOVAVG FUNCTION
# m is how many future data points you want to predict.
movavg <- function(x, n, m, type=c("s", "t", "w", "m", "e", "r")) {
  stopifnot(is.numeric(x), is.numeric(n), is.character(type))
  if (length(n) != 1 || ceiling(n != floor(n)) || n <= 1)
    stop("Window length 'n' must be a single integer greater 1.")
  nx <- length(x)
  if (n >= nx)
    stop("Window length 'n' must be greater then length of time series.")
  y <- numeric(nx)
  if (type == "s") {         # simple
    for (k in 1:(n-1))  y[k] <- mean(x[1:k])
    for (k in n:nx)     y[k] <- mean(x[(k-n+1):k])
    for (k in (nx+1):(nx+m)) {
      y[k] <- mean(x[(k-n):(k-1)]) 
      x[k] <- y[k]
    }
  } else if (type == "t") {  # triangular
    n <- ceiling((n + 1)/2)
    s <- movavg(x, n, "s")
    y <- movavg(s, n, "s")
  } else if (type == "w") {  # weighted
    for (k in 1:(n-1))  y[k] <- 2 * sum((k:1)*x[k:1]) / (k*(k+1))
    for (k in n:nx)     y[k] <- 2 * sum((n:1)*x[k:(k-n+1)]) / (n*(n+1))
  } else if (type == "m") {  # modified
    y[1] <- x[1]
    for (k in 2:nx)     y[k] <- y[k-1] + (x[k] - y[k-1])/n
  } else if (type == "e") {  # exponential
    a <- 2/(n+1)
    y[1] <- x[1]
    for (k in 2:nx)     y[k] <- a*x[k] + (1-a)*y[k-1]
    for (k in (nx+1):(nx+m)) {
      x[k] <- 0
      y[k] <- a*x[k] + (1-a)*y[k-1]
    }
  } else if (type == "r") {  # running
    a <- 1/n
    y[1] <- x[1]
    for (k in 2:nx)     y[k] <- a*x[k] + (1-a)*y[k-1]
  } else
    stop("The type must be one of 's', 't', 'w', 'm', 'e', or 'r'.")
  return(y)
}

citibike_1 <- citibike_firsttwo$`72`
citibike_2 <- citibike_firsttwo$`79`
plot.ts(citibike_1, main = "Station 72", xlab = "Time", ylab = "Usage")
y_1_30 <- movavg(citibike_1, 30, 3, "s")
lines(y_1_30, col = 2)
y_1_40 <- movavg(citibike_1, 40, 3, "s")
lines(y_1_40, col = 3)
y_1_50 <- movavg(citibike_1, 50, 3, "s")
lines(y_1_50, col = 4)
y_1_e <- movavg(citibike_1, 2, 3, "e")
lines(y_1_e, col = 5)
tail(y_1_30, 3)
tail(y_1_40, 3)
tail(y_1_50, 3)
tail(y_1_e, 3)


plot.ts(citibike_2, main = "Station 79", xlab = "Time", ylab = "Usage")
y_2_30 <- movavg(citibike_2, 30, 3, "s")
lines(y_2_30, col = 2)
y_2_40 <- movavg(citibike_2, 40, 3, "s")
lines(y_2_40, col = 3)
y_2_50 <- movavg(citibike_2, 50, 3, "s")
lines(y_2_50, col = 4)
y_2_e <- movavg(citibike_2, 2, 3, "e")
lines(y_2_e, col = 5)
tail(y_2_30, 3)
tail(y_2_40, 3)
tail(y_2_50, 3)
tail(y_2_e, 3)


library(dplyr)

citibike_agg <- citibike_firsttwo
citibike_agg$time <- substring(citibike_agg$time,0, 10)
citibike_agg$`72`
citibike_agg <- citibike_agg %>%
  group_by(time) %>%
  summarise(station_72 = sum(`72`), station_79 = sum(`79`))

citibike_1 <- citibike_agg$station_72
citibike_2 <- citibike_agg$station_79
plot.ts(citibike_1, main = "Station 72", xlab = "Time", ylab = "Usage")
y_1_30 <- movavg(citibike_1, 5, 3, "s")
lines(y_1_30, col = 2)
y_1_40 <- movavg(citibike_1, 10, 3, "s")
lines(y_1_40, col = 3)
y_1_50 <- movavg(citibike_1, 15, 3, "s")
lines(y_1_50, col = 4)
y_1_e <- movavg(citibike_1, 2, 3, "e")
lines(y_1_e, col = 5)
tail(y_1_30, 3)
tail(y_1_40, 3)
tail(y_1_50, 3)
tail(y_1_e, 3)


plot.ts(citibike_2, main = "Station 79", xlab = "Time", ylab = "Usage")
y_2_30 <- movavg(citibike_2, 5, 3, "s")
lines(y_2_30, col = 2)
y_2_40 <- movavg(citibike_2, 10, 3, "s")
lines(y_2_40, col = 3)
y_2_50 <- movavg(citibike_2, 15, 3, "s")
lines(y_2_50, col = 4)
y_2_e <- movavg(citibike_2, 2, 3, "e")
lines(y_2_e, col = 5)
tail(y_2_30, 3)
tail(y_2_40, 3)
tail(y_2_50, 3)
tail(y_2_e, 3)
