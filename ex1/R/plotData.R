plotData <- function(x, y) {
  plot(y ~ x, 
       main = 'Food Truck Business',
       xlab = 'Population of the city in 10,000s',
       ylab = 'Profit in 10,000s',
       pch  = 20,
       col  = 'red')
}