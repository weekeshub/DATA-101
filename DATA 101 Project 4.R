library(tidyverse)

X <- scan()
X

mean2 <- function(X) {
  average <- ((sum(X)) / (length(X)))
  print(paste('The mean is equal to', average))
}
mean2(X)

std_dev <- function(X){
  for(i in seq_along(X)){
    Y[i] <- ((X[i] - ((sum(X)) / (length(X))))^2)
  }
  sum(Y)
  Z <- sqrt(sum(Y) / (length(X)-1))
  print(paste('The standard deviation is equal to', Z))
}
std_dev(X)

sample_size <- function(X){
  size <- length(X)
  print(paste('The sample size is equal to', size))
}
sample_size(X)

complete <- function(X){
  mean2(X)
  std_dev(X)
  sample_size(X)
}
complete(X)
