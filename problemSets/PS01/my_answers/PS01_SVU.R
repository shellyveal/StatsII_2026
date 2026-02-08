#####################
# load libraries
# set wd
# clear global .envir
#####################

# remove objects
rm(list=ls())
# detach all libraries
detachAllPackages <- function() {
  basic.packages <- c("package:stats", "package:graphics", "package:grDevices", "package:utils", "package:datasets", "package:methods", "package:base")
  package.list <- search()[ifelse(unlist(gregexpr("package:", search()))==1, TRUE, FALSE)]
  package.list <- setdiff(package.list, basic.packages)
  if (length(package.list)>0)  for (package in package.list) detach(package,  character.only=TRUE)
}
detachAllPackages()

# load libraries
pkgTest <- function(pkg){
  new.pkg <- pkg[!(pkg %in% installed.packages()[,  "Package"])]
  if (length(new.pkg)) 
    install.packages(new.pkg,  dependencies = TRUE)
  sapply(pkg,  require,  character.only = TRUE)
}

# here is where you load any necessary packages
# ex: stringr
# lapply(c("stringr"),  pkgTest)

lapply(c("tidyverse", "dplyr"),  pkgTest)
theme_set(theme_minimal())
# set wd for current folder
setwd(dirname(rstudioapi::getActiveDocumentContext()$path))

#####################
# Problem 1
#####################

set.seed(123)
# create empirical distribution of observed data
data_1 <- rcauchy(1000, location = 0, scale = 1)
ECDF <- ecdf(data_1)
empiricalCDF <- ECDF(data_1)
# generate test statistic
D <- max(abs(empiricalCDF - pnorm(data_1)))

df <- data.frame(
  x = data_1,
  y = empiricalCDF
)

ECDF_plot <- ggplot(df, aes(x = x, y =y)) + # Visualizing
  geom_point() +
  labs(title = "ECDF Data") +
  theme(plot.title = element_text(hjust = 0.5))

ggsave("ECDF.pdf", ECDF_plot, width = 8, height = 5, units = "in")

KS_test <- function(F_Fun, x) {
  
  if (identical(F_Fun, ecdf)) {
    x <- sort(x)
    n <- length(x)
    i <- seq_along(x)
    F_inter <- F_Fun(x)
    FVals <-  F_inter(x)
    }
  else {
    n <- length(x)
    i <- seq_along(x)
    FVals <- F_Fun(x)
  }
  
  D <- max(abs(FVals - pnorm(x)))
  p_val <- (sqrt(2*pi)/D)*sum(exp(-(2*i-1)^2*pi^2)/(8*(D^2)))

  return(list(
    D = D,
    p_value = p_val))
}

KS_test(F_Fun = ecdf, x = data_1)
ks.test(data_1, "pnorm")

# idea for function to be useful in multiple contexts;
# using pexp for example

pexp_y <- pexp(data_1)

df_pexp <- data.frame(
  x = data_1,
  y = pexp_y
)

exp_plot <- ggplot(df_pexp, aes(x = x, y =y)) + # Visualizing
  geom_point() +
  labs(title = "EXP Data") +
  theme(plot.title = element_text(hjust = 0.5))

KS_test(F_Fun = pexp, x = data_1)
ks.test(data_1, pexp)

ggsave("Exp.pdf", exp_plot, width = 8, height = 5, units = "in")

#####################
# Problem 2
#####################

set.seed (123)
data_2 <- data.frame(x = runif(200, 1, 10))
data_2$y <- 0 + 2.75*data_2$x + rnorm(200, 0, 1.5)

squared_errors <- function(outcome, input, parameter) {
  input_mat <- as.matrix(input)
  n <- nrow(input_mat)
  k <- ncol(input_mat)
  beta <- parameter[1:k]
  sum_sq_e <- sum((outcome - input_mat%*%beta)^2)
  return(sum_sq_e)
}

results_OLS <- optim(fn=squared_errors, outcome=data_2$y, 
                     input=cbind(1, data_2$x), par=c(1,1), 
                     hessian=T, method="BFGS")

results_OLS$par
lm(data_2$y ~ data_2$x)
