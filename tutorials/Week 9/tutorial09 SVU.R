#######################
# Tutorial 9: Poisson #
#######################

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
theme_set(theme_minimal())
lapply(c("tidyverse", "ggplot2", "patchwork"),  pkgTest)

# set wd for current folder
setwd(dirname(rstudioapi::getActiveDocumentContext()$path))

# Data: Research Productivity of Doctoral Students in Bio-chemistry (Long 1990) 
# Productivity of doctoral students in biochemistry during the last three yearsof their PhD programmes. 
# The response variables the number of articles published during this period (art)
# Explanatory variables include:
# - gender of the student (fem=1 for women, 0=men)
# - student’s marital status (mar= 1 if married, 0 otherwise)
# - student’s number of children five years old or younger (kid5); 
# - a rating of the prestige of the student’sPhD department (phd); 
# - number of articles published by the student’s mentor during the three-yearperiod (ment)

# Make sure your data are in the correct format.

# (a) Examine the distribution of the response variable. 
# Does least-squares linear regression appear a promising strategy for these data?
# Do we meet the OLS assumptions?

# (b) Perform a Poisson regression of number of articles published on the explanatory variables. 
# Do we meet assumptions for Poisson?
# What conclusions would you draw from this analysis (i.e. interpret your estimated coefficients)?

# What is the predicted number of articles for a married male PhD researcher with 1 child at 2-rated institute whose PhD supervisor published 5 articles?
# Plot predictions vs count.
# Calculate pseudo R squared.
# Calculate RMSE.
# Should we add an interaction for gender with our covariates?

# (c) Consider the possibility of over-dispersion, either by fitting an over-dispersed Poisson model. 
# Is there evidence for over-dispersion? How, if at all, do the results change when over-dispersion is taken into account?

long_data <- read.table("http://statmath.wu.ac.at/courses/StatsWithR/Long.txt", header=T)

str(long_data)

ggplot(long_data, aes(x = art)) +
  geom_histogram()

mean(long_data$art)
var(long_data$art)

coefficients(model_pois)
mentor <- ggplot(long_data, aes(x = ment, y = art)) +
  geom_jitter(alpha = 0.2, color = "darkgreen")

mentor

kids <- ggplot(long_data, aes(x = kid5, y = art)) +
  geom_jitter(alpha = 0.2, color = "steelblue")

mentor + kids /prestige

prestige <- ggplot(long_data, aes(x = phd, y = art)) +
  geom_jitter(alpha = 0.2, color = "pink2")

model_lm <- lm(art ~ ., data = long_data)

summary(model_lm)

model_pois <- glm(art ~ ., data = long_data, family = poisson)

summary(model_pois)

model_pois$null.deviance

model_int <- glm(art ~ fem* (ment + phd + mar + kid5), data = long_data, family = poisson)

summary(model_int)


anova(model_pois, model_int)
library(AER)


dispersiontest(model_pois)
# data is overdispersed

library(pscl)

model_zip <- zeroinfl(art ~ ., data = long_data, dist = "poisson")
summary(model_zip)

# first of the two is typical Poisson on non-zero results
# second is probability that 0 vs 1 for existent or not existent results

dispersiontest(model_zip)


AIC(model_int, model_zip)
