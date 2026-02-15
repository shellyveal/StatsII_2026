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

lapply(c("tidyverse", "ggplot2", "readxl", "xtable", "stargazer"),  pkgTest)
options(scipen = 000)

# set wd for current folder
setwd(dirname(rstudioapi::getActiveDocumentContext()$path))

#####################
# Problem 1
#####################

# load data
load(url("https://github.com/ASDS-TCD/StatsII_2026/blob/main/datasets/climateSupport.RData?raw=true"))

data <- climateSupport

df <- data %>% mutate(
  sanctions = factor(sanctions, ordered = FALSE),
  countries = factor(countries, ordered = FALSE))

ggplot(df, aes(x = sanctions, y = choice)) +
  geom_point(position = position_jitter(width = 0.13, height = 0.05), alpha = 0.15)

ggplot(df, aes(x = countries, y = choice)) +
  geom_point(position = position_jitter(width = 0.13, height = 0.05), alpha = 0.15)

sanctions_ct <- table(df$choice, df$sanctions)
sanctions_chisq <- chisq.test(sanctions_ct)

countries_ct <- table(df$choice, df$countries)
countries_chisq <- chisq.test(countries_ct)

sanctions_chisq
countries_chisq

prop.table(countries_ct, margin = 2)
prop.table(sanctions_ct, margin = 2)

m_1 <- glm(data = df, choice ~ sanctions + countries, 
    family = binomial)

summary(m_1)

m_null <- glm(data = df, choice ~ 1, family = binomial)
m1_v_mnull <- anova(m_1, m_null, test = "LRT")


m_2 <- glm(data = df, choice ~ sanctions * countries, 
           family = binomial)

summary(m_2)

m1_v_m2 <- anova(m_1, m_2, test = "LRT")


stargazer(m_1, 
          type = "latex",
          title = "Factors Impacting Support for Climate Policy (Additive)",
          covariate.labels = c("5 Percent Sanctions", 
                               "15 Percent Sanctions",
                               "20 Percent Sanctions", 
                               "80 Participating Countries",
                               "160 Participating Countries"),
          dep.var.labels = "Choice")

stargazer(m1_v_mnull, 
          type = "latex",
          title = "ANOVA Comparison of Additive and Null Models")

stargazer(m1_v_m2, 
          type = "latex",
          title = "ANOVA Comparison of Additive and Interactive Models")







