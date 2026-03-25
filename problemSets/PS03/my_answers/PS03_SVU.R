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


lapply(c("nnet", "MASS", "tidyverse", "xtable",
         "stargazer", "AER", "pscl", "ggrepel"),  pkgTest)

# set wd for current folder
setwd(dirname(rstudioapi::getActiveDocumentContext()$path))

#####################
# Problem 1
#####################

# load data
gdp_data <- read.csv("https://raw.githubusercontent.com/ASDS-TCD/StatsII_2026/main/datasets/gdpChange.csv", stringsAsFactors = F)

# Remove NA's and narrow dataset to relevant variables
df <- gdp_data %>% select(
  COUNTRY, CTYNAME, YEAR, GDPW, OIL, REG, GDPWdiff
) %>%
  na.omit()

# Basic visualization
basic_viz <- ggplot(df, aes(x = GDPWdiff)) +
  geom_histogram(binwidth = 10) +
  xlim(-500, 500)

# check proportion of data in potential "no change" category range
prop_nochange_25 <- mean(df$GDPWdiff >= -25 & df$GDPWdiff <= 25)
prop_nochange_25  # approximately 10 percent in this range. 

# Setting the ranges, reference category.
df<- df %>% mutate(
  GDPcat = as.factor(case_when(
    GDPWdiff < -25 ~ "negative change",
    GDPWdiff >= -25 & GDPWdiff <= 25 ~ "no change",
    GDPWdiff > 25 ~ "positive change"
)))
df$GDPcat <- relevel(df$GDPcat, ref = "no change")

# Running the unordered multinomial regression
logit_unordered <- multinom(GDPcat ~ OIL + REG, data = df)
summary(logit_unordered)

# Using Wald tests to estimate p-values
z_unordered <- summary(logit_unordered)$coefficients / 
  summary(logit_unordered)$standard.errors
p_unordered <- (1 - pnorm(abs(z_unordered), 0, 1)) * 2
xtable(p_unordered)


# Re-leveling for ordered multinomial
df$GDP_ordered <- ordered(df$GDPcat,
                          levels = c("negative change",
                                     "no change",
                                     "positive change"))

# Running ordered multinomial logit regression
logit_ordered <- polr(GDP_ordered ~ OIL + REG, data = df, Hess = TRUE)

summary(logit_ordered)

# organizing the information output from polr():
ctable <- coef(summary(logit_ordered))
p_ordered <- pnorm(abs(ctable[,"t value"]), lower.tail = FALSE) * 2
ctable <- cbind(ctable, "p value" = p_ordered)
cutoffs <- ctable[3:4, 1]
ci <- confint(logit_ordered)
ctable_OR <- cbind(OR = exp(coef(logit_ordered)), ci)
ctable_OR

ctable <- cbind(ctable, "p value" = p_ordered)
ctable
# Calculate confidence intervals
ci <- confint(logit_ordered)
# Convert to odds ratio
ctable_OR <- exp(cbind(OR = coef(logit_ordered), ci))

stargazer(logit_unordered, 
          type = "latex", 
          title = "Unordered Multinomial Logistic Regression",
          p = list(p_unordered))


stargazer(logit_ordered,
          type = "latex",
          title = "Ordered Multinomial Logistic Regression",
          p = list(p_ordered),
          add.lines = list(
            c("AIC", round(AIC(logit_ordered), 2))
          ))
ggsave("basic_viz.pdf", basic_viz)

xtable(ctable_OR)
xtable(ctable)


#####################
# Problem 2
#####################

# load data
mexico_elections <- read.csv("https://raw.githubusercontent.com/ASDS-TCD/StatsII_2026/main/datasets/MexicoMuniData.csv")
mx <- mexico_elections

ggplot(mx, aes(x = PAN.visits.06)) +
  geom_histogram(binwidth = 1) +
  xlim(-1, 6)
# basic visualization
idx <- which(mx$PAN.visits.06 == 35)
ggplot(mx, aes(x = competitive.district,
               y = PAN.visits.06)) +
  geom_jitter(alpha = 0.2, color = "darkred") +
  geom_text_repel(data = mx[idx, ], aes(label = "outlier"),
                  min.segment.length = 0.3)

glimpse(mx)

nrow(mx[mx$PAN.visits.06 == 0, ])
nrow(mx[mx$PAN.visits.06 == 1, ])
nrow(mx[mx$PAN.visits.06 == 2, ])
nrow(mx[mx$PAN.visits.06 == 3, ])

mx_pois <- glm(PAN.visits.06 ~ competitive.district + marginality.06 + PAN.governor.06,
               data = mx,
               family = poisson)

summary(mx_pois)
dispersiontest(mx_pois)
AIC(mx_pois)

# Zero-Inflation Model because many of our observations are 0
mx_zip <- zeroinfl(PAN.visits.06 ~ competitive.district + marginality.06 + PAN.governor.06,
                   data = mx,
                   dist = "poisson")

summary(mx_zip)

AIC(mx_pois, mx_zip)
anova(mx_pois, mx_zip)


mx_quasi <- glm(PAN.visits.06 ~ competitive.district + marginality.06 + PAN.governor.06,
                data = mx,
                family = quasipoisson)

summary(mx_quasi)
# Negative Binomial Model
mx_nb <- glm.nb(PAN.visits.06 ~ competitive.district + marginality.06 + PAN.governor.06,
                data = mx)

summary(mx_nb)
aic <- AIC(mx_pois, mx_quasi, mx_zip, mx_nb)

# Case for question 2 (c)
predict_for <- data.frame(
  competitive.district = 1,
  marginality.06 = 0,
  PAN.governor.06 = 1
)

predict(mx_pois, newdata = predict_for, type = "response")

# mx_pois:
# ln(lambda) = -3.81 - 0.08(competition) -2.08(marginality) -0.312(governor)
exp(coef(mx_pois)[1] + coef(mx_pois)[2] + coef(mx_pois)[4])

# mx_zip:
# ln(lamda) = -1.91 + 0.402(competition) - 1.24(margianality) - 0.47(governor)

exp(coef(mx_zip)[1] + coef(mx_zip)[2] + coef(mx_zip)[4])

# mx_nb:
# ln(lambda) = -3.61 - 0.217(competition) - 1.97(marginality) - 0.382(governor)

exp(coef(mx_nb)[1] + coef(mx_nb)[2] + coef(mx_nb)[4])

unique(mx$PAN.governor.06)
stargazer(aic, 
          type = "latex", 
          title = "Poisson Regression")

xtable(aic)
