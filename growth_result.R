library(ivreg)
library("stats")
library("dplyr")
library("lmtest")
library(mfx)

probit <- glm(homelessrate ~ percap_consmp + housing_per_income, homeless_data,
              family = binomial(link = "probit"))
summary(probit)

lm1 <- lm(gini_index~ineql_index, homeless_data)
summary(lm1)

ols1 <- lm(homelessrate ~ ineql_index + saving1 + housing_per_income, homeless_data)
summary(ols1)

ols2 <- lm(homelessrate ~ ineql_index + saving2 + housing_per_income, homeless_data)
summary(ols2)


ols3 <- lm(homelessrate ~ ineql_index+ saving2 + housing_per_income + saving2*ineql_index, homeless_data)
summary(ols3)

ols4 <- lm(homelessrate ~ ineql_index + saving1 + housing_per_income
           + saving1*ineql_index, homeless_data)
summary(ols4)


