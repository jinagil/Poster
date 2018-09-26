rm(list=ls())

#Dataset
library(haven)
setwd("C:/Users/Jina Gil/Documents/Spring 2018/Applied Statistics/Project")
gdpdata <- read_dta("data/gdpdatacleaned.dta")
names(gdpdata)

#Dependent variable: gdpgrowthper_lag
#Main independent variables: dem_stock, regimetype(categorical)
#Confounders: v2x_corr, mediascore, loggdppercap, lifeexpec, traderate, oilshock(categorical, binomial)
#popgrow, sumconfv414, loginv, primaryedu, logindep

summary(gdpdata$dem_stock)
table(gdpdata$regimetype)

#Factorize categorical variables
gdpdata$regime_f <- factor(gdpdata$regimetype, levels=c("authoritarian", "democratic", "dominant"))
table(gdpdata$regime_f)
gdpdata$oil_f <- factor(gdpdata$oilshock, levels=c(0,1), labels=c("no","yes"))
table(gdpdata$oil_f)

#Generate a new dependent variable as a categorical variable
#to check the distribution of democracy stock values
gdpdata$dem_c <- NA
gdpdata$dem_c[gdpdata$dem_stock > 40] <- "High Dem"
gdpdata$dem_c[gdpdata$dem_stock > 20 & gdpdata$dem_stock <= 40] <- "Middle Dem"
gdpdata$dem_c[gdpdata$dem_stock <= 20] <- "Low Dem"
gdpdata$dem_f <- factor(gdpdata$dem_c, levels=c("Low Dem", "Middle Dem", "High Dem"))
table(gdpdata$dem_f)

#rough model
f1 <- lm(gdpgrowthper_lag ~ dem_f + regime_f + dem_f*regime_f + v2x_corr + mediascore + 
           loggdppercap + lifeexpec + traderate + oil_f + popgrow + sumconfv414 + loginv + primaryedu 
         +logindep, data=gdpdata)
summary(f1)
library(SDSRegressionR)
library(car)
residFitted(f1)
vif(f1)
cooksPlot(f1, key.variable="unique", sort.obs=TRUE, save.cutoff=TRUE)
cp <- cooksPlot(f1, key.variable = "unique", print.obs = TRUE, sort.obs = TRUE)
"%not in%" <- Negate("%in%")
g_gdp <- gdpdata[gdpdata$unique %not in%  cp$unique[1:5],]

#Because there are aliased coefficients in the f1 model with high democracy stock score and democracy regime, 
#I will drop the observations with high democracy stock score for this analysis.
#Which means, the relationship that I will look at is only limited to those regimes with
#low or middle (or, less or equal to 40) democracy stock score.
gdp_n1 <- subset(g_gdp, dem_stock <= 40)

#Subset 2: drop all the observations with missing regime type (~2008, Kenneth Greene (2009))
gdp_n <- subset(gdp_n1, complete.cases(gdp_n1$regime_f))
table(gdp_n$regime_f)
table(gdp_n$dem_f)

#Descriptive Statistics
summary(gdp_n$dem_stock)
summary(gdp_n$gdpgrowthper_lag)
summary(gdp_n)
sd(gdp_n$loggdppercap, na.rm=TRUE)
sd(gdp_n$lifeexpec, na.rm=TRUE)
sd(gdp_n$traderate, na.rm=TRUE)
sd(gdp_n$popgrow, na.rm=TRUE)
sd(gdp_n$logindep, na.rm=TRUE)
sd(gdp_n$sumconfv414, na.rm=TRUE)
sd(gdp_n$primaryedu, na.rm=TRUE)
sd(gdp_n$loginv, na.rm=TRUE)
table(gdp_n$oil_f)

#full model (with quantitative democracy stock variable, since all the high values are omitted)
f2 <- lm(gdpgrowthper_lag ~ dem_stock + regime_f + dem_stock*regime_f + v2x_corr + mediascore + 
           loggdppercap + lifeexpec + traderate + oil_f + popgrow + sumconfv414 + loginv +
           logindep + primaryedu, data=gdp_n)
residFitted(f2)
vif(f2)
summary(f2)

#Significance
Anova(f2, type="III")

#Graph
library(ggthemes)
library(dplyr)
qplot(x=dem_stock, y=gdpgrowthper_lag, facets=~regime_f, data = f2, alpha=0.1) + geom_smooth(method = "lm") +
  labs(title="Regime Type Interaction", 
       x="Democracy Stock", y="Lagged GDP per Capita Growth (%)") + theme_bw()
g <- ggplot(f2, aes(x=dem_stock, y=gdpgrowthper_lag, color=regime_f)) +
  stat_smooth(method=lm, se = FALSE, fullrange=TRUE) +
  labs(title="Regime Type Interaction", 
       x="Democracy Stock", y="Lagged GDP per Capita Growth (%)") +
  theme_bw() + scale_colour_ptol() + scale_fill_ptol()
g

