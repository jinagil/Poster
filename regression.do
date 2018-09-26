******** Regression!
clear
cd "C:\Users\Jina Gil\Documents\Spring 2018\Research\Data"
set more off
use gdpdata.dta, replace
log using "gdpdatalog", replace

*** Delete duplicates (-999)
drop if country == "Hong Kong"

drop if country == "Macao"

drop if country == "New Caledonia"

*** Lagging data
sort country year
by country: gen gdpgrowthper_lag = gdpgrowthper[_n-1]
label variable gdpgrowthper_lag "Lagged GDP Growth per Year (1 year, %)"

*** log the investment
gen loginv = log(invest +1)

*** log the independence? 
gen logindep = log(independence)

*** Save as a new dataset for R
save gdpdatacleaned.dta, replace

*** Notice (by Mike)
* Country and year fixed effects. These fixed effects account omitted variables that are time invariant.
* You clustered standard errors by country
* It is an OLS regression with economic growth rate as the dependent variable
* dominant party variable significant at 1%, 5% level (i dont remember which one)
* dominant party variable only coded until 2008
* don't forget that democracy is your starting point, so your effect on dominant parties is additional based on the effect of democracy
* literacy drops a lot of the observations -- might be necessary to use multiple imputation in later rounds

*** Panel Analysis
xtset ccode_gled year

*** encoding

encode regimetype, gen(regimetype_enc)
encode country, gen(country_enc)


*** Distributions
twoway scatter dem_stock country_enc if regimetype=="authoritarian"
twoway scatter dem_stock country_enc if regimetype=="dominant"
twoway scatter dem_stock country_enc if regimetype=="democratic"
twoway scatter dem_stock regimetype_enc if independence<50

*** Models
* Original Gerring (Model 1: Original)
xtreg gdpgrowthper_lag dem_stock ///
primaryedu loginv traderate oilshock popgrow sumconfv414 ///
loggdppercap lifeexpec logindep i.year, vce(cluster country)

* Original Gerring + New Confounder Variables (Model 2: New Confounders)
xtreg gdpgrowthper_lag dem_stock ///
v2x_corr mediascore primaryedu loginv traderate oilshock popgrow sumconfv414 ///
loggdppercap lifeexpec logindep i.year, vce(cluster country)

*** Without Moderation (Model 3: Full Without Interaction)
* Random effect
xtreg gdpgrowthper_lag dem_stock i.regimetype_enc v2x_corr ///
mediascore primaryedu loginv traderate oilshock popgrow sumconfv414 ///
loggdppercap lifeexpec logindep i.year if dem_stock<30, vce(cluster country)

*** Full Model with Fixed effect (original, Final Model)
* Random effect
xtreg gdpgrowthper_lag dem_stock i.regimetype_enc regimetype_enc#c.dem_stock ///
v2x_corr mediascore loginv traderate primaryedu oilshock popgrow sumconfv414 ///
loggdppercap lifeexpec logindep i.year if dem_stock<30, vce(cluster country)


***Table Export Function
outreg2 level using , //
title("Model 1") //
word replace


*** Testing additional variables
* Original Gerring + Regime Type Interaction
xtreg gdpgrowthper_lag dem_stock i.regimetype_enc regimetype_enc#c.dem_stock ///
primaryedu loginv traderate oilshock popgrow sumconfv414 ///
loggdppercap lifeexpec logindep i.year, vce(cluster country)

*** Gerring et al. with trend
xtreg gdpgrowthper_lag dem_stock ///
primaryedu loginv traderate oilshock popgrow sumconfv414 ///
loggdppercap lifeexpec logindep i.trend i.year, vce(cluster country)

* without trend
xtreg gdpgrowthper_lag dem_stock ///
primaryedu loginv traderate oilshock popgrow sumconfv414 ///
loggdppercap lifeexpec logindep i.year, vce(cluster country)

*** Only Main Variables
xtreg gdpgrowthper_lag dem_stock dsrg1 dsrg2 regimetype_d1 regimetype_d2, fe vce(cluster ccode)

*** Randomized? 
xtreg gdpgrowthper_lag dem_stock dsrg1 dsrg2 regimetype_d1 regimetype_d2 v2x_corr mediascore secedu ppp traderate oilshock popgrow durable independence sumconfv414 loggdppercap lifeexpec, re vce(cluster country)

*** Dummies (in case)
gen regimetype_d1=""
destring regimetype_d1, replace
replace regimetype_d1=1 if regimetype=="authoritarian"
replace regimetype_d1=0 if regimetype=="dominant"
replace regimetype_d1=0 if regimetype=="democratic"
gen regimetype_d2=""
destring regimetype_d2, replace
replace regimetype_d2=0 if regimetype=="authoritarian"
replace regimetype_d2=1 if regimetype=="dominant"
replace regimetype_d2=0 if regimetype=="democratic"
gen regimetype_d3=""
destring regimetype_d3, replace
replace regimetype_d3=0 if regimetype=="authoritarian"
replace regimetype_d3=0 if regimetype=="dominant"
replace regimetype_d3=1 if regimetype=="democratic"

gen dsrt1 = dem_stock*regimetype_d1
gen dsrt2 = dem_stock*regimetype_d2
gen dsrt3 = dem_stock*regimetype_d3
