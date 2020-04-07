* Time Series Modeling and Forecasting
* Spring 2020

clear
set more off

*set the working directory to wherever you have the data"
cd "C:\Users\jdewey\Documents\A S20 Time Series\data and do files"

*import the data
**we will be using data on the US Federal Budget
**Talk about FRED***

import delimited "Federal_Government_Budget_Quarterly.txt"

rename b230rc0q173sbea population
*population, thousands

rename gdpdef_20191220 gdpdef
*100 in 2012

rename na000283q fedexp
*Current expenditures, millions

rename na000304q fedrcpt
*Current receipts, millions

rename na000334q_20191220 gdp
*GDP, millions

rename observation_date datestring
gen datec=date(datestring,"YMD")
gen date=qofd(datec)
format date %tq
tsset date

*generate variable
generate rgdppc=gdp/(pop*gdpdef/100)
gen reratio=fedrcpt/fedexp
gen lnreratio=ln(reratio)
gen lnrgdppc=ln(rgdppc)
generate quarter=quarter(datec)

gen q1=0
replace q1=1 if quarter==1
gen q2=0
replace q2=1 if quarter==2
gen q3=0
replace q3=1 if quarter==3
gen q4=0
replace q4=1 if quarter==4


*Before, we arrived at this model for a point forecast:
*reg d.lnreratio l(1,4,6,8)d.lnreratio l(1/4)d.lnrgdp q2 q4 if tin(1980q1,2019q3)

/* Up to now, we have looked over a small set of models
Let us consider automating the search over many models
GSREG will search over many lags of independent and dependent variables
Need to give it some crieteria to rank models
We will use a weighted average of AIC, BIC, and out of sample RMSE
Also, GSREG does not get factor notation or time series operators
*/

******run ssc install gsreg *****

gen dlnreratio=d.lnreratio // gsreg does not like time series operators

gen l1dlnreratio=l1d.lnreratio // could use its dlag command for these
gen l2dlnreratio=l2d.lnreratio
gen l3dlnreratio=l3d.lnreratio
gen l4dlnreratio=l4d.lnreratio
gen l5dlnreratio=l5d.lnreratio
gen l6dlnreratio=l6d.lnreratio
gen l7dlnreratio=l7d.lnreratio
gen l8dlnreratio=l8d.lnreratio

gen l1dlnrgdppc=l1d.lnrgdppc // could use ilag option for these, but... 
gen l2dlnrgdppc=l2d.lnrgdppc
gen l3dlnrgdppc=l3d.lnrgdppc
gen l4dlnrgdppc=l4d.lnrgdppc


/*Here is the command we will run:
gsreg dlnreratio l1dlnreratio l2dlnreratio l3dlnreratio l4dlnreratio ///
	l5dlnreratio l6dlnreratio l7dlnreratio l8dlnreratio /// 
	l1dlnrgdppc l2dlnrgdppc l3dlnrgdppc l4dlnrgdppc if tin(1980q1, 2019q3), ///
	ncomb(1,8) aic outsample(10) fix(q2 q4) samesample ///
	nindex( -0.3 aic -0.3 bic -0.4 rmse_out) results(gsreg_dlnrer) replace

	What are these things?
	fix?  ***** why did I fix q2 and a4? *********
	ncomb?
	dlags?
	ilags?
	aic?
	outsample?
	nindex?
	samesample?
	results?
	-0.3, -0.3 -0.4?

GSREG vs Stepwise?
1) Out of sample or IC selection (canned stepwise routine does not have it)
2) Checks all subsets (time and compute intensive) 
*/
  
  
gsreg dlnreratio l1dlnreratio l2dlnreratio l3dlnreratio l4dlnreratio ///
	l5dlnreratio l6dlnreratio l7dlnreratio l8dlnreratio /// 
	l1dlnrgdppc l2dlnrgdppc l3dlnrgdppc l4dlnrgdppc if tin(1980q1, 2019q3), ///
	ncomb(1,8) aic outsample(10) fix(q2 q4) samesample ///
	nindex( -0.3 aic -0.3 bic -0.4 rmse_out) results(gsreg_dlnrer) replace
	
  
/*Look at results file for models worth comparing further
We will just do these four to keep it simple for class, should do more

	1: 1,2,6,8,10,13 
	2: 1,2,3,6,8,10,13
	15: 1,2,6,8,10
	26: 1,2,6,8,13

reg d.lnreratio l(1,4,6,8)d.lnreratio l(1/4)d.lnrgdp q2 q4 , the one from before
reg d.lnreratio l(4,6,8)d.lnreratio l(3)d.lnrgdp q2 q4 , gsreg #1
reg d.lnreratio l(1,4,6,8)d.lnreratio l(3)d.lnrgdp q2 q4, gsreg #2
reg d.lnreratio l(4,6,8)d.lnreratio q2 q4, gsreg #15
reg d.lnreratio l(4,6)d.lnreratio l(3)d.lnrgdp q2 q4 , gsreg #26

How are we going to cross validate and select from these?
