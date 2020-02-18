
clear 
set more off

* Importing the data
*cd "/Users/angelsarmiento/Documents/Graduate/First Year/Time Series/STATA/HW2"

import delimited "data.csv"

*Creating monthly time date
rename date datestring
gen datec=date(datestring,"YMD")
gen mdate=mofd(datec)
format mdate %tm
tsset mdate



*Converting all string values to Float. Ignoring NA values.
* ALL OF THESE VARIABLES WERE RENAMED IN R
destring fl_lf, replace ignore("NA")
destring fl_bp, replace ignore("NA")
destring us_epr_25to54, replace ignore("NA")

* Creating log versions of the variables 
gen ln_fl_nonfarm = ln(fl_nonfarm)
gen ln_fl_lf = ln(fl_lf)
gen ln_fl_bp = ln(fl_bp)
gen ln_us_epr = ln(us_epr_25to54)

*Finding the correlations of each variable with respect to a single time lag
cor ln_fl_nonfarm l1.ln_fl_nonfarm
cor ln_fl_lf l1.ln_fl_lf
cor ln_fl_bp l1.ln_fl_bp
cor ln_us_epr l1.ln_us_epr

*Looks like serial correlation to me 

*Autocorrelogram and PAC for each variable
ac ln_fl_nonfarm
pac ln_fl_nonfarm

ac ln_fl_lf
pac ln_fl_lf

ac ln_fl_bp
pac ln_fl_bp

ac ln_us_epr
pac ln_us_epr

*Dickey Fuller Test for each variable.
estimates clear
dfuller ln_fl_nonfarm if tin(1988m1, 2019m12), trend lags(12) regress
eststo dfuller_1
*esttab dfuller_1 using "dfuller_nonfarm.csv", replace

dfuller ln_fl_lf if tin(1988m1, 2019m12), trend lags(12) regress
eststo dfuller_2
*esttab dfuller_2 using "dfuller_lf.csv", replace

dfuller ln_fl_bp if tin(1988m1, 2019m12), trend lags(12) regress
eststo dfuller_3
*esttab dfuller_3 using "dfuller_bp.csv", replace

dfuller ln_us_epr if tin(1988m1, 2019m12), trend lags(12) regress
eststo dfuller_4
*esttab dfuller_4 using "dfuller_epr.csv", replace

esttab dfuller_* using "all_dfuller.csv", replace

generate month = month(mdate)

reg d.ln_fl_nonfarm l(1,12, 24)d.ln_fl_nonfarm l(1/12,24)d.ln_fl_lf l(1/12,24)d.ln_fl_bp l(1/12,24)d.ln_us_epr i.month mdate if tin(1998m1, 2019m11)
eststo ardl 
esttab ardl using "ardl.csv", replace
*PAc of the residuals
predict resid if e(sample)==1, residual 
pac resid


*Breusch Godfrey Test
reg d.ln_fl_nonfarm l(1,12,24)d.ln_fl_nonfarm l(1/12)d.ln_fl_lf l(1/12,24)d.ln_fl_bp l(1/12)d.ln_us_epr i.month if tin(1998m1, 2019m11)
estat bgodfrey, lag(1/24)
eststo bgodfrey_1
esttab bgodfrey_1 using "bgodfrey.csv", replace

predict resstatic, residual
pac resstatic



*model without Newey-West
reg d.ln_fl_nonfarm l(0/4)d.ln_fl_bp i.month
predict residua if e(sample)==1, residual 
pac residua

*Model with Newey West
newey d.ln_fl_nonfarm l(0/4)d.ln_fl_bp i.month, lag(4)
predict residu if e(sample)==1, residual 
pac residu



reg d.ln_fl_nonfarm l(1,12,24)d.ln_fl_nonfarm l(1/12)d.ln_fl_lf l(1/12,24)d.ln_fl_bp l(1/12)d.ln_us_epr i.month if tin(1998m1, 2019m11)
predict resid_ardl_wo if e(sample)==1, residual 
pac resid_ardl_wo

newey d.ln_fl_nonfarm l(1,12,24)d.ln_fl_nonfarm l(1/12)d.ln_fl_lf l(1/12,24)d.ln_fl_bp l(1/12)d.ln_us_epr i.month if tin(1998m1, 2019m11), lag(12)
predict resid_ardl if e(sample) ==1, residual
pac resid_ardl





