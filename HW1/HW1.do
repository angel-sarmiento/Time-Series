
clear 
set more off

* Importing the data
*cd "/Users/angelsarmiento/Documents/Graduate/First Year/Time Series/STATA/HW1"

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


*Creating plots of each of the static models
*twoway (tsline ln_fl_bp) (tsline ln_fl_nonfarm, yaxis(2)) , name(ln_fl_bp), replace
*twoway (tsline ln_fl_lf) (tsline ln_fl_nonfarm, yaxis(2)), name(ln_fl_lf), replace
*twoway (tsline ln_fl_lf) (tsline ln_fl_nonfarm, yaxis(2)), name(ln_us_epr), replace

*this is the regression outputs for all of the variables without accounting for the time trends
reg ln_fl_nonfarm ln_fl_lf ln_fl_bp ln_us_epr
eststo lnnonfarm1
* this is the regression output for all of the variables using monthly dates and accounting for time trends
gen monthly = month(datec)


reg ln_fl_nonfarm ln_fl_lf ln_fl_bp ln_us_epr i.monthly datec
eststo lnnonfarm2
esttab lnnonfarm*

esttab lnnonfarm* using "ln_fl_nonfarm_models.csv", replace


* Finite Distributed Lag Model
reg ln_fl_nonfarm l(0/12).(ln_fl_lf ln_fl_bp ln_us_epr) 
eststo nonfarm_lag
esttab nonfarm_lag


reg ln_fl_nonfarm l(0/12).(ln_fl_lf ln_fl_bp ln_us_epr) datec i.monthly 
eststo nonfarm_lag2
esttab nonfarm_lag2


*more parsimonious models

*This model is chosen because the lags past around lag 4 seem to be more and more insignificant.
reg ln_fl_nonfarm l(0/3).(ln_fl_lf ln_fl_bp ln_us_epr) datec i.monthly 
eststo nonfarm_lag3
esttab nonfarm_lag3

reg ln_fl_nonfarm l(0/4, 8).(ln_fl_lf ln_fl_bp ln_us_epr) datec i.monthly if tin(1998m1, 2019m11)
eststo nonfarm_lag4
esttab nonfarm_lag4



esttab nonfarm_lag* using "ln_fl_nonfarm_lag_models.csv", replace








