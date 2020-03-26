
clear 
set more off

* Importing the data
*cd "/Users/angelsarmiento/Documents/Graduate/First Year/Time Series/STATA/HW4"

import delimited "data.csv"

*Creating monthly time date
rename date datestring
gen datec=date(datestring,"YMD")
gen mdate=mofd(datec)
format mdate %tm
tsset mdate

generate month = month(mdate)
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




* Model 1
reg d.ln_fl_nonfarm l(1/12)d.ln_fl_nonfarm l(1/12)d.ln_fl_lf l(1/12)d.ln_fl_bp l(1/12)d.ln_us_epr i.month if tin(1998m1, 2018m11)
predict pdln_fl_nonfarm
gen ubpdln_fl_nonfarm=pdln_fl_nonfarm+1.96*e(rmse)
gen lbpdln_fl_nonfarm=pdln_fl_nonfarm-1.96*e(rmse)
tsline pdln_fl_nonfarm lbpd ubpd d.ln_fl_nonfarm if tin(2016m1,2019m11)

*getting standard errors and ln nonfarm predictions
predict stderrfcst1, stdf
predict preln_fl_nonfarm1

*transforming back to nonfarm
gen prenonfarm1 = l.preln_fl_nonfarm1+pdln_fl_nonfarm
gen mseout1=(preln_fl_nonfarm1-ln_fl_nonfarm)^2 if tin(2019m1,2019m11)
gen oosrmse1 = sqrt(mseout1) if tin(2019m1, 2019m11)
* IN CASE IT IS FORGOTTEN, OOS RMSE IS 9.101146

crossfold reg d.ln_fl_nonfarm l(1/12)d.ln_fl_nonfarm l(1/12)d.ln_fl_lf l(1/12)d.ln_fl_bp ///
	l(1/12)d.ln_us_epr i.month if tin(1998m1, 2018m11), k(10)

	
	
	
	
	
	

* Model 2
reg d.ln_fl_nonfarm l(1/12)d.ln_fl_nonfarm l(1/2)d.ln_fl_lf l(1/2)d.ln_fl_bp l(1/2)d.ln_us_epr i.month if tin(1998m1, 2018m11)
predict pdln_fl_nonfarm
gen ubpdln_fl_nonfarm=pdln_fl_nonfarm+1.96*e(rmse)
gen lbpdln_fl_nonfarm=pdln_fl_nonfarm-1.96*e(rmse)
tsline pdln_fl_nonfarm lbpd ubpd d.ln_fl_nonfarm if tin(2016m1,2019m11)

predict stderrfcst2, stdf
predict preln_fl_nonfarm2

gen prenonfarm2 = l.preln_fl_nonfarm2+pdln_fl_nonfarmout2
gen mseout2=(preln_fl_nonfarm2-ln_fl_nonfarm)^2 if tin(2019m1,2019m11)
gen oosrmse2 = sqrt(mseout2) if tin(2019m1, 2019m11)
*OOS RMSE is 9.10057891

crossfold reg d.fl_nonfarm l(1/12)d.fl_nonfarm l(1/2)d.fl_lf l(1/2)d.fl_bp l(1/2)d.us_epr_25to54 i.month if tin(1998m1, 2018m11), k(10)


* Best model Actual values
reg d.fl_nonfarm l(1/12)d.fl_nonfarm l(1/2)d.fl_lf l(1/2)d.fl_bp l(1/2)d.us_epr_25to54 i.month if tin(1998m1, 2018m11)
predict pdnonfarmout
predict stdfnonfarmout, stdf

gen pnonfarm2=l.fl_nonfarm+pdnonfarmout
gen ubpout2=pnonfarm2+1.96*stdfnonfarmout
gen lbpout2=pnonfarm2-1.96*stdfnonfarmout
tsline pnonfarm2 lbpout2 ubpout2 fl_nonfarm if tin(2018m1,2019m11)



*Empirical Approach
reg d.fl_nonfarm l(1/12)d.fl_nonfarm l(1/2)d.fl_lf l(1/2)d.fl_bp l(1/2)d.us_epr_25to54 i.month if tin(1998m1, 2018m11)

predict pres if tin(1998m1,2018m11), residual
_pctile pres, percentile(2.5,97.5)
return list
gen lbpdlnoute=preln_fl_nonfarm2+r(r1)
gen ubpdlnoute=preln_fl_nonfarm2+r(r2)


gen exppres=exp(pres) if tin(1998m1,2018m11)
summ exppres
gen prenonfarmoute=exp(l.ln_fl_nonfarm+pdln_fl_nonfarm)*r(mean)
gen ubpoute=exp(l.ln_fl_nonfarm+pdln_fl_nonfarm+ubpdlnoute)*r(mean)
gen lbpoute=exp(l.ln_fl_nonfarm+pdln_fl_nonfarm+lbpdlnoute)*r(mean)
tsline prenonfarmoute lbpoute ubpoute fl_nonfarm if tin(2018m1,2019m11)




*Model 3
reg d.ln_fl_nonfarm l(1/12)d.ln_fl_nonfarm l(1/2, 12)d.ln_fl_lf l(1/2, 12)d.ln_fl_bp l(1/2, 12)d.ln_us_epr i.month if tin(1998m1, 2018m11)
predict pdln_fl_nonfarm
gen ubpdln_fl_nonfarm=pdln_fl_nonfarm+1.96*e(rmse)
gen lbpdln_fl_nonfarm=pdln_fl_nonfarm-1.96*e(rmse)
tsline pdln_fl_nonfarm lbpd ubpd d.ln_fl_nonfarm if tin(2016m1,2019m11)

predict stderrfcst3, stdf
predict preln_fl_nonfarm3

gen prenonfarm3 = l.preln_fl_nonfarm3+pdln_fl_nonfarm
gen mseout3=(preln_fl_nonfarm3-ln_fl_nonfarm)^2 if tin(2019m1,2019m11)
gen oosrmse3 = sqrt(mseout3) if tin(2019m1, 2019m11)

crossfold reg d.ln_fl_nonfarm l(1/12)d.ln_fl_nonfarm l(1/2, 12)d.ln_fl_lf l(1/2, 12)d.ln_fl_bp l(1/2, 12)d.ln_us_epr i.month if tin(1998m1, 2018m11), k(10)





*Model 4
reg d.ln_fl_nonfarm l(1/2, 12, 24)d.ln_fl_nonfarm l(1/2, 12, 24)d.ln_fl_lf l(1/2, 12, 24)d.ln_fl_bp l(1/2, 12, 24)d.ln_us_epr i.month if tin(1998m1, 2018m11)
predict pdln_fl_nonfarm
gen ubpdln_fl_nonfarm=pdln_fl_nonfarm+1.96*e(rmse)
gen lbpdln_fl_nonfarm=pdln_fl_nonfarm-1.96*e(rmse)
tsline pdln_fl_nonfarm lbpd ubpd d.ln_fl_nonfarm if tin(2016m1,2019m11)

predict stderrfcst4, stdf
predict preln_fl_nonfarm4

gen prenonfarm4 = l.preln_fl_nonfarm4+pdln_fl_nonfarm
gen mseout4=(preln_fl_nonfarm4-ln_fl_nonfarm)^2 if tin(2019m1,2019m11)
gen oosrmse4 = sqrt(mseout4) if tin(2019m1, 2019m11)

crossfold reg d.ln_fl_nonfarm l(1/2, 12, 24)d.ln_fl_nonfarm l(1/2, 12, 24)d.ln_fl_lf l(1/2, 12, 24)d.ln_fl_bp l(1/2, 12, 24)d.ln_us_epr i.month if tin(1998m1, 2018m11), k(10)


/*

*/
