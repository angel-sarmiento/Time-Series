
clear 
set more off

* Importing the data
*cd "/Users/angelsarmiento/Documents/Graduate/First Year/Time Series/STATA/HW4"

*import delimited "data.csv"

freduse LNU02300000 FLBPPRIV PERMITNSA FLLFN LNU02300000 LREM25TTUSM156N FLNAN

*renaming variables
*New Private Housing Units Authorized by Building Permits for Florida
rename FLBPPRIV fl_bp

*New Private Housing Units Authorized by Building Permits for USA
rename PERMITNSA us_bp

*Civilian Labor Force in Florida
rename FLLFN fl_lf

*All Employees: Total Nonfarm in Florida
rename FLNAN fl_nonfarm

*Employment Population Ratio
rename LNU02300000 us_epr

*Employment Population Ratio 25 to 54 years old
rename LREM25TTUSM156N us_epr_25to54

*Datestring generation
rename date datestring
gen datec=date(datestring,"YMD")
gen date=mofd(datec)
format date %tm
tsset date

*Natural logs
gen ln_fl_bp = ln(fl_bp)
gen ln_fl_lf = ln(fl_lf)
gen ln_fl_nonfarm = ln(fl_nonfarm)
gen ln_us_epr_bum = ln(us_epr)
gen ln_us_epr = ln(us_epr_25to54)
gen lnus_bp = ln(us_bp)

* Month indicators
generate month=month(datec)
gen m1=0
replace m1=1 if month==1
gen m2=0
replace m2=1 if month==1
gen m3=0
replace m3=1 if month==1
gen m4=0
replace m4=1 if month==1
gen m5=0
replace m5=1 if month==1
gen m6=0
replace m6=1 if month==1
gen m7=0
replace m7=1 if month==1
gen m8=0
replace m8=1 if month==1
gen m9=0
replace m9=1 if month==1
gen m10=0
replace m10=1 if month==1
gen m11=0
replace m11=1 if month==1
gen m12=0
replace m12=1 if month==1





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
reg d.ln_fl_nonfarm l(1/12)d.ln_fl_nonfarm l(1/2)d.ln_fl_lf l(1/2)d.ln_fl_bp l(1/2)d.ln_us_epr i.month if tin(1998m1, 2018m12)
predict pdln_fl_nonfarm
gen ubpdln_fl_nonfarm=pdln_fl_nonfarm+1.96*e(rmse)
gen lbpdln_fl_nonfarm=pdln_fl_nonfarm-1.96*e(rmse)
tsline pdln_fl_nonfarm lbpd ubpd d.ln_fl_nonfarm if tin(2016m1,2019m12)

predict stderrfcst2, stdf
predict preln_fl_nonfarm2

gen prenonfarm2 = l.preln_fl_nonfarm2+preln_fl_nonfarm2
gen mseout2=(preln_fl_nonfarm2-ln_fl_nonfarm)^2 if tin(2019m1,2019m11)
gen oosrmse2 = sqrt(mseout2) if tin(2019m1, 2019m11)
*OOS RMSE is 9.10057891

crossfold reg d.fl_nonfarm l(1/12)d.fl_nonfarm l(1/2)d.fl_lf l(1/2)d.fl_bp l(1/2)d.us_epr_25to54 i.month if tin(1998m1, 2018m11), k(10)


* Best model Actual values
reg d.fl_nonfarm l(1/12)d.fl_nonfarm l(1/2)d.fl_lf l(1/2)d.fl_bp l(1/2)d.us_epr_25to54 i.month if tin(1998m1, 2018m12)
predict pdnonfarmout
predict stdfnonfarmout, stdf

gen pnonfarm2=l.fl_nonfarm+pdnonfarmout
gen ubpout2=pnonfarm2+1.96*stdfnonfarmout
gen lbpout2=pnonfarm2-1.96*stdfnonfarmout
tsline pnonfarm2 lbpout2 ubpout2 fl_nonfarm if tin(2018m1,2019m12)



*Empirical Approach
reg d.ln_fl_nonfarm l(1/12)d.ln_fl_nonfarm l(1/2)d.ln_fl_lf l(1/2)d.ln_fl_bp l(1/2)d.ln_us_epr i.month if tin(1998m1, 2018m12)
predict pdln_fl_nonfarm

predict pres if tin(1998m1,2018m12), residual
_pctile pres, percentile(2.5,97.5)
return list
gen lbpdlnoute=pdln_fl_nonfarm+r(r1)
gen ubpdlnoute=pdln_fl_nonfarm+r(r2)


gen exppres=exp(pres) if tin(1998m1,2018m12)
summ exppres
gen prenonfarmoute=exp(l.ln_fl_nonfarm+pdln_fl_nonfarm)*r(mean)
gen ubpoute=exp(l.ln_fl_nonfarm+ubpdlnoute)*r(mean)
gen lbpoute=exp(l.ln_fl_nonfarm+lbpdlnoute)*r(mean)
tsline prenonfarmoute lbpoute ubpoute fl_nonfarm if tin(2018m1,2019m12)


*Adding January of 2020

tsappend, add(1)
replace month=month(dofm(date)) if month==.


reg d.ln_fl_nonfarm l(1/12)d.ln_fl_nonfarm l(1/2)d.ln_fl_lf l(1/2)d.ln_fl_bp l(1/2)d.ln_us_epr i.month if tin(1998m1, 2018m11)
predict pdln_fl_nonfarm2




* log estimate for 2020m1
reg d.ln_fl_nonfarm l(1/12)d.ln_fl_nonfarm l(1/2)d.ln_fl_lf l(1/2)d.ln_fl_bp l(1/2)d.ln_us_epr i.month if tin(1998m1, 2019m12)
predict pdln_fl_nonfarm2

gen ubpdln_fl_nonfarm=pdln_fl_nonfarm2+1.96*e(rmse)
gen lbpdln_fl_nonfarm=pdln_fl_nonfarm2-1.96*e(rmse)
tsline pdln_fl_nonfarm2 lbpd ubpd d.ln_fl_nonfarm if tin(2016m1,2020m1)







* true estimate for 2020m1
reg d.ln_fl_nonfarm l(1/12)d.ln_fl_nonfarm l(1/2)d.ln_fl_lf l(1/2)d.ln_fl_bp l(1/2)d.ln_us_epr i.month if tin(1998m1, 2019m12)
predict pdln_fl_nonfarm3

predict pres if tin(1998m1,2019m12), residual
_pctile pres, percentile(2.5,97.5)
return list
gen lbpdlnoute=pdln_fl_nonfarm3+r(r1)
gen ubpdlnoute=pdln_fl_nonfarm3+r(r2)


gen exppres=exp(pres) if tin(1998m1,2019m12)
summ exppres
gen prenonfarmoute=exp(l.ln_fl_nonfarm+pdln_fl_nonfarm3)*r(mean)
gen ubpoute=exp(l.ln_fl_nonfarm+ubpdlnoute)*r(mean)
gen lbpoute=exp(l.ln_fl_nonfarm+lbpdlnoute)*r(mean)
tsline prenonfarmoute lbpoute ubpoute fl_nonfarm if tin(2018m1,2020m1)



* true estimate for 2020m2
reg d.ln_fl_nonfarm l(1/12)d.ln_fl_nonfarm l(1/2)d.ln_fl_lf l(1/2)d.ln_fl_bp l(1/2)d.ln_us_epr i.month if tin(1998m1, 2019m12)
predict pdln_fl_nonfarm3

predict pres if tin(1998m1,2019m12), residual
_pctile pres, percentile(2.5,97.5)
return list
gen lbpdlnoute=pdln_fl_nonfarm3+r(r1)
gen ubpdlnoute=pdln_fl_nonfarm3+r(r2)


gen exppres=exp(pres) if tin(1998m1,2019m12)
summ exppres
gen prenonfarmoute=exp(l.ln_fl_nonfarm+pdln_fl_nonfarm3)*r(mean)
gen ubpoute=exp(l.ln_fl_nonfarm+ubpdlnoute)*r(mean)
gen lbpoute=exp(l.ln_fl_nonfarm+lbpdlnoute)*r(mean)
tsline prenonfarmoute lbpoute ubpoute fl_nonfarm if tin(2018m1,2020m2)



* true estimate for 2020m2 FINAL PROBLEM
reg d.ln_fl_nonfarm l(1/12)d.ln_fl_nonfarm l(1/2)d.ln_fl_lf l(1/2)d.ln_fl_bp l(1/2)d.ln_us_epr i.month if tin(1998m1, 2019m12)
predict pdln_fl_nonfarm3

predict pres if tin(1998m1,2019m12), residual
_pctile pres, percentile(2.5,97.5)
return list
gen lbpdlnoute=pdln_fl_nonfarm3+r(r1)
gen ubpdlnoute=pdln_fl_nonfarm3+r(r2)


gen exppres=exp(pres) if tin(1998m1,2019m12)
summ exppres
gen prenonfarmoute=exp(l.ln_fl_nonfarm+pdln_fl_nonfarm3)*r(mean)
gen ubpoute=exp(l.ln_fl_nonfarm+ubpdlnoute)*r(mean)
gen lbpoute=exp(l.ln_fl_nonfarm+lbpdlnoute)*r(mean)
tsline prenonfarmoute lbpoute ubpoute fl_nonfarm if tin(2019m1,2020m2)



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
