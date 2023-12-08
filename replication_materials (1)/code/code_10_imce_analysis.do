
* ssc install estout, replace

import delimited "..\data\data_08_imce_point.csv", clear

* Table 2

eststo clear

label define imce_sign ///
-1 "Negative" ///
 0 "Unreliable" ///
 1 "Positive"

local i = 1
foreach var of varlist imce_????? ///
{
  gen `var'_low  = `var' - 1.96 * `var'_se
  gen `var'_high = `var' + 1.96 * `var'_se
  gen `var'_sign = 0
  replace `var'_sign = -1 if (`var'_low<0&`var'_high<0)
  replace `var'_sign =  1 if (`var'_low>0&`var'_high>0)
	label values `var'_sign imce_sign
  eststo m`i': estpost tab `var'_sign, nototal
	local i = `i' + 1
}

esttab using "..\tables\table2_output.txt", cells("b(fmt(0)) pct(fmt(1))") ///
compress noobs nostar replace not

* Table 3

eststo clear

estpost correlate imce_?????, matrix listwise
esttab using "..\tables\table3_output.txt", ///
unstack b(2) compress noobs nostar replace not

* Table S3 log

log using "..\tables\tableS3_log.smcl", replace

foreach var of varlist imce_????? ///
{
  sum `var', detail
	sktest `var', noadjust
}

log close

translate "..\tables\tableS3_log.smcl" "..\tables\tableS3_log_text.txt", ///
replace
