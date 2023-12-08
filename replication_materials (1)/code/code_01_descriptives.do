
use "..\data\data_01_survey.dta", clear

* Mean age

sum age

* Male to female ratio

tab gender

* Share of college-educated

gen college = (education>5) if education > 0

label define college ///
0 "Less than college" ///
1 "College or higher"

label values college college

tab college

* Median income

centile income

labelbook income

* Share of non-Hispanic whites

gen nhwhite = (race==1&hispanic==0)

replace nhwhite = . if race==. | hispanic==.

label define nhwhite ///
1 "Non-Hispanic white" ///
0 "Other"

label values nhwhite nhwhite

tab nhwhite

* Partisanship, 3 categories
* (leaners included in the "independent" category)

recode pid (1/2=1) (3/5=3) (6/7=2) (else=.), gen(pid3)

label define pid3 ///
1 "Democrat" ///
2 "Republican" ///
3 "Independent"

label values pid3 pid3

tab pid3
