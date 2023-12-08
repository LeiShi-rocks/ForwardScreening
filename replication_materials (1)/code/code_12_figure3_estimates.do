
* ssc install estout, replace

*** POINT ESTIMATES ***

import delimited "..\data\data_08_imce_point.csv", clear

* Merge

merge m:1 respid using "..\data\data_01_survey.dta"

* Drop respondents with no variance on shown (dichotomized) attribute values
* (for whom IMCEs cannot be estimated)

drop if _merge < 3 // only 1 respondent in this case

drop _merge

* Normalize

gen educ01   = (educ-1)/7
gen ethnoc01 = (ethnocentrism-1)/6
gen pid01    = (pid-1)/6

* Export estimates

eststo clear

local i 0
gen x1 = .
foreach var of varlist imce_????? ///
{
  local ++i
  replace x1 = educ01
  quietly eststo m1`i': reg `var' x1
  replace x1 = ethnoc01
  quietly eststo m2`i': reg `var' x1
  replace x1 = pid01
  quietly eststo m3`i': reg `var' x1
}
drop x1

esttab, se nostar drop(_cons)

matrix C = r(coefs)

matrix rownames C = r1

eststo clear

local rnames : rownames C

local models : coleq C

local models : list uniq models

local i 0

foreach name of local rnames {
  local ++i
  local j 0
  capture matrix drop b
  capture matrix drop se
  foreach model of local models {
    local ++j
    matrix tmp = C[`i', 2*`j'-1]
    if tmp[1,1]<. {
      matrix colnames tmp = `model'
      matrix b = nullmat(b), tmp
      matrix tmp[1,1] = C[`i', 2*`j']
      matrix se = nullmat(se), tmp
    }
  }
  ereturn post b
  quietly estadd matrix se
  eststo `name'
}

esttab using "..\data\data_11_Figure3_point_output.txt", replace ///
se noobs nonumbers b(3) wide nogap plain nonotes

*** DRAWS: NORMALITY ASSUMPTION ***

import delimited "..\data\data_09_imce_draws_normal.csv", clear

* Merge

merge m:1 respid using "..\data\data_01_survey.dta"

* Drop respondents with no variance on shown (dichotomized) attribute values
* (for whom IMCEs cannot be estimated)

drop if _merge < 3 // only 1 respondent in this case

drop _merge

* Declare draws as imputations

foreach var of varlist imce_????? ///
{
  replace `var' = . if impno == 0
}

mi import flong, m(impno) id(respid) imputed(imce_?????) clear

* Normalize

gen educ01   = (educ-1)/7
gen ethnoc01 = (ethnocentrism-1)/6
gen pid01    = (pid-1)/6

* Export estimates

eststo clear

local i 0
gen x1 = .
foreach var of varlist imce_????? ///
{
  local ++i
  replace x1 = educ01
  quietly eststo m1`i': mi est, post: reg `var' x1
  replace x1 = ethnoc01
  quietly eststo m2`i': mi est, post: reg `var' x1
  replace x1 = pid01
  quietly eststo m3`i': mi est, post: reg `var' x1
}
drop x1

esttab, se nostar drop(_cons)

matrix C = r(coefs)

matrix rownames C = r1

eststo clear

local rnames : rownames C

local models : coleq C

local models : list uniq models

local i 0

foreach name of local rnames {
  local ++i
  local j 0
  capture matrix drop b
  capture matrix drop se
  foreach model of local models {
    local ++j
    matrix tmp = C[`i', 2*`j'-1]
    if tmp[1,1]<. {
      matrix colnames tmp = `model'
      matrix b = nullmat(b), tmp
      matrix tmp[1,1] = C[`i', 2*`j']
      matrix se = nullmat(se), tmp
    }
  }
  ereturn post b
  quietly estadd matrix se
  eststo `name'
}

esttab using "..\data\data_12_Figure3_normal_output.txt", replace ///
se noobs nonumbers b(3) wide nogap plain nonotes

*** DRAWS: NONPARAMETRIC BOOTSTRAP ***

import delimited "..\data\data_10_imce_draws_bootstrap.csv", clear

* Merge

merge m:1 respid using "..\data\data_01_survey.dta"

* Drop respondents with no variance on shown (dichotomized) attribute values
* (for whom IMCEs cannot be estimated)

drop if _merge < 3 // only 1 respondent in this case

drop _merge

* Declare draws as imputations

foreach var of varlist imce_????? ///
{
  replace `var' = . if impno == 0
}

mi import flong, m(impno) id(respid) imputed(imce_?????) clear

* Normalize

gen educ01   = (educ-1)/7
gen ethnoc01 = (ethnocentrism-1)/6
gen pid01    = (pid-1)/6

* Export estimates

eststo clear

local i 0
gen x1 = .
foreach var of varlist imce_????? ///
{
  local ++i
  replace x1 = educ01
  quietly eststo m1`i': mi est, post: reg `var' x1
  replace x1 = ethnoc01
  quietly eststo m2`i': mi est, post: reg `var' x1
  replace x1 = pid01
  quietly eststo m3`i': mi est, post: reg `var' x1
}
drop x1

esttab, se nostar drop(_cons)

matrix C = r(coefs)

matrix rownames C = r1

eststo clear

local rnames : rownames C

local models : coleq C

local models : list uniq models

local i 0

foreach name of local rnames {
  local ++i
  local j 0
  capture matrix drop b
  capture matrix drop se
  foreach model of local models {
    local ++j
    matrix tmp = C[`i', 2*`j'-1]
    if tmp[1,1]<. {
      matrix colnames tmp = `model'
      matrix b = nullmat(b), tmp
      matrix tmp[1,1] = C[`i', 2*`j']
      matrix se = nullmat(se), tmp
    }
  }
  ereturn post b
  quietly estadd matrix se
  eststo `name'
}

esttab using "..\data\data_13_Figure3_bootstrap_output.txt", replace ///
se noobs nonumbers b(3) wide nogap plain nonotes
