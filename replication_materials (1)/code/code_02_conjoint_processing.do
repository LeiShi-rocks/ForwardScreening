
* ssc install egenmore, replace

use "..\data\data_02_conjoint_unprocessed.dta", clear

* Conjoint attributes

label define attribute ///
1 "Age" ///
2 "Gender" ///
3 "Race/ethnicity" ///
4 "Education" ///
5 "English proficiency" ///
6 "Prior trips to U.S."

forvalues k = 1/6 {
  encode F_1_`k', gen(attr_`k') label(attribute)
  label var attr_`k' "Attribute `k'"
}

drop F_?_? F_1?_?

* Reshape by scenario

forvalues i = 1/15 {
  forvalues j = 1/2 {
    forvalues k = 1/6 {
      rename F_`i'_`j'_`k' val_`k'_`j'_`i'
	  }
  }
}

reshape long ///
val_1_1_ val_2_1_ val_3_1_ val_4_1_ val_5_1_ val_6_1_ ///
val_1_2_ val_2_2_ val_3_2_ val_4_2_ val_5_2_ val_6_2_ ///
rate_1_ rate_2_ ///
, ///
i(respid) j(scenario)

label variable scenario "Conjoint scenario ID"

rename val_?_?_ val_?_?

rename rate_?_ rate_?

* Reshape by profile

reshape long ///
val_1_ val_2_ val_3_ val_4_ val_5_ val_6_ ///
rate_ ///
, ///
i(respid scenario) j(profile)

label variable profile "Conjoint profile ID"

rename val_?_ val_?

rename rate_ rate

label variable rate "Conjoint rating outcome"

* Attribute values

gen conj_age  = ""
gen conj_sex  = ""
gen conj_race = ""
gen conj_educ = ""
gen conj_lang = ""
gen conj_trip = ""

forvalues k = 1/6 {
  replace conj_age  = val_`k' if attr_`k' == 1
  replace conj_sex  = val_`k' if attr_`k' == 2
  replace conj_race = val_`k' if attr_`k' == 3
  replace conj_educ = val_`k' if attr_`k' == 4
  replace conj_lang = val_`k' if attr_`k' == 5
  replace conj_trip = val_`k' if attr_`k' == 6
}

drop attr_? val_?

* Values: Age

destring conj_age, gen(age_int)

label variable age_int "Conjoint values: Age, interval"

recode age_int (26/39 = 2) (40/55 = 1), gen(age_bin)

label define age_bin ///
1 "40+" ///
2 "39-"

label values age_bin age_bin

label variable age_bin "Conjoint values: Age, binary"

* Values: Gender

label define sex_bin ///
1 "Male" ///
2 "Female"

encode conj_sex, gen(sex_bin) label(sex_bin)

label variable sex_bin "Conjoint values: Gender"

* Values: Race

label define race_cat ///
1 "White" ///
2 "Black" ///
3 "Hispanic" ///
4 "Asian"

encode conj_race, gen(race_cat) label(race_cat)

label values race_cat race_cat

label variable race_cat "Conjoint values: Race, 4 categories"

label define race_bin ///
1 "Non-white" ///
2 "White"

recode race_cat (1 = 2) (2/4 = 1), gen(race_bin)

label values race_bin race_bin

label variable race_bin "Conjoint values: Race, binary"

* Values: Education

label define educ_cat ///
1 "Elementary school" ///
2 "Middle school" ///
3 "High school" ///
4 "2-year college" ///
5 "4-year college" ///
6 "Graduate degree"

encode conj_educ, gen(educ_cat) label(educ_cat)

label variable educ_cat "Conjoint values: Education, 6 categories"

label define educ_bin ///
1 "Less than college" ///
2 "College or higher"

recode educ_cat (1/3 = 1) (4/6 = 2), gen(educ_bin)

label values educ_bin educ_bin

label variable educ_bin "Conjoint values: Education, binary"

* Values: English proficiency

label define lang_cat ///
1 "Very low" ///
2 "Low" ///
3 "High" ///
4 "Very high"

encode conj_lang, gen(lang_cat) label(lang_cat)

label variable lang_cat "Conjoint values: English proficiency, 4 categories"

label define lang_bin ///
1 "Poor" ///
2 "Good"

recode lang_cat (1 2 = 1) (3 4 = 2), gen(lang_bin)

label values lang_bin lang_bin

label variable lang_bin "Conjoint values: Education, binary"

* Values: Prior trips to U.S.

label define trip_cat ///
1 "Yes, unauthorized" ///
2 "Yes, overstayed visa" ///
3 "No" ///
4 "Yes, on a visa"

encode conj_trip, gen(trip_cat) label(trip_cat)

label variable trip_cat "Conjoint values: Prior trips to U.S., 4 categories"

label values trip_cat trip_cat

label define trip_bin ///
1 "Violation" ///
2 "No violation"

recode trip_cat (1 2 = 1) (3 4 = 2), gen(trip_bin)

label values trip_bin trip_bin

label variable trip_bin "Conjoint values: Prior trips to U.S., binary"

* Drop respondents with no variance on shown (dichotomized) attribute values

foreach var of varlist *_bin ///
{
  egen nvals_`var' = nvals(`var'), by(respid)
  drop if nvals_`var' < 2
  drop nvals_`var'
}

* Saving

drop conj_*

compress

saveold "..\data\data_03_conjoint_processed.dta", version(12) replace
