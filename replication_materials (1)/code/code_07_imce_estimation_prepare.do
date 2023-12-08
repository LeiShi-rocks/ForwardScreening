
use "..\data\data_03_conjoint_processed.dta", clear

* Generate conjoint attribute variables with 0/1 values

gen conj_young = (age_bin==2) // 0 = age 40+, 1= age 39-

gen conj_femal = (sex_bin==2) // 0 = male, 1 = female

gen conj_white = (race_bin==2) // 0 = non-white, 1 = white

gen conj_collg = (educ_bin==2) // 0 = no college, 1 = college

gen conj_engls = (lang_bin==2) // 0 = poor, 1 = good

gen conj_authr = (trip_bin==2) // 0 = unauthorized, 1 = authorized

* Export to R

keep respid rate conj_*

label var respid ""

label var rate ""

compress

saveold "..\data\data_07_imce_estimation.dta", version(12) replace
