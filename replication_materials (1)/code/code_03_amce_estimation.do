
* ssc install estout, replace

use "..\data\data_03_conjoint_processed.dta", clear

* Figure 2

eststo clear

eststo m1: reg rate i.*_bin, vce(cluster respid)

esttab m1 using "..\data\data_04_Figure2_output.txt", replace ///
se noobs nonumbers b(3) wide nogap plain nonotes ///
drop(_cons) ///
coeflabels( ///
1.age_bin  "     Older" ///
2.age_bin  "     Young" ///
1.sex_bin  "     Male" ///
2.sex_bin  "     Female" ///
1.race_bin "     Non-white" ///
2.race_bin "     White" ///
1.educ_bin "     Less than college" ///
2.educ_bin "     Some college or higher" ///
1.lang_bin "     Poor" ///
2.lang_bin "     Good" ///
1.trip_bin "     Violation" ///
2.trip_bin "     No violation" ///
)

* Figure S1

eststo clear

eststo m1: reg rate age_int i.sex_bin i.*_cat, vce(cluster respid)

esttab m1 using "..\data\data_05_FigureS1_output.txt", replace ///
se noobs nonumbers b(3) wide nogap plain nonotes ///
drop(_cons) ///
coeflabels( ///
1.age_bin  "     Older" ///
2.age_bin  "     Young" ///
1.sex_bin  "     Male" ///
2.sex_bin  "     Female" ///
1.race_bin "     Non-white" ///
2.race_bin "     White" ///
1.educ_bin "     Less than college" ///
2.educ_bin "     Some college or higher" ///
1.lang_bin "     Poor" ///
2.lang_bin "     Good" ///
1.trip_bin "     Violation" ///
2.trip_bin "     No violation" ///
)

* Figure S2

eststo clear

gen late = (scenario>8)

eststo m1: reg rate i.*_bin if late==0, vce(cluster respid)
eststo m2: reg rate i.*_bin if late==1, vce(cluster respid)

esttab m1 m2 using "..\data\data_06_FigureS2_output.txt", replace ///
se noobs nonumbers b(3) wide nogap plain nonotes ///
drop(_cons) ///
coeflabels( ///
1.age_bin  "     Older" ///
2.age_bin  "     Young" ///
1.sex_bin  "     Male" ///
2.sex_bin  "     Female" ///
1.race_bin "     Non-white" ///
2.race_bin "     White" ///
1.educ_bin "     Less than college" ///
2.educ_bin "     Some college or higher" ///
1.lang_bin "     Poor" ///
2.lang_bin "     Good" ///
1.trip_bin "     Violation" ///
2.trip_bin "     No violation" ///
)
