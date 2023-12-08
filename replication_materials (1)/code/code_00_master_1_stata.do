
clear all

* ssc install egenmore, replace
* ssc install estout, replace

do "code_01_descriptives.do"

do "code_02_conjoint_processing.do"

do "code_03_amce_estimation.do"

do "code_07_imce_estimation_prepare.do"

do "code_10_imce_analysis.do"

do "code_12_Figure3_estimates.do"
