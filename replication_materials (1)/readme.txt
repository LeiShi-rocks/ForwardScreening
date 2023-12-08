
CODE:

"code_00_master_1_stata" =
master Stata code

"code_00_master_2_r" =
master R code

"code_01_despcriptives" =
code to generate core demographic characteristics of the data presented in the paper; uses "data_01_survey"

"code_02_conjoint_processing" =
code to process conjoint data; uses "data_02_conjoint_unprocessed"; produces "data_03_conjoint_processed"

"code_03_amce_estimation" =
code to estimate conjoint AMCEs; uses "data_03_conjoint_processed"; produces estimates for Figures 2, S1, and S2

"code_04_Figure2" =
code to produce Figure 2 in the paper; uses "data_04_Figure2"

"code_05_FigureS1" =
code to produce Figure S1 in Supplementary Material; uses "data_05_FigureS1"

"code_06_FigureS2" =
code to produce Figure S2 in Supplementary Material; uses "data_06_FigureS2"

"code_07_imce_estimation_prepare" =
code to produce data formatted to estimate IMCEs; uses "data_03_conjoint_processed"; produces "data_07_imce_estimation"

"code_08_imce_estimation" =
code to obtain IMCE point estimates and draw plausible IMCE values using the normality assumption; uses "data_07_imce_estimation"; produces "data_08_imce_point" and "data_09_imce_draws_normal"

"code_09_imce_estimation_bootstrap" =
code to draw plausible IMCE values using nonparametric bootstrap; uses "data_07_imce_estimation"; produces "data_10_imce_draws__bootstrap"

"code_10_imce_analysis" =
code to estimate IMCE distributions and correlations between preference dimentions; uses "data_08_imce_point"; produce estimates for Tables 2 and 3 in the paper and Table S3 in Supplementary Material

"code_11_FigureS3" =
code to produce Figure S3 in Supplementary Material; uses "data_08_imce_point"

"code_12_Figure3_estimates" =
code to produce estimates for Figure 3; uses "data_01_survey", "data_08_imce_point", "data_09_imce_draws_normal", and "data_10_imce_draws_bootstrap"

"code_13_Figure3" =
code to produce Figure 3 in the paper; uses "data_11_Figure3_point", "data_12_Figure3_normal", and "data_13_Figure3_bootstrap"


DATA:

"data_01_survey" =
survey data without the conjoint component

"data_02_conjoint_unprocessed" =
conjoint data before processing

"data_03_conjoint_processed" =
conjoint data after processing; produced from "data_02_conjoint_unprocessed" using "code_02_conjoint_processing"

"data_04_Figure2" =
saved estimates to create Figure 2 in the paper

"data_05_FigureS1" =
saved estimates to create Figure S1 in Summplementary Material

"data_06_FigureS2" =
saved estimates to create Figure S2 in Summplementary Material

"data_07_imce_estimation" =
data formatted to estimate IMCEs in R; produced from "data_03_conjoint_processed" using "code_07_imce_estimation_prepare"

"data_08_imce_point" =
data containing IMCE point estimates; produced from "data_07_imce_estimation" using "code_08_imce_estimation"

"data_09_imce_draws_normal" =
data containing IMCE plausible values from normal approximation; produced from "data_07_imce_estimation" using "code_08_imce_estimation"

"data_10_imce_draws_bootstrap" =
data containing IMCE plausible values from nonparametric bootstrap; produced from "data_07_imce_estimation" using "code_09_imce_estimation_bootstrap"

"data_11_Figure3_point" =
saved estimates to create Figure 3 (part) in the paper

"data_12_Figure3_normal" =
saved estimates to create Figure 3 (part) in the paper

"data_13_Figure3_bootstrap" =
saved estimates to create Figure 3 (part) in the paper


SYSTEM:

CPU Intel(R) Core(TM) i7-6700HQ CPU @ 2.60GHz, RAM 16.0 GB, OS Windows 10


SOFTWARE:

Codes were run in Stata 16.1 SE and R 4.0.3 / RStudio 1.3

Stata add-on package required: "egenmore", "estout"

R add-on packages required: "ggplot2" 3.3.2, "foreign" 0.8-80, "reshape2" 1.4.4, "dplyr" 1.02, "gridExtra" 2.3, "ggpubr" 0.4.0

Running times: ~164 seconds Stata code, ~485 seconds R code


NOTE:

In the replication process, the numerical results for Table S3 were reproduced but not properly exported because of matrix creation issues (likely, due to differing Stata versions). Therefore, the replication materials include Stata log for Table S3 instead of formatted output. Statistics presented in Table S3 are higlighted in "tableS3_log_text_highlight"
