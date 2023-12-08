
library(foreign)

data = read.dta('../data/data_07_imce_estimation.dta')

data$respid = as.factor(data$respid)
n = length(levels(data$respid))

# Estimate IMCEs for each respondent/attribute

coef = matrix(0, nrow=n, ncol=6)
se = matrix(0, nrow=n, ncol=6)
for (i in 1:n) {
  mod = lm(
    rate ~ conj_young,
    data = data,
    subset = (respid==levels(data$respid)[i])
  )
  summ = summary(mod)
  coef[i,1] = summ$coefficients[2,1]
  se[i,1] = summ$coefficients[2,2]
  mod =  lm(
    rate ~ conj_femal,
    data = data,
    subset = (respid==levels(data$respid)[i])
  )
  summ = summary(mod)
  coef[i,2] = summ$coefficients[2,1]
  se[i,2] = summ$coefficients[2,2]
  mod =  lm(
    rate ~ conj_white,
    data = data,
    subset = (respid==levels(data$respid)[i])
  )
  summ = summary(mod)
  coef[i,3] = summ$coefficients[2,1]
  se[i,3] = summ$coefficients[2,2]
  mod =  lm(
    rate ~ conj_collg,
    data = data,
    subset = (respid==levels(data$respid)[i])
  )
  summ = summary(mod)
  coef[i,4] = summ$coefficients[2,1]
  se[i,4] = summ$coefficients[2,2]
  mod =  lm(
    rate ~ conj_engls,
    data = data,
    subset = (respid==levels(data$respid)[i])
  )
  summ = summary(mod)
  coef[i,5] = summ$coefficients[2,1]
  se[i,5] = summ$coefficients[2,2]
  mod =  lm(
    rate ~ conj_authr,
    data = data,
    subset = (respid==levels(data$respid)[i])
  )
  summ = summary(mod)
  coef[i,6] = summ$coefficients[2,1]
  se[i,6] = summ$coefficients[2,2]
}

# Export estimates to CSV

coef.exp = cbind(levels(data$respid), coef, se)
colnames(coef.exp) = c(
  'respid',
  'imce_young',
  'imce_femal',
  'imce_white',
  'imce_collg',
  'imce_engls',
  'imce_authr',
  'imce_young_se',
  'imce_femal_se',
  'imce_white_se',
  'imce_collg_se',
  'imce_engls_se',
  'imce_authr_se'
)
write.csv(
  x = coef.exp,
  file = '../data/data_08_imce_point.csv',
  row.names = FALSE
)

# Draw plausible IMCE values
# Normality assumption
# Mean = point, SD = SE

draws.data = NULL
set.seed(1710171219)
for (i in 1:n) {
  temp = cbind(levels(data$respid)[i], 0:100)
  for (j in 1:6) {
    temp = cbind(
      temp,
      c(0, rnorm(n=100, mean=coef[i,j], sd=se[i,j]))
    )
  }
  draws.data = rbind(draws.data, temp)
}

# Export drawn values to CSV

colnames(draws.data) = c(
  'respid',
  'impno',
  'imce_young',
  'imce_femal',
  'imce_white',
  'imce_collg',
  'imce_engls',
  'imce_authr'
)
write.csv(
  x = draws.data,
  file = '../data/data_09_imce_draws_normal.csv',
  row.names = FALSE
)
