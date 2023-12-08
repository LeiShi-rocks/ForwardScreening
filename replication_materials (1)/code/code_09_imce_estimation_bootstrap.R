
library(foreign)

data = read.dta('../data/data_07_imce_estimation.dta')

data$respid = as.factor(data$respid)
n = length(levels(data$respid))

# Draw plausible IMCE values
# Nonparametric bootstrap

data.i = NULL
data.b = NULL
b.young = matrix(0, nrow=100, ncol=1)
b.femal = matrix(0, nrow=100, ncol=1)
b.white = matrix(0, nrow=100, ncol=1)
b.collg = matrix(0, nrow=100, ncol=1)
b.engls = matrix(0, nrow=100, ncol=1)
b.authr = matrix(0, nrow=100, ncol=1)
draws.data = NULL
set.seed(1710171219)
for (i in 1:n) {
  data.i = data[which(data$respid==levels(data$respid)[i]),]
  b = 1
  # Repeat until there are 100 valid draws
  while (b<101) {
    data.b = data.i[sample(nrow(data.i),30,replace=T),]
    mod = lm(rate~conj_young, data=data.b)
    if (is.na(mod$coefficients[2])==0) {
      b.young[b] = mod$coefficients[2]
      b = b + 1
    }
  }
  b = 1
  while (b<101) {
    data.b = data.i[sample(nrow(data.i),30,replace=T),]
    mod = lm(rate~conj_femal, data=data.b)
    if (is.na(mod$coefficients[2])==0) {
      b.femal[b] = mod$coefficients[2]
      b = b + 1
    }
  }
  b = 1
  while (b<101) {
    data.b = data.i[sample(nrow(data.i),30,replace=T),]
    mod = lm(rate~conj_white, data=data.b)
    if (sum(is.na(mod$coefficients))==0) {
      b.white[b] = mod$coefficients[2]
      b = b + 1
    }
  }
  b = 1
  while (b<101) {
    data.b = data.i[sample(nrow(data.i),30,replace=T),]
    mod = lm(rate~conj_collg, data=data.b)
    if (is.na(mod$coefficients[2])==0) {
      b.collg[b] = mod$coefficients[2]
      b = b + 1
    }
  }
  b = 1
  while (b<101) {
    data.b = data.i[sample(nrow(data.i),30,replace=T),]
    mod = lm(rate~conj_engls, data=data.b)
    if (is.na(mod$coefficients[2])==0) {
      b.engls[b] = mod$coefficients[2]
      b = b + 1
    }
  }
  b = 1
  while (b<101) {
    data.b = data.i[sample(nrow(data.i),30,replace=T),]
    mod = lm(rate~conj_authr, data=data.b)
    if (is.na(mod$coefficients[2])==0) {
      b.authr[b] = mod$coefficients[2]
      b = b + 1
    }
  }
  temp = cbind.data.frame(
    levels(data$respid)[i],
    1:100,
    b.young,
    b.femal,
    b.white,
    b.collg,
    b.engls,
    b.authr
  )
  temp = rbind.data.frame(
    c(levels(data$respid)[i], rep(0, times=8)), temp
  )
  draws.data = rbind.data.frame(draws.data, temp)
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
  file = '../data/data_10_imce_draws_bootstrap.csv',
  row.names = FALSE
)
