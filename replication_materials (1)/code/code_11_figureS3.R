
# install.packages('ggplot2')
# install.packages('reshape2')
# install.packages('dplyr')

library(ggplot2)
library(reshape2)
library(dplyr)

data = read.csv('../data/data_08_imce_point.csv')

data.point = data[,c(1:7)]

data.long = melt(data.point, id.vars='respid')

data.long$variable = recode(
  data.long$variable,
  imce_young = "Young",
  imce_femal = "Female",
  imce_white = "White",
  imce_collg = "College",
  imce_engls = "English",
  imce_authr = "Legality"
)

g = ggplot(data.long, aes(value)) +
  stat_density(geom='line', size=1) +
  facet_wrap(.~variable, nrow=2) +
  theme_bw() +
  scale_x_continuous(
    limits = c(-11, 11)
  ) +
  scale_y_continuous(limits=c(0, 0.8)) +
  xlab('IMCE point estimates') +
  ylab('Empirical density')
g

ggsave(
  filename = '../figures/figureS3.pdf',
  plot = g,
  width = 6,
  height = 4,
  units = 'in'
)
