
# install.packages('ggplot2')
# install.packages('gridExtra')
# install.packages('ggpubr')

library(ggplot2)
library(gridExtra)
library(ggpubr)

data.1 = read.csv('../data/data_11_Figure3_point.csv')
data.2 = read.csv('../data/data_12_Figure3_normal.csv')
data.3 = read.csv('../data/data_13_Figure3_bootstrap.csv')

data = rbind.data.frame(data.1, data.2, data.3)

model = c(
  rep('Point estimates', times=18),
  rep('Draws: normal', times=18),
  rep('Draws: bootstrap', times=18)
)

term = c(
  rep('Young', times=3),
  rep('Female', times=3),
  rep('White', times=3),
  rep('College', times=3),
  rep('English', times=3),
  rep('Legality', times=3)
)

covariate = c(
  c('Education', 'Ethnocentrism', 'Partisanship')
)

coefs = cbind.data.frame(
  model, term, covariate, data[,2:3]
)

colnames(coefs) = c(
  'model', 'term', 'covariate', 'estimate', 'std.error'
)

coefs$term = factor(
  coefs$term,
  levels = c(
    'Legality',
    'English',
    'College',
    'White',
    'Female',
    'Young'
  )
)

g = ggplot(coefs) +
  theme_bw() +
  geom_pointrange(
    aes(
      x = term,
      y = estimate,
      ymin = estimate-1.96*std.error,
      ymax = estimate+1.96*std.error,
      color = model
    ),
    position = position_dodge(width=.6),
    fatten = 1
  ) +
  facet_grid(.~covariate) +
  geom_hline(yintercept=0, color='gray60', linetype=2) +
  coord_flip() +
  guides(color=guide_legend(reverse=T)) +
  theme(strip.text.y=element_text(angle=0)) +
  theme(text=element_text(size=11)) +
  theme(axis.title.x=element_text(
    margin = margin(t=5, r=0, b=0, l=0)
  )) +
  theme(axis.title.y=element_text(
    margin = margin(t=0, r=10, b=0, l=0)
  )) +
  xlab('Immigration preference') +
  ylab('Bivariate coefficient') +
  scale_color_grey(start = .8, end = .2) +
  scale_y_continuous(
    breaks = c(-1, 0, 1),
  ) +
  theme(legend.position='bottom') +
  labs(color="IMCE values:")
g

ggsave(
  filename = '../figures/figure3.pdf',
  plot = g,
  width = 6,
  height = 4,
  units = 'in'
)
