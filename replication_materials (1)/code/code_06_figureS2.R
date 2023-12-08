
# install.packages('ggplot2')

library(ggplot2)

data = read.csv(file = '../data/data_06_figureS2.csv')

data.1 = data[,1:3]
data.2 = data[,c(1,4,5)]

n = dim(data.1)[1]

colnames(data.2) = colnames(data.1)

data.3 = rbind.data.frame(data.1, data.2)

order = c(
  rep('First half', times=n),
  rep('Second half', times=n)
)

coefs = cbind.data.frame(order, data.3)

colnames(coefs) = c(
  'order', 'term', 'estimate', 'std.error'
)

coefs$term = factor(
  coefs$term,
  levels = c(
    "     No violation",
    "     Violation",
    "Prior trips to U.S.:",
    "a5",
    "     Good",
    "     Poor",
    "English proficiency:",
    "a4",
    "     Some college or higher",
    "     Less than college",
    "Education:",
    "a3",
    "     White",
    "     Non-white",
    "Race/ethnicity:",
    "a2",
    "     Female",
    "     Male",
    "Gender:",
    "a1",
    "     Young",
    "     Older",
    "Age:"
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
      color = order
    ),
    position = position_dodge(width=1),
    fatten = 1
  ) +
  geom_hline(yintercept=0, color='gray60', linetype=2) +
  coord_flip() +
  theme(axis.title.y=element_blank()) +
  theme(strip.text.y=element_text(angle=0)) +
  theme(text=element_text(size=12)) +
  ylab('AMCE estimate') +
  scale_color_grey(start=.3, end=.7) +
  scale_x_discrete(
    drop = FALSE,
    labels = c(
      "a1" = " ",
      "a2" = " ",
      "a3" = " ",
      "a4" = " ",
      "a5" = " "
    )
  ) +
  theme(axis.text.y=element_text(hjust=0)) +
  labs(color="Profile order:")
g

ggsave(
  filename = '../figures/figureS2.pdf',
  plot = g,
  width = 6,
  height = 4,
  units = 'in'
)
