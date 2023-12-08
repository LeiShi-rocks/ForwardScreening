
# install.packages('ggplot2')

library(ggplot2)

data = read.csv(file = '../data/data_04_Figure2.csv')

colnames(data) = c(
  'term', 'estimate', 'std.error'
)

data$term = factor(
  data$term,
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

g = ggplot(data) +
  theme_bw() +
  geom_pointrange(
    aes(
      x = term,
      y = estimate,
      ymin = estimate-1.96*std.error,
      ymax = estimate+1.96*std.error,
    ),
    fatten = 2
  ) +
  geom_hline(yintercept=0, color='gray60', linetype=2) +
  coord_flip() +
  theme(axis.title.y=element_blank()) +
  theme(strip.text.y=element_text(angle=0)) +
  theme(text=element_text(size=12)) +
  ylab('AMCE estimate') +
  scale_color_grey(start=.2, end=.8) +
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
  theme(axis.text.y=element_text(hjust=0))
g

ggsave(
  filename = '../figures/figure2.pdf',
  plot = g,
  width = 6,
  height = 4,
  units = 'in'
)
