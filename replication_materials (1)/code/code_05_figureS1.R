
# install.packages('ggplot2')

library(ggplot2)

data = read.csv(file = '../data/data_05_figureS1.csv')

colnames(data) = c(
  'term', 'estimate', 'std.error'
)

data$term = factor(
  data$term,
  levels = c(
    "     Yes, on a visa",
    "     No",
    "     Yes, overstayed visa",
    "     Yes, unauthorized",
    "Prior trips to U.S.:",
    "a5",
    "     Very high",
    "     High",
    "     Low",
    "     Very low",
    "English proficiency:",
    "a4",
    "     Graduate degree",
    "     4-year college",
    "     2-year college",
    "     High school",
    "     Middle school",
    "     Elementary school",
    "Education:",
    "a3",
    "     Asian",
    "     Hispanic",
    "     Black",
    "     White",
    "Race/ethnicity:",
    "a2",
    "     Female",
    "     Male",
    "Gender:",
    "a1",
    "Age (interval)"
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
  filename = '../figures/figureS1.pdf',
  plot = g,
  width = 6,
  height = 4,
  units = 'in'
)
