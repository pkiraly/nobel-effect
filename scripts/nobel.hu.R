library(tidyverse)

df <- read_csv('data/nobel.oszk.csv')
dfyear <- read_csv('data/nobel-years.csv')

dfyear %>% 
  left_join(df) %>% 
  filter(!is.na(prize_year)) %>% 
  mutate(editions = ifelse(language != 'Hungarian', editions, NA)) %>% 
  mutate(
    is_nobel_year = year == prize_year,
    label = sprintf("%d: %s", prize_year, name)
  ) %>% 
  ggplot(aes(x = year, y = reorder(label, desc(label)))) +
  geom_point(aes(size = editions, color=is_nobel_year), alpha=0.6) +
  theme_bw() +
  theme(legend.position = 'bottom') +
  labs(
    title='Nobel prize winners (1990-2022) in Hungarian translations',
    subtitle = paste(
      'Based on the National Széchényi Library catalogue.',
      'Colour blue denotes the year of winning the prize.',
      sep = "\n"
    )
  ) +
  ylab(NULL) +
  xlab('publication year of the translation') +
  guides(color = "none", size = "legend") +
  scale_x_continuous(
    limits=c(1950, 2023), 
    breaks = seq(1950, 2025, 10)
  ) +
  scale_size_continuous(breaks=c(1, 3, 5, 7))

ggsave('images/nobel.hu.png', width = 2400, height = 2000, 
       units = "px", dpi = 300)
