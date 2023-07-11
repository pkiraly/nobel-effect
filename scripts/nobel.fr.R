library(tidyverse)

df <- read_tsv('data/NobelLiterature.bnf.tsv')
dfyear <- read_csv('data/nobel-years.csv')

dfyear %>% 
  left_join(df) %>% 
  filter(!is.na(prize_year)) %>% 
  mutate(editions = ifelse(language != 'French', editions, NA)) %>% 
  mutate(
    is_nobel_year = year == prize_year,
    label = sprintf("%d: %s", prize_year, name)
  ) %>% 
  ggplot(aes(x = year, y = reorder(label, desc(label)))) +
  geom_point(aes(size = editions, color=is_nobel_year), alpha=0.6) +
  theme_bw() +
  theme(legend.position = 'bottom') +
  labs(
    title='Nobel prize winners (1990-2022) in French translations',
    subtitle = paste(
      'Based on the BNF (collected by Etienne Cavalié).',
      'Colour blue denotes the year of winning the prize.',
      sep = "\n"
    )
    ) +
  ylab(NULL) +
  xlab('publication year of the translation') +
  guides(color = "none") +
  scale_x_continuous(
    limits=c(1950, 2023), 
    breaks = seq(1950, 2025, 10)
  )

ggsave('images/nobel.fr.png', width = 2400, height = 2000, 
       units = "px", dpi = 300)
