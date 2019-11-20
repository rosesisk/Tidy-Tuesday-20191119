####################################################
### Tidy Tuesday 19/11/2019: NZ Bird of the Year
### Author: Rose Sisk
### Date created: 20/11/2019
####################################################
library('tidyverse')
library(magrittr)
library(ggthemes)

nz_bird <- readr::read_csv("https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2019/2019-11-19/nz_bird.csv")

### show find top n birds (by n votes overall).
tab10 <- nz_bird %>% group_by(bird_breed) %>%
  filter(!is.na(bird_breed)) %>%
  summarise(n = n()) %>%
  top_n(5, n) %>%
  arrange(desc(n)) 

## extract the names of top 5 birds.
birds <- tab10$bird_breed

daily_votes <- nz_bird %>%
  filter(bird_breed %in% birds) %>%
  group_by(date, bird_breed) %>%
  mutate(rank = as.numeric(substr(vote_rank, 6, 6))) %>%
  summarise(sumvotes = n(), mean_vote = mean(rank)) %>%
  ungroup() %>%
  group_by(bird_breed) %>%
  arrange(bird_breed, date) %>%
  mutate(run_mean = cummean(mean_vote), cum_count = cumsum(sumvotes)) 

## plot using different colours for each bird, higher rank = larger icons.
ggplot(daily_votes, aes(x = date, y = cum_count, colour = bird_breed)) + 
  geom_line(aes(colour = bird_breed)) +
  geom_point(aes(size = (6-run_mean)), show.legend = T) +
    theme_solarized_2(base_size = 10, light = F) +
  scale_colour_solarized("blue") +
  labs(x = "Date", 
       y = "Cumulative vote count",
       title = "Cumulative number of votes for the top 5 species in NZ Bird of the Year",
       col = "Breed",
       subtitle = "The larger the point, the higher the average rank. Average rank is calculated as a rolling daily average.",
       size = "Average daily rank (1-5, 1 = Best)") +
  scale_x_date(date_breaks = "2 days",
               date_labels = "%d %b") +
  scale_size_continuous(breaks = seq(2.35, 3.2, 0.1),
                        labels = as.character(6-seq(2.35, 3.2, 0.1))) +
  guides(size = guide_legend(reverse=TRUE))


  
