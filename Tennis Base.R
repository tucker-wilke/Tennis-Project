library(tidyverse)
library(readr)

t_match_score <- read_csv("/Users/tuckerwilke/Desktop/Tennis/match_scores_1991-2016_unindexed.csv")
t_match_stats <-read_csv("/Users/tuckerwilke/Desktop/Tennis/match_stats_1991-2016_unindexed.csv")


year_match <- separate(t_match_score, col=1, sep= "-", into = c("year", "tourney_id")) 
nf_peak <- filter(year_match, year > 2004 & year < 2011)
nf_peak_matches <- filter(nf_peak, winner_name %in% c("Roger Federer", "Rafael Nadal") | loser_name%in% c("Roger Federer", "Rafael Nadal"))
nf_peak_matches_relevant <- select(nf_peak_matches, col= 1,4, 6, 9, 12, 13, 14, 15, 16, 17, 18, 19, 24 ) 


nf_peak_stats <- nf_peak_matches_relevant %>%
  inner_join(t_match_stats, by = "match_id")

fed_peak_matches <- filter(nf_peak_matches_relevant, winner_name == "Roger Federer" | loser_name == "Roger Federer")
fed_win_rate <- table(fed_peak_matches$winner_name)
nadal_peak_matches <-filter(nf_peak_matches_relevant, winner_name == "Rafael Nadal" | loser_name == "Rafael Nadal")
nadal_win_rate <-table(nadal_peak_matches$winner_name)
fed_win_rate
674/786
nadal_win_rate
650/762
fedal_wins <- filter(nf_peak_matches, winner_name %in% c("Roger Federer","Rafael Nadal"), !(loser_name %in% c("Roger Federer", "Rafael Nadal")))
fedal_losses <-filter(nf_peak_matches, loser_name %in% c("Roger Federer", "Rafael Nadal"), !(winner_name %in% c("Roger Federer", "Rafael Nadal")))
fedal_beaters <- count(fedal_losses, 'winner_name')
fedal_beaters <- as_tibble(fedal_beaters)
fedal_losers <-count(fedal_wins, 'loser_name')
fedal_beaters <- fedal_beaters %>% 
  dplyr::rename(name = winner_name) %>% 
  dplyr::rename(freq_win = freq)
fedal_losers <- fedal_losers %>%
  dplyr::rename(name = loser_name) %>%
  dplyr::rename(freq_loss = freq)
fedal_net <-full_join(fedal_beaters, fedal_losers, by = "name") %>%
  replace_na(list(freq_loss=0, freq_win=0)) %>%
  mutate(net_record=freq_loss - freq_win)
fedal_net%>% arrange(desc(net_record))
fedal_graveyard <- filter(fedal_net, net_record >= 19)
ggplot(data = fedal_graveyard) + geom_col(mapping = aes(x=name, y=net_record))

fedal_net%>% filter(freq_loss>=10) %>%
  arrange(freq_win)


