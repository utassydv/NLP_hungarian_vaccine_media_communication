library(dplyr)
library(Hmisc)
library(reshape2)
library(tidyverse)
library(tidytext)
library(lubridate)
library(ggplot2)
library(viridis)
library(stringr)
library(igraph)
library(ggraph)

# data preparation ------------------------------------------------------------

articles_telex <- readRDS('data/raw/articles_telex.rds')
articles_24hu <- readRDS('data/raw/articles_24hu.rds')
articles_index<- readRDS('data/raw/articles_index.rds')
articles_origo<- readRDS('data/raw/articles_origo.rds')

# some cleaning
articles_origo <- articles_origo %>% select(title=headline,
                                            url,
                                            date_time=published, 
                                            content, 
                                            site)

# get date from url with stringr
# date_pattern <- "[0-9]{4}/[0-9]{2}/[0-9]{2}"
# articles_444hu$date_time <- str_extract(articles_444hu$url, date_pattern)

# cleaning date_time columns with lubridate
articles_telex$date <- as.Date(ymd_hm(articles_telex$date_time))
articles_24hu$date <- as.Date(ymd_hm(articles_24hu$date_time))
articles_index$date <- as.Date(ymd_hms(articles_index$date_time))
articles_origo$date <- as.Date(ymd_hms(articles_origo$date_time))

# filtering for covid-19 times > 2020

articles_telex <- articles_telex %>% filter(date >= as.Date('2020-01-01'))
articles_24hu <- articles_24hu %>% filter(date >= as.Date('2020-01-01'))
articles_index <- articles_index %>% filter(date >= as.Date('2020-01-01'))
articles_origo <- articles_origo %>% filter(date >= as.Date('2020-01-01'))

# combine all the news sites:
all_sites <- rbind(articles_telex,articles_24hu, articles_index, articles_origo)

# concatenating paragraphs withing articles
articles_telex$content_full <- mapply(paste, articles_telex$content, sep = ' ', collapse =' ')
articles_24hu$content_full <- mapply(paste, articles_24hu$content, sep = ' ', collapse =' ')
articles_index$content_full <- mapply(paste, articles_index$content, sep = ' ', collapse =' ')
articles_origo$content_full <- mapply(paste, articles_origo$content, sep = ' ', collapse =' ')

all_sites$content_full <- mapply(paste, all_sites$content, sep = ' ', collapse =' ')


# word frequency analysis -------------------------------------------------

# unnest_tokens
tidy_articles_telex <- articles_telex %>% unnest_tokens(word, content_full)
tidy_articles_24hu <- articles_24hu %>% unnest_tokens(word, content_full)
tidy_articles_index <- articles_index %>% unnest_tokens(word, content_full)
tidy_articles_origo <- articles_origo %>% unnest_tokens(word, content_full)

# removing stop words
hu_stop_word <- read_csv("https://raw.githubusercontent.com/stopwords-iso/stopwords-hu/master/stopwords-hu.txt", col_names = FALSE)
colnames(hu_stop_word) <- "word"

tidy_articles_telex <- tidy_articles_telex %>% anti_join(hu_stop_word)
tidy_articles_24hu <- tidy_articles_24hu %>% anti_join(hu_stop_word)
tidy_articles_index <- tidy_articles_index %>% anti_join(hu_stop_word)
tidy_articles_origo <- tidy_articles_origo %>% anti_join(hu_stop_word)

# some basic summary
summary_telex <- tidy_articles_telex %>% count(word, sort = TRUE)
summary_24hu <- tidy_articles_24hu %>% count(word, sort = TRUE)
summary_index <- tidy_articles_index %>% count(word, sort = TRUE)
summary_origo <- tidy_articles_origo %>% count(word, sort = TRUE)

# some basic plots
tidy_articles_telex %>%
  count(word, sort = TRUE) %>%
  filter(n > 800) %>%
  mutate(word = reorder(word, n)) %>%
  ggplot(aes(word, n)) + geom_col() + xlab(NULL) + coord_flip()

tidy_articles_origo %>%
  count(word, sort = TRUE) %>%
  filter(n > 2000) %>%
  mutate(word = reorder(word, n)) %>%
  ggplot(aes(word, n)) + geom_col() + xlab(NULL) + coord_flip()

# comparing sites on word frequency
frequency <- bind_rows(mutate(tidy_articles_origo, site = "www.origo.hu"),
                       mutate(tidy_articles_telex, site = "www.telex.hu"),
                       mutate(tidy_articles_index, site = "www.index.hu"),
                       mutate(tidy_articles_24hu, site = "www.24.hu")
                      ) %>%
  count(site, word) %>%
  group_by(site) %>%
  mutate(proportion = n / sum(n)) %>% 
  select(-n) %>% 
  spread(site, proportion)

frequency <- frequency %>% mutate(www.origo.hu = coalesce(www.origo.hu,0),
                            www.telex.hu = coalesce(www.telex.hu,0),
                            www.index.hu = coalesce(www.index.hu,0),
                            www.24.hu = coalesce(www.24.hu,0))

# creating correlation matrix
ds_cor <- frequency %>%
  select(-word) %>% 
  as.matrix() %>%
  rcorr(type = "spearman")
correlation_matrix = as.data.frame(ds_cor$r)


matrix <- as.matrix(correlation_matrix)
diag(matrix) <- NA

heatmap<- ggplot(melt(matrix), aes(x = Var1, y = Var2, fill = value)) +
  geom_tile(colour = "white") +
  labs(x = 'Site', y = 'Site') +
  scale_fill_viridis(alpha = 0.7, begin = 1, end = 0.5, direction = 1, option = "A") +
  #theme_bg() +
  theme(legend.position = "right",
        legend.text = element_text(size=6),
        legend.title =element_text(size=6)
)
ggsave("plots/heatmap.png", plot = heatmap)

p <- ggplot(frequency, aes(x = www.telex.hu, y = www.origo.hu, label = word)) + 
  geom_point(alpha=0.3) +
  geom_text(aes(label=word),hjust=0.5, vjust=-1, alpha=0.5) +
  geom_abline()
ggsave("plots/compare_telex_origo.png", plot = p)

# Sentiment analysis ------------------------------------------------------

positive_words <- read_csv("data/PrecoSenti/PrecoPos.txt", col_names = FALSE) %>%
  mutate(sentiment=1)
negative_words <- read_csv("data/PrecoSenti/PrecoNeg.txt", col_names = FALSE) %>%
  mutate(sentiment=-1)

hungarian_sentiment <- rbind(positive_words, negative_words)
colnames(hungarian_sentiment) <- c('word', 'sentiment')

articles_telex_senitiment <- tidy_articles_telex %>% inner_join(hungarian_sentiment)

sentiments_contribution_telex <- articles_telex_senitiment %>%
  count(word, sentiment) %>% mutate(word = reorder(word, n)) %>%
  filter(n>80) %>% 
  ggplot(aes(word, n * sentiment, fill = sentiment)) + geom_col(show.legend = FALSE) +
  labs(y = "Contribution to sentiment", x = NULL) + coord_flip()
ggsave("plots/sentiments_contribution_telex.png", plot = sentiments_contribution_telex)


# tf-idf ------------------------------------------------------------------

site_words <- all_sites %>% 
  unnest_tokens(word, content_full) %>% 
  count(site, word, sort = TRUE) %>% 
  ungroup()

total_words <- site_words %>% 
  group_by(site) %>% 
  summarise(total = sum(n))

site_words <- left_join(site_words, total_words)

term_freq <- ggplot(site_words, aes(n/total, fill=site)) +
  geom_histogram(show.legend = FALSE) +
  xlim(NA, 0.00015) +
  facet_wrap(~site, ncol=2, scales = "free_y")
ggsave("plots/term_freq.png", plot = term_freq)

freq_by_rank <- site_words %>% 
  group_by(site) %>% 
  mutate(rank = row_number(),
         term_freq = n/total)
  
site_words <- site_words %>% 
  bind_tf_idf(word, site, n)

site_words %>% 
  arrange(desc(tf_idf)) %>% 
  mutate(word = factor(word, levels = rev(unique(word)))) %>% 
  group_by(site) %>% 
  top_n(10) %>% 
  ungroup() %>% 
  ggplot(aes(word, tf_idf, fill = site)) +
  geom_col(show.legend = F) +
  labs(x=NULL, y='tf-idf') +
  facet_wrap(~site, ncol=2, scales = "free") +
  coord_flip()
# there are lot of unmeaningful terms here
# TODO more data cleaning needed!



# n-gramm analysis --------------------------------------------------------

bigrams_all_sites <- all_sites %>% unnest_tokens(bigram, content_full, token = "ngrams", n = 2) 

# getting hungarian stopwords
hu_stop_word <- read_csv("https://raw.githubusercontent.com/stopwords-iso/stopwords-hu/master/stopwords-hu.txt", col_names = FALSE)
colnames(hu_stop_word) <- "word"

# separating bigrams
bigrams_all_sites <- bigrams_all_sites %>% separate(bigram, c("word1", "word2"), sep = " ", remove  = F)

# filtering out bigrams containing stopwords
filtered_bigrams_all_sites <- bigrams_all_sites %>% 
  filter(!word1 %in% hu_stop_word$word) %>% 
  filter(!word2 %in% hu_stop_word$word)

# some basic summary
summary_bigrams_all_sites <- filtered_bigrams_all_sites %>% 
  count(word1, word2, sort = TRUE)


# some basic plots
bigram_freq <- filtered_bigrams_all_sites %>% 
  group_by(site) %>% 
  count(bigram, sort = TRUE) %>%
  top_n(10) %>%
  mutate(
    site = as.factor(site),
    bigram = reorder_within(bigram, n, site)) %>%
  ggplot(aes(bigram, n, fill=site)) + geom_col(show.legend = FALSE) + xlab(NULL) + coord_flip() +
  facet_wrap(~site, ncol=2, scales = "free") +
  scale_x_reordered()

bigram_freq
ggsave("plots/bigram_freq.png", plot = bigram_freq)

bigrams_all_sites %>% 
  filter(word1 == 'nem') %>% 
  count(bigram, sort = TRUE)

not_words <- bigrams_all_sites %>% 
  filter(word1 == 'nem') %>% 
  inner_join(hungarian_sentiment, by = c(word2 = 'word')) %>% 
  count(word2, sentiment, sort = TRUE) %>% 
  ungroup()

not_words_contribution <- not_words %>%  
  mutate(contribution = n * sentiment) %>% 
  arrange(desc(abs(contribution))) %>% 
  head(20) %>% 
  mutate(word2 = reorder(word2, contribution)) %>% 
  ggplot(aes(word2, contribution, fill = contribution > 0)) +
  geom_col(show.legend = F) +
  xlab('Word preceded by "nem" which is the hungarian form of "not".') +
  ylab('Sentiment score * number of occurences') +
  coord_flip()

ggsave("plots/not_words_contribution.png", plot = not_words_contribution)

# network visualizations

# all sites
set.seed(2021)
network_all <- summary_bigrams_all_sites %>%
  filter(n >= 300) %>%
  graph_from_data_frame() %>%
  ggraph(layout = "fr") +
  geom_edge_link(aes(edge_alpha = 0.6, edge_width = n), edge_colour = "cyan4") +
  geom_node_point(size = 5) +
  geom_node_text(aes(label = name), repel = TRUE, 
                 point.padding = unit(0.2, "lines")) +
  theme_void()
network_all
ggsave("plots/network_all.png", plot = network_all)


# TODO: network graphs by articles

# TODO time graphs
# TODO time related stuff

# TODO covid sentiment lexicon
# TODO better hungarian lexicon

# TODO LDA




