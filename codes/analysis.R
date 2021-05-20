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
library(topicmodels)
library(tidyr)
library(widyr)

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


# combine all the news sites:
all_sites <- rbind(articles_telex,articles_24hu, articles_index, articles_origo)

# filtering for covid-19 times > 2020
all_sites <- all_sites %>% filter(date >= as.Date('2020-01-01'))

# concatenating paragraphs withing articles
all_sites$content_full <- mapply(paste, all_sites$content, sep = ' ', collapse =' ')



# word frequency analysis -------------------------------------------------

# unnest_tokens + remove numbers
tidy_all_sites <- all_sites %>% 
  unnest_tokens(word, content_full) %>% 
  filter(!str_detect(word, "\\d+"))

# removing stop words
hu_stop_word <- read_csv("https://raw.githubusercontent.com/stopwords-iso/stopwords-hu/master/stopwords-hu.txt", col_names = FALSE)
colnames(hu_stop_word) <- "word"

tidy_all_sites <- tidy_all_sites %>% anti_join(hu_stop_word)

# summary plot on frequent words
frequent_words <- tidy_all_sites %>%
  group_by(site) %>% 
  count(word, sort = TRUE) %>%
  top_n(15) %>%
  mutate(word = reorder_within(word, n, site)) %>%
  ggplot(aes(word, n, fill=site)) + geom_col(show.legend = FALSE) + xlab(NULL) + ylab(NULL) +  coord_flip() +
  facet_wrap(~site, ncol=2, scales = "free") +
  scale_x_reordered()
frequent_words
ggsave("plots/frequent_words.png", plot = frequent_words)

# comparing sites on word frequency
frequency <- tidy_all_sites %>% 
  count(site, word) %>%
  group_by(site) %>%
  mutate(proportion = n / sum(n)) %>% 
  select(-n) %>% 
  spread(site, proportion)

colnames(frequency) <- c("word", "www.24.hu", "www.index.hu", "www.origo.hu", "www.telex.hu")

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
heatmap
ggsave("plots/heatmap.png", plot = heatmap)

telex_origo <- ggplot(frequency, aes(x = www.telex.hu, y = www.origo.hu, label = word)) + 
  geom_point(alpha=0.3) +
  geom_text(aes(label=word),hjust=0.5, vjust=-1, alpha=0.5) +
  geom_abline()
ggsave("plots/compare_telex_origo.png", plot = telex_origo)


# Sentiment analysis ------------------------------------------------------
positive_words <- read_csv("data/PrecoSenti/PrecoPos.txt", col_names = FALSE) %>%
  mutate(sentiment=1)
negative_words <- read_csv("data/PrecoSenti/PrecoNeg.txt", col_names = FALSE) %>%
  mutate(sentiment=-1)

hungarian_sentiment <- rbind(positive_words, negative_words)
colnames(hungarian_sentiment) <- c('word', 'sentiment')

tidy_all_sites_sentiment <- tidy_all_sites %>% inner_join(hungarian_sentiment)

sentiment_contributions <- tidy_all_sites_sentiment %>%
  group_by(site) %>% 
  count(word, sentiment) %>% 
  mutate(word = reorder_within(word, abs(n), site)) %>%
  top_n(20) %>% 
  mutate(word = reorder_within(word, n, site)) %>%
  ggplot(aes(word, n * sentiment, fill = sentiment)) + geom_col(show.legend = FALSE) +
  labs(y = "Contribution to sentiment", x = NULL) + coord_flip() +
  facet_wrap(~site, ncol=2, scales = "free") +
  scale_x_reordered()
sentiment_contributions
ggsave("plots/sentiment_contributions.png", plot = sentiment_contributions)

# positive negative issue:
hungarian_sentiment <- hungarian_sentiment %>% 
  filter(!(word %in% c('pozitív','negatív')))
tidy_all_sites_sentiment <- tidy_all_sites %>% inner_join(hungarian_sentiment)

sentiment_contributions_fixed <- tidy_all_sites_sentiment %>%
  group_by(site) %>% 
  count(word, sentiment) %>% 
  mutate(word = reorder_within(word, abs(n), site)) %>%
  top_n(20) %>% 
  mutate(word = reorder_within(word, n, site)) %>%
  ggplot(aes(word, n * sentiment, fill = sentiment)) + geom_col(show.legend = FALSE) +
  labs(y = "Contribution to sentiment", x = NULL) + coord_flip() +
  facet_wrap(~site, ncol=2, scales = "free") +
  scale_x_reordered()
sentiment_contributions_fixed
ggsave("plots/sentiment_contributions_fixed.png", plot = sentiment_contributions_fixed)

#TODO: time series sentiment
#TODO: compare average sentiment of sites


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
term_freq
ggsave("plots/term_freq.png", plot = term_freq)

freq_by_rank <- site_words %>% 
  group_by(site) %>% 
  mutate(rank = row_number(),
         term_freq = n/total)
  
site_words <- site_words %>% 
  bind_tf_idf(word, site, n)

tf_idf <- site_words %>% 
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
tf_idf
ggsave("plots/tf_idf.png", plot = tf_idf)

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
not_words_contribution
ggsave("plots/not_words_contribution.png", plot = not_words_contribution)

negative <- bigrams_all_sites %>% 
  filter(word1 == 'negatív') %>% 
  count(bigram, sort = TRUE)

positive <- bigrams_all_sites %>% 
  filter(word1 == 'pozitív') %>% 
  count(bigram, sort = TRUE)

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



# LDA ---------------------------------------------------------------------

#prepocessing data for LDA

words_all_sites_by_article <-tidy_all_sites %>%
  group_by(site, title) %>% 
  count(word, sort = TRUE)

word_all_sites_by_article_dtm <- words_all_sites_by_article %>%
  cast_dtm(document=title, word, n)

all_sites_dtm <- tidy_all_sites %>% 
  group_by(site, title) %>% 
  count(title, word, sort = TRUE) %>% 
  cast_dtm(document=title, word, n)

# LDA on articles ------------------------------------------------------------

#trying to find two topics
art_lda_2 <- LDA(word_all_sites_by_article_dtm, k = 2, control = list(seed = 1234))
art_topics_2 <- tidy(art_lda_2, matrix = "beta")

art_top_terms_2 <- art_topics_2 %>%
  group_by(topic) %>%
  top_n(10, beta) %>%
  ungroup() %>%
  arrange(topic, -beta)

art_topics_plot_2 <- art_top_terms_2 %>%
  mutate(term = reorder_within(term, beta, topic)) %>%
  ggplot(aes(term, beta, fill = factor(topic))) +
  geom_col(show.legend = FALSE) +
  facet_wrap(~ topic, scales = "free") +
  coord_flip() +
  scale_x_reordered()
art_topics_plot_2
ggsave("plots/art_topics_plot_2.png", plot = art_topics_plot_2)


art_beta_spread_2 <- art_topics_2 %>%
  mutate(topic = paste0("topic", topic)) %>%
  spread(topic, beta) %>%
  filter(topic1 > .001 | topic2 > .001) %>%
  mutate(log_ratio = log2(topic2 / topic1)) %>% 
  mutate(term = reorder(term, abs(log_ratio)))

art_beta_spread_plot_2 <- art_beta_spread_2 %>% 
  filter(abs(log_ratio) > 4 ) %>% 
  mutate(term = reorder(term, log_ratio)) %>% 
  ggplot( aes(term, log_ratio)) + geom_col(show.legend = FALSE) +
  labs(y = "Log2 ratio of beta in topic2/ topic1", x = NULL) + coord_flip() 
art_beta_spread_plot_2
ggsave("plots/art_beta_spread_plot_2.png", plot = art_beta_spread_plot_2)

#TODO: match sites


# trying to find 4 topics
art_lda_4 <- LDA(word_all_sites_by_article_dtm, k = 4, control = list(seed = 1234))
art_topics_4 <- tidy(art_lda_4, matrix = "beta")

art_top_terms_4 <- art_topics_4 %>%
  group_by(topic) %>%
  top_n(10, beta) %>%
  ungroup() %>%
  arrange(topic, -beta)

art_topics_plot_4 <- art_top_terms_4 %>%
  mutate(term = reorder_within(term, beta, topic)) %>%
  ggplot(aes(term, beta, fill = factor(topic))) +
  geom_col(show.legend = FALSE) +
  facet_wrap(~ topic, scales = "free") +
  coord_flip() +
  scale_x_reordered()
art_topics_plot_4
ggsave("plots/art_topics_plot_4.png", plot = art_topics_plot_4)


# trying to find 10 topics
art_lda <- LDA(words_all_sites_by_article_dtm, k = 10, control = list(seed = 1234))
art_topics <- tidy(art_lda, matrix = "beta")

art_top_terms <- art_topics %>%
  group_by(topic) %>%
  top_n(5, beta) %>%
  ungroup() %>%
  arrange(topic, -beta)

art_topics_plot <- art_top_terms %>%
  mutate(term = reorder_within(term, beta, topic)) %>%
  ggplot(aes(term, beta, fill = factor(topic))) +
  geom_col(show.legend = FALSE) +
  facet_wrap(~ topic, scales = "free") +
  coord_flip() +
  scale_x_reordered()
art_topics_plot
ggsave("plots/art_topics_plot.png", plot = art_topics_plot)

# Categorizing articles
# w 4 topics
gamma <- art_lda_4 %>%
  tidy(matrix = "gamma")

tmp <- left_join(gamma, all_sites, by = c("document" = "title"))

plot_4 <- tmp %>% 
  mutate(site = reorder(site, gamma * topic)) %>%
  ggplot(aes(factor(topic), gamma)) +
  geom_boxplot() +
  facet_wrap(~ site) +
  labs(x = "Topic",
       y = "# of messages where this was the highest % topic")
plot_4
ggsave("plots/plot_4.png", plot = plot_4)


########
# Categorizing articles
# w 4 topics
gamma <- art_lda_2 %>%
  tidy(matrix = "gamma")

tmp <- left_join(gamma, all_sites, by = c("document" = "title"))

plot_2 <- tmp %>% 
  mutate(site = reorder(site, gamma * topic)) %>%
  ggplot(aes(factor(topic), gamma)) +
  geom_boxplot() +
  facet_wrap(~ site) +
  labs(x = "Topic",
       y = "# of messages where this was the highest % topic")
plot_2
ggsave("plots/plot_2.png", plot = plot_2)


# more meaningfull graph visualisations -----------------------------------
library(widyr)

#with content of articles
tidy_all_sites_by_article <- all_sites %>% 
  unnest_tokens(word, content_full) %>% 
  filter(!str_detect(word, "\\d+")) %>% 
  anti_join(hu_stop_word) %>% 
  count(title, word)

coocurring_words <- tidy_all_sites_by_article %>% pairwise_count(word, title, sort = TRUE, upper = FALSE)
saveRDS(coocurring_words, file = 'data/raw/coocurring_words_content.rds')


set.seed(1234)
coocurring_words_content_plot <- coocurring_words %>%
  filter(n >= 750) %>%
  graph_from_data_frame() %>%
  ggraph(layout = "fr") +
  geom_edge_link(aes(edge_alpha = n, edge_width = n), edge_colour = "cyan4") +
  geom_node_point(size = 5) +
  geom_node_text(aes(label = name), repel = TRUE, 
                 point.padding = unit(0.2, "lines")) +
  theme_void()
coocurring_words_content_plot
ggsave("plots/coocurring_words_content_plot.png", plot = coocurring_words_content_plot)

# with content of titles
tidy_all_sites_by_title <- all_sites %>% 
  unnest_tokens(word, title, drop=F) %>% 
  filter(!str_detect(word, "\\d+")) %>% 
  anti_join(hu_stop_word) %>% 
  count(title, word)

coocurring_words_title <- tidy_all_sites_by_title %>% pairwise_count(word, title, sort = TRUE, upper = FALSE)
saveRDS(coocurring_words_title, file = 'data/raw/coocurring_words_title.rds')

set.seed(1234)

coocurring_words_content_plot_title <- coocurring_words_title %>%
  filter(n >= 15) %>%
  graph_from_data_frame() %>%
  ggraph(layout = "fr") +
  geom_edge_link(aes(edge_alpha = n, edge_width = n), edge_colour = "cyan4") +
  geom_node_point(size = 5) +
  geom_node_text(aes(label = name), repel = TRUE, 
                 point.padding = unit(0.2, "lines")) +
  theme_void()
coocurring_words_content_plot_title
ggsave("plots/coocurring_words_content_plot_title.png", plot = coocurring_words_content_plot_title)


# TODOS -------------------------------------------------------------------

# TODO time graphs
# TODO time related stuff

# TODO covid sentiment lexicon
# TODO better hungarian lexicon