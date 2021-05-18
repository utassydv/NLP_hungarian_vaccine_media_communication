library(dplyr)
library(Hmisc)
library(reshape2)
library(tidyverse)
library(tidytext)
library(ggplot2)
library(viridis)

# loading data ------------------------------------------------------------

articles_telex<- readRDS('data/raw/articles_telex.rds')
articles_24hu <- readRDS('data/raw/articles_24hu.rds')
articles_444hu <- readRDS('data/raw/articles_444hu.rds')
articles_index<- readRDS('data/raw/articles_index.rds')
articles_origo<- readRDS('data/raw/articles_origo.rds')

# concatenating paragraphs withing articles
articles_telex$content_full <- mapply(paste, articles_telex$content, sep = ' ', collapse =' ')
articles_24hu$content_full <- mapply(paste, articles_24hu$content, sep = ' ', collapse =' ')
articles_444hu$content_full <- mapply(paste, articles_444hu$content, sep = ' ', collapse =' ')
articles_index$content_full <- mapply(paste, articles_index$content, sep = ' ', collapse =' ')
articles_origo$content_full <- mapply(paste, articles_origo$content, sep = ' ', collapse =' ')


# unnest_tokens
articles_telex <- articles_telex %>% unnest_tokens(word, content_full)
articles_24hu <- articles_24hu %>% unnest_tokens(word, content_full)
articles_444hu <- articles_444hu %>% unnest_tokens(word, content_full)
articles_index <- articles_index %>% unnest_tokens(word, content_full)
articles_origo <- articles_origo %>% unnest_tokens(word, content_full)



# removing stop words
hu_stop_word <- read_csv("https://raw.githubusercontent.com/stopwords-iso/stopwords-hu/master/stopwords-hu.txt", col_names = FALSE)
colnames(hu_stop_word) <- "word"

articles_telex <- articles_telex %>% anti_join(hu_stop_word)
articles_24hu <- articles_24hu %>% anti_join(hu_stop_word)
articles_444hu <- articles_444hu %>% anti_join(hu_stop_word)
articles_index <- articles_index %>% anti_join(hu_stop_word)
articles_origo <- articles_origo %>% anti_join(hu_stop_word)

# some basic summary
summary_telex <- articles_telex %>% count(word, sort = TRUE)
summary_24hu <- articles_24hu %>% count(word, sort = TRUE)
summary_444hu <- articles_444hu %>% count(word, sort = TRUE)
summary_index <- articles_index %>% count(word, sort = TRUE)
summary_origo <- articles_origo %>% count(word, sort = TRUE)

# some basic plots
articles_telex %>%
  count(word, sort = TRUE) %>%
  filter(n > 800) %>%
  mutate(word = reorder(word, n)) %>%
  ggplot(aes(word, n)) + geom_col() + xlab(NULL) + coord_flip()

articles_origo %>%
  count(word, sort = TRUE) %>%
  filter(n > 2000) %>%
  mutate(word = reorder(word, n)) %>%
  ggplot(aes(word, n)) + geom_col() + xlab(NULL) + coord_flip()

# comparing sites on word frequency
frequency <- bind_rows(mutate(articles_origo, site = "www.origo.hu"),
                       mutate(articles_telex, site = "www.telex.hu"),
                       mutate(articles_index, site = "www.index.hu"),
                       mutate(articles_24hu, site = "www.24.hu"),
                       mutate(articles_444hu, site = "www.444.hu"),
                      ) %>%
  count(site, word) %>%
  group_by(site) %>%
  mutate(proportion = n / sum(n)) %>% 
  select(-n) %>% 
  spread(site, proportion)

frequency <- frequency %>% mutate(www.origo.hu = coalesce(www.origo.hu,0),
                            www.telex.hu = coalesce(www.telex.hu,0),
                            www.index.hu = coalesce(www.index.hu,0),
                            www.24.hu = coalesce(www.24.hu,0),
                            www.444.hu = coalesce(www.444.hu,0))

# creating correlation matrix
ds_cor <- frequency %>%
  select(-word) %>% 
  as.matrix() %>%
  rcorr(type = "spearman")
correlation_matrix = as.data.frame(ds_cor$r)

heatmap(as.matrix(correlation_matrix))

#corrplot(matrix, diag = FALSE, cl.lim=c(0.2,0.4))#, type='lower', order = 'hclust', tl.srt = 45)

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
ggsave("plot.png", plot = p)

articles_telex_senitiment <- articles_telex %>% inner_join(hungarian_sentiment)

sentiments_contribution_telex <- example %>%
  count(word, sentiment) %>% mutate(word = reorder(word, n)) %>%
  filter(n>80) %>% 
  ggplot(aes(word, n * sentiment, fill = sentiment)) + geom_col(show.legend = FALSE) +
  labs(y = "Contribution to sentiment", x = NULL) + coord_flip()
ggsave("plots/sentiments_contribution_telex.png", plot = sentiments_contribution_telex)


