library(jsonlite)
library(rvest)
library(data.table)
library(utils)

source("codes/scraper_helpers.R")


# www.origo.hu ------------------------------------------------------------

# Collect vaccination related articles
articles_origo <- fromJSON('https://www.origo.hu/api/article-search?h=www.origo.hu&h=kepek.origo.hu&iai=true&q=vakcina&hits=8000')$result
pb <- txtProgressBar(min = 0, max = nrow(articles_origo), style = 3)

# Getting all the articles
articles_origo <- scrape_articles_origo(articles_origo)

articles_origo$site <- 'origo'
saveRDS(articles_origo, file = 'data/raw/articles_origo.rds')
#TODO rerun without VPN!


# www.index.hu ------------------------------------------------------------

articles_index <- get_urls_from_index(84)
#articles_index <- filter(articles_index, date_time > 2020)

articles_index <- scrape_articles_index(articles_index)

articles_index$site <- 'index'
saveRDS(articles_index, file= 'data/raw/articles_index.rds')

# www.telex.hu ------------------------------------------------------------

# Collect vaccination related articles
articles_telex <- get_telex_urls('vakcina', 53)

# Getting all the articles
articles_telex <- scrape_articles_telex(articles_telex)

articles_telex$site <- 'telex'
saveRDS(articles_telex, file = 'data/raw/articles_telex.rds')


# www.24.hu ---------------------------------------------------------------

# Collect vaccination related articles
articles_24hu <- get_24hu_urls('vakcina', 52)

# Scrape text from the given urls
articles_24hu <- scrape_articles_24hu(articles_24hu)

articles_24hu$site <- '24hu'
saveRDS(articles_24hu, file= 'data/raw/articles_24hu_new.rds')

# www.444.hu ---------------------------------------------------------------

# Collect vaccination related articles
articles_444hu <- get_444hu_urls('vakcina', 28)

articles_444hu <- scrape_articles_444hu(articles_444hu)

articles_444hu$site <- '444hu'
saveRDS(articles_444hu, file= 'data/raw/articles_444hu.rds')