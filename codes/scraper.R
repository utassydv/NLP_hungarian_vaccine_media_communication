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

saveRDS(articles_origo, file = 'data/raw/articles_origo.rds')
#TODO rerun without VPN!


# www.index.hu ------------------------------------------------------------

articles_index <- get_urls_from_index(84)
articles_index <- filter(articles_index, date_time > 2020)

pb <- txtProgressBar(min = 0, max = nrow(articles_index), style = 3)
articles_index$content <- lapply(seq_along(articles_index$url),function(i){
  #Sys.sleep(3) # sleep is set otherwise Index blocks me
  
  t<- read_html(articles_index$url[[i]])
  article_text <- t %>% 
    html_nodes('.anti_xsl p , p+ p , div+ p') %>% 
    html_text()
  print(articles_index$url[[i]])
  setTxtProgressBar(pb, i)
  return(article_text)
}
)

saveRDS(articles_index, file= 'data/raw/articles_index.rds')

#TODO refactor to helpers
#TODO try to solve blocking issues 

# www.telex.hu ------------------------------------------------------------

# Collect vaccination related articles
articles_telex <- get_telex_urls('vakcina', 53)

# Getting all the articles
articles_telex <- scrape_articles_telex(articles_telex)

saveRDS(articles_telex, file = 'data/raw/articles_telex.rds')


# www.24.hu ---------------------------------------------------------------

# Collect vaccination related articles
articles_24hu <- get_24hu_urls('vakcina', 42)
articles_24hu <- head(articles_24hu)
pb <- txtProgressBar(min = 0, max = nrow(articles_24hu), style = 3)

# Scrape text from the given urls
articles_24hu$content <- lapply(seq_along(articles_24hu$url), function(i){
  Sys.sleep(1) # sleep is set otherwise 24.hu blocks me
  t <- read_html(articles_24hu$url[[i]])
  article_text <- t %>% html_nodes('.post-body p , p+ ul li') %>% html_text()
  setTxtProgressBar(pb, i)
  return(article_text)
}
)

saveRDS(articles_24hu, file= 'data/raw/articles_24hu.rds')

# www.444.hu ---------------------------------------------------------------

# Collect vaccination related articles
articles_444hu <- get_444hu_urls('vakcina', 28)
pb <- txtProgressBar(min = 0, max = nrow(articles_444hu), style = 3)

# Scrape text from the given urls
articles_444hu$content <- lapply(seq_along(articles_444hu$url),function(i){
  Sys.sleep(1) # sleep is set otherwise 444 blocks me
  t<- read_html(articles_444hu$url[[i]])
  article_text <- t %>% html_node('#content-main') %>% html_nodes('p') %>% html_text()
  article_text <- article_text[1:length(article_text)-1]
  setTxtProgressBar(pb, i)
  return(article_text)
}
)

saveRDS(articles_444hu, file= 'data/raw/articles_444hu.rds')