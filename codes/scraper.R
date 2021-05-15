library(jsonlite)
library(rvest)


# www.origo.hu ------------------------------------------------------------

# Collect vaccination related articles
articles_origo <- fromJSON('https://www.origo.hu/api/article-search?h=www.origo.hu&h=kepek.origo.hu&iai=true&q=vakcina&hits=8000')$result
articles_origo <- head(articles_origo) #TODO run without this

# Scrape text from the given urls
articles_origo$content<- lapply(articles_origo$url,function(x){
  t <- read_html(x)
  article_text <- t %>% html_nodes('p') %>% html_text()
  return(article_text)
}
)

saveRDS(articles_origo, file = 'data/raw/articles_origo.rds')


