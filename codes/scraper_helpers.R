# origo.hu helpers --------------------------------------------------------


scrape_articles_origo <- function(articles_origo){
  
  pb <- txtProgressBar(min = 0, max = nrow(articles_origo), style = 3)
  
  articles_origo$content <- lapply(seq_along(articles_origo$url),function(i){
    t <- tryCatch(
      {
        read_html(articles_origo$url[[i]])
      },
      error=function(cond) 
      {
        message(paste("URL caused a error:", articles_origo$url[[i]]))
        message("Here's the original error message:")
        message(cond)
        return(NA)
      },
      warning=function(cond) 
      {
        message(paste("URL caused a warning:", articles_origo$url[[i]]))
        message("Here's the original warning message:")
        message(cond)
        return(NA)
      },
      finally={
        #setTxtProgressBar(pb, i)
      }
    )
    
    article_text <- tryCatch(
      {
        t %>% html_nodes('p') %>% html_text()
      },
      error=function(cond)
      {
        message("ERROR: Skipping html_node")
      },
      warning=function(cond) 
      {
        message("WARNING: Skipping html_node")
      },
      finally={
        setTxtProgressBar(pb, i)
      }
    )
    return(article_text)
  }
  )
  return(articles_origo)
}

# index.hu helpers --------------------------------------------------------
require(httr)
library(dplyr)
library(utils)

get_urls_from_index <- function(number_of_pages){
  pb <- txtProgressBar(min = 0, max = number_of_pages, style = 3)
  articles_df <- data.frame(list._mod_ts=character(), list.cim=character(), list.url=character())
  for ( page in 0:number_of_pages){
    data = list(
      `datum` = '2021-05-15',
      `rovat` = '24ora/',
      `url_params[ig]` = '2021-05-15',
      `url_params[pepe]` = '1',
      `url_params[rovat]` = '',
      `url_params[s]` = 'vakcina',
      `url_params[tol]` = '1999-01-01',
      `url_params[alllowRovatChoose]` = '0',
      `url_params[profil]` = '',
      `url_params[cimke]` = '',
      `url_params[word]` = '1',
      `url_params[p]` = page
    )
    t <- POST('https://index.hu/api/json/', body = data)
    df <- as.data.frame(fromJSON(content(t,'text', encoding = "UTF-8")))
    
    df <- df %>% 
      select(list._mod_ts, list.cim, list.url)
    
    df$list.url <- paste0('https:',df$list.url)
    
    articles_df <- bind_rows(articles_df,df)
    setTxtProgressBar(pb, page)
  }
  
  articles_df <- rename(articles_df, 
                        date_time=list._mod_ts, title=list.cim,url=list.url)
  
  return(articles_df)
}

scrape_articles_index <- function(articles_index){
  ua <- user_agent("Mozilla/5.0 (Windows NT 10.0; Win64; x64) AppleWebKit/537.36 (KHTML, like Gecko) Chrome/58.0.3029.110 Safari/537.36")
  
  pb <- txtProgressBar(min = 0, max = nrow(articles_index), style = 3)
  
  
  articles_index$content <- lapply(seq_along(articles_index$url),function(i){
    #Sys.sleep(3) # sleep is set otherwise Index blocks me
    
    t <- NULL
    
    try(
      t<- read_html(articles_index$url[[i]], user_agent=ua)
    )
    
    if (is.null(t)){
      attempt <- 1
      while(is.null(t) && attempt <= 5){
        Sys.sleep(sample(1:5, 1))
        attempt <- attempt + 1
        try(
          t<- read_html(articles_index$url[[i]], user_agent=ua)
        )
      }
    }
    
    if (is.null(t)){
      article_text <- NaN
      message("MISSED ARTICLE", articles_index$url[[i]])
      setTxtProgressBar(pb, i)
    }
    else{
      article_text <- t %>% 
        html_nodes('.anti_xsl p , p+ p , div+ p') %>% 
        html_text()
      print(articles_index$url[[i]])
      setTxtProgressBar(pb, i)
    }
    
    return(article_text)
  }
  )
  
  return(articles_index)
}



# telex.hu helpers --------------------------------------------------------
get_urls_from_one_page_telex  <- function(my_url) {
  print(my_url)
  t <- read_html(my_url)
  boxes <- t %>% html_nodes('.article')
  x <- boxes[[1]]
  boxes_df <- lapply(boxes, function(x){
    t_list <- list()
    t_list[['title']] <- x %>% html_nodes(".list-item") %>% html_nodes("a.list-item__title") %>% html_text()
    t_list[['url']] <- paste0('https://telex.hu',x %>% html_nodes(".list-item") %>% html_nodes("a.list-item__title") %>% html_attr('href'))
    t_list[['date_time']]  <- x  %>% html_nodes(".list-item") %>% html_nodes(".article_date span")  %>% html_text()
    return(data.frame(t_list))  
  })
  
  df <- rbindlist(boxes_df)
  return(df)
}

get_telex_urls <- function(searchterm, page_to_download) {
  # create links
  links_to_get <- 
    paste0('https://telex.hu/archivum?oldal=', seq(1, page_to_download) ,'&term=',searchterm)
  ret_df <- rbindlist(lapply(links_to_get, get_urls_from_one_page_telex))
  return(ret_df)
}

scrape_articles_telex <- function(articles_telex){
  
  pb <- txtProgressBar(min = 0, max = nrow(articles_telex), style = 3)
  
  articles_telex$content<- lapply(seq_along(articles_telex$url),function(i){
    t <- read_html(articles_telex$url[[i]])
    article_text <- t %>% html_nodes('.article-html-content') %>%html_nodes('p') %>% html_text()
    setTxtProgressBar(pb, i)
    return(article_text)
  }
  )
  return(articles_telex)
}

# Scrape text from the given urls

  

# 24.hu helpers -----------------------------------------------------------
get_urls_from_one_page_24hu  <- function(my_url) {
  print(my_url)
  t <- read_html(my_url)
  boxes <- t %>% html_nodes('.-hasImg')
  boxes <- append(boxes, t %>% html_nodes('.-noImg')) 
  x <- boxes[[1]]
  boxes_df <- lapply(boxes, function(x){
    t_list <- list()
    t_list[['title']] <- x %>% html_nodes(".m-articleWidget__link") %>% html_text()
    t_list[['url']] <- x %>% html_nodes(".m-articleWidget__link") %>% html_attr('href')
    t_list[['date_time']] <- x  %>%  html_nodes(".a-date") %>% html_text()
    return(data.frame(t_list))  
  })
  
  df <- rbindlist(boxes_df)
  return(df)
}

get_24hu_urls <- function(searchterm, page_to_download) {
  # create links
  links_to_get <- paste0('https://24.hu/page/', seq(1, page_to_download) ,'/?s=', searchterm)
  ret_df <- rbindlist(lapply(links_to_get, get_urls_from_one_page_24hu))
  return(ret_df)
}

scrape_articles_24hu <- function(articles_24hu){
  
  pb <- txtProgressBar(min = 0, max = nrow(articles_24hu), style = 3)
  
  articles_24hu$content <- lapply(seq_along(articles_24hu$url), function(i){
    Sys.sleep(1) # sleep is set otherwise 24.hu blocks me
    t <- read_html(articles_24hu$url[[i]])
    article_text <- t %>% html_nodes('.post-body p , p+ ul li') %>% html_text()
    setTxtProgressBar(pb, i)
    return(article_text)
  }
  )
  return(articles_24hu)
}

# 444.hu helpers -----------------------------------------------------------
my_url <- 'https://444.hu/kereses?q=vakcina&page=1'
get_urls_from_one_page_444hu  <- function(my_url) {
  print(my_url)
  t <- read_html(my_url)
  boxes <- t %>% html_nodes('.card')
  x <- boxes[[1]]
  boxes_df <- lapply(boxes, function(x){
    t_list <- list()
    texts <- x %>% html_nodes("a") %>% html_text()
    t_list[['title']] <- texts[1]
    refs <- x %>% html_nodes("a") %>% html_attr('href')
    t_list[['url']] <- refs[1]
    t_list[['date_time']] <- x  %>%  html_nodes(".byline__info") %>% html_text()
    return(data.frame(t_list))  
  })
  
  df <- rbindlist(boxes_df)
  return(df)
}

get_444hu_urls <- function(searchterm, page_to_download) {
  # create links
  links_to_get <- paste0('https://444.hu/kereses?q=',searchterm, '&page=', seq(1, page_to_download))
  ret_df <- rbindlist(lapply(links_to_get, get_urls_from_one_page_444hu))
  return(ret_df)
}


scrape_articles_444hu <- function(articles_444hu){
  
  pb <- txtProgressBar(min = 0, max = nrow(articles_444hu), style = 3)
  
  # Scrape text from the given urls
  articles_444hu$content <- lapply(seq_along(articles_444hu$url),function(i){
    Sys.sleep(1) # sleep is set otherwise 444 blocks me
    t<- read_html(articles_444hu$url[[i]])
    article_text <- t %>% html_node('#content-main') %>% html_nodes('p') %>% html_text()
    article_text <- article_text[2:length(article_text)]
    setTxtProgressBar(pb, i)
    return(article_text)
  }
  )
  
  return(articles_444hu)
}

# !startsWith(articles_444hu$content[[1]], 'A 444-en az elénk')
# c("A 444-en az elénk",
#   "A kormány nem enged újságírókat az",
#   "A független magyar sajtónak soha nem volt olyan nehéz dolga")
  