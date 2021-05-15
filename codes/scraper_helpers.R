
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
  

# 24.hu helpers -----------------------------------------------------------
get_urls_from_one_page_24hu  <- function(my_url) {
  print(my_url)
  t <- read_html(my_url)
  boxes <- t %>% html_nodes('.-hasImg')
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

  