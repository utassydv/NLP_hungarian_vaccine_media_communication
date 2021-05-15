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