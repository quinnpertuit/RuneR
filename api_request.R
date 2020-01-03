library(httr)
library(tidyverse)
library(jsonlite)

base_url <- "http://services.runescape.com/m=itemdb_oldschool/api/"

#item info and prices
item_detail <- function(item_id) {

  test <- GET(url = str_glue("http://services.runescape.com/m=itemdb_oldschool/api/catalogue/detail.json?item=",
                             item_id)) %>%
  content("text") %>%
  jsonlite::fromJSON(flatten = TRUE) %>%
  as.data.frame() %>%
  as_tibble() %>%
  select(-c(1, 2, 3, 5)) %>%
  rename(
    Item_type = item.type,
    Item_name = item.name,
    Item_description = item.description,
    Item_price_trend = item.current.trend,
    Current_price = item.current.price,
    Today_trend = item.today.trend,
    Today_price_diff = item.today.price,
    Is_it_members = item.members,
    Thirty_days_trend = item.day30.trend,
    Thirty_days_change = item.day30.trend,
    Ninety_days_trend = item.day90.trend,
    Ninety_days_change = item.day90.change,
    Half_year_trend = item.day180.trend,
    Half_year_change = item.day180.change
  ) %>%
  return()
}

#all last 180 prices
item_prices <- function(item_id, type = "daily") {
  
  test <- GET(url = str_glue(base_url, "graph/", item_id, ".json")) %>%
    content("text") %>%
    jsonlite::fromJSON(flatten = TRUE)
  
    case_when(type == "daily" ~ return(tibble(test$daily)),
              type == "average" ~ return(tibble(test$average)))
    
}

#ge last update 
ge_info <- function() {
  
  test <- GET(url = str_glue(base_url, "info.json")) %>%
    content("text") %>%
    jsonlite::fromJSON(flatten = TRUE) %>%
    return()
  
}

item_ctlg <- function(init_letter, page_number) {
  
  test <- GET(url = str_glue(base_url,
                             "catalogue/items.json?category=1",
                             "&alpha=", init_letter,
                             "&page=", page_number)) %>%
    content("text") %>%
    jsonlite::fromJSON(flatten = TRUE)
  
  return(test$items)
  
}

#tests to test the program

query_test <- item_detail("4156")
price_test <- item_prices("4156")
info_test <- ge_info()  
ctlg_test <- item_ctlg(init_letter = "a",
                      page_number = 1)

class(query_test)

str(query_test)