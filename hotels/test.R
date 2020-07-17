library(tidyverse)
library(feasts)

hotels <- readr::read_csv(paste0('https://raw.githubusercontent.com/',
                                 'rfordatascience/tidytuesday/master/',
                                 'data/2020/2020-02-11/hotels.csv'))
                          

hotels %>% 
    filter(hotel == "City Hotel") %>%
    mutate(date = glue::glue("{arrival_date_year}-{arrival_date_month}-{arrival_date_day_of_month}"),
           date = parse_date(date, format = "%Y-%B-%d")) %>% 
    select(date, everything()) %>% 
    arrange(date) %>% 
    count(date) %>% 
    rename(daily_bookings = n) %>% 
    tsibble::as_tsibble() %>% 
    model(STL(daily_bookings ~ season(window = Inf))) %>% 
    components() %>% autoplot()
