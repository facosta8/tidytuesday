# libraries ---------

library(ggplot2)
library(dplyr)
library(tidyr)

# data -------------

limpiar_nombres <- function(x) {
    x <- sub(' GDPR', '', x)
    x <- sub(' GDRP', '', x)
    x <- sub(',', '', x)
    x <- sub('Art.', 'Art', x)
    x <- sub('of the', '', x)
    x <- sub('(1)', '', x, fixed = TRUE)
    x <- sub('(2)', '', x, fixed = TRUE)
    x <- sub('(3)', '', x, fixed = TRUE)
    x <- sub('(4)', '', x, fixed = TRUE)
    x <- sub('(5)', '', x, fixed = TRUE)
    x <- sub('a)', '', x, fixed = TRUE)
    x <- sub('b)', '', x, fixed = TRUE)
    x <- sub('c)', '', x, fixed = TRUE)
    x <- sub('d)', '', x, fixed = TRUE)
    x <- sub('e)', '', x, fixed = TRUE)
    x <- sub('f)', '', x, fixed = TRUE)
    x <- gsub(' ', '', x, fixed = TRUE)
}



gv <- readr::read_tsv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2020/2020-04-21/gdpr_violations.tsv')
gv$date <- as.Date(gv$date, format = '%m/%d/%Y')
gv <-  
    gv %>% 
    mutate(article_violated = strsplit(article_violated, '|', fixed = T)) %>% 
    rowwise() %>% 
    mutate(num_articles = length(article_violated)) %>% 
    ungroup()


# Other stuff ---------------

gt <- readr::read_tsv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2020/2020-04-21/gdpr_text.tsv')


# Most common articles

ab <- 
    gv$article_violated %>% 
    map(.f = limpiar_nombres) %>% 
    unlist() %>% table() %>% as_tibble()

colnames(ab) <- c('art', 'n')

v(ab)