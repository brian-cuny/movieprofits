library(tidyverse)
library(magrittr)
library(jsonlite)
library(yaml)
library(XML)
library(httr)
library(RCurl)
library(scales)
library(ggrepel)

Links <- function(value){
  cbind(value %>% xmlValue, value %>% xmlGetAttr('href'))
}


movie.data.html <- getURL('http://www.boxofficemojo.com/yearly/chart/?yr=2017&p=.htm') %>% 
  htmlParse()

movie.data <- movie.data.html %>%
  xpathSApply('//*[@id="body"]/table[3]//tr//td[2]//a', Links) %>%
  t() %>%
  as.tibble() %>%
  filter(row_number() != 1) %>%
  setNames(c('Movie', 'Link')) %>%
  mutate(Link = Link %>% str_extract('(?<=\\?)(.*)'))

test <- getURL(paste('http://www.boxofficemojo.com/movies/?page=daily&view=chart&', movie.data[1, ]$Link, sep='')) %>% 
  htmlParse() %>%
  xpathSApply('//*[@class="chart-wide"]//tr//td[4]', xmlValue) %>%
  as.tibble() %>%
  filter(row_number() != 1) %>%
  setNames(c('profit')) %>%
  mutate(profit = profit %>% parse_number()) %>%
  rowid_to_column('day')

ggplot(test, aes(x=day, y=profit)) +
  geom_point() +
  geom_smooth(se=FALSE)

ggplot(test, aes(x=day, y=profit %>% cumsum())) +
  geom_point() +
  geom_smooth(se=FALSE)




