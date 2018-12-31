

### final project

## questions

#' [] what factors correlate with price?
#' 
#' 
##' what do i want to check first?
#' 
#' [done] color
#' [done] type
#' [done] cmc

## data import

library(jsonlite)
library(rvest)
pacman::p_load(tidyverse, lubridate, stringi)
setwd("C:/git/M335_FA18_Corum_Jaco/data")

dat.cardAttr.json <- fromJSON('mtg.json', simplifyDataFrame = T)

download.file('https://mtgjson.com/v4/json/AllCards.json', destfile = 'mtg.json')

dat.cardAttr.tibble <- readRDS('dat.cardAttr.tibble.Rds')

saveRDS(dat.cardAttr.tibble, file = 'dat.cardAttr.tibble.Rds')





dat.cardAttr.tibble <- read_rds('dat.cardAttr.tibble.Rds')
stand.prices <- read_rds('stand.prices.Rda')
modern.prices1 <- read_rds('modern.prices1.Rds')
modern.prices2 <- read_rds('modern.prices2.Rds')
legacy.prices1 <- read_rds('legacy.prices1.Rds')
legacy.prices2 <- read_rds('legacy.prices2.Rds')







## clean data

dat.mtg.json %>% # gives error that tbl must be a data frame
  sample_n(100)

`%l0%` <- function(x, y) if (length(x) > 0) x else y


dat.cardAttr.tibble <- map_df(dat.cardAttr.json, ~{
  data_frame(
    name = .x$name %l0% NA_character_,
    colors = .x$colors %l0% 'colorless',
    type = .x$type,
    cmc = .x$convertedManaCost %l0% NA_character_
  )
})


## i have to make a web scraper because i can't find pricing data... also i should have used a formula...

library(rvest)

html.mtggoldfish.stand <- read_html("https://www.mtggoldfish.com/prices/paper/standard")
stand.names <- html.mtggoldfish.stand %>% 
  html_nodes('.priceList-prices-card a') %>%
  html_text()
saveRDS(stand.names, file = 'stand.names.Rda')
stand.names

html.mtggoldfish.stand <- read_html("https://www.mtggoldfish.com/prices/paper/standard")
stand.prices <- html.mtggoldfish.stand %>% 
  html_nodes('.priceList-price-price-wrapper') %>%
  html_text()
saveRDS(stand.prices, file = 'stand.prices.Rda')
stand.prices

stand.names <- stand.names %>% 
  as_tibble(validate = F) %>% 
  rename(card.name = value) %>% 
  mutate(row = rownames(card.name))

stand.prices <- stand.prices %>% 
  as_tibble(validate = F) %>% 
  rename(price = value) 
as.numeric(stand.prices$price)

standard <- stand.names %>% 
  mutate(price = stand.prices$price) 
standard$price <- as.numeric(standard$price)
saveRDS(standard, file = 'standard.Rds')




html.mtggoldfish.modern1 <- read_html('https://www.mtggoldfish.com/prices/paper/modern_two')
modern.names1 <- html.mtggoldfish.modern1 %>% 
  html_nodes('.priceList-prices-card a') %>% 
  html_text()
modern.names1
saveRDS(modern.names1, file = 'modern.names1.Rds')

html.mtggoldfish.modern1 <- read_html('https://www.mtggoldfish.com/prices/paper/modern_two')
modern.prices1 <- html.mtggoldfish.modern1 %>% 
  html_nodes('.priceList-price-price-wrapper') %>% 
  html_text()
modern.prices1
saveRDS(modern.prices1, file = 'modern.prices1.Rds')

modern.names1 <- modern.names1 %>% 
  as_tibble(validate = F) %>% 
  rename(card.name = value)

modern.prices1 <- modern.prices1 %>%
  as_tibble(validate = F) %>% 
  rename(price = value)
as.numeric(modern.prices1$price)

modern <- modern.names1 %>% 
  mutate(price = modern.prices1$price)
modern$price <- as.numeric(modern$price)




html.mtggoldfish.modern2 <- read_html('https://www.mtggoldfish.com/prices/paper/modern_one')
modern.names2 <- html.mtggoldfish.modern2 %>% 
  html_nodes('.priceList-prices-card a') %>% 
  html_text()
modern.names2
saveRDS(modern.names2, file = 'modern.names2.Rds')

html.mtggoldfish.modern2 <- read_html('https://www.mtggoldfish.com/prices/paper/modern_one')
modern.prices2 <- html.mtggoldfish.modern2 %>% 
  html_nodes('.priceList-price-price-wrapper') %>% 
  html_text()
modern.prices2
saveRDS(modern.prices2, file = 'modern.prices2.Rds')

modern.names2 <- modern.names2 %>% 
  as_tibble(validate = F) %>% 
  rename(card.name = value)

modern.prices2 <- modern.prices2 %>%
  as_tibble(validate = F) %>% 
  rename(price = value)
as.numeric(modern.prices2$price)

modern2 <- modern.names2 %>% 
  mutate(price = modern.prices2$price)
modern2$price <- as.numeric(modern2$price)




html.mtggoldfish.legacy1 <- read_html('https://www.mtggoldfish.com/prices/paper/legacy_two')
legacy.prices1 <- html.mtggoldfish.legacy1 %>% 
  html_nodes('.priceList-price-price-wrapper') %>% 
  html_text()
legacy.prices1
saveRDS(legacy.prices1, file = 'legacy.prices1.Rds')

html.mtggoldfish.legacy1 <- read_html('https://www.mtggoldfish.com/prices/paper/legacy_two')
legacy.names1 <- html.mtggoldfish.legacy1 %>% 
  html_nodes('.priceList-prices-card a') %>% 
  html_text()
legacy.names1
saveRDS(legacy.names1, file = 'legacy.names1.Rds')

legacy.names1 <- legacy.names1 %>% 
  as_tibble(validate = F) %>% 
  rename(card.name = value)

legacy.prices1 <- legacy.prices1 %>%
  as_tibble(validate = F) %>%
  rename(price = value)
as.numeric(legacy.prices1$price)

legacy1 <- legacy.names1 %>% 
  mutate(price = legacy.prices1$price)
legacy1$price <- as.numeric(legacy1$price)





html.mtggoldfish.legacy2 <- read_html('https://www.mtggoldfish.com/prices/paper/legacy_one')
legacy.prices2 <- html.mtggoldfish.legacy2 %>% 
  html_nodes('.priceList-price-price-wrapper') %>% 
  html_text()
legacy.prices2
saveRDS(legacy.prices2, file = 'legacy.prices2.Rds')

html.mtggoldfish.legacy2 <- read_html('https://www.mtggoldfish.com/prices/paper/legacy_one')
legacy.names2 <- html.mtggoldfish.legacy2 %>% 
  html_nodes('.priceList-prices-card a') %>% 
  html_text()
legacy.names2
saveRDS(legacy.names2, file = 'legacy.prices2.Rds')

legacy.names2 <- legacy.names2 %>% 
  as_tibble(validate = F) %>% 
  rename(card.name = value)

legacy.prices2 <- legacy.prices2 %>%
  as_tibble(validate = F) %>%
  rename(price = value)
as.numeric(legacy.prices2$price)

legacy2 <- legacy.names2 %>% 
  mutate(price = legacy.prices2$price)
legacy2$price <- as.numeric(legacy2$price)



## join all the data

all.cards <- legacy1 %>% 
  bind_rows(legacy2) %>%
  bind_rows(modern) %>% 
  bind_rows(modern2) %>% 
  bind_rows(standard) %>% glimpse


all.cards.tidy <- all.cards %>% 
  rename(name = card.name) %>% 
  left_join(dat.cardAttr.tibble, by = 'name')
saveRDS(all.cards.tidy, file = 'allCards1.Rds')


## finally make a graphic!!!!!!!!!!!!
pacman::p_load(tidyverse, lvplot)
setwd("C:/git/M335_FA18_Corum_Jaco/data")
all.cards.tidy <- read_rds('allCards2.Rds')



# price by cmc
all.cards.tidy %>% 
  ggplot(
    aes(
      x = cmc, 
      y = price
    )
  ) +
  geom_jitter() +
  geom_smooth(
    aes(alpha = 0.1)
  ) +
  labs(
    title = "Cards With CMC of 4 or Less are the Most Valuable", 
    x = "Converted Mana Cost", 
    y = "Price (US Dollars)"
  ) +
  guides(
    alpha = 'none'
  ) +
  theme_bw()
ggsave('priceByCmc.png')

#price by color
# what is the NA in color? answer: its boxes of cards
all.cards.tidy %>% 
  filter(!colors %in% NA) %>% 
  ggplot(
    aes(
      x = colors, 
      y = price
    )
  ) +
  geom_jitter() +
  geom_lv(
    aes(
      fill = ..LV..
    ), 
    alpha = 0.5, 
    show.legend = F
  ) +
  labs(
    title = "Colorless Cards are the Most Powerful Color", 
    x = "Card Color", 
    y = "Price (US Dollars)"
  ) +
  theme_bw()


all.cards.tidy %>% 
  str_replace(colors, c('B' = 'Black', 'G' = 'Green', 'R' = 'Red', 'U' = 'Blue', 'W' = 'White'))

all.cards.tidy$colors[is.na(all.cards.tidy$colors)] <- 'Box'
all.cards.tidy$colors <- str_to_title(all.cards.tidy$colors, 'Colorless')
saveRDS(all.cards.tidy, file = 'allCards2.Rds')
all.cards.tidy <- read_rds('allCards2.Rds')

all.cards.tidy %>% 
  filter(colors %in% 'Box') %>% glimpse

case_when(letters %in% 'a' == 0 ~ 'happy')




## get a plot comparing price of color + cmc vs all other cards

all.cards.tidy %>% 
  mutate(
    buy = case_when(
      colors %in% 'Colorless' & cmc <= 4 ~ 'Low CMC Colorless', 
      T ~ 'All Other Cards'
    )
  ) %>% 
  ggplot(
    aes(
      x = buy, 
      y = price
    )
  ) +
  geom_jitter() +
  geom_lv(
    aes(fill = ..LV..), 
    alpha = 0.5, 
    show.legend = F
  ) +
  labs(
    x = ' ',
    y = "Price (US Dollars)"
  )

library(lvplot)

# price based on type

all.cards.tidy <- all.cards.tidy %>% 
  separate(type, into = c('type.general', 'type.specific'))
saveRDS(all.cards.tidy, file = 'allCards3.Rds')


all.cards.tidy %>% 
  filter(!is.na(type.general)) %>% 
  ggplot(
    aes(
      x = type.general, 
      y = price
    )
  ) +
  geom_jitter() + 
  geom_lv(
    aes(
      fill = ..LV..
    ),
    alpha = 0.5, 
    show.legend = F
  )


all.cards.tidy %>% 
  ggplot(
    aes(
      x = type.specific, 
      y = price
    )
  ) +
  geom_histogram(
    stat = 'identity'
  ) +
  coord_flip()



read_rds('dat.cardAttr.tibble.Rds')

pacman::p_load(tidyverse, lubridate, rvest, stringi)
all.cards.tidy <- read_rds('allCards3.Rds')
library(rvest)

sets.html <- read_html('https://mtgjson.com/v4/sets.html')
raw.sets <- sets.html %>% 
  html_nodes('td') %>% 
  html_text()
View(raw.sets)
write_rds(raw.sets, path = 'mtgDates.Rds')
raw.sets <- read_rds('mtgDates.Rds')

setwd("C:/git/M335_FA18_Corum_Jaco/data")

split.sets <- str_split_fixed(raw.sets, '-', n = 2)
colnames(split.sets) <- c('names', 'dates')
splitTibble <- as.tibble(split.sets)
splitAbbr <- stri_sub(splitTibble$names, from = -4) 
splitAbbr <- as.tibble(splitAbbr)
colnames(splitAbbr) <- 'setAbbr'
View(sets)

cardDates <- splitTibble %>% 
  mutate(setAbbr = splitAbbr$setAbbr)












