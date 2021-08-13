library(tidyverse)
library(dplyr)
library(rvest)

chan <- "https://boards.4channel.org/a/thread/212247903"
html <- read_html(chan)
comments <- html_nodes(html, ".postMessage")
dates <- html_nodes(html, ".dateTime")
postnum <- html_nodes(html, ".postNum a+ a")
img <- html_nodes(html, "a img")


length(img)

comments[1:50]
dates[1:442]

comtext <- html_text(comments, trim=TRUE)
datetext <- html_text(dates, trim=TRUE)
pnumtext <- html_text(postnum, trim=TRUE)

comtext <- as.data.frame(comtext)

datetext <- as.data.frame(datetext)
datetext <- datetext %>% filter(row_number() %% 2 != 1)

pnumtext <- as.data.frame(pnumtext)
pnumtext <- pnumtext %>% filter(row_number() %% 2 != 1)

comdat <- cbind(datetext, pnumtext, comtext)
comdat <- comdat %>% 
  mutate(post_url = paste("https://boards.4channel.org/a/thread/", pnumtext, sep=""))


write_csv(comdat, "4chan_test.csv")

