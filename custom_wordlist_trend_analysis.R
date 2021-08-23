library(tidyverse)
library(lubridate)
library(ggplot2)
library(plyr)
library(scales)

# Read in data that has already been filtered through full_gloss, then format dates.
dat <- read.csv("data.csv", stringsAsFactors = FALSE)
dat$DATEposted <- as.Date(dat$DATEposted)
dat <- as_tibble(dat)

# Add new categories from glossary terms here
antisemitic <- c("Israel", "isreal", "izreal", "joos", "sionist", "zog", "j3w$", "jevv", "shlomo", "israhell", "kike", "pissrael")
as_virulent <- c("zionazi", "shlomo")


# Filter for specific rhetoric using relevant gloss terms--when adding a new target of concern per client's protected group designation, grey out unneeded lists and update as needed.
dat1 <- dat %>%
  select(DATEposted, term) %>%
  unnest(term) %>%
  filter(term %in% antisemitic) # Replace object after %in% with list of terms needed.
  
# Changes comment publication date to YYYY-MM format.  Adjust as needed for daily, yearly, or hourly analysis.
dat$DATEposted <- floor_date(dat$DATEposted, unit="week")
dat1$DATEposted <- floor_date(dat1$DATEposted, unit="week")



# Frequency of each term's use in a given month.
termfreq <- count(dat1, vars=c("DATEposted", "term"))

# Frequency of total occurrances of targeted vocabulary in a given month.
monthfreq <- count(dat$DATEposted)
monthfreq1 <- count(dat1$DATEposted)

# Use for tracking density of comments in a given category--un-grey lines as appropriate
dens <- monthfreq1$freq/monthfreq$freq 
density <- cbind(monthfreq, dens)



# Plots a frequency chart based on monthly totals.
ggplot(density, aes(x, dens)) +
  geom_point() +
  ggtitle("Long-term Density of Anti-Semitic Sentiment") +
  xlab("Date") +
  ylab("Density of Term Usage") +
  scale_x_date(breaks = date_breaks("1 month")) +
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5)) +
  geom_smooth()

# Plots a bar graph based on weekly totals.
ggplot(density, aes(x, dens)) +
  geom_bar(stat="identity") +
  ggtitle("Long-term Density of Anti-Semitic Sentiment") +
  xlab("Date") +
  ylab("Density of Term Usage") +
  scale_x_date(breaks = date_breaks("1 week")) +
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5)) #+
#geom_smooth()

# Plots a frequency chart that specifies term usage.
ggplot(termfreq, aes(DATEposted, freq)) +
  geom_point() +
  geom_text(aes(label=term)) +
  geom_smooth()

write.csv(dat, file="data_FINAL.csv")
