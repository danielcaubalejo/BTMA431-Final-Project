# Installing and loading packages
install.packages("rvest")
library(rvest)
install.packages("dplyr")
library(dplyr)
install.packages("tidyr")
library(tidyr)

####################################################################################################################

#### Main Question 1: How are cinemas being affected by streaming services? ####

## How are theaters performing since the rise of streaming services? ##



## How long after a movie release does it take for it to air on streaming platforms?



####################################################################################################################


#### Main Question 2: How does movie industry performance compare before and after the pandemic? ####


## How do streaming services change TV industries?
## (What is the impact on Cable TV, news, Daily Shows, etc.)

# Netflix and Amazon users and change
# Sources: https://explodingtopics.com/blog/video-streaming-stats

link.explodingtopics = "https://explodingtopics.com/blog/video-streaming-stats"
page.explodingtopcs = read_html(link.explodingtopics)

# Scraping for Netflix users and change over time
year.netflix = page.explodingtopcs %>% html_nodes("table:nth-child(69) tr+ tr td:nth-child(1)") %>% html_text()
netflix.users <- page.explodingtopcs %>% html_nodes("table:nth-child(69) tr+ tr td:nth-child(2)") %>% html_text()
netflix.change.over.previous.year <- page.explodingtopcs %>% html_nodes("table:nth-child(69) tr+ tr td~ td+ td") %>% html_text()

# Creating data frame for Netflix users and change
netflix.users.df <- data.frame(year.netflix, netflix.users, netflix.change.over.previous.year)


# Scraping for Amazon users and change over time
link.explodingtopics = "https://explodingtopics.com/blog/video-streaming-stats"
page.explodingtopcs = read_html(link.explodingtopics)


amazon.table = page.explodingtopcs %>% html_nodes("table:nth-child(72) td") %>% html_text()

# We have 3 headers, data divide by 3
amazon.table.n <- length(amazon.table) / 3

# Creating a tibble
amazon.tibble <- tibble(
  key = rep(seq_len(amazon.table.n), each = 3),
  variable = rep(c("Year", "Amazon Prime Members", "Change Over Previous Year"), times = amazon.table.n),
  value = amazon.table
  )

# Using Pivot_wider
amazon.tibble.wide <- amazon.tibble %>%
  pivot_wider(names_from = variable, values_from = value)

# Removing the first row and column of our table
amazon.users.df <- amazon.tibble.wide[-1, -1]



## How has the number of people that watch in cinemas change over time? How has box office revenue changed?

# Sources:
# https://www.statista.com/statistics/187073/tickets-sold-at-the-north-american-box-office-since-1980/
# https://www.the-numbers.com/market/

link.thenumbers = "https://www.the-numbers.com/market/"
page.thenumbers = read_html(link.thenumbers)

# Scraping Annual Ticket sales table
year = page.thenumbers %>% html_nodes("center:nth-child(9) a") %>% html_text()
tickets.sold = page.thenumbers %>% html_nodes("center:nth-child(9) .data:nth-child(2)") %>% html_text()
total.box.office = page.thenumbers %>% html_nodes("center:nth-child(9) .data:nth-child(3)") %>% html_text()
total.inflation.adj.box.office = page.thenumbers %>% html_nodes("center:nth-child(9) .data:nth-child(4)") %>% html_text()
avg.ticket.price = page.thenumbers %>% html_nodes("center:nth-child(9) .data:nth-child(5)") %>% html_text()

# Creating the scraped data into a data frame
annual.ticket.sales.df <- data.frame(year, tickets.sold, total.box.office, total.inflation.adj.box.office, avg.ticket.price)


####################################################################################################################

#### Main Question 3: How has taste, in terms of movies sentiment changed over time? ####

## What genre has the most frequency of social media buzz?
## (Based on # of views, # of comments, Top search, frequency of words)
## (What words appear most often?)


## What is the most popular genre per year?

