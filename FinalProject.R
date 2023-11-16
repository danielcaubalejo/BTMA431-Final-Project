# Installing and loading packages
install.packages("rvest")
library(rvest)
install.packages("dplyr")
library(dplyr)

####################################################################################################################

#### Main Question 1: How are cinemas being affected by streaming services? ####

## How are theaters performing since the rise of streaming services? ##



## How long after a movie release does it take for it to air on streaming platforms?



####################################################################################################################


#### Main Question 2: How does movie industry performance compare before and after the pandemic? ####


## How do streaming services change TV industries?
## (What is the impact on Cable TV, news, Daily Shows, etc.)



## How has the number of people that watch in cinemas change over time? How has box office revenue changed?

# Sources:
# https://www.statista.com/statistics/187073/tickets-sold-at-the-north-american-box-office-since-1980/
# https://www.the-numbers.com/market/

link = "https://www.the-numbers.com/market/"
page = read_html(link)

# Scraping Annual Ticket sales table
year = page %>% html_nodes("center:nth-child(9) a") %>% html_text()
tickets.sold = page %>% html_nodes("center:nth-child(9) .data:nth-child(2)") %>% html_text()
total.box.office = page %>% html_nodes("center:nth-child(9) .data:nth-child(3)") %>% html_text()
total.inflation.adj.box.office = page %>% html_nodes("center:nth-child(9) .data:nth-child(4)") %>% html_text()
avg.ticket.price = page %>% html_nodes("center:nth-child(9) .data:nth-child(5)") %>% html_text()

# Creating the scraped data into a data frame
annual.ticket.sales.df <- data.frame(year, tickets.sold, total.box.office, total.inflation.adj.box.office, avg.ticket.price)



####################################################################################################################

#### Main Question 3: How has taste, in terms of movies sentiment changed over time? ####

## What genre has the most frequency of social media buzz?
## (Based on # of views, # of comments, Top search, frequency of words)
## (What words appear most often?)


## What is the most popular genre per year?

