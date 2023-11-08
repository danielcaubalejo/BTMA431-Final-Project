#getwd()
#setwd("C:/Users/Aaron/OneDrive - University of Calgary/BTMA 431/Final Project")

install.packages("rvest")
install.packages("dplyr")

library(rvest)
library(dplyr)

# https://www.statista.com/statistics/187073/tickets-sold-at-the-north-american-box-office-since-1980/
# Testing scraping on Movie theater annual ticket sales
link = "https://www.the-numbers.com/market/"
page = read_html(link)

# Annual Ticket sales table
year = page %>% html_nodes("center:nth-child(9) a") %>% html_text()
tickets.sold = page %>% html_nodes("center:nth-child(9) .data:nth-child(2)") %>% html_text()
total.box.office = page %>% html_nodes("center:nth-child(9) .data:nth-child(3)") %>% html_text()
total.inflation.adj.box.office = page %>% html_nodes("center:nth-child(9) .data:nth-child(4)") %>% html_text()
avg.ticket.price = page %>% html_nodes("center:nth-child(9) .data:nth-child(5)") %>% html_text()

annual.ticket.sales.df <- data.frame(year, tickets.sold, total.box.office, total.inflation.adj.box.office, avg.ticket.price)

print(annual.ticket.sales.df)
