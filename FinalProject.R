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
link.movies.releases = "https://www.boxofficemojo.com/year/?grossesOption=calendarGrosses"
page.movies.releases = read_html(link.movies.releases)

link.thenumbers = "https://www.the-numbers.com/market/"
page.thenumbers = read_html(link.thenumbers)

year.movie.releases = page.thenumbers %>% html_nodes("center:nth-child(9) a") %>% html_text()
quantity.movie.releases = page.movies.releases %>% html_nodes(".mojo-field-type-positive_integer")%>% html_text()
tickets.sold = page.thenumbers %>% html_nodes("center:nth-child(9) .data:nth-child(2)")%>% html_text()
cinema.box.office = page.thenumbers %>% html_nodes("center:nth-child(9) .data:nth-child(3)")%>% html_text()  

box.office.performance.overtime <- data.frame(year.movie.releases, tickets.sold, cinema.box.office)

## How long after a movie release does it take for it to air on streaming platforms?


# Aneesha's code
# Specify the URL
statista_url <- "https://www.statista.com/statistics/947757/theaters-streaming-watching-movies/"

# Read HTML page
page_statista <- read_html(statista_url)

# Scraping for theaters streaming and watching movies
theaters_data <- page_statista %>%
  html_nodes("table:nth-child(15) tr") %>%
  html_text() %>%
  strsplit("\n") %>%
  matrix(ncol = 3, byrow = TRUE) %>%
  as.data.frame()

# Naming columns
colnames(theaters_data) <- c("Year", "Number of Theaters Streaming", "Number of Theaters Watching Movies")

# Cleaning data
theaters_data <- theaters_data[-1, ]  # Remove the header row
theaters_data <- theaters_data[-nrow(theaters_data), ]  # Remove the last row (total)

# Convert columns to appropriate data types if needed
theaters_data$Year <- as.numeric(theaters_data$Year)
theaters_data$Number_of_Theaters_Streaming <- as.numeric(theaters_data$Number_of_Theaters_Streaming)
theaters_data$Number_of_Theaters_Watching_Movies <- as.numeric(theaters_data$Number_of_Theaters_Watching_Movies)

# Print the resulting data frame
print(theaters_data)




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


# Cable TV users in the US


# Source: https://en.wikipedia.org/wiki/Cable_television_in_the_United_States

link.wikipedia = "https://en.wikipedia.org/wiki/Cable_television_in_the_United_States"
page.wikipedia = read_html(link.wikipedia)

wikitable.df <- page.wikipedia %>% html_nodes("table.wikitable") %>% html_table() %>% .[[1]]

# Removing [#] from the ends of the numbers
wikitable.df$`Cable TV subscribers` <- sub("\\[\\d+\\]", "", wikitable.df$`Cable TV subscribers`)
wikitable.df$`Telephone company TV subscribers` <- sub("\\[\\d+\\]", "", wikitable.df$`Telephone company TV subscribers`)




# Cord cutters / Cable or Satellite cancelation projected data

# Source: https://techjury.net/blog/cable-tv-subscribers-statistics/

link.techjury <- "https://techjury.net/blog/cable-tv-subscribers-statistics/"
page.techjury = read_html(link.techjury)

cordcutter.years <- page.techjury %>% html_nodes("#us-cord-cutters-2022-2026+ .table-wrapper tr+ tr td:nth-child(1)") %>% html_text()
number.of.cord.cutters <- page.techjury %>% html_nodes("#us-cord-cutters-2022-2026+ .table-wrapper tr+ tr td:nth-child(2)") %>% html_text()
cordcutter.percentage <- page.techjury %>% html_nodes("tr+ tr td~ td+ td") %>% html_text()

# Put projected data into a  dataframe
projected.cordcutters.df <- data.frame(cordcutter.years, number.of.cord.cutters, cordcutter.percentage)

# Turn numbers into a numeric
projected.cordcutters.df$number.of.cord.cutters <- as.numeric(projected.cordcutters.df$number.of.cord.cutters)

# Turn numbers into millions
projected.cordcutters.df$number.of.cord.cutters <- projected.cordcutters.df$number.of.cord.cutters * 1e6


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
#since the data for Netflix and amazon is from 2013 onwards, I'm filtering the data for tickets to be the same
annual.ticket.sales.df.relevant = annual.ticket.sales.df[annual.ticket.sales.df$year>= 2013, ]
#arrange them in accending order of year
annual.ticket.sales.df.relevant = annual.ticket.sales.df.relevant[order(annual.ticket.sales.df.relevant$year), ]
#add columns for delta of tickets sold and total revenue
annual.ticket.sales.df.relevant$tickets.sold = as.numeric(gsub(",", "", annual.ticket.sales.df.relevant$tickets.sold))
annual.ticket.sales.df.relevant$total.box.office <- as.numeric(gsub("[^0-9.]", "", gsub(",", "", annual.ticket.sales.df.relevant$total.box.office)))
annual.ticket.sales.df.relevant = annual.ticket.sales.df.relevant %>%
  select(year,tickets.sold,total.box.office)%>%
  mutate(tickets_sold_delta = c(NA, diff(tickets.sold)),
         revenue_delta = c(NA, diff(total.box.office)))

####################################################################################################################

#### Main Question 3: How has taste, in terms of movies sentiment changed over time? ####

## What genre has the most frequency of social media buzz?
## (Based on # of views, # of comments, Top search, frequency of words)
## (What words appear most often?)


## What is the most popular genre per year?

