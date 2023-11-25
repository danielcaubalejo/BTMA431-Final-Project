# Installing and loading packages
install.packages("rvest")
library(rvest)
install.packages("dplyr")
library(dplyr)
install.packages("tidyr")
library(tidyr)
install.packages("data.table")
library(data.table)
install.packages("tm")
library(tm)
install.packages("NLP")
library(NLP)
install.packages("syuzhet")
library(syuzhet)
install.packages("wordcloud")
library(wordcloud)
install.packages("textstem")
library(textstem)
install.packages("ggplot2")
library(ggplot2)

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

# Change data type for Netflix to be numeric & converting into millions
netflix.users.df$netflix.users <- gsub(" million", "", netflix.users.df$netflix.users)
netflix.users.df$netflix.users <- as.numeric(netflix.users.df$netflix.users)
netflix.users.df$netflix.users <- netflix.users.df$netflix.users * 1e6

netflix.users.df$netflix.change.over.previous.year <- gsub("↑| million", "", netflix.users.df$netflix.change.over.previous.year)
netflix.users.df$netflix.change.over.previous.year <- as.numeric(netflix.users.df$netflix.change.over.previous.year)
netflix.users.df$netflix.change.over.previous.year <- netflix.users.df$netflix.change.over.previous.year * 1e6

netflix.users.df$year.netflix <- as.numeric(netflix.users.df$year.netflix)

# Scraping for Amazon users and change over time
link.explodingtopics = "https://explodingtopics.com/blog/video-streaming-stats"
page.explodingtopcs = read_html(link.explodingtopics)


amazon.table = page.explodingtopcs %>% html_nodes("table:nth-child(72) td") %>% html_text()

# We have 3 headers, data divide by 3
amazon.table.n <- length(amazon.table) / 3

# Creating a tibble
amazon.tibble <- tibble(
  key = rep(seq_len(amazon.table.n), each = 3),
  variable = rep(c("year.amazon", "amazon.users", "amazon.change.over.previous.year"), times = amazon.table.n),
  value = amazon.table
  )

# Using Pivot_wider
amazon.tibble.wide <- amazon.tibble %>%
  pivot_wider(names_from = variable, values_from = value)

# Removing the first row and column of our table
amazon.users.df <- amazon.tibble.wide[-1, -1]

# Change data type for Amazon to be numeric & converting into millions
amazon.users.df$amazon.users <- gsub(" million", "", amazon.users.df$amazon.users)
amazon.users.df$amazon.users <- as.numeric(amazon.users.df$amazon.users)
amazon.users.df$amazon.users <- amazon.users.df$amazon.users * 1e6

amazon.users.df$amazon.change.over.previous.year <- gsub("↑| million", "", amazon.users.df$amazon.change.over.previous.year)
amazon.users.df$amazon.change.over.previous.year <- as.numeric(amazon.users.df$amazon.change.over.previous.year)
amazon.users.df$amazon.change.over.previous.year <- amazon.users.df$amazon.change.over.previous.year * 1e6

amazon.users.df$year.amazon <- as.numeric(amazon.users.df$year.amazon)
amazon.users.df <- amazon.users.df[amazon.users.df$year.amazon >= 2013 & amazon.users.df$year.amazon <= 2023, ]



# Cable TV users in the US

# Source: https://en.wikipedia.org/wiki/Cable_television_in_the_United_States

link.wikipedia = "https://en.wikipedia.org/wiki/Cable_television_in_the_United_States"
page.wikipedia = read_html(link.wikipedia)

cable.users.df <- page.wikipedia %>% html_nodes("table.wikitable") %>% html_table() %>% .[[1]]

# Removing [#] from the ends of the numbers
cable.users.df$`Cable TV subscribers` <- sub("\\[\\d+\\]", "", cable.users.df$`Cable TV subscribers`)
cable.users.df$`Telephone company TV subscribers` <- sub("\\[\\d+\\]", "", cable.users.df$`Telephone company TV subscribers`)

# Removing month from years
cable.users.df$Year <- as.numeric(sub("\\w+\\.\\s", "", cable.users.df$Year))

# Turn Cable TV into numeric
cable.users.df$`Cable TV subscribers` <- as.numeric(gsub(",", "",cable.users.df$`Cable TV subscribers`))

# Removing unused Telephone Company TV subscribers column
cable.users.df$`Telephone company TV subscribers` <- NULL

# Sub-setting data to be from 2013 to 2023
cable.users.df <- cable.users.df[cable.users.df$Year >= 2013 & cable.users.df$Year <= 2023, ]

# Adding change over time column
cable.users.df$change.over.previous.year <- c(NA, diff(cable.users.df$`Cable TV subscribers`))



# Cable TV users predictions for next 5 years

# Making regression model to predict how many cable users in the next 5 years
cable.model <- lm(`Cable TV subscribers` ~ Year + change.over.previous.year, data = cable.users.df)

# Creating a sequence for the next 5 years
next.5.years <- data.frame(Year = seq(from = 2023, to = 2028, by = 1))

# Calculate change for the next.5.years based on the last available change
last.observation <- tail(cable.users.df$`Cable TV subscribers`, 1)
next.5.years$change.over.previous.year <- last.observation - cable.users.df$`Cable TV subscribers`[nrow(cable.users.df)] 

# Predict for the next 5 years
predictions <- predict(cable.model, newdata = next.5.years)

# Add predictions into a data frame
cable.users.predictions <- data.frame(Year = next.5.years$Year, predicted.users = predictions)

print(cable.users.predictions)




# Making a correlation matrix to determine how correlated the change in subscribers between Amazon, Netflix and Cable TV is

# Merge data frames based on year column
subscribers.merged.df <- merge(merge(amazon.users.df, netflix.users.df, by.x = "year.amazon", by.y = "year.netflix"), cable.users.df, by.x = "year.amazon", by.y = "Year")

# Getting rid of rows with NA
subscribers.merged.df <- subscribers.merged.df[-1 , ]

# Calculate the correlation matrix based on change over previous year
correlation.matrix <- cor(subscribers.merged.df[, c("amazon.change.over.previous.year", "netflix.change.over.previous.year", "change.over.previous.year")])

# Display the correlation matrix
print(correlation.matrix)

# Amazon v. Netflix has a correlation of 0.27, slightly positively correlated, increase as each other increase
# Amazon v Cable TV has a negative correlation of -0.15, as amazon increases, Cable TV subscribers decrease



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



# Making regression model to analyze the trends in annual ticket sales
model.tickets.sold <- lm(annual.ticket.sales.df.relevant$tickets.sold ~ annual.ticket.sales.df.relevant$year, data = annual.ticket.sales.df.relevant)

# Plot the regression lines
plot(annual.ticket.sales.df.relevant$year, annual.ticket.sales.df.relevant$tickets.sold, col = "blue", xlab = "Year", ylab = "Ticket Sales", main = "Ticket Sale Trend Analysis")
lines(annual.ticket.sales.df.relevant$year, predict(model.tickets.sold), col = "blue", lty = 2)

# Making regression model to analyze trends in total box office
model.box.office <- lm(annual.ticket.sales.df.relevant$total.box.office ~ annual.ticket.sales.df.relevant$year, data = annual.ticket.sales.df.relevant)

# Plot the regression lines
plot(annual.ticket.sales.df.relevant$year, annual.ticket.sales.df.relevant$total.box.office, col = "red", xlab = "Year", ylab = "Total Box Office Revenue", main = "Box Office Revenue Trend Analysis")
lines(annual.ticket.sales.df.relevant$year, predict(model.box.office), col = "red", lty = 2)

# In both graphs we see a significant drop in tickets sold and revenue in 2019, most likely due to the COVID 19 pandemic

######################################################################################################################################

##Is there a significant relationship between a movie's budget and its box office revenue?
movies_data = read.csv("https://raw.githubusercontent.com/danielgrijalva/movie-stats/master/movies.csv")
movies_data_cleaned = na.omit(movies_data)
movie_budget = lm(gross ~ budget, data = movies_data_cleaned)
summary(movie_budget)
#at a chosen significance level of 0.01 we can reject the null hypothesis that there isnt a significant relationship 
# between revenue and movie budget.

# Scatter plot
ggplot(movies_data, aes(x = budget, y = gross)) +
  geom_point(color = "blue", size = 3) +
  geom_smooth(method = "lm", se = FALSE, color = "red") +
  labs(title = "Scatter Plot of Gross Revenue vs. Budget",
       x = "Budget",
       y = "Gross Revenue") +
  theme_minimal()
##Does the genre of a movie impact the relationship between its budget and box office revenue?
model_genre = lm(gross ~ budget + genre, data = movies_data_cleaned)
summary(model_genre)
## looking at the summary, some of the genre have significant p values.
anova(movie_budget, model_genre)
##The anova test shows that including genre in the model significantly improves the performance of the model
####################################################################################################################

#### Main Question 3: How has sentiment, in terms of movie reviews changed over time? ####


#csv files scraped from imdb -> movie_sentiment.R
Infinity_War <- read.csv("InfWar_Reviews.csv")
The_Matrix <- read.csv("Matrix_Reviews.csv")
Spirited_Away <- read.csv("Spirited_Reviews.csv")
Whiplash <- read.csv("Whiplash_Reviews.csv")

#removing NA rows
Infinity_War <- na.omit(Infinity_War)
The_Matrix <- na.omit(The_Matrix)
Spirited_Away <- na.omit(Spirited_Away)
Whiplash <- na.omit(Whiplash)

Infinity_WarCorpus <- data.frame(doc_id = row.names(Infinity_War),
                                 text = Infinity_War$reviews2)

#preparing words for analysis
Infinity_WarCorpus <- VCorpus(DataframeSource(Infinity_WarCorpus))
#transform words to all lowercase
Infinity_WarCorpus <- tm_map(Infinity_WarCorpus, content_transformer(tolower))
#removing english stopwords-> stopwords("english")
Infinity_WarCorpus <- tm_map(Infinity_WarCorpus, removeWords, stopwords("english"))
#removing any numbers
Infinity_WarCorpus <- tm_map(Infinity_WarCorpus, removeNumbers)
#removing punctuations
Infinity_WarCorpus <- tm_map(Infinity_WarCorpus, removePunctuation)
#removing whitespace 
Infinity_WarCorpus <- tm_map(Infinity_WarCorpus, stripWhitespace)
#lemmatizing words
Infinity_WarCorpus <- tm_map(Infinity_WarCorpus, lemmatize_words)

#document-term matrix
Infinity_dtm <- DocumentTermMatrix(Infinity_WarCorpus)
#term-document matrix
Infinity_tdm <- TermDocumentMatrix(Infinity_WarCorpus)

#wordcloud test
m = as.matrix(Infinity_tdm)
wordFreq = sort(rowSums(m), decreasing = TRUE)
set.seed(2)
wordcloud(words = names(wordFreq), freq = wordFreq, min.freq = 40, random.order = F)

#review length correlation tests

Whiplash$dates1 <- as.Date(Whiplash$dates1)

Whiplash <- Whiplash[order(Whiplash$dates1), ]
View(Whiplash)
Whiplash_avgratings <- Whiplash %>%
  group_by(dates1) %>%
  summarise(avg_rating = mean(ratings1, na.rm = TRUE))

ggplot(Whiplash_avgratings, aes(x = dates1, y = avg_rating)) +
  geom_line() +
  labs(x = "Date", y = "Average Rating", title = "Trend of Movie Ratings Over Time")

## Does movie genre determine what sort of reviews people make?
## Comparing review word frequency between good and bad reviews
## Using regression analysis to predict review rating

