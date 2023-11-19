library(tidyverse)
library(RSelenium)
library(dplyr)
library(stringr)


rD <- rsDriver(browser = "firefox",
               chromever = NULL)

remDr <- rD[["client"]]

scroll_navigation <- function(){
  
  #click load more code taken from: https://github.com/ggSamoora/TutorialsBySamoora/blob/main/rate_my_professor_script.Rmd
  loadmore <- remDr$findElement(using = "xpath", value = '//*[@id="load-more-trigger"]')
  
  #initialize page number
  page <- 0
  #large enough page # until "load more" does not appear
  while (page <= 30) {
    #Navigate to the webpage with the current page number
    #url <- paste0("https://www.rottentomatoes.com/browse/movies_at_home/sort:popular?page=", page)
    
    #webpage scroll test
    #y_position <- loadmore$getElementLocation()$y - 100
    #remDr$executeScript(sprintf("window.scrollTo(0, %f)", y_position))
    
    loadmore$clickElement()
    # Increment the page number
    #page <- page + 1
    # Pause code to ensure webpage loads properly
    Sys.sleep(1)
  }
}

dropdown_all <- function() {
  # Assuming remDr and button_class are already defined
    button_class <- "expander-icon-wrapper"  # Replace with the actual class name
  
  # Find all buttons with the specified class
  buttons <- remDr$findElements(using = "css selector", value = paste(".", button_class, sep = ""))
  
  # Click each button
  for (button in buttons) {
    button$clickElement()
  }
}

#####whiplash (2013) sentiment#####

remDr$navigate("https://www.imdb.com/title/tt2582802/reviews?ref_=tt_urv")
scroll_navigation()
dropdown_all()

usernamesElem <- remDr$findElements(using = 'xpath', "//span[starts-with(@class, 'display-name-link')]")
usernames <- unlist(lapply(usernamesElem, function(x){x$getElementText()}))
usernames <- head(usernames, 1650)

titleElem <- remDr$findElements(using = 'xpath', "//a[starts-with(@class, 'title')]")
titles <- unlist(lapply(titleElem, function(x){x$getElementText()}))
titles <- head(titles, 1650)

dateElem <- remDr$findElements(using = 'xpath', "//span[starts-with(@class, 'review-date')]")
dates <- unlist(lapply(dateElem, function(x){x$getElementText()}))
dates <- head(dates, 1650)

reviewElem <- remDr$findElements(using = 'xpath', "//div[starts-with(@class, 'text show-more__control')]")
reviews <- unlist(lapply(reviewElem, function(x){x$getElementText()}))
reviews <- head(reviews,1650)

ratingElem <- remDr$findElements(using = "css", value = "span.rating-other-user-rating")
ratings <- sapply(ratingElem, function(x) {x$getElementText()}) %>% str_extract("\\d+")
ratings <- head(ratings,1650)

Whiplash_Reviews <- data.frame(usernames, titles, dates, reviews, ratings)
View(Whiplash_Reviews)

#####Avengers: Infinity War sentiment#####

remDr$navigate("https://www.imdb.com/title/tt4154756/reviews?ref_=tt_urv")
scroll_navigation()
dropdown_all()

usernamesElem <- remDr$findElements(using = 'xpath', "//span[starts-with(@class, 'display-name-link')]")
usernames <- unlist(lapply(usernamesElem, function(x){x$getElementText()}))
usernames <- head(usernames, 1650)

titleElem <- remDr$findElements(using = 'xpath', "//a[starts-with(@class, 'title')]")
titles <- unlist(lapply(titleElem, function(x){x$getElementText()}))
titles <- head(titles, 1650)

dateElem <- remDr$findElements(using = 'xpath', "//span[starts-with(@class, 'review-date')]")
dates <- unlist(lapply(dateElem, function(x){x$getElementText()}))
dates <- head(dates, 1650)

reviewElem <- remDr$findElements(using = 'xpath', "//div[starts-with(@class, 'text')]")
reviews <- unlist(lapply(reviewElem, function(x){x$getElementText()}))
reviews <- head(reviews,1650)

ratingElem <- remDr$findElements(using = "css", value = "span.rating-other-user-rating")
ratings <- sapply(ratingElem, function(x) {x$getElementText()}) %>% str_extract("\\d+")
ratings <- head(ratings,1650)

InfWar_Reviews <- data.frame(usernames, titles, dates, reviews, ratings)
View(InfWar_Reviews)


# rD[["server"]]$stop()
# system("taskkill /im java.exe /f") # Stops the java.exe
# rm(list = ls())













