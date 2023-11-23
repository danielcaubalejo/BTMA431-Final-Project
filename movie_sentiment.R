library(tidyverse)
library(RSelenium)
library(dplyr)
library(stringr)


rD <- rsDriver(browser = "firefox",
               chromever = NULL)

remDr <- rD[["client"]]

scroll_navigation <- function() {
  while(TRUE) {
    # Check if the "Load More" button exists
    loadmoreElem <- tryCatch({
      remDr$findElements(using = "xpath", value = '//*[@id="load-more-trigger"]')
    }, error = function(e) NULL)
    
    # Check if the "Load More" button's parent div has the 'ipl-load-more--loaded-all' class
    allLoadedElem <- tryCatch({
      remDr$findElements(using = "css", value = '.ipl-load-more.ipl-load-more--loaded-all')
    }, error = function(e) NULL)
    
    # If the "Load More" button does not exist or all reviews have been loaded, break the loop
    if (length(loadmoreElem) == 0 || length(allLoadedElem) > 0) {
      break
    }
    
    # If the "Load More" button exists, click it
    loadmore <- loadmoreElem[[1]]
    loadmore$clickElement()
    
    # Pause code to ensure webpage loads properly
    #Sys.sleep(1.5)
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

Sys.sleep(5)

#####whiplash (2013) sentiment#####

remDr$navigate("https://www.imdb.com/title/tt2582802/reviews?ref_=tt_urv")
Sys.sleep(2)

scroll_navigation()
dropdown_all()

usernamesElem1 <- remDr$findElements(using = 'xpath', "//span[starts-with(@class, 'display-name-link')]")
usernames1 <- unlist(lapply(usernamesElem1, function(x){x$getElementText()}))
usernames1 <- head(usernames1, 1500)

titleElem1 <- remDr$findElements(using = 'xpath', "//a[starts-with(@class, 'title')]")
titles1 <- unlist(lapply(titleElem1, function(x){x$getElementText()}))
titles1 <- head(titles1, 1500)

dateElem1 <- remDr$findElements(using = 'xpath', "//span[starts-with(@class, 'review-date')]")
dates1 <- unlist(lapply(dateElem1, function(x){x$getElementText()}))
dates1 <- head(dates1, 1500)

reviewElem1 <- remDr$findElements(using = 'xpath', "//div[starts-with(@class, 'text show-more__control')]")
reviews1 <- unlist(lapply(reviewElem1, function(x){x$getElementText()}))
reviews1 <- head(reviews1,1500)

#due to some reviews having no ratings 
all_reviews <- remDr$findElements(using = 'xpath', "//div[starts-with(@class, 'lister-item-content')]")

ratings1 <- vector("character", length(all_reviews))

for (i in seq_along(all_reviews)) {
  review <- all_reviews[[i]]
  
  ratingElem <- tryCatch({
    review$findChildElement("css", value = "span.rating-other-user-rating")
  }, error = function(e) NULL)
  
  ratings1[i] <- if (!is.null(ratingElem)) ratingElem$getElementText() %>% str_extract("\\d+") else NA
}

ratings1 <- head(ratings1,1500)

Whiplash_Reviews <- data.frame(usernames1, titles1, dates1, reviews1, ratings1)
View(Whiplash_Reviews)

csv_file_path <- "C:/Users/Dany/Downloads/Whiplash_Reviews.csv"
write.csv(Whiplash_Reviews, csv_file_path, row.names = FALSE)

#####Avengers: Infinity War sentiment#####

remDr$navigate("https://www.imdb.com/title/tt4154756/reviews?ref_=tt_urv")
Sys.sleep(2)

scroll_navigation()
dropdown_all()

usernamesElem2 <- remDr$findElements(using = 'xpath', "//span[starts-with(@class, 'display-name-link')]")
usernames2 <- unlist(lapply(usernamesElem2, function(x){x$getElementText()}))
usernames2 <- head(usernames2, 1500)

titleElem2 <- remDr$findElements(using = 'xpath', "//a[starts-with(@class, 'title')]")
titles2 <- unlist(lapply(titleElem2, function(x){x$getElementText()}))
titles2 <- head(titles2, 1500)

dateElem2 <- remDr$findElements(using = 'xpath', "//span[starts-with(@class, 'review-date')]")
dates2 <- unlist(lapply(dateElem2, function(x){x$getElementText()}))
dates2 <- head(dates2, 1500)

reviewElem2 <- remDr$findElements(using = 'xpath', "//div[starts-with(@class, 'text show-more__control')]")
reviews2 <- unlist(lapply(reviewElem2, function(x){x$getElementText()}))
reviews2 <- head(reviews2,1500)

all_reviews2 <- remDr$findElements(using = 'xpath', "//div[starts-with(@class, 'lister-item-content')]")

ratings2 <- vector("character", length(all_reviews2))

for (i in seq_along(all_reviews2)) {
  review <- all_reviews2[[i]]
  
  ratingElem <- tryCatch({
    review$findChildElement("css", value = "span.rating-other-user-rating")
  }, error = function(e) NULL)
  
  ratings2[i] <- if (!is.null(ratingElem)) ratingElem$getElementText() %>% str_extract("\\d+") else NA
}

ratings2 <- head(ratings2,1500)

InfWar_Reviews <- data.frame(usernames2, titles2, dates2, reviews2, ratings2)
View(InfWar_Reviews)

csv_file_path2 <- "C:/Users/Dany/Downloads/InfWar_Reviews.csv"
write.csv(InfWar_Reviews, csv_file_path2, row.names = FALSE)

#####The Matrix sentiment#####

remDr$navigate("https://www.imdb.com/title/tt0133093/reviews?ref_=tt_urv")
Sys.sleep(2)

scroll_navigation()
dropdown_all()

usernamesElem3 <- remDr$findElements(using = 'xpath', "//span[starts-with(@class, 'display-name-link')]")
usernames3 <- unlist(lapply(usernamesElem3, function(x){x$getElementText()}))
usernames3 <- head(usernames3, 1500)

titleElem3 <- remDr$findElements(using = 'xpath', "//a[starts-with(@class, 'title')]")
titles3 <- unlist(lapply(titleElem3, function(x){x$getElementText()}))
titles3 <- head(titles3, 1500)

dateElem3 <- remDr$findElements(using = 'xpath', "//span[starts-with(@class, 'review-date')]")
dates3 <- unlist(lapply(dateElem3, function(x){x$getElementText()}))
dates3 <- head(dates3, 1500)

reviewElem3 <- remDr$findElements(using = 'xpath', "//div[starts-with(@class, 'text show-more__control')]")
reviews3 <- unlist(lapply(reviewElem3, function(x){x$getElementText()}))
reviews3 <- head(reviews3,1500)

all_reviews3 <- remDr$findElements(using = 'xpath', "//div[starts-with(@class, 'lister-item-content')]")

ratings3 <- vector("character", length(all_reviews3))

for (i in seq_along(all_reviews3)) {
  review <- all_reviews3[[i]]
  
  ratingElem <- tryCatch({
    review$findChildElement("css", value = "span.rating-other-user-rating")
  }, error = function(e) NULL)
  
  ratings3[i] <- if (!is.null(ratingElem)) ratingElem$getElementText() %>% str_extract("\\d+") else NA
}

ratings3 <- head(ratings3,1500)

Matrix_Reviews <- data.frame(usernames3, titles3, dates3, reviews3, ratings3)
View(Matrix_Reviews)

csv_file_path3 <- "C:/Users/Dany/Downloads/Matrix_Reviews.csv"
write.csv(Matrix_Reviews, csv_file_path3, row.names = FALSE)

#####Spirited Away#####

remDr$navigate("https://www.imdb.com/title/tt0245429/reviews?ref_=tt_urv")
Sys.sleep(2)

scroll_navigation()
dropdown_all()

usernamesElem4 <- remDr$findElements(using = 'xpath', "//span[starts-with(@class, 'display-name-link')]")
usernames4 <- unlist(lapply(usernamesElem4, function(x){x$getElementText()}))
usernames4 <- head(usernames4, 1500)

titleElem4 <- remDr$findElements(using = 'xpath', "//a[starts-with(@class, 'title')]")
titles4 <- unlist(lapply(titleElem4, function(x){x$getElementText()}))
titles4 <- head(titles4, 1500)

dateElem4 <- remDr$findElements(using = 'xpath', "//span[starts-with(@class, 'review-date')]")
dates4 <- unlist(lapply(dateElem4, function(x){x$getElementText()}))
dates4 <- head(dates4, 1500)

reviewElem4 <- remDr$findElements(using = 'xpath', "//div[starts-with(@class, 'text show-more__control')]")
reviews4 <- unlist(lapply(reviewElem4, function(x){x$getElementText()}))
reviews4 <- head(reviews4,1500)

all_reviews4 <- remDr$findElements(using = 'xpath', "//div[starts-with(@class, 'lister-item-content')]")

ratings4 <- vector("character", length(all_reviews4))

for (i in seq_along(all_reviews4)) {
  review <- all_reviews4[[i]]
  
  ratingElem <- tryCatch({
    review$findChildElement("css", value = "span.rating-other-user-rating")
  }, error = function(e) NULL)
  
  ratings4[i] <- if (!is.null(ratingElem)) ratingElem$getElementText() %>% str_extract("\\d+") else NA
}

ratings4 <- head(ratings4,1500)

Spirited_Reviews <- data.frame(usernames4, titles4, dates4, reviews4, ratings4)
View(Spirited_Reviews)

csv_file_path4 <- "C:/Users/Dany/Downloads/Spirited_Reviews.csv"
write.csv(Spirited_Reviews, csv_file_path4, row.names = FALSE)

# rD[["server"]]$stop()
# system("taskkill /im java.exe /f") # Stops the java.exe
# rm(list = ls())











