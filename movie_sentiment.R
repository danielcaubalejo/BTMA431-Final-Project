library(tidyverse)

#movies sorted by audience score "fresh" - at least 60% positive
remDr$navigate("https://www.rottentomatoes.com/browse/movies_at_home/audience:upright~sort:popular")

title_navigation <- function(){
  
  #click load more code taken from: https://github.com/ggSamoora/TutorialsBySamoora/blob/main/rate_my_professor_script.Rmd
  loadmore <- remDr$findElement(using = "xpath", value = '//*[@data-qa="dlp-load-more-button"]')
  
  #initialize page number
  page <- 0
  # Loop until page 15, anything 15> webpage begins to load very slow
  while (page < 15) {
    #Navigate to the webpage with the current page number
    #url <- paste0("https://www.rottentomatoes.com/browse/movies_at_home/sort:popular?page=", page)
    
    #webpage scroll test
    #y_position <- loadmore$getElementLocation()$y - 100
    #remDr$executeScript(sprintf("window.scrollTo(0, %f)", y_position))
    
    loadmore$clickElement()
    # Increment the page number
    page <- page + 1
    # Pause code to ensure webpage loads properly
    Sys.sleep(1)
  }
}

title_navigation()

webElems1 <- remDr$findElements(using = 'xpath', "//span[starts-with(@class, 'p--small')]")

webElems2 <- remDr$findElements(using = 'xpath', "//a[starts-with(@data-track, 'scores')]")
movie_urls <- map(webElems2, ~.$getElementAttribute("href") %>% unlist())

movieTitles <- unlist(lapply(webElems1, function(x){x$getElementText()}))
movieTitles <- as.data.frame(movieTitles)
movieTitles <- movieTitles[!apply(movieTitles == "", 1, all),]

remDr$navigate("https://www.rottentomatoes.com/m/fifty_shades_of_grey")
webElems3 <- remDr$findElements(using = 'xpath', "//span[starts-with(@class, 'genre')]")
genre <- unlist(lapply(webElems3, function(x){x$getElementText()}))

remDr$navigate("https://www.rottentomatoes.com/m/fifty_shades_of_grey/reviews")
webElems4 <- remDr$findElements(using = 'xpath', "//div[starts-with(@class, 'review-row')]")
reviews <- unlist(lapply(webElems4, function(x){x$getElementText()}))
reviews <- as.data.frame(reviews)
View(reviews)
