install.packages('RSelenium')
library(RSelenium)


rD <- rsDriver(browser = "firefox",
               chromever = NULL)

remDr <- rD[["client"]]

#going to specific website
remDr$navigate("https://www.rottentomatoes.com/browse/movies_at_home/sort:popular")
remDr$screenshot(display = TRUE)

#entering text into search
#webElem1 <- remDr$findElement(using = "name", value = "title")
#webElem1$sendKeysToElement(list('information systems', key = 'enter'))
#webElem1$clearElement()

#finding specific elements
webElems1 <- remDr$findElements(using = 'xpath', "//span[starts-with(@class, 'p--small')]")

movieTitles <- unlist(lapply(webElems1, function(x){x$getElementText()}))
movieTitles <- as.data.frame(movieTitles)
movieTitles <- movieTitles[!apply(movieTitles == "", 1, all),]

webElems2 <- remDr$findElements(using = 'tag name', "i")
authornames <- unlist(lapply(webElems2, function(x){x$getElementText()}))

books <- cbind(bookTitles,authornames)

webElems3 <- remDr$findElements(using = 'tag name', "td")
allTableData <- unlist(lapply(webElems3, function(x){x$getElementText()}))
allTableData <- as.data.frame(allTableData)
View(allTableData)

allTableData2 <- tail(allTableData, -15)
View(allTableData2)

allTableData3 <- as.matrix(allTableData2, nrow = 4)
dim(allTableData3) <- c(4, n + 1) # Recall n <- NROW(bookTitles)

# This tells R to make the matrix 4 rows by n columns
View(allTableData3)



remDr$getTitle()
remDr$getCurrentUrl()

# rD[["server"]]$stop()
# system("taskkill /im java.exe /f") # Stops the java.exe
# rm(list = ls())



