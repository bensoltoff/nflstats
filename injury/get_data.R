library(rvest)
library(stringr)
library(plyr)
library(dplyr)
library(ggvis)
library(knitr)
options(digits = 4)

# Let phantomJS scrape injury report, output is written to injury.html
url <- "http://www.nfl.com/injuries?week=1"

writeLines(sprintf("var page = require('webpage').create();
page.open('%s', function () {
                   page.evaluate(function(){

                   });
                   fs.write('1.html', page.content, 'w');

                   phantom.exit();
                   });", url), con = "injury/scrape.js")

system("injury/phantomjs injury/test.js > injury/scrape.html")

test <- read_html("injury/1.html") %>%
  html_nodes("data-table-PIT-1 data-injuries") %>%
  html_text() %>%
  cat








