# Google Home example
# 2019/9/10

# product review and review date
# Review carefully 'for' loop and cbind and rbind. 'for' loop enables you to collect multiple pages of reviews
# cbind, column bind, makes you to have nice a table of reviews which can be easily analyzed by statistical approach such as regression.
# http://www.endmemo.com/program/R/rbind.php

# 1. Install two packages
install.packages('rvest')
install.packages('RCurl')

library(rvest)
library(RCurl)

# 2. Identify the web address 
# search product - click reviews - sort by recent
# remove the page number at the end of the URL

url <- "https://www.amazon.com/product-reviews/B001FWXKTA/ref=cm_cr_arp_d_viewopt_srt?ie=UTF8&filterByStar=five_star&reviewerType=all_reviews&sortBy=recent&pageNumber="
  #"https://www.amazon.com/product-reviews/B01MDJ0HVG/ref=cm_cr_arp_d_viewopt_srt?sortBy=recent&pageNumber="
#url <- " your url "

# 3. Specify how many pages you would like to scrape
N_pages <- 300 # It would be easier to test with small number of pages and it depends on your own source website

A <- NULL
for (j in 1: N_pages){
  gh <- read_html(paste0(url, j)) 
  #help paste: http://www.cookbook-r.com/Strings/Creating_strings_from_variables/
  B <- cbind(gh %>% 
               html_nodes(".review-text") %>% 
               html_text(), gh %>%     
               html_nodes("#cm_cr-review_list .review-date") %>% 
               html_text()     )
# I replaced html_nodes for review date with "#cm_cr-review_list .review-date"
# "#cm_cr-review_list" makes two irrelevant parts for the top positive/critical reviews and '#'is the magic sign for unselect the part
  A <- rbind(A,B)
}

# 4. Make sure what you got
print(j) # This command shows the progress of the for loop. This example it means number of pages.


#A[,1] # this will print the first column of your output

# 4.1 Another way to double-check
head(A)
tail(A,10)

# 5. Save the output
write.csv(data.frame(A),"warm5star.csv") #you can change the file name
