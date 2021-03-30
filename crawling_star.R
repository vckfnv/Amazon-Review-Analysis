
# Install three programs
# selenium-server-standalone-3.11.0.jar



# ---------------------- < basics > ----------------------
rm(list = ls())
setwd("C:/Users/Taewan Kim/Desktop/texts")

# ---------------------- < packages > ----------------------

# 1. Install two packages
#install.packages('rvest')

Needed <- c("tidyverse", "plyr", "magrittr", "data.table", "gridExtra", 
		"rvest", "RCurl","tm","wordcloud","wordcloud2",
		"cluster","fpc","factoextra","Rtsne","RSelenium",
		"httr","stringr","XML","utils","KoNLP" ,"installr")

# 
install.packages(Needed, dependencies = TRUE)
# 
install.packages("RSelenium")


library(RSelenium)
library(tidyverse)
library(plyr)
library(magrittr)
library(data.table)
library(gridExtra)

library(rvest) # 크롤링
library(RCurl) # 크롤링
library(tm) # 텍스트 마이닝
library(wordcloud) # 워드 클라우드
library(wordcloud2) # 워드 클라우드2

library(cluster) # 클러스터링
library(fpc) # 클러스터링 시각화
library(factoextra) # 클러스터링 추가
library(Rtsne) # 시각화를 위한 차원 축소
library(RSelenium)

library(httr)
library(stringr)
library(XML)
library(utils)
library(KoNLP)
library(installr)

# ---------------------- < assigning web address > ----------------------


url <- "https://www.amazon.com/Nintendo-Switch-Neon-Joy-Discontinued-Manufacturer/product-reviews/B01MUAGZ49/ref=cm_cr_arp_d_viewopt_srt?ie=UTF8&reviewerType=all_reviews&sortBy=recent&pageNumber="

N_pages <- 10 # same system. you can change the number. 

 

# ---------------------- < crawling > ----------------------
# 	visit the below web and install them
#	https://sites.google.com/a/chromium.org/chromedriver/downloads
#	Be careful. Before you choose, you need to check the version of chrome that you are having.
 
ch <- wdman::chrome(port = 4445L, "78.0.3904.105")  # I am using Chrome version 78 so I installed this.


remDr <- remoteDriver(remoteServerAddr = "localhost",

                      port = 9515, # If you run the chromeDriver, you can find your port number in the 1st line

                      browserName = "chrome")

remDr$open()

data <- NULL
for (j in 1:N_pages) {
    remDr$navigate(paste0(url, j))
    frontPage <- remDr$getPageSource()
    B <- cbind(read_html(frontPage[[1]]) %>%
                   html_nodes(".review-text") %>% 
                   html_text(),
               read_html(frontPage[[1]]) %>%
                   html_nodes("#cm_cr-review_list .review-date") %>% 
                   html_text()							    )

    a <- read_html(frontPage[[1]]) %>%
        html_nodes("div.a-section.celwidget div.a-row a.a-link-normal") %>%
        html_attr("title")
    a <- a[!is.na(a)]
    a <- str_sub(a, 1, 1)
    B <- cbind(B, a)
    data <- rbind(data, B)
    print(paste0(round(j / N_pages * 100, 2), "% 완료 되었습니다."))
}

try(remDr$close())

rm(a, j, B, N_pages, frontPage, remDr, url, ch)


# ---------------------- < 텍스트 마이닝 > ----------------------

name <- "newdata.csv"

write.csv(data, name, row.names = F)

 