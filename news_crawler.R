library(httr)
library(rvest)
library(xml2)
library(stringr)

counter <- 1
word_counts <- c()

news_crawl <- function(input) {

  keyword <- gsub("\\s+", "", input$keyword)
  
  withProgress(message = "네이버 뉴스 크롤링 중...", {
    while (TRUE) {
      url <-
        paste0(
          "https://search.naver.com/search.naver?&where=news&query=",
          keyword,
          "&pd=3&ds=",
          input$date_range[1],
          "&de=",
          input$date_range[2],
          "&start=",
          counter
        )

      data <- GET(url)

      if (data$status_code != 200) {
        break
      }
      
      title <- data %>%
        read_html(encoding = "UTF-8") %>%
        html_nodes(xpath = '//*[@id="sp_nws1"]/div/div/a') %>%
        html_attr("title")
      
      if (length(title) == 0 || counter > 10) {
        break
      }

      # 특수문자 제거 및 단어 단위로 분리
      clean_titles <-
        str_replace_all(title, "[^[:alnum:]\\s]", "") # 특수문자 제거
      words <- strsplit(clean_titles, "\\s+") # 단어 단위로 분리

      # 단어 빈도 카운트
      word_counts <- c(word_counts, unlist(words))
      
      counter <- counter + 1
      incProgress(1 / 10)
    }
  })

  if (is.null(word_counts)) {
    word_counts <- c("-","검색","내역","없음")
    word_freq <- table(word_counts)
    sorted_freq <- sort(word_freq, decreasing = TRUE) %>% as.data.frame()
    
  } 
    word_freq <- table(word_counts)
    sorted_freq <- sort(word_freq, decreasing = TRUE) %>% as.data.frame()

  return(sorted_freq)
}
