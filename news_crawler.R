library(httr)
library(rvest)
library(xml2)
library(stringr)

news_crawl <- function(input) {
  counter <- 1
  word_counts <- c()
  while (TRUE) {
    url <-
      paste0(
        "https://search.naver.com/search.naver?&where=news&query=",
        input$keyword,
        "&pd=3&ds=",
        input$date_range[1],
        "&de=",
        input$date_range[2],
        "&start=",
        counter
      )

    data <- GET(url)

    print(data$status_code)
    
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
    print(title)
  }

  word_freq <- table(word_counts)
  filtered_freq <- word_freq[word_freq > 1]
  sorted_freq <- sort(filtered_freq, decreasing = TRUE) %>% as.data.frame()
  
  print(sorted_freq)
  return(sorted_freq)
}