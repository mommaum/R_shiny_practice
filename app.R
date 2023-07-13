# install.packages("shiny")
# install.packages("ggplot2")
# install.packages("jsonlite")
# install.packages("tseries")
# install.packages("httr")
# install.packages("dotenv")
# install.packages("shinyFeedback")
# install.packages("thematic")
# install.packages("magrittr")
# install.packages("dplyr")
# install.packages("styler")
# install.packages("waiter")
# install.packages("reactlog")
# install.packages("shinyloadtest")
# install.packages('wordcloud2')

library(shiny)
library(ggplot2)
library(jsonlite)
library(tseries)
library(httr)
library(dotenv)
library(shinyFeedback)
library(thematic)
library(magrittr)
library(dplyr)
library(styler)
library(waiter)
library(reactlog)
library(shinyloadtest)
library(wordcloud2)
library(RMySQL)
library(DBI)
library(tidyverse)
library(ragg)

source("api_call.R")
source("single_analysis.R")
source("compare_analysis.R")
source("utils.R")
source("validation.R")
source("modal.R")
source("news_crawler.R")
source("inputs.R")
source("database_insert.R")
source("global.R")
source("update_query.R")
source("database_setting.R")

ui <- fluidPage(
  theme = bslib::bs_theme(bootswatch = "minty"),
  useShinyFeedback(),

  # UI 구성
  titlePanel("네이버 검색어 분석"),
  tabsetPanel(
    tabPanel(
      "검색어 분석하기",
      sidebarLayout(
        sidebarPanel(
          general_inputs,
          input_keyword("keyword"),
          detail_inputs,
          actionButton("submit", "분석하기"),
          actionButton("reset", "설정 초기화"),
          textOutput("empty_input_warning"),
          textOutput("date_order_warning"),
        ),
        mainPanel(
          textOutput("single_analysis_explain"),
          verbatimTextOutput("single_analysis_description"),
          verbatimTextOutput("single_analysis"),
          plotOutput("single_plot"),
          verbatimTextOutput("word_freq_analysis_description"),
          fluidRow(
            column(
              width = 4,
              tableOutput("word_freq_data_frame")
            ),
            column(
              width = 8,
              wordcloud2Output("wordcloud_output")
            )
          ),
        )
      ),
    ),
    tabPanel(
      "검색어 비교하기",
      sidebarLayout(
        sidebarPanel(
          general_inputs,
          input_keyword("keyword_1"),
          input_keyword("keyword_2"),
          detail_inputs,
          actionButton(inputId = "submit_2", label = "입력 확인"),
          textOutput("empty_input_warning_1"),
          textOutput("empty_input_warning_2"),
        ),
        mainPanel(
          textOutput("compare_analysis_explain"),
          plotOutput("compare_plot"),
          verbatimTextOutput("cor_information"),
          textOutput("cor"),
          plotOutput("scatter_plot"),
        )
      )
    ),
    tabPanel(
      "데이터베이스",
      sidebarLayout(
        sidebarPanel(
          radioButtons("type_of_searching", "검색방법", choiceNames = c("키워드로", "연관검색어로"), choiceValues = c("keyword", "related")),
          textInput("search_by_keyword", "검색"),
          # dateInput("search_by_date", "날짜로찾기"),
          radioButtons("sort_by_keyword", "정렬", choiceNames = c("없음", "오름차순", "내림차순"), choiceValues = c("", "ORDER BY 키워드 ASC", "ORDER BY 키워드 DESC")),
          radioButtons("search_by_trend", "추세로찾기", choiceNames = c("전부", "없음", "상승", "하락"), choiceValues = c("", "트렌드 = \"없음\"", "트렌드 = \"상승\"", "트렌드 = \"하락\"")),
          actionButton("reset_search_condition", "설정 초기화"),
        ),
        mainPanel(
          textOutput("database_guide"),
          actionButton(inputId = "submit_3", label = "DB 갱신"),
          tableOutput("database")
        )
      )
    )
  ),
)

server <- function(input, output, session) {
  thematic_shiny()
  output$single_analysis_explain <- renderText("분석하고 싶은 키워드를 입력해보세요 ! 검색량 추세 및 네이버 뉴스 크롤링 데이터 제공합니다")
  output$compare_analysis_explain <- renderText("두 검색어 간의 검색량 비율과 상관관계를 분석합니다")

  output$date_order_warning <-
    renderText(validate_date_order(input))

  output$empty_input_warning <-
    renderText(check_empty_input(input, "keyword"))
  output$empty_input_warning_1 <-
    renderText(check_empty_input(input, "keyword_1"))
  output$empty_input_warning_2 <-
    renderText(check_empty_input(input, "keyword_2"))

  observeEvent(input$closeModal, removeModal())

  observeEvent(input$reset, {
    updateTextInput(session, "keyword", value = "")
    updateRadioButtons(session, "device", selected = "전체")
    updateRadioButtons(session, "gender", selected = "전체")
    updateRadioButtons(session, "ages", selected = "전체")
  })

  observeEvent(input$submit, {
    
    if (input$keyword != "") {
    
      jsonData <-
        input %>%
        process_data_with_params()

      output$download_report <- renderUI(downloadButton("download_report", "보고서 다운로드"))

      output$wordcloud_output <- renderWordcloud2({
        wordcloud2(word_freq_table)
      })

      word_freq_table <-  news_crawl(input)

      output$word_freq_data_frame <- renderTable(
        word_freq_table
      )

      single_analysis <- analysis_single_data(jsonData, input)

      output$single_analysis <-
        renderPrint(single_analysis$trend_analysis_result)
      output$single_plot <- renderPlot(single_analysis$graph)
      output$single_analysis_description <- renderPrint({
        cat("검색량 추세 확인을 위한 시계열 분석 \nAugmented Dickey-Fuller 검정을 사용하였습니다. (유의성 5%)")
      })

      output$word_freq_analysis_description <- renderPrint({
        cat("키워드로 검색한 네이버 뉴스 제목을 크롤링하여 자주 등장하는 단어들을 모았습니다.\n10개의 기사를 검색했습니다 (관련성 기준).")
      })

      # 보고서 다운로드
      # downloadHandler(
      #   filename = "report.html",
      #   content = function(file) {
      #     id <- showNotification("리포트 작성중...",
      #       duration = NULL,
      #       closeButton = FALSE
      #     )
      #     on.exit(removeNotification(id), add = TRUE)
      #     rmarkdown::render("report.Rmd",
      #       output_file = file
      #     )
      #   }
      # )
    } else {
      showModal(create_modal("안내", "키워드가 비어있습니다."))
    }

    data_insert(input, single_analysis$trend, word_freq_table)
    database <- renderTable(dbGetQuery(db, "SELECT * FROM keyword"))
  })


  observeEvent(input$submit_2, {
    if (input$keyword_1 != "" && input$keyword_2 != "") {
      id <- showNotification("분석중 ...", duration = NULL, closeButton = FALSE)
      on.exit(removeNotification(id), add = TRUE)

      jsonData <- process_data_with_params_2(input)
      compare_analysis <-
        compare_process_data(jsonData, input)

      output$scatter_plot <- renderPlot({
        compare_analysis$scatter_plot
      })
      output$compare_plot <- renderPlot({
        compare_analysis$graph
      })
      output$cor <- renderText({
        compare_analysis$cor
      })

      output$cor_information <- renderPrint(cat("|상관계수| >= 0.7 : 강한 상관관계 \n0.7 > |상관계수| > 0.3: 약한 상관관계 \n|상관계수| <= 0.3: 상관관계 없음"))
    } else {
      showModal(create_modal("안내", "키워드가 비어있습니다"))
    }
  })

  observeEvent(input$submit_3, {
    dbGetQuery(db, "select * from keyword")
    output$database <- renderTable({
      dbGetQuery(db, query())
    })
  })
  
  observeEvent(input$reset_search_condition, {
    updateTextInput(session, "search_by_keyword", value = "")
    updateRadioButtons(session, "sort_by_keyword", selected = "")
    updateRadioButtons(session, "search_by_trend", selected = "")
  })
  
  query <- reactive({
    update_query(input)
  })

  output$database <- renderTable({
    dbGetQuery(db, query())
  })
  
  output$database_guide <- renderText("검색어 분석하기 탭의 결과가 저장됩니다. ")
}

shinyApp(ui = ui, server = server)
