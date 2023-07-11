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

source("api_call.R")
source("single_analysis.R")
source("compare_analysis.R")
source("utils.R")
source("validation.R")
source("modal.R")
source("news_crawler.R")
source("inputs.R")

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
            column(width = 4,
                   tableOutput("word_freq_data_frame")
            ),
            column(width = 8,
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
      "데이터베이스"
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
      single_analysis <-
        input %>%
        process_data_with_params() %>%
        analysis_single_data()

      output$download_report <- renderUI(downloadButton("download_report", "보고서 다운로드"))

      output$wordcloud_output <- renderWordcloud2({
        wordcloud2(word_freq_table)
      })

      word_freq_table <- input %>% news_crawl()
      output$word_freq_data_frame <- renderTable(word_freq_table)

      output$single_analysis <-
        renderPrint(single_analysis$trend_analysis_result)
      output$single_plot <- renderPlot(single_analysis$graph)
      output$single_analysis_description <- renderPrint({
        cat("검색량 추세 확인을 위한 시계열 분석 \nAugmented Dickey-Fuller 검정을 사용하였습니다. (유의성 5%)")
      })

      output$word_freq_analysis_description <- renderPrint({
        cat("키워드로 검색한 네이버 뉴스 제목을 크롤링하여 자주 등장하는 단어들을 모았습니다.\n30개의 기사를 검색했습니다 (관련성 기준).")
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
  })


  observeEvent(input$submit_2, {
    if (input$keyword_1 != "" && input$keyword_2 != "") {
      
      id <- showNotification("분석중 ...", duration = NULL, closeButton = FALSE)
      on.exit(removeNotification(id), add = TRUE)
      
      jsonData <- process_data_with_params_2(input)
      compare_analysis <-
        compare_process_data(jsonData)

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
}

shinyApp(ui = ui, server = server)
