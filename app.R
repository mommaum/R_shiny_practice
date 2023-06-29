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

source("api_call.R")
source("single_analysis.R")
source("compare_analysis.R")
source("utils.R")

input_keyword <- function(id) {
  textInput(id, label = "키워드")
}

general_inputs <- list(
  dateRangeInput("date_range", "기간", start = Sys.Date() - 365, end = Sys.Date()),
  selectInput(
    inputId = "time_unit",
    label = "구간 단위",
    choices = c("일간" = "date", "주간" = "week", "월간" = "month")
  )
)

detail_inputs <- list(
  radioButtons(
    inputId = "device",
    label = "기기",
    choices = c("전체", "pc", "mo"),
    selected = "전체"
  ),
  radioButtons(
    inputId = "gender",
    label = "성별",
    choices = c("전체", "남성", "여성"),
    selected = "전체"
  ),
  radioButtons(
    inputId = "ages",
    label = "나이",
    choices = c("전체", "미성년", "20대", "30대", "40대", "50대", "60대 이상"),
    selected = "전체"
  )
)

ui <- fluidPage(
  theme = bslib::bs_theme(bootswatch = "minty"), # 테마 사용
  useShinyFeedback(), # 피드백 사용
  titlePanel("네이버 검색어 분석기"),
  tabsetPanel(
    tabPanel("검색어 분석",
             sidebarLayout(
               sidebarPanel(
                 general_inputs,
                 input_keyword("keyword"),
                 textOutput("empty_input_warning"),
                 textOutput("date_order_warning"),
                 detail_inputs,
                 actionButton(inputId = "submit", label = "입력 확인"),
                 downloadButton("download_report")
               ),
               mainPanel(plotOutput("single_plot"),
                         textOutput("single_analysis"),)
             ),),
    tabPanel("검색어 비교",
             sidebarLayout(
               sidebarPanel(
                 general_inputs,
                 input_keyword("keyword_1"),
                 input_keyword("keyword_2"),
                 detail_inputs,
                 actionButton(inputId = "submit_2", label = "입력 확인"),
               ),
               mainPanel(
                 plotOutput("compare_plot"),
                 plotOutput("scatter_plot"),
                 textOutput("cor")
               )
             )),
    tabPanel("검색기록",)
  ),
)

server <- function(input, output, session) {
  thematic::thematic_shiny()
  
  check_empty_input <- reactive({
    is_empty <- input$keyword == ""
    shinyFeedback::feedbackWarning("keyword", is_empty, "검색어를 입력해주세요.")
  })
  
  validate_date_order <- reactive({
    wrong_date_order <- input$date_range[1] > input$date_range[2]
    shinyFeedback::feedbackWarning("date_range", wrong_date_order, "검색 시작 날짜가 더 빨라야 합니다.")
  })
  
  output$empty_input_warning <- renderText(check_empty_input())
  output$date_order_warning <- renderText(validate_date_order())
  
  observeEvent(input$submit, {
    single_analysis <-
      input %>% process_data_with_params() %>% analysis_single_data()
    
    output$single_analysis <- renderText({
      single_analysis$trend_analysis_result
    })
    output$single_plot <- renderPlot({
      single_analysis$graph
    })
    output$download_report <- downloadHandler(
      filename = "report.html",
      content = function(file) {
        
        id <- showNotification(
          "리포트 작성중...",
          duration = NULL,
          closeButton = FALSE
        )
        on.exit(removeNotification(id), add = TRUE)
        rmarkdown::render(
          "report.Rmd",
          output_file = file
        )
      }
    )
  })
  
  observeEvent(input$submit_2, {
    params <- process_data_with_params(input)
    
    compare_analysis <-
      compare_process_data(params)
    
    output$scatter_plot <- renderPlot({
      compare_analysis$scatter_plot
    })
    output$compare_plot <- renderPlot({
      compare_analysis$graph
    })
    output$cor <- renderText({
      compare_analysis$cor
    })
  })
}

shinyApp(ui = ui, server = server)
