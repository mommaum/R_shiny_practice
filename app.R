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

source("api_call.R")
source("single_analysis.R")
source("compare_analysis.R")
source("utils.R")
source("validation.R")

input_keyword <- function(id) {
  textInput(id, label = "키워드")
}

general_inputs <- list(
  dateRangeInput("date_range", "기간", start = Sys.Date() - 90, end = Sys.Date()),
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
  theme = bslib::bs_theme(bootswatch = "minty"),
  # 테마 사용
  useShinyFeedback(),
  # 피드백 사용
  titlePanel("네이버 검색어 분석기"),
  tabsetPanel(
    tabPanel(
      "추세 확인",
      sidebarLayout(
        sidebarPanel(
          general_inputs,
          input_keyword("keyword"),
          textOutput("empty_input_warning"),
          textOutput("date_order_warning"),
          detail_inputs,
          actionButton(inputId = "submit", label = "입력 확인"),
        ),
        mainPanel(
          plotOutput("single_plot"),
          textOutput("single_analysis"),
          conditionalPanel(
            condition = "input.submit > 0",
            downloadButton("download_report", "보고서 다운로드")
          )
        )
      ),
    ),
    tabPanel(
      "검색어 비교",
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
          plotOutput("compare_plot"),
          plotOutput("scatter_plot"),
          textOutput("cor")
        )
      )
    ),
  ),
)

server <- function(input, output, session) {
  thematic::thematic_shiny()

  output$date_order_warning <-
    renderText(validate_date_order(input))

  output$empty_input_warning <-
    renderText(check_empty_input(input, "keyword"))
  output$empty_input_warning_1 <-
    renderText(check_empty_input(input, "keyword_1"))
  output$empty_input_warning_2 <-
    renderText(check_empty_input(input, "keyword_2"))

  observeEvent(input$closeModal, removeModal())

  observeEvent(input$submit, {
    if (input$keyword != "") {
      id <-
        showNotification("분석 중...", duration = NULL, closeButton = FALSE)
      on.exit(removeNotification(id), add = TRUE)

      single_analysis <-
        input %>%
        process_data_with_params() %>%
        analysis_single_data()

      output$single_analysis <-
        renderText(single_analysis$trend_analysis_result)
      output$single_plot <- renderPlot(single_analysis$graph)

      output$download_report <- downloadHandler(
        filename = "report.html",
        content = function(file) {
          id <- showNotification("리포트 작성중...",
            duration = NULL,
            closeButton = FALSE
          )
          on.exit(removeNotification(id), add = TRUE)
          rmarkdown::render("report.Rmd",
            output_file = file
          )
        }
      )
    } else {
      showModal(modalDialog(
        title = "경고",
        "키워드를 입력하세요.",
        easyClose = TRUE,
        footer = tagList(actionButton("closeModal", "닫기"))
      ))
    }
  })

  observeEvent(input$submit_2, {
    if (input$keyword_1 != "" && input$keyword_2 != "") {
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
    } else {
      showModal(modalDialog(
        title = "경고",
        "키워드를 입력하세요.",
        easyClose = TRUE,
        footer = tagList(actionButton("closeModal", "닫기"))
      ))
    }
  })
}

shinyApp(ui = ui, server = server)
