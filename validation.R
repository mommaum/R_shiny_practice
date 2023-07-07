check_empty_input <- function(input, inputID) {
  is_empty <- input[[inputID]] == ""
  shinyFeedback::feedbackWarning(inputID, is_empty, "검색어를 입력해주세요.")
}

validate_date_order <- function(input) {
  wrong_date_order <- input$date_range[1] > input$date_range[2]
  shinyFeedback::feedbackWarning("date_range", wrong_date_order, "검색 시작 날짜가 더 빨라야 합니다.")
}
