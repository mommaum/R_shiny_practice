input_keyword <- function(id) {
  textInput(id, label = "키워드")
}

general_inputs <- list(
  dateRangeInput("date_range", "기간", start = Sys.Date() - 90, end = Sys.Date()),
  selectInput(
    "time_unit",
    "구간 단위",
    choices = c("일간" = "date", "주간" = "week", "월간" = "month")
  )
)

detail_inputs <- list(
  radioButtons(
    "device",
    "기기",
    choices = c("전체", "pc", "mo"),
    selected = "전체"
  ),
  radioButtons(
    "gender",
    "성별",
    choices = c("전체", "남성", "여성"),
    selected = "전체"
  ),
  radioButtons(
    "ages",
    "나이",
    choices = c("전체", "미성년", "20대", "30대", "40대", "50대", "60대 이상"),
    selected = "전체"
  )
)
