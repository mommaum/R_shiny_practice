process_data_with_params <- function(input) {
  
  keyword <- gsub("\\s+", "", input$keyword)
  
  params <- list(
    startDate = as.character(input$date_range[1]),
    endDate = as.character(input$date_range[2]),
    timeUnit = input$time_unit,
    keywordGroups = list(list(
      groupName = keyword,
      keywords = list(keyword)
    ))
  )

  params$device <- if (input$device != "전체") ifelse(input$device == "pc", "pc", "mo")
  params$gender <- if (input$gender != "전체") ifelse(input$gender == "남성", "m", "f")

  if (input$ages == "미성년") {
    params$ages <- list("1", "2")
  } else if (input$ages == "20대") {
    params$ages <- list("3", "4")
  } else if (input$ages == "30대") {
    params$ages <- list("5", "6")
  } else if (input$ages == "40대") {
    params$ages <- list("7", "8")
  } else if (input$ages == "50대") {
    params$ages <- list("9", "10")
  } else if (input$ages == "60대 이상") {
    params$ages <- list("11")
  }
  
  print(params)

  jsonData <- toJSON(params, auto_unbox = TRUE)

  print(jsonData)
  return(jsonData)
}

process_data_with_params_2 <- function(input) {
  
  keyword_1 <- gsub("\\s+", "", input$keyword_1)
  keyword_2 <- gsub("\\s+", "", input$keyword_2)
  
  params <- list(
    startDate = as.character(input$date_range[1]),
    endDate = as.character(input$date_range[2]),
    timeUnit = input$time_unit,
    keywordGroups = list(
      list(groupName = keyword_1, keywords = list(keyword_1)),
      list(groupName = keyword_2, keywords = list(keyword_2))
    )
  )

  params$device <- if (input$device != "전체") ifelse(input$device == "pc", "pc", "mo")
  params$gender <- if (input$gender != "전체") ifelse(input$gender == "남성", "m", "f")

  if (input$ages == "미성년") {
    params$ages <- list("1", "2")
  } else if (input$ages == "20대") {
    params$ages <- list("3", "4")
  } else if (input$ages == "30대") {
    params$ages <- list("5", "6")
  } else if (input$ages == "40대") {
    params$ages <- list("7", "8")
  } else if (input$ages == "50대") {
    params$ages <- list("9", "10")
  } else if (input$ages == "60대 이상") {
    params$ages <- list("11")
  }

  print(params)
  
  jsonData <- toJSON(params, auto_unbox = TRUE)

  return(jsonData)
}
