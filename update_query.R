update_query <- function(input) {
  query <- "SELECT * FROM keyword"
  is_exist_keyword <- FALSE

  if (input$search_by_keyword != "") {
    if (input$type_of_searching == "keyword") {
      query <-
        paste(query,
          " WHERE 키워드 =",
          "'",
          input$search_by_keyword,
          "'",
          sep = ""
        )
    } else if (input$type_of_searching == "related") {
      query <-
        paste(
          query,
          " WHERE",
          "'",
          input$search_by_keyword,
          "'",
          "IN (연관검색어, 연관검색어_2, 연관검색어_3, 연관검색어_4)",
          sep = ""
        )
    }

    is_exist_keyword <- TRUE
  }

  if (is_exist_keyword == TRUE && input$search_by_trend != "") {
    query <-
      paste(
        query,
        "AND",
        input$search_by_trend,
        input$sort_by_keyword
      )
  } else if (is_exist_keyword == FALSE && input$search_by_trend != ""){
    query <-
      paste(
        query,
        "WHERE",
        input$search_by_trend,
        input$sort_by_keyword
      )
  } else {
    query <-
      paste(
        query,
        input$search_by_trend,
        input$sort_by_keyword
      )
  }
  print(query)
  query
}
