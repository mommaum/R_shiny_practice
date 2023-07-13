data_insert <- function(input, trend, related) {
  if (nrow(related) == 0) {
    return()
  }

  query <-
    paste0(
      "INSERT INTO keyword VALUE(",
      '"',
      input$keyword,
      '"',
      ",",
      '"',
      Sys.Date(),
      '"',
      ",",
      '"',
      trend,
      '"',
      ",",
      '"',
      related$word_count[[1]],
      '"',
      ",",
      '"',
      related$word_count[[2]],
      '"',
      ",",
      '"',
      related$word_count[[3]],
      '"',
      ",",
      '"',
      related$word_count[[4]],
      '"',
      ")"
    )

  print(query)
  dbGetQuery(db, query)
}
