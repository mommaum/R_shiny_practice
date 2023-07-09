source("api_call.R")

compare_process_data <-
  function(jsonData) {
    response <- send_post_request(jsonData)
    
    object_data <- fromJSON(response)
    
    print(object_data$results[3][[1]][[1]])
    print(object_data$results[3][[1]][[2]])
    
    if (length(object_data$results[3][[1]][[1]]) == 0 || length(object_data$results[3][[1]][[2]]) == 0) {
      showModal(modalDialog(
        title = "경고",
        "결과가 수집되지 않는 검색어입니다. 다른 검색어를 입력하세요",
        easyClose = TRUE,
        footer = tagList(actionButton("closeModal", "닫기"))
      ))
      return(NULL)
    }

    parsed_data <- fromJSON(response)
    parsed_data_frame <- as.data.frame(parsed_data$results$data)
    parsed_data_frame$period <- as.Date(parsed_data_frame$period)
    parsed_data_frame$period.1 <-
      as.Date(parsed_data_frame$period.1)

    graph <- ggplot(parsed_data_frame, aes(x = period)) +
      geom_line(aes(y = ratio, color = "keyword_1")) +
      geom_line(aes(y = ratio.1, color = "keyword_2")) +
      scale_y_continuous(limits = c(0, 100), breaks = seq(0, 100, 20)) +
      labs(x = "date", y = "search volume")

    scatter_plot <-
      ggplot(parsed_data_frame, aes(x = ratio, y = ratio.1)) +
      geom_point() +
      xlab("keyword_1") +
      ylab("keyword_2") +
      ggtitle("scatter plot") +
      coord_fixed(ratio = max(parsed_data_frame$ratio) / max(parsed_data_frame$ratio.1))

    correlation <- round(cor(parsed_data_frame$ratio, parsed_data_frame$ratio.1), 2)


    if (correlation >= -0.3 && correlation <= 0.3) {
      correlation <- paste(correlation, "상관관계가 없습니다.")
    } else if ((correlation >= -0.7 &&
      correlation < -0.3) ||
      (correlation > 0.3 && correlation <= 0.7)) {
      correlation <- paste(correlation, "약한 상관관계가 있습니다.")
    } else {
      correlation <- paste(correlation, "강한 상관관계가 있습니다.")
    }

    return(list(
      scatter_plot = scatter_plot,
      graph = graph,
      correlation = correlation
    ))
  }
